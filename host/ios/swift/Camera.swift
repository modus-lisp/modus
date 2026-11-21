// Camera.swift — the camera, for Lisp: a live preview, the frames themselves for the image to read, a photo, and a
// video with sound.  AVFoundation, on the phone and on the Mac (the macOS window host builds this file too; the two
// differ only in which cameras there are and in what a preview view is).  Lisp's side is vessel's camera.lisp
// (package KILN-CAMERA).
//
//   cam_devices(buf, cap)             the cameras' names, one per line, UTF-8 into buf; the length (copied if it fits)
//   cam_open(index)                   camera INDEX (0: the first) -- a handle.  The first open asks the person; the
//                                     answer is a CAMERA event (cam 1|0), and the frames start when it is yes
//   cam_preview_new(cam, x, y, w, h)  a live preview of it, a UI handle (ui_remove takes it away)
//   cam_frame(cam, buf, cap)          the latest frame, BGRA rows (4 bytes a pixel, no padding), into buf if it
//                                     fits; its length -- 0 when no frame has come since the last one read
//   cam_frame_size(cam)               that frame's width | height << 16
//   cam_photo(cam, path, len)         a still, written as JPEG to PATH; then a PHOTO event (cam path), or (cam "")
//   cam_record(cam, path, len)        start recording video, and sound if the microphone may be used, to PATH (.mov);
//                                     a RECORDING event (cam path) when it has started (a second or so later)
//   cam_stop(cam)                     stop it; then a RECORDED event (cam path), or (cam "") if it failed
//   cam_close(cam)                    the camera off; its preview goes dark
//
// A frame is copied out of the capture queue under a lock, the latest only: the image reads at its own pace and
// misses frames rather than queueing them.

#if os(macOS)
import AppKit
#else
import UIKit
#endif
import AVFoundation

private var cameras: [UInt: Camera] = [:]

private func allCameras() -> [AVCaptureDevice] {
    #if os(macOS)
    let types: [AVCaptureDevice.DeviceType] = [.builtInWideAngleCamera, .external, .continuityCamera]
    #else
    let types: [AVCaptureDevice.DeviceType] = [.builtInWideAngleCamera, .builtInUltraWideCamera, .builtInTelephotoCamera]
    #endif
    return AVCaptureDevice.DiscoverySession(deviceTypes: types, mediaType: .video, position: .unspecified).devices
}

private func onMain(_ f: @escaping () -> Void) {
    if Thread.isMainThread { f() } else { DispatchQueue.main.async(execute: f) }
}

final class Camera: NSObject, AVCaptureVideoDataOutputSampleBufferDelegate, AVCapturePhotoCaptureDelegate,
                    AVCaptureFileOutputRecordingDelegate {
    let handle: UInt
    let device: AVCaptureDevice
    let session = AVCaptureSession()
    private let frames = AVCaptureVideoDataOutput()
    private let photos = AVCapturePhotoOutput()
    private let movie = AVCaptureMovieFileOutput()
    private let queue = DispatchQueue(label: "modus.camera")

    // the latest frame, and whether the image has read it yet
    private let lock = NSLock()
    private var frame: [UInt8] = []
    private var width = 0, height = 0
    private var fresh = false

    private var photoPath: String?
    private var audioAdded = false

    init(handle: UInt, device: AVCaptureDevice) {
        self.handle = handle
        self.device = device
        super.init()
    }

    // The session, once the person has said yes: the camera in, frames, photos and a movie out.  On its own queue:
    // startRunning blocks until the camera is up.
    func start() {
        queue.async { [self] in
            session.beginConfiguration()
            if session.canSetSessionPreset(.hd1280x720) { session.sessionPreset = .hd1280x720 }
            if let input = try? AVCaptureDeviceInput(device: device), session.canAddInput(input) {
                session.addInput(input)
            }
            frames.videoSettings = [kCVPixelBufferPixelFormatTypeKey as String: kCVPixelFormatType_32BGRA]
            frames.alwaysDiscardsLateVideoFrames = true
            frames.setSampleBufferDelegate(self, queue: queue)
            if session.canAddOutput(frames) { session.addOutput(frames) }
            if session.canAddOutput(photos) { session.addOutput(photos) }
            #if os(macOS)
            // the phone cannot record a movie and hand out frames at once; it adds the movie output to record
            if session.canAddOutput(movie) { session.addOutput(movie) }
            // and the movie's sound, now, if the microphone may already be used: added at the first recording, it
            // would cost that recording its first seconds
            if AVCaptureDevice.authorizationStatus(for: .audio) == .authorized,
               let mic = AVCaptureDevice.default(for: .audio).flatMap({ try? AVCaptureDeviceInput(device: $0) }),
               session.canAddInput(mic) {
                session.addInput(mic)
                audioAdded = true
            }
            #endif
            session.commitConfiguration()
            session.startRunning()
        }
    }

    func stop() {
        queue.async { [self] in
            if movie.isRecording { movie.stopRecording() }
            session.stopRunning()
        }
    }

    // MARK: frames

    func captureOutput(_ output: AVCaptureOutput, didOutput sample: CMSampleBuffer, from connection: AVCaptureConnection) {
        guard let pixels = CMSampleBufferGetImageBuffer(sample) else { return }
        CVPixelBufferLockBaseAddress(pixels, .readOnly)
        defer { CVPixelBufferUnlockBaseAddress(pixels, .readOnly) }
        guard let base = CVPixelBufferGetBaseAddress(pixels) else { return }
        let w = CVPixelBufferGetWidth(pixels), h = CVPixelBufferGetHeight(pixels)
        let rowBytes = CVPixelBufferGetBytesPerRow(pixels)
        lock.lock()
        if frame.count != w * h * 4 { frame = [UInt8](repeating: 0, count: w * h * 4) }
        frame.withUnsafeMutableBytes { out in
            for y in 0..<h {
                memcpy(out.baseAddress! + y * w * 4, base + y * rowBytes, w * 4)
            }
        }
        width = w
        height = h
        fresh = true
        lock.unlock()
    }

    /// The latest frame into BUF if it fits and it is new; its length, or 0.
    func latest(_ buf: UInt, _ cap: UInt) -> UInt {
        lock.lock()
        defer { lock.unlock() }
        guard fresh, !frame.isEmpty else { return 0 }
        if frame.count <= Int(cap), let out = UnsafeMutableRawPointer(bitPattern: buf) {
            frame.withUnsafeBytes { out.copyMemory(from: $0.baseAddress!, byteCount: frame.count) }
            fresh = false
        }
        return UInt(frame.count)
    }

    func size() -> UInt {
        lock.lock()
        defer { lock.unlock() }
        return UInt(width) | (UInt(height) << 16)
    }

    // MARK: photos

    func takePhoto(_ path: String) {
        queue.async { [self] in
            guard photoPath == nil, session.isRunning else {
                onMain { send("PHOTO", [.int(Int(self.handle)), .string("")]) }
                return
            }
            photoPath = path
            photos.capturePhoto(with: AVCapturePhotoSettings(format: [AVVideoCodecKey: AVVideoCodecType.jpeg]),
                                delegate: self)
        }
    }

    func photoOutput(_ output: AVCapturePhotoOutput, didFinishProcessingPhoto photo: AVCapturePhoto, error: Error?) {
        let path = photoPath ?? ""
        photoPath = nil
        var written = ""
        if error == nil, let data = photo.fileDataRepresentation(),
           (try? data.write(to: URL(fileURLWithPath: path))) != nil {
            written = path
        }
        onMain { send("PHOTO", [.int(Int(self.handle)), .string(written)]) }
    }

    // MARK: video

    func record(_ path: String) {
        let go = { [self] in
            queue.async { [self] in
                // THE SESSION IS CHANGED only when it must be (sound the first time; the phone's outputs): a
                // reconfiguration costs the movie's start a second or two
                let mic = audioAdded || AVCaptureDevice.authorizationStatus(for: .audio) != .authorized
                    ? nil : AVCaptureDevice.default(for: .audio).flatMap { try? AVCaptureDeviceInput(device: $0) }
                #if os(iOS)
                let swap = true
                #else
                let swap = false
                #endif
                if mic != nil || swap {
                    session.beginConfiguration()
                    // sound too, when the microphone may be used
                    if let mic, session.canAddInput(mic) {
                        session.addInput(mic)
                        audioAdded = true
                    }
                    #if os(iOS)
                    session.removeOutput(frames)
                    if session.canAddOutput(movie) { session.addOutput(movie) }
                    #endif
                    session.commitConfiguration()
                }
                let url = URL(fileURLWithPath: path)
                try? FileManager.default.removeItem(at: url)
                movie.startRecording(to: url, recordingDelegate: self)
            }
        }
        if AVCaptureDevice.authorizationStatus(for: .audio) == .notDetermined {
            AVCaptureDevice.requestAccess(for: .audio) { _ in go() }
        } else {
            go()
        }
    }

    func stopRecording() {
        queue.async { [self] in
            if movie.isRecording { movie.stopRecording() }
        }
    }

    func fileOutput(_ output: AVCaptureFileOutput, didStartRecordingTo url: URL, from connections: [AVCaptureConnection]) {
        onMain { send("RECORDING", [.int(Int(self.handle)), .string(url.path)]) }
    }

    func fileOutput(_ output: AVCaptureFileOutput, didFinishRecordingTo url: URL, from connections: [AVCaptureConnection],
                    error: Error?) {
        // an error can still leave a playable file (the disk filled, say): it says so in its user info
        let ok = error == nil
            || ((error as NSError?)?.userInfo[AVErrorRecordingSuccessfullyFinishedKey] as? Bool ?? false)
        #if os(iOS)
        queue.async { [self] in
            session.beginConfiguration()
            session.removeOutput(movie)
            if session.canAddOutput(frames) { session.addOutput(frames) }
            session.commitConfiguration()
        }
        #endif
        onMain { send("RECORDED", [.int(Int(self.handle)), .string(ok ? url.path : "")]) }
    }
}

// MARK: - the preview

#if os(macOS)
final class CameraPreview: NSView {
    init(frame: CGRect, session: AVCaptureSession) {
        super.init(frame: frame)
        let preview = AVCaptureVideoPreviewLayer(session: session)
        preview.videoGravity = .resizeAspectFill
        layer = preview                      // a layer-hosting view: the preview layer is its layer
        wantsLayer = true
    }
    required init?(coder: NSCoder) { fatalError("not from a nib") }
}
#else
final class CameraPreview: UIView {
    override class var layerClass: AnyClass { AVCaptureVideoPreviewLayer.self }
    init(frame: CGRect, session: AVCaptureSession) {
        super.init(frame: frame)
        let preview = layer as! AVCaptureVideoPreviewLayer
        preview.session = session
        preview.videoGravity = .resizeAspectFill
    }
    required init?(coder: NSCoder) { fatalError("not from a nib") }
}
#endif

// MARK: - the calls

let camDevices: Call = { buf, cap, _, _, _, _ in
    let bytes = Array(allCameras().map(\.localizedName).joined(separator: "\n").utf8)
    if !bytes.isEmpty, bytes.count <= Int(cap), let out = UnsafeMutableRawPointer(bitPattern: buf) {
        bytes.withUnsafeBytes { out.copyMemory(from: $0.baseAddress!, byteCount: bytes.count) }
    }
    return UInt(bytes.count)
}

let camOpen: Call = { index, _, _, _, _, _ in
    let all = allCameras()
    guard Int(index) < all.count else { return 0 }
    let handle = nextHandle                 // the components' own counter, as GPU.swift's handles are
    nextHandle += 1
    let camera = Camera(handle: handle, device: all[Int(index)])
    cameras[handle] = camera
    switch AVCaptureDevice.authorizationStatus(for: .video) {
    case .authorized:
        camera.start()
        send("CAMERA", [.int(Int(handle)), .int(1)])
    case .notDetermined:
        AVCaptureDevice.requestAccess(for: .video) { allowed in
            if allowed { camera.start() }
            onMain { send("CAMERA", [.int(Int(handle)), .int(allowed ? 1 : 0)]) }
        }
    default:
        send("CAMERA", [.int(Int(handle)), .int(0)])
    }
    return handle
}

let camPreviewNew: Call = { cam, x, y, w, h, _ in
    guard let camera = cameras[cam] else { return 0 }
    return adopt(CameraPreview(frame: rect(x, y, w, h), session: camera.session))
}

let camFrame: Call = { cam, buf, cap, _, _, _ in
    cameras[cam]?.latest(buf, cap) ?? 0
}

let camFrameSize: Call = { cam, _, _, _, _, _ in
    cameras[cam]?.size() ?? 0
}

let camPhoto: Call = { cam, p, n, _, _, _ in
    guard let camera = cameras[cam] else { return 0 }
    camera.takePhoto(text(p, n))
    return 1
}

let camRecord: Call = { cam, p, n, _, _, _ in
    guard let camera = cameras[cam] else { return 0 }
    camera.record(text(p, n))
    return 1
}

let camStop: Call = { cam, _, _, _, _, _ in
    cameras[cam]?.stopRecording()
    return 0
}

let camClose: Call = { cam, _, _, _, _, _ in
    cameras[cam]?.stop()
    cameras[cam] = nil
    return 0
}

func installCamera() {
    register("cam_devices", camDevices)
    register("cam_open", camOpen)
    register("cam_preview_new", camPreviewNew)
    register("cam_frame", camFrame)
    register("cam_frame_size", camFrameSize)
    register("cam_photo", camPhoto)
    register("cam_record", camRecord)
    register("cam_stop", camStop)
    register("cam_close", camClose)
}
