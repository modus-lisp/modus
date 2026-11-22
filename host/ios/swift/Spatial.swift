// Spatial.swift — sound placed in three dimensions, for Lisp: Apple's binaural renderer (AVAudioEngine's environment
// node), so a source is heard in front, behind, above or below -- and, with AirPods (macOS 15, iOS 18, tvOS 18),
// stays put as the head turns.  The same on the Mac, the phone and the TV.  Lisp's side is vessel's sound.lisp
// (SPATIAL-...).  The sounds are Lisp's own samples; only the placing and the rendering are Apple's.
//
//   spatial_start()                    the engine and its listener; 1, or 0 when there is no output
//   spatial_sound(samples, frames, rate)   a sound: FRAMES mono single-floats at RATE, copied; a handle
//   spatial_voice(sound, loop)         a voice playing SOUND (looping when LOOP is 1), placed at the listener; a handle
//   spatial_place(voice, xyzg)         the voice at x y z (metres), at gain g: four single-floats at XYZG
//   spatial_listener(nine)             the listener: position, forward and up vectors, nine single-floats
//   spatial_stop(voice)
//   spatial_volume(permille)           everything's loudness, 0..1000
//   spatial_head(on)                   follow the listener's head (AirPods) or not; off until asked
//   spatial_head_permission()          0 not asked yet, 1 allowed, 2 refused, 3 none needed (a Mac): an app can say
//                                      what the system's question is for before it is asked
//
// Coordinates are a right-handed world, y up, as the GPU's are: what a game draws where is heard there.

import AVFoundation
#if os(iOS)
import CoreMotion
#endif

private let spatialEngine = AVAudioEngine()
private let environment = AVAudioEnvironmentNode()
private var spatialStarted = false
private var spatialSounds: [UInt: AVAudioPCMBuffer] = [:]
private var spatialVoices: [UInt: AVAudioPlayerNode] = [:]

// THE HEAD, ON THE PHONE: the environment's own head tracking turns a Mac's listener, but on iOS it does not engage
// for an app's own renderer (Control Center's spatial control is offered for what the system spatializes, not for
// this).  So the AirPods' attitude is read here (CMHeadphoneMotionManager) and the listener turned by it: yaw from
// where the head was when tracking began, and pitch.  The first use asks for Motion & Fitness (Info.plist's
// NSMotionUsageDescription).
#if os(iOS)
private let headphones = CMHeadphoneMotionManager()
private var headYaw = 0.0, headPitch = 0.0
private var headYawAtStart: Double?

private func startHeadTracking() {
    guard headphones.isDeviceMotionAvailable, !headphones.isDeviceMotionActive else { return }
    headYawAtStart = nil                      // "ahead" is where the head is now
    headphones.startDeviceMotionUpdates(to: .main) { motion, error in
        guard let a = motion?.attitude else {
            if let error { NSLog("modus spatial: headphone motion: \(error)") }
            return
        }
        if headYawAtStart == nil { headYawAtStart = a.yaw }
        headYaw = a.yaw - (headYawAtStart ?? 0)
        headPitch = a.pitch
    }
}
#endif

/// FORWARD and UP turned by the head: by its yaw about UP (positive: the head turned left), then its pitch about the
/// listener's right (positive: looking up).  Identity where nothing tracks the head here.
private func turnedByHead(_ f: (Float, Float, Float), _ u: (Float, Float, Float))
    -> ((Float, Float, Float), (Float, Float, Float)) {
    #if os(iOS)
    let yaw = Float(headYaw), pitch = Float(headPitch)
    guard yaw != 0 || pitch != 0 else { return (f, u) }
    // left = up x forward
    let l = (u.1 * f.2 - u.2 * f.1, u.2 * f.0 - u.0 * f.2, u.0 * f.1 - u.1 * f.0)
    let (cy, sy) = (cos(yaw), sin(yaw))
    let f1 = (f.0 * cy + l.0 * sy, f.1 * cy + l.1 * sy, f.2 * cy + l.2 * sy)
    let (cp, sp) = (cos(pitch), sin(pitch))
    let f2 = (f1.0 * cp + u.0 * sp, f1.1 * cp + u.1 * sp, f1.2 * cp + u.2 * sp)
    let u2 = (u.0 * cp - f1.0 * sp, u.1 * cp - f1.1 * sp, u.2 * cp - f1.2 * sp)
    return (f2, u2)
    #else
    return (f, u)
    #endif
}

let spatialStart: Call = { _, _, _, _, _, _ in
    if spatialStarted { return 1 }
    spatialEngine.attach(environment)
    spatialEngine.connect(environment, to: spatialEngine.mainMixerNode, format: nil)
    // full within 2.5 m, then fading as the inverse of distance, gone by 80
    let d = environment.distanceAttenuationParameters
    d.distanceAttenuationModel = .inverse
    d.referenceDistance = 2.5
    d.maximumDistance = 80
    d.rolloffFactor = 1
    // the head followed: AirPods' motion turns the listener, so a sound stays where it is in the world
    #if os(iOS) || os(tvOS)
    try? AVAudioSession.sharedInstance().setCategory(.playback, mode: .default, options: [.mixWithOthers])
    try? AVAudioSession.sharedInstance().setActive(true)
    #endif
    do {
        try spatialEngine.start()
    } catch {
        NSLog("modus spatial: the engine does not start: \(error)")
        return 0
    }
    spatialStarted = true
    return 1
}

let spatialSound: Call = { p, frames, rate, _, _, _ in
    guard frames > 0, let src = UnsafePointer<Float>(bitPattern: p),
          let format = AVAudioFormat(standardFormatWithSampleRate: Double(rate), channels: 1),
          let buffer = AVAudioPCMBuffer(pcmFormat: format, frameCapacity: AVAudioFrameCount(frames)),
          let out = buffer.floatChannelData?[0] else { return 0 }
    buffer.frameLength = AVAudioFrameCount(frames)
    out.update(from: src, count: Int(frames))
    let handle = nextHandle
    nextHandle += 1
    spatialSounds[handle] = buffer
    return handle
}

let spatialVoice: Call = { s, loop, _, _, _, _ in
    guard spatialStarted, let buffer = spatialSounds[s] else { return 0 }
    let player = AVAudioPlayerNode()
    spatialEngine.attach(player)
    spatialEngine.connect(player, to: environment, format: buffer.format)   // mono: placed by the environment
    player.renderingAlgorithm = .auto          // binaural on headphones, spatial on AirPods, what suits the speakers
    player.sourceMode = .pointSource
    player.position = environment.listenerPosition
    player.scheduleBuffer(buffer, at: nil, options: loop != 0 ? .loops : [], completionHandler: nil)
    player.play()
    let handle = nextHandle
    nextHandle += 1
    spatialVoices[handle] = player
    return handle
}

let spatialPlace: Call = { v, p, _, _, _, _ in
    guard let player = spatialVoices[v], let f = UnsafePointer<Float>(bitPattern: p) else { return 0 }
    player.position = AVAudio3DPoint(x: f[0], y: f[1], z: f[2])
    player.volume = max(0, f[3])
    return 1
}

let spatialListener: Call = { p, _, _, _, _, _ in
    guard let f = UnsafePointer<Float>(bitPattern: p) else { return 0 }
    environment.listenerPosition = AVAudio3DPoint(x: f[0], y: f[1], z: f[2])
    let (fw, up) = turnedByHead((f[3], f[4], f[5]), (f[6], f[7], f[8]))
    environment.listenerVectorOrientation = AVAudio3DVectorOrientation(
        forward: AVAudio3DVector(x: fw.0, y: fw.1, z: fw.2), up: AVAudio3DVector(x: up.0, y: up.1, z: up.2))
    return 1
}

let spatialStop: Call = { v, _, _, _, _, _ in
    guard let player = spatialVoices.removeValue(forKey: v) else { return 0 }
    player.stop()
    spatialEngine.detach(player)
    return 1
}

let spatialVolume: Call = { permille, _, _, _, _, _ in
    environment.outputVolume = Float(min(permille, 1000)) / 1000
    return 1
}

let spatialHead: Call = { on, _, _, _, _, _ in
    #if os(iOS)
    if on != 0 {
        startHeadTracking()                   // the first time, the system asks for Motion & Fitness
    } else {
        headphones.stopDeviceMotionUpdates()
        headYaw = 0; headPitch = 0
    }
    #else
    if #available(macOS 15.0, tvOS 18.0, *) { environment.isListenerHeadTrackingEnabled = on != 0 }
    #endif
    return 1
}

let spatialHeadPermission: Call = { _, _, _, _, _, _ in
    #if os(iOS)
    switch CMHeadphoneMotionManager.authorizationStatus() {
    case .notDetermined: return 0
    case .authorized: return 1
    default: return 2
    }
    #else
    return 3
    #endif
}

func installSpatial() {
    register("spatial_head", spatialHead)
    register("spatial_head_permission", spatialHeadPermission)
    register("spatial_start", spatialStart)
    register("spatial_sound", spatialSound)
    register("spatial_voice", spatialVoice)
    register("spatial_place", spatialPlace)
    register("spatial_listener", spatialListener)
    register("spatial_stop", spatialStop)
    register("spatial_volume", spatialVolume)
}
