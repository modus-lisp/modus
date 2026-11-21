// Microphone.swift — the microphone's own sound, for Lisp: mono signed 16-bit samples at the rate Lisp asks for,
// the speaker's format (pseudo-syscall 1011), into a ring that Lisp drains each frame.  The same on the phone and on
// the Mac.  Lisp's side is vessel's microphone.lisp.
//
// POLLED, as the ear is: start returns at once, and the state says what is happening now.
//
//   mic_start(rate)        ask for the microphone, then capture at RATE Hz (0: 48000); 1
//   mic_state()            0 off, 1 asking, 2 capturing, 3 not allowed, 4 failed
//   mic_read(buf, cap)     the oldest samples waiting, at most CAP, into BUF -- element 0 of a (SIGNED-BYTE 16)
//                          array as modus stores it, a fixnum (value<<1) to an 8-byte word; how many
//   mic_waiting()          samples waiting
//   mic_dropped()          samples lost since the start: the ring was full (Lisp read too slowly)
//   mic_level()            the input level now, in millionths of full scale
//   mic_stop()

import AVFoundation

private enum MicState: UInt { case off = 0, asking, capturing, denied, failed }

private final class Microphone {
    var state = MicState.off
    var engine: AVAudioEngine?
    var level = 0.0

    // THE RING, written on the audio thread and read on Lisp's: four seconds
    let lock = NSLock()
    var ring = [Int16]()
    var head = 0                  // the next to read
    var count = 0                 // how many are waiting
    var dropped = 0

    func put(_ p: UnsafePointer<Int16>, _ n: Int) {
        lock.lock()
        defer { lock.unlock() }
        let cap = ring.count
        guard cap > 0 else { return }
        for i in 0..<n {
            if count == cap { head = (head + 1) % cap; count -= 1; dropped += 1 }   // the oldest gives way
            ring[(head + count) % cap] = p[i]
            count += 1
        }
    }

    /// Into OUT as modus stores a (SIGNED-BYTE 16) array: a simple-vector of fixnums, one 8-byte word each, the
    /// value shifted left one (as the speaker, pseudo-syscall 1011, reads them).
    func take(_ out: UnsafeMutablePointer<Int64>, _ max: Int) -> Int {
        lock.lock()
        defer { lock.unlock() }
        let n = min(max, count)
        let cap = ring.count
        for i in 0..<n { out[i] = Int64(ring[(head + i) % cap]) << 1 }
        if cap > 0 { head = (head + n) % cap }
        count -= n
        return n
    }

    func start(_ rate: Double) {
        if state == .asking || state == .capturing { return }
        state = .asking
        AVAudioApplication.requestRecordPermission { granted in
            DispatchQueue.main.async {
                guard granted else { self.state = .denied; return }
                self.run(rate)
            }
        }
    }

    private func run(_ rate: Double) {
        #if os(iOS)
        let session = AVAudioSession.sharedInstance()
        try? session.setCategory(.playAndRecord, mode: .default,
                                 options: [.defaultToSpeaker, .mixWithOthers, .allowBluetoothA2DP])
        try? session.setActive(true)
        #endif
        lock.lock()
        ring = [Int16](repeating: 0, count: Int(rate) * 4)
        head = 0; count = 0; dropped = 0
        lock.unlock()

        let engine = AVAudioEngine()
        let input = engine.inputNode
        let from = input.outputFormat(forBus: 0)
        guard from.sampleRate > 0,
              let to = AVAudioFormat(commonFormat: .pcmFormatInt16, sampleRate: rate, channels: 1, interleaved: true),
              let converter = AVAudioConverter(from: from, to: to) else {
            NSLog("modus mic: no input to capture (\(from))")
            state = .failed
            return
        }
        converter.downmix = true                          // every channel, mixed down to one
        input.installTap(onBus: 0, bufferSize: 1024, format: from) { buffer, _ in
            if let samples = buffer.floatChannelData?[0], buffer.frameLength > 0 {
                var sum = 0.0
                for i in 0..<Int(buffer.frameLength) { sum += Double(samples[i] * samples[i]) }
                let rms = (sum / Double(buffer.frameLength)).squareRoot()
                DispatchQueue.main.async { self.level = rms }
            }
            let room = AVAudioFrameCount(Double(buffer.frameLength) * rate / from.sampleRate) + 64
            guard let out = AVAudioPCMBuffer(pcmFormat: to, frameCapacity: room) else { return }
            var given = false
            var error: NSError?
            converter.convert(to: out, error: &error) { _, status in
                if given { status.pointee = .noDataNow; return nil }
                given = true
                status.pointee = .haveData
                return buffer
            }
            if error == nil, let p = out.int16ChannelData?[0], out.frameLength > 0 {
                self.put(p, Int(out.frameLength))
            }
        }
        do {
            engine.prepare()
            try engine.start()
        } catch {
            NSLog("modus mic: the microphone does not start: \(error)")
            input.removeTap(onBus: 0)
            state = .failed
            return
        }
        self.engine = engine
        state = .capturing
    }

    func stop() {
        if let engine {
            engine.stop()
            engine.inputNode.removeTap(onBus: 0)
        }
        engine = nil
        level = 0
        if state != .denied { state = .off }
    }
}

private let microphone = Microphone()

let micStart: Call = { rate, _, _, _, _, _ in
    microphone.start(rate == 0 ? 48000 : Double(rate))
    return 1
}
let micState: Call = { _, _, _, _, _, _ in microphone.state.rawValue }
let micRead: Call = { buf, cap, _, _, _, _ in
    guard let out = UnsafeMutablePointer<Int64>(bitPattern: buf) else { return 0 }
    return UInt(microphone.take(out, Int(cap)))
}
let micWaiting: Call = { _, _, _, _, _, _ in
    microphone.lock.lock()
    defer { microphone.lock.unlock() }
    return UInt(microphone.count)
}
let micDropped: Call = { _, _, _, _, _, _ in
    microphone.lock.lock()
    defer { microphone.lock.unlock() }
    return UInt(microphone.dropped)
}
let micLevel: Call = { _, _, _, _, _, _ in UInt(min(1.0, max(0.0, microphone.level)) * 1_000_000) }
let micStop: Call = { _, _, _, _, _, _ in microphone.stop(); return 1 }

func installMicrophone() {
    register("mic_start", micStart)
    register("mic_state", micState)
    register("mic_read", micRead)
    register("mic_waiting", micWaiting)
    register("mic_dropped", micDropped)
    register("mic_level", micLevel)
    register("mic_stop", micStop)
}
