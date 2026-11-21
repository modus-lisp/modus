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
//
// Coordinates are a right-handed world, y up, as the GPU's are: what a game draws where is heard there.

import AVFoundation

private let spatialEngine = AVAudioEngine()
private let environment = AVAudioEnvironmentNode()
private var spatialStarted = false
private var spatialSounds: [UInt: AVAudioPCMBuffer] = [:]
private var spatialVoices: [UInt: AVAudioPlayerNode] = [:]

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
    if #available(macOS 15.0, iOS 18.0, tvOS 18.0, *) {
        environment.isListenerHeadTrackingEnabled = true
    }
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
    environment.listenerVectorOrientation = AVAudio3DVectorOrientation(
        forward: AVAudio3DVector(x: f[3], y: f[4], z: f[5]), up: AVAudio3DVector(x: f[6], y: f[7], z: f[8]))
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

func installSpatial() {
    register("spatial_start", spatialStart)
    register("spatial_sound", spatialSound)
    register("spatial_voice", spatialVoice)
    register("spatial_place", spatialPlace)
    register("spatial_listener", spatialListener)
    register("spatial_stop", spatialStop)
    register("spatial_volume", spatialVolume)
}
