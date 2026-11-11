// Speech.swift — the platform's voice and ear, for Lisp: AVSpeechSynthesizer says text, SFSpeechRecognizer
// transcribes the microphone or a file.  Lisp's side is vessel's speech.lisp, which puts them behind glass's
// voice and ear (the same Speak and Listen apps a desktop runs over chord and stave).
//
// Everything is POLLED, as a desk app polls its state each frame: say and listen return at once, and the state
// calls answer what is happening now.
//
//   speech_say(text, len)                 queue TEXT; 1
//   speech_hush()                         stop at once
//   speech_speaking()                     1 while anything is being said
//   speech_to_file(text, len, path, len)  say TEXT into a 16-bit WAV at PATH, not aloud; a handle
//   speech_file_done(h)                   0 writing, 1 written, -1 failed
//   ear_start()                           ask for the microphone and recognition, then listen; 1
//   ear_file(path, len)                   transcribe the audio file at PATH into the transcript
//   ear_stop()                            stop listening, keeping the transcript
//   ear_clear()                           forget the transcript
//   ear_state()                           0 idle, 1 asking, 2 listening, 3 denied, 4 failed, 5 transcribing a file
//   ear_text(buf, cap)                    the transcript: what was finished, then what is being heard (bytes)
//   ear_partial(buf, cap)                 only what is being heard and may still change (bytes)
//   ear_level()                           the input level, in millionths of full scale

import AVFoundation
import Speech

private let synthesizer = AVSpeechSynthesizer()

private func say(_ text: String) -> AVSpeechUtterance {
    let u = AVSpeechUtterance(string: text)
    u.voice = AVSpeechSynthesisVoice(language: "en-US")
    return u
}

let speechSay: Call = { p, n, _, _, _, _ in
    // the speaker, as the media player has it: playback, through the silent switch
    try? AVAudioSession.sharedInstance().setCategory(.playAndRecord, mode: .default,
                                                     options: [.defaultToSpeaker, .mixWithOthers])
    try? AVAudioSession.sharedInstance().setActive(true)
    synthesizer.speak(say(text(p, n)))
    return 1
}

let speechHush: Call = { _, _, _, _, _, _ in
    synthesizer.stopSpeaking(at: .immediate)
    return 1
}

let speechSpeaking: Call = { _, _, _, _, _, _ in synthesizer.isSpeaking ? 1 : 0 }

/// Files being written, by handle: 0 writing, 1 written, -1 failed.  Each write has a synthesizer of its own (one
/// that writes cannot also speak aloud), kept until it is done.
private var fileStates: [UInt: Int] = [:]
private var fileWriters: [UInt: AVSpeechSynthesizer] = [:]

let speechToFile: Call = { p, n, pp, pn, _, _ in
    let handle = nextHandle
    nextHandle += 1
    let url = URL(fileURLWithPath: text(pp, pn))
    let writer = AVSpeechSynthesizer()
    fileWriters[handle] = writer
    fileStates[handle] = 0
    var file: AVAudioFile?
    writer.write(say(text(p, n))) { buffer in
        guard let pcm = buffer as? AVAudioPCMBuffer else { return }
        if pcm.frameLength == 0 {                     // the end
            DispatchQueue.main.async {
                fileStates[handle] = file == nil ? -1 : 1
                file = nil
                fileWriters[handle] = nil
            }
            return
        }
        do {
            if file == nil {
                file = try AVAudioFile(forWriting: url, settings: pcm.format.settings,
                                       commonFormat: pcm.format.commonFormat, interleaved: pcm.format.isInterleaved)
            }
            try file?.write(from: pcm)
        } catch {
            NSLog("modus speech: writing \(url.path): \(error)")
            DispatchQueue.main.async { fileStates[handle] = -1 }
        }
    }
    return handle
}

let speechFileDone: Call = { h, _, _, _, _, _ in UInt(bitPattern: fileStates[h] ?? -1) }

// ---- the ear ------------------------------------------------------------------------------------------------

private enum EarState: UInt { case idle = 0, asking, listening, denied, failed, file }

private final class Ear {
    var state = EarState.idle
    var finished = ""                         // utterances the recognizer has finished, in order
    var partial = ""                          // what it is hearing now, and may revise
    var level = 0.0
    let recognizer = SFSpeechRecognizer(locale: Locale(identifier: "en-US"))
    let engine = AVAudioEngine()
    var request: SFSpeechAudioBufferRecognitionRequest?
    var task: SFSpeechRecognitionTask?

    func commit() {
        let p = partial.trimmingCharacters(in: .whitespaces)
        if !p.isEmpty { finished += (finished.isEmpty ? "" : " ") + p }
        partial = ""
    }

    /// A result: the partial is replaced each time; a final one is committed.
    func take(_ result: SFSpeechRecognitionResult?, _ error: Error?) {
        if let result {
            partial = result.bestTranscription.formattedString
            if result.isFinal { commit() }
        }
        if error != nil || (result?.isFinal ?? false) {
            if state == .file { state = .idle }
            if error != nil, partial.isEmpty, state == .listening { state = .failed }
        }
    }

    func authorize(then go: @escaping () -> Void) {
        state = .asking
        SFSpeechRecognizer.requestAuthorization { status in
            DispatchQueue.main.async {
                guard status == .authorized else { self.state = .denied; return }
                go()
            }
        }
    }

    func listen() {
        authorize {
            AVAudioApplication.requestRecordPermission { granted in
                DispatchQueue.main.async {
                    guard granted else { self.state = .denied; return }
                    self.startEngine()
                }
            }
        }
    }

    private func startEngine() {
        let session = AVAudioSession.sharedInstance()
        try? session.setCategory(.playAndRecord, mode: .measurement, options: [.defaultToSpeaker, .mixWithOthers])
        try? session.setActive(true)
        let request = SFSpeechAudioBufferRecognitionRequest()
        request.shouldReportPartialResults = true
        request.requiresOnDeviceRecognition = false
        self.request = request
        let input = engine.inputNode
        let format = input.outputFormat(forBus: 0)
        input.removeTap(onBus: 0)
        input.installTap(onBus: 0, bufferSize: 1024, format: format) { buffer, _ in
            request.append(buffer)
            if let samples = buffer.floatChannelData?[0] {
                var sum = 0.0
                for i in 0..<Int(buffer.frameLength) { sum += Double(samples[i] * samples[i]) }
                let rms = buffer.frameLength > 0 ? (sum / Double(buffer.frameLength)).squareRoot() : 0
                DispatchQueue.main.async { self.level = rms }
            }
        }
        do {
            engine.prepare()
            try engine.start()
        } catch {
            NSLog("modus ear: the microphone does not start: \(error)")
            state = .failed
            return
        }
        state = .listening
        task = recognizer?.recognitionTask(with: request) { result, error in
            DispatchQueue.main.async { self.take(result, error) }
        }
    }

    func stop() {
        if engine.isRunning {
            engine.stop()
            engine.inputNode.removeTap(onBus: 0)
        }
        request?.endAudio()
        task?.finish()
        request = nil
        task = nil
        commit()
        level = 0
        if state != .denied { state = .idle }
    }

    func transcribe(_ url: URL) {
        authorize {
            let request = SFSpeechURLRecognitionRequest(url: url)
            request.shouldReportPartialResults = false
            request.requiresOnDeviceRecognition = false
            self.state = .file
            self.task = self.recognizer?.recognitionTask(with: request) { result, error in
                DispatchQueue.main.async {
                    // no transcript and an error: the recognizer could not do it (a Simulator has none that works)
                    if let error, result == nil {
                        NSLog("modus ear: \(url.lastPathComponent): \(error)")
                        if self.state == .file { self.state = .failed }
                        return
                    }
                    self.take(result, error)
                }
            }
        }
    }
}

private let ear = Ear()

private func copyOut(_ s: String, _ buf: UInt, _ cap: UInt) -> UInt {
    let bytes = Array(s.utf8)
    if !bytes.isEmpty, bytes.count <= Int(cap), let out = UnsafeMutableRawPointer(bitPattern: buf) {
        bytes.withUnsafeBytes { out.copyMemory(from: $0.baseAddress!, byteCount: bytes.count) }
    }
    return UInt(bytes.count)
}

let earStart: Call = { _, _, _, _, _, _ in
    if ear.state != .listening && ear.state != .asking { ear.listen() }
    return 1
}
let earFile: Call = { p, n, _, _, _, _ in
    ear.transcribe(URL(fileURLWithPath: text(p, n)))
    return 1
}
let earStop: Call = { _, _, _, _, _, _ in ear.stop(); return 1 }
let earClear: Call = { _, _, _, _, _, _ in ear.finished = ""; ear.partial = ""; return 1 }
let earState: Call = { _, _, _, _, _, _ in ear.state.rawValue }
let earText: Call = { buf, cap, _, _, _, _ in
    let p = ear.partial.trimmingCharacters(in: .whitespaces)
    return copyOut(p.isEmpty ? ear.finished : (ear.finished.isEmpty ? p : ear.finished + " " + p), buf, cap)
}
let earPartial: Call = { buf, cap, _, _, _, _ in copyOut(ear.partial, buf, cap) }
let earLevel: Call = { _, _, _, _, _, _ in UInt(min(1.0, max(0.0, ear.level)) * 1_000_000) }

func installSpeech() {
    register("speech_say", speechSay)
    register("speech_hush", speechHush)
    register("speech_speaking", speechSpeaking)
    register("speech_to_file", speechToFile)
    register("speech_file_done", speechFileDone)
    register("ear_start", earStart)
    register("ear_file", earFile)
    register("ear_stop", earStop)
    register("ear_clear", earClear)
    register("ear_state", earState)
    register("ear_text", earText)
    register("ear_partial", earPartial)
    register("ear_level", earLevel)
}
