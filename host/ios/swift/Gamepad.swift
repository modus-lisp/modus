// Gamepad.swift — game controllers for Lisp, through Apple's GameController framework: the same on the Mac, the
// phone and the TV (an Xbox, PlayStation or Switch Pro controller, an MFi pad, an Apple TV remote's extended
// profile).  Lisp's side is vessel's gamepad.lisp.
//
// POLLED, as a game reads its input: once a frame, a controller's whole state into a vector of floats.
//
//   pad_count()                  how many controllers are connected
//   pad_name(index, buf, cap)    the INDEXth's name, UTF-8 (its length, copied when it fits)
//   pad_state(index, buf, n)     its state into BUF, N single-floats (at most PAD_FLOATS, below): 0 when there is
//                                no such controller, else how many were written
//   pad_light(index, rgb)        its light's colour (a PlayStation controller's light bar), where it has one
//   pad_rumble(index, strength, ms)   a buzz, STRENGTH 0..1000, for MS milliseconds, where it can
//
// THE STATE, in order (axes -1..1 with up and right positive, triggers and buttons 0..1, pressure where there is):
//   0 left x   1 left y   2 right x   3 right y   4 left trigger   5 right trigger   6 d-pad x   7 d-pad y
//   8 A (south)   9 B (east)   10 X (west)   11 Y (north)   12 left shoulder   13 right shoulder
//   14 left stick click   15 right stick click   16 menu (start)   17 options (select)   18 home

import Foundation
import GameController
import CoreHaptics

private let padFloats = 19

private func pads() -> [GCController] {
    GCController.controllers().filter { $0.extendedGamepad != nil || $0.microGamepad != nil }
}

private func value(_ b: GCControllerButtonInput?) -> Float { b?.value ?? 0 }

let padCount: Call = { _, _, _, _, _, _ in UInt(pads().count) }

let padName: Call = { index, buf, cap, _, _, _ in
    let all = pads()
    guard Int(index) < all.count else { return 0 }
    let name = all[Int(index)].vendorName ?? "controller"
    let bytes = Array(name.utf8)
    if !bytes.isEmpty, bytes.count <= Int(cap), let out = UnsafeMutableRawPointer(bitPattern: buf) {
        bytes.withUnsafeBytes { out.copyMemory(from: $0.baseAddress!, byteCount: bytes.count) }
    }
    return UInt(bytes.count)
}

let padState: Call = { index, buf, n, _, _, _ in
    let all = pads()
    guard Int(index) < all.count, let out = UnsafeMutablePointer<Float>(bitPattern: buf) else { return 0 }
    var s = [Float](repeating: 0, count: padFloats)
    let pad = all[Int(index)]
    if let g = pad.extendedGamepad {
        s[0] = g.leftThumbstick.xAxis.value;  s[1] = g.leftThumbstick.yAxis.value
        s[2] = g.rightThumbstick.xAxis.value; s[3] = g.rightThumbstick.yAxis.value
        s[4] = g.leftTrigger.value;           s[5] = g.rightTrigger.value
        s[6] = g.dpad.xAxis.value;            s[7] = g.dpad.yAxis.value
        s[8] = g.buttonA.value;  s[9] = g.buttonB.value;  s[10] = g.buttonX.value;  s[11] = g.buttonY.value
        s[12] = g.leftShoulder.value;         s[13] = g.rightShoulder.value
        s[14] = value(g.leftThumbstickButton); s[15] = value(g.rightThumbstickButton)
        s[16] = g.buttonMenu.value;           s[17] = value(g.buttonOptions)
        s[18] = value(g.buttonHome)
    } else if let m = pad.microGamepad {             // an Apple TV remote: its touch surface is the left stick
        s[0] = m.dpad.xAxis.value; s[1] = m.dpad.yAxis.value
        s[8] = m.buttonA.value;    s[10] = m.buttonX.value
        s[16] = m.buttonMenu.value
    }
    let count = min(Int(n), padFloats)
    for i in 0..<count { out[i] = s[i] }
    return UInt(count)
}

let padLight: Call = { index, rgb, _, _, _, _ in
    let all = pads()
    guard Int(index) < all.count, let light = all[Int(index)].light else { return 0 }
    light.color = GCColor(red: Float((rgb >> 16) & 255) / 255, green: Float((rgb >> 8) & 255) / 255,
                          blue: Float(rgb & 255) / 255)
    return 1
}

/// A haptic engine a controller has, made once and kept.
private var rumblers: [ObjectIdentifier: CHHapticEngine] = [:]

let padRumble: Call = { index, strength, ms, _, _, _ in
    let all = pads()
    guard Int(index) < all.count, let haptics = all[Int(index)].haptics else { return 0 }
    let key = ObjectIdentifier(all[Int(index)])
    if rumblers[key] == nil, let engine = haptics.createEngine(withLocality: .default) {
        try? engine.start()
        rumblers[key] = engine
    }
    guard let engine = rumblers[key] else { return 0 }
    let event = CHHapticEvent(eventType: .hapticContinuous,
                              parameters: [CHHapticEventParameter(parameterID: .hapticIntensity,
                                                                  value: Float(min(strength, 1000)) / 1000)],
                              relativeTime: 0, duration: Double(ms) / 1000)
    guard let pattern = try? CHHapticPattern(events: [event], parameters: []),
          let player = try? engine.makePlayer(with: pattern) else { return 0 }
    try? player.start(atTime: CHHapticTimeImmediate)
    return 1
}

func installGamepad() {
    // a controller paired in the system's Bluetooth settings is simply there; its state is read on the main queue,
    // where Lisp's calls are
    #if os(macOS)
    GCController.shouldMonitorBackgroundEvents = false
    #endif
    register("pad_count", padCount)
    register("pad_name", padName)
    register("pad_state", padState)
    register("pad_light", padLight)
    register("pad_rumble", padRumble)
}
