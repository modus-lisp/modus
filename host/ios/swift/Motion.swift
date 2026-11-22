// Motion.swift — the device's own turn, for Lisp: how the phone is held now, against how it was held when it began
// to be followed (or was last recentered).  The headphones' turn is Spatial.swift's (spatial_head_attitude); this is
// the phone's, beside it -- and on a watch, the watch's (sent on to the phone by Lisp: Link.swift).  The same names
// on the Mac, where nothing is measured.  Lisp's side is vessel's motion.lisp.
//
//   device_motion(on)          follow the device's motion (1) or stop (0); 1 if it can be followed here
//   device_attitude(nine)      the device's turn into nine floats: a 3x3 rotation, row-major, from the device's own
//                              axes (x to the right of its screen held upright, y to its top, z out of its screen)
//                              to the screen's as it was at the neutral pose (x right, y up, z out of the screen, as
//                              the GPU's are, in the interface's orientation: a phone held sideways is wide; a
//                              watch's screen does not turn);
//                              1 while it is reported, else 0
//   device_recenter()          the device's pose now is neutral
//
// No permission is asked: the device's attitude is not Motion & Fitness data.

#if os(iOS) || os(watchOS)
import CoreMotion
#if os(iOS)
import UIKit
#endif

private let motion = CMMotionManager()
// rows: the interface's axes (right, up) in the device's own -- how the screen is turned in the hand
private var screenAxes: ((Double, Double), (Double, Double)) = ((1, 0), (0, 1))
private var deviceReference: CMRotationMatrix?
private var deviceNow: CMRotationMatrix?
// like the head's: the first half second's reports settle the neutral pose
private var deviceSettleUntil = Date.distantPast

private func interfaceAxes() -> ((Double, Double), (Double, Double)) {
    #if os(watchOS)
    return ((1, 0), (0, 1))
    #else
    let scene = UIApplication.shared.connectedScenes.compactMap { $0 as? UIWindowScene }.first
    switch scene?.interfaceOrientation {
    case .portraitUpsideDown: return ((-1, 0), (0, -1))
    case .landscapeRight: return ((0, -1), (1, 0))       // the device's top to the left: the home side on the right
    case .landscapeLeft: return ((0, 1), (-1, 0))
    default: return ((1, 0), (0, 1))
    }
    #endif
}

private func startDeviceMotion() -> Bool {
    guard motion.isDeviceMotionAvailable else { return false }
    if motion.isDeviceMotionActive { return true }
    deviceReference = nil
    deviceSettleUntil = Date().addingTimeInterval(0.5)
    motion.deviceMotionUpdateInterval = 1.0 / 60
    motion.startDeviceMotionUpdates(using: .xArbitraryZVertical, to: .main) { m, error in
        guard let m else {
            if let error { NSLog("modus motion: \(error)") }
            return
        }
        let r = m.attitude.rotationMatrix
        if deviceReference == nil || Date() < deviceSettleUntil {
            deviceReference = r
            screenAxes = interfaceAxes()
        }
        deviceNow = r
    }
    return true
}

private func rows(_ m: CMRotationMatrix) -> [[Double]] {
    [[m.m11, m.m12, m.m13], [m.m21, m.m22, m.m23], [m.m31, m.m32, m.m33]]
}

let deviceMotion: Call = { on, _, _, _, _, _ in
    if on != 0 { return startDeviceMotion() ? 1 : 0 }
    motion.stopDeviceMotionUpdates()
    deviceNow = nil
    return 1
}

let deviceAttitude: Call = { p, _, _, _, _, _ in
    guard let f = UnsafeMutablePointer<Float>(bitPattern: p) else { return 0 }
    guard let now = deviceNow, let ref = deviceReference else {
        for i in 0..<9 { f[i] = i % 4 == 0 ? 1 : 0 }
        return 0
    }
    // CoreMotion's matrix takes the world's coordinates to the device's; so the device's now to its neutral pose's
    // is REF . NOW^T
    let a = rows(ref), b = rows(now)
    var m = [[Double]](repeating: [0, 0, 0], count: 3)
    for i in 0..<3 { for j in 0..<3 { m[i][j] = (0..<3).reduce(0) { $0 + a[i][$1] * b[j][$1] } } }
    // then the neutral pose's device axes to the screen's: right and up as the interface was turned, z the same
    let (rx, uy) = screenAxes
    let to = [[rx.0, rx.1, 0], [uy.0, uy.1, 0], [0, 0, 1]]
    for i in 0..<3 {
        for j in 0..<3 {
            f[i * 3 + j] = Float((0..<3).reduce(0) { $0 + to[i][$1] * m[$1][j] })
        }
    }
    return 1
}

let deviceRecenter: Call = { _, _, _, _, _, _ in
    deviceReference = nil
    deviceSettleUntil = Date().addingTimeInterval(0.5)
    return 1
}
#else
// a Mac: nothing to follow
let deviceMotion: Call = { _, _, _, _, _, _ in 0 }
let deviceAttitude: Call = { p, _, _, _, _, _ in
    if let f = UnsafeMutablePointer<Float>(bitPattern: p) { for i in 0..<9 { f[i] = i % 4 == 0 ? 1 : 0 } }
    return 0
}
let deviceRecenter: Call = { _, _, _, _, _, _ in 1 }
#endif

func installMotion() {
    register("device_motion", deviceMotion)
    register("device_attitude", deviceAttitude)
    register("device_recenter", deviceRecenter)
}
