// modus-watch.swift — the framebuffer and touch events on watchOS
// (docs/macos-hosting.md, "On the watch").
//
// The watchOS twin of host/ios/modus-ui.m: the same three C entry points the
// shim calls, so pseudo-syscalls 1001-1004 (info, fill, present, next event)
// mean the same thing on the wrist as on the phone.  watchOS has no public
// UIKit, so the view is SwiftUI: the buffer is shown as an Image, and a
// zero-distance drag gesture reports down / move / up.
//
// The shim's main() starts the image thread and then calls modus_watch_main,
// which runs this app on the main thread for the rest of the process.

import SwiftUI
import os

private let scale = 2
private var fb: UnsafeMutablePointer<UInt32>?
private var fbW = 0, fbH = 0
private let ready = DispatchSemaphore(value: 0)
private let events = OSAllocatedUnfairLock(initialState: [UInt64]())
private let presentPending = OSAllocatedUnfairLock(initialState: false)

final class Frame: ObservableObject {
    @Published var image: CGImage?
}
private let frameModel = Frame()

private func pushEvent(_ type: UInt64, _ p: CGPoint) {
    let x = UInt64(max(0, min(fbW - 1, Int(p.x) * scale)))
    let y = UInt64(max(0, min(fbH - 1, Int(p.y) * scale)))
    events.withLock { q in
        if q.count < 512 { q.append((type << 40) | (y << 20) | x) }
    }
}

/// Size the buffer from the view and let the image's first UI call through.
private func start(_ size: CGSize) {
    if fb != nil { return }
    fbW = Int(size.width) * scale
    fbH = Int(size.height) * scale
    fb = UnsafeMutablePointer<UInt32>.allocate(capacity: fbW * fbH)
    fb!.initialize(repeating: 0, count: fbW * fbH)
    ready.signal()
}

private func present() {
    let already = presentPending.withLock { p -> Bool in let was = p; p = true; return was }
    if already { return }
    DispatchQueue.main.async {
        presentPending.withLock { $0 = false }
        guard let fb = fb else { return }
        let cs = CGColorSpaceCreateDeviceRGB()
        let info = CGImageAlphaInfo.noneSkipFirst.rawValue | CGBitmapInfo.byteOrder32Little.rawValue
        if let cx = CGContext(data: fb, width: fbW, height: fbH, bitsPerComponent: 8,
                              bytesPerRow: fbW * 4, space: cs, bitmapInfo: info) {
            frameModel.image = cx.makeImage()          // copies the pixels
        }
    }
}

private func fill(_ xy: Int, _ wh: Int, _ rgb: Int) -> Int {
    guard let fb = fb else { return 0 }
    let x = xy & 0xFFFF, y = (xy >> 16) & 0xFFFF
    var w = wh & 0xFFFF, h = (wh >> 16) & 0xFFFF
    if x >= fbW || y >= fbH { return 0 }
    w = min(w, fbW - x); h = min(h, fbH - y)
    let c = UInt32(truncatingIfNeeded: rgb & 0xFFFFFF)
    for j in 0..<h {
        (fb + (y + j) * fbW + x).update(repeating: c, count: w)
    }
    return 0
}

private let waited = OSAllocatedUnfairLock(initialState: false)

@_cdecl("modus_ui_init") public func modusUIInit() {}
@_cdecl("modus_ui_start") public func modusUIStart() {}

@_cdecl("modus_ui_call")
public func modusUICall(_ nr: Int, _ a0: Int, _ a1: Int, _ a2: Int, _ a3: Int) -> Int {
    // The image thread starts before the view exists: wait for it, once.
    if !waited.withLock({ $0 }) {
        ready.wait(); ready.signal()
        waited.withLock { $0 = true }
    }
    switch nr {
    case 1001: return a0 == 0 ? fbW : a0 == 1 ? fbH : scale
    case 1002: return fill(a0, a1, a2)
    case 1003: present(); return 0
    case 1004: return Int(truncatingIfNeeded: events.withLock { q -> UInt64 in q.isEmpty ? 0 : q.removeFirst() })
    default: return -38
    }
}

struct ModusView: View {
    @ObservedObject var f = frameModel
    @State private var down = false
    var body: some View {
        GeometryReader { g in
            ZStack {
                Color.black
                if let img = f.image {
                    Image(decorative: img, scale: CGFloat(scale))
                        .interpolation(.none)
                        .resizable()
                        .frame(width: g.size.width, height: g.size.height)
                }
            }
            .contentShape(Rectangle())
            .gesture(DragGesture(minimumDistance: 0)
                .onChanged { v in pushEvent(down ? 2 : 1, v.location); down = true }
                .onEnded { v in pushEvent(3, v.location); down = false })
            .onAppear { start(g.size) }
        }
        .ignoresSafeArea()
    }
}

struct ModusWatchApp: App {
    var body: some Scene { WindowGroup { ModusView() } }
}

@_cdecl("modus_watch_main")
public func modusWatchMain() -> Never {
    ModusWatchApp.main()
    exit(0)
}
