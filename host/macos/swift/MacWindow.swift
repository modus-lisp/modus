// MacWindow.swift — the modus window itself, from Lisp: its title, its chrome, its size, full screen, and where it
// was.  A Mac's own calls (the phone has one screen and no window), named window_*; vessel's mac-window.lisp calls
// them.  The app's NAME is the bundle's (build-macos.sh, MODUS_MAC_NAME): the Dock's, the menu bar's, and the
// window's first title.
//
//   window_title(text, length)     the title in the title bar (and the Window menu, and Mission Control)
//   window_chrome(style)           0 standard: a title bar with the title and the three buttons
//                                  1 the bar and its buttons, no title
//                                  2 transparent: the content runs under the bar, the buttons float over it
//                                  3 none: as 2, without the buttons -- nothing but the content (the bar's strip
//                                    still drags the window, and Command-W, -M and -Q still work)
//   window_size(w, h)              the content's size, in points
//   window_min_size(w, h)          the smallest the person can make it
//   window_fullscreen(on)          into full screen (1) or out of it (0)
//   window_remember(name, length)  the window's frame kept under NAME: where it was is where it opens next time
//
// With styles 2 and 3 the top of the content is under the bar; the window host reports how much as the top of the
// safe area (pseudo-syscall 1001 3), and sends a resize, so an image that lays itself out in the safe area (the
// desk does) keeps clear of it.

import AppKit

private func modusWindow() -> NSWindow? {
    rootView()?.window
}

let windowTitle: Call = { p, n, _, _, _, _ in
    modusWindow()?.title = text(p, n)
    return 0
}

let windowChrome: Call = { style, _, _, _, _, _ in
    guard let window = modusWindow(), style <= 3 else { return 0 }
    // the title survives the change: a new style mask makes a new title bar, which came back with the bundle's name
    let title = window.title
    defer { window.title = title }
    if style >= 2 {
        window.styleMask.insert(.fullSizeContentView)
    } else {
        window.styleMask.remove(.fullSizeContentView)
    }
    window.titlebarAppearsTransparent = style >= 2
    window.titleVisibility = style >= 1 ? .hidden : .visible
    for button in [NSWindow.ButtonType.closeButton, .miniaturizeButton, .zoomButton] {
        window.standardWindowButton(button)?.isHidden = style == 3
    }
    modus_mac_relayout()
    return 1
}

let windowSize: Call = { w, h, _, _, _, _ in
    guard let window = modusWindow(), w > 0, h > 0 else { return 0 }
    window.setContentSize(NSSize(width: CGFloat(w), height: CGFloat(h)))
    return 1
}

let windowMinSize: Call = { w, h, _, _, _, _ in
    modusWindow()?.contentMinSize = NSSize(width: CGFloat(w), height: CGFloat(h))
    return 0
}

let windowFullscreen: Call = { on, _, _, _, _, _ in
    guard let window = modusWindow() else { return 0 }
    if window.styleMask.contains(.fullScreen) != (on != 0) { window.toggleFullScreen(nil) }
    return 1
}

// The frame is saved under NAME as the person moves and resizes the window, and restored now if it was saved before.
let windowRemember: Call = { p, n, _, _, _, _ in
    guard let window = modusWindow() else { return 0 }
    let name = text(p, n)
    guard !name.isEmpty else { return 0 }
    window.setFrameUsingName(name)
    return window.setFrameAutosaveName(name) ? 1 : 0
}

func installWindow() {
    register("window_title", windowTitle)
    register("window_chrome", windowChrome)
    register("window_size", windowSize)
    register("window_min_size", windowMinSize)
    register("window_fullscreen", windowFullscreen)
    register("window_remember", windowRemember)
}
