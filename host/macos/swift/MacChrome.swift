// MacChrome.swift — the Mac's own chrome, for Lisp: Liquid Glass, the menu bar's menus, and status items.  Lisp's
// side is vessel's mac-window.lisp (package KILN-MAC).
//
// GLASS (macOS 26's NSGlassEffectView; before it, a frosted NSVisualEffectView in the same place):
//   glass_new(x, y, w, h, radius, tint)    a glass panel, TINT #RRGGBBAA (0: none); a UI handle.  Views added to it
//                                          (ui_add) sit inside, their x and y from its top left
//   glass_button(button, on)               a button drawn as glass (bezel style .glass), or back to rounded
//
// MENUS, in the menu bar between the app's and Window, or alone for a status item:
//   menu_new(title, len, inbar)            a menu; INBAR 1 puts it in the menu bar
//   menu_item_new(menu, title, len, key, klen)   an item, with Command-KEY when KEY is not ""; choosing it is a
//                                          MENU event (the item's handle)
//   menu_separator(menu)
//   menu_item_set(item, checked, enabled)  a check mark, and whether it can be chosen
//
// STATUS ITEMS, at the right of the menu bar:
//   status_item_new(title, len, symbol, slen)   text, an SF Symbol, or both; a click is a STATUS event (its handle)
//                                          unless it has a menu
//   status_item_title(item, title, len)    its text, changed
//   status_item_menu(item, menu)           the menu it opens when clicked
//
// WINDOWS, more than the one the desk is in:
//   window_new(title, len, w, h, palette)   a window, W x H points, centred, shown -- PALETTE 1 a floating palette
//                                          (a small-titled panel over the app's windows, hidden while another app is
//                                          in front); a UI handle -- what ui_add puts in
//                                          it sits inside, placed from its top left (a view that fills its parent fills
//                                          the window).  Closed by its button, a WINCLOSE event (handle), and it is
//                                          hidden, not gone: window_show brings it back.  Resized, WINRESIZE (handle
//                                          w * 65536 + h, points)
//   window_show(win)   window_hide(win)   window_retitle(win, title, len)
//
// THE APP'S OWN EVENTS (handle 0): OPENED (a file's path, or a link, opened with the app: Finder's Open With, `open`,
// the app's URL scheme), DROPPED (a file dropped on the window), APPSTATE ("active", "inactive", "hidden",
// "shown"), and QUIT once mac_on_quit(1) asked for it -- the app then waits (five seconds at most) for mac_quit_reply.
//   mac_on_quit(on)   mac_quit_reply()
//   dock_badge(text, len)                  the Dock icon's badge ("" clears it)
//   dock_bounce(critical)                  the Dock icon bounces once (until the app is active, if CRITICAL)

import AppKit

// ---- glass ---------------------------------------------------------------------------------------------------------

/// A glass panel's content view, by the panel's handle: where ui_add puts what goes inside (ModusSwiftMac.swift).
var glassContent: [UInt: NSView] = [:]

let glassNew: Call = { x, y, w, h, radius, tint in
    let frame = rect(x, y, w, h)
    let content = FlippedView(frame: CGRect(origin: .zero, size: frame.size))
    content.autoresizingMask = [.width, .height]
    let panel: NSView
    if #available(macOS 26.0, *) {
        let glass = NSGlassEffectView(frame: frame)
        glass.cornerRadius = CGFloat(radius)
        if tint != 0 {
            glass.tintColor = NSColor(srgbRed: CGFloat((tint >> 24) & 255) / 255, green: CGFloat((tint >> 16) & 255) / 255,
                                      blue: CGFloat((tint >> 8) & 255) / 255, alpha: CGFloat(tint & 255) / 255)
        }
        glass.contentView = content
        panel = glass
    } else {
        let frosted = NSVisualEffectView(frame: frame)
        frosted.material = .hudWindow
        frosted.blendingMode = .withinWindow
        frosted.state = .active
        frosted.wantsLayer = true
        frosted.layer?.cornerRadius = CGFloat(radius)
        frosted.layer?.masksToBounds = true
        frosted.addSubview(content)
        panel = frosted
    }
    let handle = adopt(panel)
    glassContent[handle] = content
    return handle
}

let glassButton: Call = { h, on, _, _, _, _ in
    guard let button = views[h] as? NSButton else { return 0 }
    if #available(macOS 26.0, *) {
        button.bezelStyle = on != 0 ? .glass : .rounded
    }
    return 1
}

// ---- menus and status items ----------------------------------------------------------------------------------------

private var menus: [UInt: NSMenu] = [:]
private var menuItems: [UInt: NSMenuItem] = [:]
private var statusItems: [UInt: NSStatusItem] = [:]

/// Where a chosen menu item or a clicked status item goes: an event, with the handle its tag carries.
private final class ChromeTarget: NSObject {
    @objc func chose(_ sender: NSMenuItem) { send("MENU", [.int(sender.tag)]) }
    @objc func clicked(_ sender: NSStatusBarButton) { send("STATUS", [.int(sender.tag)]) }
}
private let chromeTarget = ChromeTarget()

private func handleFor(_ object: AnyObject) -> UInt {
    let h = nextHandle
    nextHandle += 1
    return h
}

let menuNew: Call = { p, n, inbar, _, _, _ in
    let title = text(p, n)
    let menu = NSMenu(title: title)
    menu.autoenablesItems = false
    let h = handleFor(menu)
    menus[h] = menu
    if inbar != 0, let bar = NSApp.mainMenu {
        let item = NSMenuItem(title: title, action: nil, keyEquivalent: "")
        item.submenu = menu
        // before the Window menu, as an app's own menus are
        let at = bar.items.firstIndex { $0.submenu != nil && $0.submenu === NSApp.windowsMenu } ?? bar.items.count
        bar.insertItem(item, at: at)
    }
    return h
}

let menuItemNew: Call = { m, p, n, kp, kn, _ in
    guard let menu = menus[m] else { return 0 }
    let item = NSMenuItem(title: text(p, n), action: #selector(ChromeTarget.chose(_:)), keyEquivalent: text(kp, kn))
    item.target = chromeTarget
    let h = handleFor(item)
    item.tag = Int(h)
    menu.addItem(item)
    menuItems[h] = item
    return h
}

let menuSeparator: Call = { m, _, _, _, _, _ in
    guard let menu = menus[m] else { return 0 }
    menu.addItem(.separator())
    return 1
}

let menuItemSet: Call = { i, checked, enabled, _, _, _ in
    guard let item = menuItems[i] else { return 0 }
    item.state = checked != 0 ? .on : .off
    item.isEnabled = enabled != 0
    return 1
}

let statusItemNew: Call = { p, n, sp, sn, _, _ in
    let item = NSStatusBar.system.statusItem(withLength: NSStatusItem.variableLength)
    let h = handleFor(item)
    if let button = item.button {
        button.title = text(p, n)
        let symbol = text(sp, sn)
        if !symbol.isEmpty {
            button.image = NSImage(systemSymbolName: symbol, accessibilityDescription: symbol)
            button.imagePosition = button.title.isEmpty ? .imageOnly : .imageLeading
        }
        button.tag = Int(h)
        button.target = chromeTarget
        button.action = #selector(ChromeTarget.clicked(_:))
    }
    statusItems[h] = item
    return h
}

let statusItemTitle: Call = { s, p, n, _, _, _ in
    guard let button = statusItems[s]?.button else { return 0 }
    let title = text(p, n)
    // digits of one width, so a number that changes does not make the item jitter; and with an icon, the icon
    // then the text (made with no text, it was the icon alone, and text drawn later went under it)
    button.attributedTitle = NSAttributedString(string: title, attributes: [
        .font: NSFont.monospacedDigitSystemFont(ofSize: NSFont.systemFontSize, weight: .regular)])
    if button.image != nil { button.imagePosition = title.isEmpty ? .imageOnly : .imageLeading }
    return 1
}

let statusItemMenu: Call = { s, m, _, _, _, _ in
    guard let item = statusItems[s], let menu = menus[m] else { return 0 }
    item.menu = menu
    return 1
}

// ---- windows ----------------------------------------------------------------------------------------------------

/// A window's content view, by the window's handle: where ui_add puts what goes inside (ModusSwiftMac.swift).
var windowContent: [UInt: NSView] = [:]
private var windows: [UInt: NSWindow] = [:]
private var windowDelegates: [UInt: WindowDelegate] = [:]

private final class WindowDelegate: NSObject, NSWindowDelegate {
    let handle: UInt
    init(_ handle: UInt) { self.handle = handle }
    // closed by its button: hidden, and Lisp told; it can be shown again
    func windowShouldClose(_ sender: NSWindow) -> Bool {
        sender.orderOut(nil)
        send("WINCLOSE", [.int(Int(handle))])
        return false
    }
    func windowDidResize(_ n: Notification) {
        guard let w = n.object as? NSWindow, let c = w.contentView else { return }
        send("WINRESIZE", [.int(Int(handle)), .int(Int(c.bounds.width) * 65536 + Int(c.bounds.height))])
    }
}

let windowNew: Call = { p, n, w, h, palette, _ in
    let frame = CGRect(x: 0, y: 0, width: Int(max(w, 100)), height: Int(max(h, 80)))
    let window: NSWindow
    if palette != 0 {
        let panel = NSPanel(contentRect: frame, styleMask: [.titled, .closable, .resizable, .utilityWindow],
                            backing: .buffered, defer: false)
        panel.isFloatingPanel = true             // over the app's own windows
        panel.hidesOnDeactivate = true            // and gone while another app is in front
        panel.becomesKeyOnlyIfNeeded = true       // the keys stay with the main window
        window = panel
    } else {
        window = NSWindow(contentRect: frame, styleMask: [.titled, .closable, .miniaturizable, .resizable],
                          backing: .buffered, defer: false)
    }
    window.title = text(p, n)
    window.isReleasedWhenClosed = false
    window.appearance = darkAppearance
    let content = FlippedView(frame: window.contentLayoutRect)
    content.autoresizingMask = [.width, .height]
    window.contentView = content
    let handle = nextHandle
    nextHandle += 1
    let delegate = WindowDelegate(handle)
    window.delegate = delegate
    windowDelegates[handle] = delegate
    windows[handle] = window
    windowContent[handle] = content
    views[handle] = content
    window.center()
    window.makeKeyAndOrderFront(nil)
    return handle
}

let windowShow: Call = { h, _, _, _, _, _ in
    guard let window = windows[h] else { return 0 }
    window.makeKeyAndOrderFront(nil)
    return 1
}

let windowHide: Call = { h, _, _, _, _, _ in
    guard let window = windows[h] else { return 0 }
    window.orderOut(nil)
    return 1
}

let windowRetitle: Call = { h, p, n, _, _, _ in
    guard let window = windows[h] else { return 0 }
    window.title = text(p, n)
    return 1
}

// ---- the app's own events, and the Dock -------------------------------------------------------------------------------

private var quitWanted = false
private var appStateObservers: [NSObjectProtocol] = []

let macOnQuit: Call = { on, _, _, _, _, _ in quitWanted = on != 0; return 1 }
let macQuitReply: Call = { _, _, _, _, _, _ in NSApp.reply(toApplicationShouldTerminate: true); return 1 }

let dockBadge: Call = { p, n, _, _, _, _ in
    let t = text(p, n)
    NSApp.dockTile.badgeLabel = t.isEmpty ? nil : t
    return 1
}

let dockBounce: Call = { critical, _, _, _, _, _ in
    NSApp.requestUserAttention(critical != 0 ? .criticalRequest : .informationalRequest)
    return 1
}

private func installAppEvents() {
    modus_mac_opened_hook = { kind, url in
        guard let url else { return }
        send(kind == 0 ? "OPENED" : "DROPPED", [.int(0), .string(String(cString: url))])
    }
    modus_mac_quit_hook = {
        guard quitWanted else { return 0 }
        send("QUIT", [.int(0)])
        return 1
    }
    modus_mac_opened_flush()                          // what was opened before now (at launch)
    let states: [(Notification.Name, String)] = [
        (NSApplication.didBecomeActiveNotification, "active"), (NSApplication.didResignActiveNotification, "inactive"),
        (NSApplication.didHideNotification, "hidden"), (NSApplication.didUnhideNotification, "shown")]
    for (name, state) in states {
        appStateObservers.append(NotificationCenter.default.addObserver(forName: name, object: nil, queue: .main) { _ in
            send("APPSTATE", [.int(0), .string(state)])
        })
    }
}

func installChrome() {
    register("window_new", windowNew)
    register("window_show", windowShow)
    register("window_hide", windowHide)
    register("window_retitle", windowRetitle)
    installAppEvents()
    register("mac_on_quit", macOnQuit)
    register("mac_quit_reply", macQuitReply)
    register("dock_badge", dockBadge)
    register("dock_bounce", dockBounce)
    register("glass_new", glassNew)
    register("glass_button", glassButton)
    register("menu_new", menuNew)
    register("menu_item_new", menuItemNew)
    register("menu_separator", menuSeparator)
    register("menu_item_set", menuItemSet)
    register("status_item_new", statusItemNew)
    register("status_item_title", statusItemTitle)
    register("status_item_menu", statusItemMenu)
}
