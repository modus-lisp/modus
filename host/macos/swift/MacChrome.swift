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
    button.title = text(p, n)
    return 1
}

let statusItemMenu: Call = { s, m, _, _, _, _ in
    guard let item = statusItems[s], let menu = menus[m] else { return 0 }
    item.menu = menu
    return 1
}

func installChrome() {
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
