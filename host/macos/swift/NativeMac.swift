// NativeMac.swift — the phone's app-grade kit (host/ios/swift/Native.swift) on the Mac: what a modus app needs to
// look like an ordinary Mac app rather than a window over glass's desk.  The SAME function names (app_*), the same
// arguments and the same events, so vessel's ios-native.lisp, and the apps built on it, run on either.
// As in ModusSwiftMac.swift, Lisp calls these by name (host/ios/modus-swift.m), on the main thread, every argument
// a word, text a pointer and a length, structured data a term; the events go back as terms through ui_events_next.
//
// Where the Mac has no such thing, the nearest one it has, and the same events:
//
//   the navigation controller          a container filling the modus window: a bar on top (back, the title
//                                      centred between the bar's buttons, the buttons and menus) over the screen
//                                      on top of the stack; a push slides in, back slides out, and a POP follows.
//                                      Back is the chevron, or Command-[.  app_nav_present shows one as a sheet.
//   inset-grouped tables               NSTableView in its inset style; sections are group rows; a click is a
//                                      SELECT; swipe actions are the trackpad's row actions AND a right click's menu
//   pull to refresh                    Command-R while the table's screen is shown, or Refresh in its right-click
//                                      menu: the same REFRESH, a small spinner until app_table_refresh_end
//   the browser                        always as the phone is in landscape: the folder and its parent as columns,
//                                      or the folder beside the preview pane (Quick Look's view, our text, image
//                                      and hex views); a hex line's right-click menu edits it (HEXEDIT)
//   alerts, forms, the invoice         NSAlert as a sheet (its fields an accessory view); forms and the invoice as
//                                      sheets on the window; Open in Wallet asks NSWorkspace for a lightning: URL
//   the status ring in the bar         a status bar along the bottom of the navigation container
//   document picker, Quick Look,       NSOpenPanel (the choice copied into tmp, as the phone's picker does);
//   share sheet                        Quick Look's view in a sheet; NSSharingServicePicker
//   orientation                        a Mac is always landscape
//
// The kit styles itself dark, as the phone's does in dark mode.

import AppKit
import CoreImage
import ImageIO
import Quartz
import UniformTypeIdentifiers

// MARK: - what the handles name

private var navs: [UInt: MacNav] = [:]
private var navRoots: [UInt: UInt] = [:]
private var screens: [UInt: MacScreen] = [:]
private var tables: [UInt: MacTable] = [:]
private var barItems: [UInt: MacBarItem] = [:]
private var nativeAlerts: [UInt: MacAlert] = [:]
private var forms: [UInt: MacForm] = [:]
private var invoices: [UInt: MacInvoice] = [:]
private var browsers: [UInt: MacBrowser] = [:]          // by the browser screen's handle
private var composers: [UInt: MacComposer] = [:]
// What answers later (a picker, a preview, a share): kept until it has.
private var held: [UInt: AnyObject] = [:]

// MARK: - terms

/// A plist term's values by key name.
private func plist(_ t: Term) -> [String: Term] {
    var out: [String: Term] = [:]
    let items = t.listItems ?? []
    var i = 0
    while i + 1 < items.count {
        if case .symbol(let key, _) = items[i] { out[key] = items[i + 1] }
        i += 2
    }
    return out
}

private func str(_ t: Term?) -> String? {
    if case .string(let s)? = t { return s }
    return nil
}

private func flag(_ t: Term?) -> Bool {
    switch t {
    case nil, .null?: return false
    case .int(let v)?: return v != 0
    default: return true
    }
}

private func strings(_ t: Term?) -> [String] {
    (t?.listItems ?? []).compactMap { str($0) }
}

/// An RGB integer, or the name of one of the label colours (:LABEL :SECONDARY :TERTIARY :GRAY :DARKGRAY), which
/// here are the dark appearance's.
private func color(_ t: Term?) -> NSColor? {
    if case .symbol(let name, _)? = t {
        switch name {
        case "LABEL": return .labelColor
        case "SECONDARY": return .secondaryLabelColor
        case "TERTIARY": return .tertiaryLabelColor
        case "GRAY": return .systemGray
        case "DARKGRAY": return NSColor(srgbRed: 0.39, green: 0.39, blue: 0.4, alpha: 1)   // the phone's dark systemGray2
        default: return nil
        }
    }
    guard case .int(let rgb)? = t else { return nil }
    return colorOf(UInt(truncatingIfNeeded: rgb & 0xFFFFFF))
}

private func termBytes(_ p: UInt, _ n: UInt) -> Term? {
    guard n > 0, let base = UnsafeRawPointer(bitPattern: p) else { return nil }
    return try? decodeTerm(Array(UnsafeRawBufferPointer(start: base, count: Int(n))))
}

private func menuChoices(_ t: Term?) -> [MenuChoice] {
    (t?.listItems ?? []).compactMap { item in
        let a = plist(item)
        guard case .int(let id)? = a["ID"] else { return nil }
        return MenuChoice(id: id, title: str(a["TITLE"]) ?? "", image: str(a["IMAGE"]), destructive: flag(a["DESTRUCTIVE"]))
    }
}

// MARK: - the theme, views and buttons

// THE THEME (app_theme): the system's accent, or MONO -- the label colour (white, in the dark).
private var monoTheme = false
private var accent: NSColor { monoTheme ? .labelColor : .controlAccentColor }

/// A flipped view filled with one colour, drawn (not a layer's colour) so a semantic colour follows the appearance.
private class FillView: FlippedView {
    var fill: NSColor? { didSet { needsDisplay = true } }
    override func draw(_ dirtyRect: NSRect) {
        guard let fill else { return }
        fill.setFill()
        // only its own bounds: since macOS 14 a view does not clip its drawing, and the dirty rect can reach past it
        // (the bar, drawn last, filled the whole window over the screens under it)
        dirtyRect.intersection(bounds).fill()
    }
}

/// A button whose click is a closure, or which pops up a menu under itself.
private final class MacButton: NSButton {
    var onClick: (() -> Void)?
    var popMenu: NSMenu?

    func arm() {
        target = self
        action = #selector(clicked(_:))
    }

    @objc func clicked(_ sender: Any?) {
        if let menu = popMenu {
            menu.popUp(positioning: nil, at: NSPoint(x: 0, y: isFlipped ? bounds.height + 4 : -4), in: self)
        } else {
            onClick?()
        }
    }
}

/// A bar's button: an SF Symbol when NAME names one, else the text; its border shows under the pointer.
private func barButton(symbol name: String?, title: String, _ click: (() -> Void)?) -> MacButton {
    let b = MacButton(frame: .zero)
    b.arm()
    b.bezelStyle = .recessed
    b.showsBorderOnlyWhileMouseInside = true
    if let name, let image = NSImage(systemSymbolName: name, accessibilityDescription: title) {
        b.image = image
        b.imagePosition = .imageOnly
        b.symbolConfiguration = .init(pointSize: 15, weight: .regular)
        b.toolTip = title
        b.setAccessibilityLabel(title)
    } else {
        b.title = title
    }
    if monoTheme { b.contentTintColor = .labelColor }
    b.onClick = click
    return b
}

private func pushButton(_ title: String, _ click: (() -> Void)?) -> MacButton {
    let b = MacButton(frame: .zero)
    b.arm()
    b.title = title
    b.bezelStyle = .rounded
    b.onClick = click
    return b
}

private func label(_ s: String, size: CGFloat = 13, color: NSColor = .labelColor, bold: Bool = false) -> NSTextField {
    let l = NSTextField(labelWithString: s)
    l.font = bold ? .boldSystemFont(ofSize: size) : .systemFont(ofSize: size)
    l.textColor = color
    l.lineBreakMode = .byTruncatingTail
    return l
}

private func animate(_ duration: TimeInterval, _ changes: @escaping () -> Void, done: (() -> Void)? = nil) {
    NSAnimationContext.runAnimationGroup({ c in
        c.duration = duration
        c.timingFunction = CAMediaTimingFunction(name: .easeOut)
        c.allowsImplicitAnimation = true
        changes()
    }, completionHandler: done)
}

// MARK: - menus

private struct MenuChoice {
    let id: Int
    let title: String
    let image: String?
    let destructive: Bool
}

/// A menu item whose choice is a closure.  The item holds its action (an item's target is weak).
private func menuItem(_ title: String, image: String? = nil, on: Bool = false, destructive: Bool = false,
                      _ fire: @escaping () -> Void) -> NSMenuItem {
    let action = Action { _ in fire() }
    let item = NSMenuItem(title: title, action: #selector(Action.act(_:)), keyEquivalent: "")
    item.target = action
    item.representedObject = action
    if let image { item.image = NSImage(systemSymbolName: image, accessibilityDescription: nil) }
    item.state = on ? .on : .off
    if destructive {
        item.attributedTitle = NSAttributedString(string: title, attributes: [
            .foregroundColor: NSColor.systemRed, .font: NSFont.menuFont(ofSize: 0)])
    }
    return item
}

/// A bar menu's items (as the phone's barMenu): plists of :ID :TITLE :SUBTITLE :IMAGE :ON :GROUP, a rule between
/// groups, a group's first item may carry :HEADER, or :SUBMENU (and :SUBMENU-IMAGE) to make the group a submenu.
/// A choice is EVENT with the button and the item's id.
private func barMenu(_ bh: UInt, _ items: Term?, event: String = "MENU") -> NSMenu {
    let menu = NSMenu()
    menu.autoenablesItems = false
    var group: Int?
    var into = menu
    for t in items?.listItems ?? [] {
        let m = plist(t)
        guard case .int(let id)? = m["ID"] else { continue }
        var g = 0
        if case .int(let k)? = m["GROUP"] { g = k }
        if group != g {
            if group != nil { menu.addItem(.separator()) }
            into = menu
            if let sub = str(m["SUBMENU"]) {
                let holder = NSMenuItem(title: sub, action: nil, keyEquivalent: "")
                if let image = str(m["SUBMENU-IMAGE"]) { holder.image = NSImage(systemSymbolName: image, accessibilityDescription: nil) }
                let inner = NSMenu(title: sub)
                inner.autoenablesItems = false
                holder.submenu = inner
                menu.addItem(holder)
                into = inner
            } else if let header = str(m["HEADER"]) {
                menu.addItem(.sectionHeader(title: header))
            }
            group = g
        }
        let item = menuItem(str(m["TITLE"]) ?? "", image: str(m["IMAGE"]), on: flag(m["ON"])) {
            send(event, [.int(Int(bh)), .int(id)])
        }
        if #available(macOS 14.4, *), let sub = str(m["SUBTITLE"]) { item.subtitle = sub }
        into.addItem(item)
    }
    return menu
}

// MARK: - sheets

private final class MacSheet: NSWindow {
    var onCancel: (() -> Void)?
    override var canBecomeKey: Bool { true }
    override func cancelOperation(_ sender: Any?) { onCancel?() }
}

/// The window to show over: the modus window, or the sheet on top of it (a form over a presented app).
private func hostWindow() -> NSWindow? {
    var w = rootView()?.window
    while let s = w?.attachedSheet { w = s }
    return w
}

private func beginSheet(_ content: NSView, size: CGSize, resizable: Bool = false) -> MacSheet? {
    guard let parent = hostWindow() else { return nil }
    let sheet = MacSheet(contentRect: CGRect(origin: .zero, size: size),
                         styleMask: resizable ? [.titled, .resizable] : [.titled], backing: .buffered, defer: false)
    sheet.appearance = darkAppearance
    sheet.isReleasedWhenClosed = false
    content.frame = CGRect(origin: .zero, size: size)
    content.autoresizingMask = [.width, .height]
    sheet.contentView = content
    parent.beginSheet(sheet)
    return sheet
}

private func endSheet(_ sheet: NSWindow) {
    if let parent = sheet.sheetParent { parent.endSheet(sheet) }
    sheet.orderOut(nil)
}

/// A content view laid out by constraints, for a sheet: its stack pinned inside, the size what it needs.
private func sheetContent(_ stack: NSStackView) -> (NSView, CGSize) {
    let container = FillView()
    container.fill = .windowBackgroundColor
    stack.translatesAutoresizingMaskIntoConstraints = false
    container.addSubview(stack)
    NSLayoutConstraint.activate([
        stack.topAnchor.constraint(equalTo: container.topAnchor),
        stack.bottomAnchor.constraint(equalTo: container.bottomAnchor),
        stack.leadingAnchor.constraint(equalTo: container.leadingAnchor),
        stack.trailingAnchor.constraint(equalTo: container.trailingAnchor),
    ])
    return (container, container.fittingSize)
}

// MARK: - the title

/// The path PARTS as one string: the last in bold, the rest grey, " / " between; centred, the beginning giving way
/// to an ellipsis when it does not fit.
private func pathString(_ parts: [String]) -> NSAttributedString {
    let bold = NSFont.boldSystemFont(ofSize: 14), body = NSFont.systemFont(ofSize: 14)
    let out = NSMutableAttributedString()
    for (i, part) in parts.enumerated() {
        let last = i == parts.count - 1
        out.append(NSAttributedString(string: part, attributes: [
            .font: last ? bold : body, .foregroundColor: last ? NSColor.labelColor : NSColor.secondaryLabelColor]))
        if !last {
            out.append(NSAttributedString(string: " / ", attributes: [.font: body, .foregroundColor: NSColor.tertiaryLabelColor]))
        }
    }
    let style = NSMutableParagraphStyle()
    style.alignment = .center
    style.lineBreakMode = .byTruncatingHead
    out.addAttribute(.paragraphStyle, value: style, range: NSRange(location: 0, length: out.length))
    return out
}

/// The bar's title: one label centred in the room between the bar's buttons (not on the window), a smaller line
/// under it when there is a subtitle, or -- the browser's columns -- a label over each column, the current in bold.
private final class MacTitle: FlippedView {
    let label = NSTextField(labelWithString: "")
    let subtitle = NSTextField(labelWithString: "")
    let segments = FlippedView()
    private var segmentLabels: [NSTextField] = []
    var subtitleSpan: CGRect? { didSet { relayout() } }

    override init(frame: NSRect) {
        super.init(frame: frame)
        label.alignment = .center
        label.lineBreakMode = .byTruncatingHead
        label.maximumNumberOfLines = 1
        subtitle.font = .systemFont(ofSize: 11)
        subtitle.textColor = .secondaryLabelColor
        subtitle.alignment = .center
        subtitle.lineBreakMode = .byTruncatingMiddle
        subtitle.isHidden = true
        segments.isHidden = true
        for v in [label, segments, subtitle] { addSubview(v) }
    }

    required init?(coder: NSCoder) { fatalError("not from a nib") }

    func setPlain(_ s: String) {
        let style = NSMutableParagraphStyle()
        style.alignment = .center
        style.lineBreakMode = .byTruncatingTail
        label.attributedStringValue = NSAttributedString(string: s, attributes: [
            .font: NSFont.boldSystemFont(ofSize: 14), .foregroundColor: NSColor.labelColor, .paragraphStyle: style])
    }

    func setPath(_ parts: [String]) {
        label.attributedStringValue = pathString(parts)
    }

    func setSubtitle(_ s: String) {
        subtitle.stringValue = s
        subtitle.isHidden = s.isEmpty
        relayout()
    }

    func setSegments(_ items: [(text: String, rect: CGRect, current: Bool)]) {
        label.isHidden = !items.isEmpty
        segments.isHidden = items.isEmpty
        while segmentLabels.count < items.count {
            let l = NSTextField(labelWithString: "")
            l.alignment = .center
            l.lineBreakMode = .byTruncatingMiddle
            segments.addSubview(l)
            segmentLabels.append(l)
        }
        for (i, l) in segmentLabels.enumerated() {
            guard i < items.count else { l.isHidden = true; continue }
            let item = items[i]
            l.isHidden = false
            l.stringValue = item.text
            l.font = item.current ? .boldSystemFont(ofSize: 14) : .systemFont(ofSize: 14)
            l.textColor = item.current ? .labelColor : .secondaryLabelColor
            l.frame = CGRect(x: item.rect.minX, y: (segments.bounds.height - 20) / 2, width: item.rect.width, height: 20)
        }
    }

    override func setFrameSize(_ newSize: NSSize) {
        super.setFrameSize(newSize)
        relayout()
    }

    // with a subtitle, the title (or the column titles) moves up to make room for it beneath
    func relayout() {
        let b = bounds
        let top = subtitle.isHidden ? b : CGRect(x: 0, y: 1, width: b.width, height: b.height - 16)
        label.frame = CGRect(x: 0, y: top.midY - 10, width: b.width, height: 20)
        segments.frame = top
        for l in segmentLabels { l.frame.origin.y = (top.height - 20) / 2 }
        let span = segments.isHidden ? nil : subtitleSpan
        subtitle.frame = CGRect(x: span?.minX ?? 0, y: b.height - 18, width: span?.width ?? b.width, height: 15)
    }
}

// MARK: - screens and the navigation container

private final class MacBarItem {
    let handle: UInt
    let button: MacButton

    init(handle: UInt, button: MacButton) {
        self.handle = handle
        self.button = button
    }
}

private final class MacScreenView: FlippedView {
    var onResize: (() -> Void)?

    override func setFrameSize(_ newSize: NSSize) {
        super.setFrameSize(newSize)
        onResize?()
    }
}

/// A screen: its title (or path), its bar buttons, and its view -- the tables that fill it, a compose bar along
/// its bottom, or a browser's pages and preview pane.
private final class MacScreen {
    let handle: UInt
    let view = MacScreenView()
    var title: String
    var path: [String]?
    var subtitle = ""
    var left: [MacBarItem] = []
    var right: [MacBarItem] = []
    var tableHandles: [UInt] = []
    var composer: MacComposer?
    weak var nav: MacNav?
    var browser: MacBrowser? { browsers[handle] }

    init(handle: UInt, title: String) {
        self.handle = handle
        self.title = title
        view.wantsLayer = true
        view.onResize = { [weak self] in self?.relayout() }
    }

    func relayout() {
        let b = view.bounds
        let bottom: CGFloat = composer == nil ? 0 : 52
        for th in tableHandles {
            tables[th]?.box.frame = CGRect(x: 0, y: 0, width: b.width, height: max(0, b.height - bottom))
        }
        composer?.frame = CGRect(x: 0, y: b.height - bottom, width: b.width, height: bottom)
        browser?.layout()
    }

    /// Its bar changed: the bar shows it again, if it is the screen on top.
    func barChanged() {
        if let nav, nav.stack.last === self { nav.refreshBar() }
    }
}

private func makeScreen(_ title: String) -> MacScreen {
    let s = MacScreen(handle: newHandle(), title: title)
    screens[s.handle] = s
    return s
}

/// The navigation container: the bar, the screen stack under it, and the status bar along the bottom.
private final class MacNav: FillView {
    let handle: UInt
    let bar = FillView()
    let barRule = FillView()
    let back: MacButton
    let leftRow = NSStackView()
    let rightRow = NSStackView()
    let title = MacTitle()
    let content = FillView()
    let status = MacStatusBar()
    var stack: [MacScreen] = []
    weak var sheet: NSWindow?
    let barHeight: CGFloat = 44

    init(handle: UInt) {
        self.handle = handle
        back = barButton(symbol: "chevron.backward", title: "Back", nil)
        super.init(frame: .zero)
        wantsLayer = true
        appearance = darkAppearance
        fill = .controlBackgroundColor
        bar.fill = .windowBackgroundColor
        barRule.fill = .separatorColor
        content.fill = .controlBackgroundColor
        content.wantsLayer = true
        content.layer?.masksToBounds = true
        for row in [leftRow, rightRow] {
            row.orientation = .horizontal
            row.spacing = 2
        }
        back.onClick = { [weak self] in self?.goBack() }
        back.isHidden = true
        for v in [back, leftRow, rightRow, title] { bar.addSubview(v) }
        for v in [content, status, bar, barRule] { addSubview(v) }
        status.isHidden = true
        status.onChange = { [weak self] in self?.relayout() }
    }

    required init?(coder: NSCoder) { fatalError("not from a nib") }

    override func setFrameSize(_ newSize: NSSize) {
        super.setFrameSize(newSize)
        relayout()
    }

    func relayout() {
        let b = bounds
        let sh: CGFloat = status.shown ? 32 : 0
        bar.frame = CGRect(x: 0, y: 0, width: b.width, height: barHeight)
        barRule.frame = CGRect(x: 0, y: barHeight - 1, width: b.width, height: 1)
        content.frame = CGRect(x: 0, y: barHeight, width: b.width, height: max(0, b.height - barHeight - sh))
        status.frame = CGRect(x: 0, y: b.height - sh, width: b.width, height: sh)
        status.isHidden = sh == 0
        for s in stack { s.view.frame = CGRect(origin: CGPoint(x: s.view.isHidden ? 0 : s.view.frame.minX, y: 0),
                                               size: content.bounds.size) }
        layoutBar()
    }

    func layoutBar() {
        let w = bar.bounds.width, h = barHeight
        var x: CGFloat = 8
        if !back.isHidden {
            back.frame = CGRect(x: 6, y: (h - 28) / 2, width: 34, height: 28)
            x = 44
        }
        let lw = leftRow.arrangedSubviews.isEmpty ? 0 : leftRow.fittingSize.width
        leftRow.frame = CGRect(x: x, y: (h - 28) / 2, width: lw, height: 28)
        if lw > 0 { x += lw + 8 }
        let rw = rightRow.arrangedSubviews.isEmpty ? 0 : rightRow.fittingSize.width
        rightRow.frame = CGRect(x: w - 8 - rw, y: (h - 28) / 2, width: rw, height: 28)
        let right = w - 8 - rw - (rw > 0 ? 8 : 0)
        title.frame = CGRect(x: x, y: 0, width: max(0, right - x), height: h - 1)
        stack.last?.browser?.layoutTitle()
    }

    /// The bar as the screen on top has it: back when there is somewhere to go back to (a screen, or a browser's
    /// folder), its buttons, its title.
    func refreshBar() {
        guard let top = stack.last else { return }
        back.isHidden = !(stack.count > 1 || (top.browser?.pages.count ?? 0) > 1)
        for row in [leftRow, rightRow] {
            for v in row.arrangedSubviews {
                row.removeArrangedSubview(v)
                v.removeFromSuperview()
            }
        }
        for item in top.left { leftRow.addArrangedSubview(item.button) }
        // the phone's right-hand items run right to left: the first added is the rightmost
        for item in top.right.reversed() { rightRow.addArrangedSubview(item.button) }
        if let p = top.path { title.setPath(p) } else { title.setPlain(top.title) }
        if top.browser == nil { title.setSegments([]) }
        title.setSubtitle(top.subtitle)
        layoutBar()
    }

    func push(_ s: MacScreen, animated: Bool) {
        let below = stack.last
        stack.append(s)
        s.nav = self
        s.view.isHidden = false
        s.view.frame = content.bounds
        content.addSubview(s.view)
        s.relayout()
        refreshBar()
        guard animated, let below, window != nil else {
            below?.view.isHidden = true
            return
        }
        let w = content.bounds.width
        s.view.setFrameOrigin(NSPoint(x: w, y: 0))
        animate(0.28, {
            s.view.animator().setFrameOrigin(.zero)
            below.view.animator().setFrameOrigin(NSPoint(x: -w * 0.3, y: 0))
        }, done: { [weak self] in
            if self?.stack.last !== below {
                below.view.isHidden = true
                below.view.setFrameOrigin(.zero)
            }
        })
    }

    /// Back one screen: it slides away, and a POP for it follows (for a browser's screen, its pages go with it).
    func pop(animated: Bool) {
        guard stack.count > 1 else { return }
        let top = stack.removeLast()
        let below = stack.last!
        below.view.isHidden = false
        below.view.frame = content.bounds
        refreshBar()
        let w = content.bounds.width
        guard animated, window != nil else {
            top.view.removeFromSuperview()
            popped(top)
            return
        }
        below.view.setFrameOrigin(NSPoint(x: -w * 0.3, y: 0))
        animate(0.25, {
            top.view.animator().setFrameOrigin(NSPoint(x: w, y: 0))
            below.view.animator().setFrameOrigin(.zero)
        }, done: {
            top.view.removeFromSuperview()
            self.popped(top)
        })
    }

    private func popped(_ s: MacScreen) {
        if let b = browsers[s.handle] {
            b.closePreview(byUser: false)
            for page in b.pages { tables[page.table] = nil }
        }
        browsers[s.handle] = nil
        send("POP", [.int(Int(s.handle))])
        screens[s.handle] = nil
    }

    /// The back button, or Command-[: a browser goes up a folder (and from its first, leaves); a screen pops.
    @discardableResult
    func goBack() -> Bool {
        guard !back.isHidden, let top = stack.last else { return false }
        if let b = top.browser { b.back() } else { pop(animated: true) }
        return true
    }

    /// Command-R: the tables of the screen on top that refresh (a browser's: the page on top), each a REFRESH.
    func refreshShown() -> Bool {
        guard let top = stack.last else { return false }
        let handles = top.browser.map { b in b.pages.last.map { [$0.table] } ?? [] } ?? top.tableHandles
        var any = false
        for th in handles {
            if let t = tables[th], t.refreshable {
                t.beginRefresh()
                any = true
            }
        }
        return any
    }
}

// The front navigation container: the one in the key window (none under an alert or a form).
private func frontNav() -> MacNav? {
    guard let key = NSApp.keyWindow else { return nil }
    return navs.filter { $0.value.window === key && !$0.value.isHidden }.max { $0.key < $1.key }?.value
}

private var keyMonitor: Any?

private func watchKeys() {
    guard keyMonitor == nil else { return }
    keyMonitor = NSEvent.addLocalMonitorForEvents(matching: .keyDown) { event in
        let mods = event.modifierFlags.intersection(.deviceIndependentFlagsMask)
        guard mods == .command, let nav = frontNav() else { return event }
        switch event.charactersIgnoringModifiers {
        case "r": return nav.refreshShown() ? nil : event
        case "[": return nav.goBack() ? nil : event
        default: return event
        }
    }
}

/// The navigation container fills the modus window -- once there is a window (the image can start before it).
private func installRoot(_ nav: MacNav, tries: Int = 200) {
    if let root = rootView() {
        nav.frame = root.bounds
        nav.autoresizingMask = [.width, .height]
        root.addSubview(nav)
        nav.relayout()
    } else if tries > 0 {
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.05) { installRoot(nav, tries: tries - 1) }
    }
}

private func newNav(_ title: String) -> MacNav {
    let nav = MacNav(handle: newHandle())
    let root = makeScreen(title)
    nav.push(root, animated: false)
    navs[nav.handle] = nav
    navRoots[nav.handle] = root.handle
    watchKeys()
    return nav
}

/// app_nav_new(title, quiet): the navigation container with a root screen titled TITLE, filling the window.  Its
/// bar always holds still here (QUIET's bar, on the phone).  Returns its handle; app_nav_root gives the screen's.
private let appNavNew: Call = { p, n, _, _, _, _ in
    let nav = newNav(text(p, n))
    installRoot(nav)
    return nav.handle
}

/// app_nav_present(title): a navigation container over what is there, as a sheet on the window -- an app within an
/// app.  app_nav_dismiss takes it away.
private let appNavPresent: Call = { p, n, _, _, _, _ in
    guard let host = hostWindow() else { return 0 }
    let nav = newNav(text(p, n))
    let have = host.contentView?.bounds.size ?? CGSize(width: 900, height: 640)
    let size = CGSize(width: max(520, have.width - 60), height: max(400, have.height - 60))
    guard let sheet = beginSheet(nav, size: size, resizable: true) else { return 0 }
    sheet.onCancel = { [weak nav] in nav?.goBack() }
    nav.sheet = sheet
    nav.relayout()
    return nav.handle
}

private let appNavDismiss: Call = { h, _, _, _, _, _ in
    guard let nav = navs[h] else { return 0 }
    if let sheet = nav.sheet { endSheet(sheet) }
    return 1
}

private let appNavRoot: Call = { h, _, _, _, _, _ in navRoots[h] ?? 0 }

/// app_push(nav, title): a new screen pushed onto NAV; its handle.
private let appPush: Call = { h, p, n, _, _, _ in
    guard let nav = navs[h] else { return 0 }
    let s = makeScreen(text(p, n))
    nav.push(s, animated: true)
    return s.handle
}

/// app_screen(title): a screen to push, not shown yet: fill it, then app_show it.
private let appScreen: Call = { p, n, _, _, _, _ in makeScreen(text(p, n)).handle }

/// app_show(nav, screen): SCREEN pushed onto NAV, laid out first.
private let appShow: Call = { h, sh, _, _, _, _ in
    guard let nav = navs[h], let s = screens[sh] else { return 0 }
    nav.push(s, animated: true)
    return 1
}

/// app_pop(nav): back one screen, as the back button does (and the screen's POP follows).
private let appPop: Call = { h, _, _, _, _, _ in
    navs[h]?.pop(animated: true)
    return 1
}

/// app_path_title(screen, term): SCREEN's title as a path, outermost first.
private let appPathTitle: Call = { h, p, n, _, _, _ in
    guard let s = screens[h], let t = termBytes(p, n) else { return 0 }
    let parts = strings(t)
    s.path = parts
    s.title = parts.last ?? ""
    s.barChanged()
    return 1
}

private let appSetTitle: Call = { h, p, n, _, _, _ in
    guard let s = screens[h] else { return 0 }
    s.title = text(p, n)
    s.path = nil
    s.barChanged()
    return 1
}

private func addBarItem(_ s: MacScreen, _ item: MacBarItem, right: Bool) {
    barItems[item.handle] = item
    if right { s.right.append(item) } else { s.left.append(item) }
    s.barChanged()
}

/// app_bar_button(screen, symbol-or-title, right): a button in SCREEN's bar -- an SF Symbol when one is named, else
/// text.  A click is a TAP with the button's handle.
private let appBarButton: Call = { h, p, n, right, _, _ in
    guard let s = screens[h] else { return 0 }
    let bh = newHandle()
    let name = text(p, n)
    let button = barButton(symbol: NSImage(systemSymbolName: name, accessibilityDescription: nil) == nil ? nil : name,
                           title: name) { send("TAP", [.int(Int(bh))]) }
    addBarItem(s, MacBarItem(handle: bh, button: button), right: right != 0)
    return bh
}

/// app_bar_menu(screen, plist): a bar button that opens a menu -- (:SYMBOL :RIGHT :ITEMS); a choice is a MENU event
/// with the button and the item's id.
private let appBarMenu: Call = { h, p, n, _, _, _ in
    guard let s = screens[h], let t = termBytes(p, n) else { return 0 }
    let m = plist(t)
    let bh = newHandle()
    let button = barButton(symbol: str(m["SYMBOL"]) ?? "ellipsis", title: "Menu", nil)
    button.popMenu = barMenu(bh, m["ITEMS"])
    addBarItem(s, MacBarItem(handle: bh, button: button), right: flag(m["RIGHT"]))
    return bh
}

/// app_bar_menu_set(button, items): the menu's items again (a checkmark moved).
private let appBarMenuSet: Call = { h, p, n, _, _, _ in
    barItems[h]?.button.popMenu = barMenu(h, termBytes(p, n))
    return 1
}

private let appBarButtonEnable: Call = { h, on, _, _, _, _ in
    barItems[h]?.button.isEnabled = on != 0
    return 1
}

// MARK: - the compose bar

/// A text field and a send button along a screen's bottom -- what a chat is written in.  Send (or Return) is a
/// COMPOSE event with the text, and the field is emptied.  The screen's tables end above it.
private final class MacComposer: FillView, NSTextFieldDelegate {
    let handle: UInt
    let field = NSTextField()
    let rule = FillView()
    var sendButton: MacButton!

    init(handle: UInt, placeholder: String) {
        self.handle = handle
        super.init(frame: .zero)
        fill = .windowBackgroundColor
        rule.fill = .separatorColor
        field.placeholderString = placeholder
        field.bezelStyle = .roundedBezel
        field.font = .systemFont(ofSize: 13)
        field.delegate = self
        sendButton = barButton(symbol: "arrow.up.circle.fill", title: "Send") { [weak self] in self?.sendText() }
        sendButton.symbolConfiguration = .init(pointSize: 22, weight: .regular)
        sendButton.showsBorderOnlyWhileMouseInside = false
        sendButton.isBordered = false
        for v in [rule, field, sendButton!] { addSubview(v) }
    }

    required init?(coder: NSCoder) { fatalError("not from a nib") }

    override func setFrameSize(_ newSize: NSSize) {
        super.setFrameSize(newSize)
        rule.frame = CGRect(x: 0, y: 0, width: newSize.width, height: 1)
        field.frame = CGRect(x: 12, y: (newSize.height - 28) / 2, width: max(0, newSize.width - 12 - 48), height: 28)
        sendButton.frame = CGRect(x: newSize.width - 42, y: (newSize.height - 32) / 2, width: 32, height: 32)
    }

    func sendText() {
        let t = field.stringValue.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !t.isEmpty else { return }
        field.stringValue = ""
        send("COMPOSE", [.int(Int(handle)), .string(t)])
    }

    func control(_ control: NSControl, textView: NSTextView, doCommandBy selector: Selector) -> Bool {
        guard selector == #selector(NSResponder.insertNewline(_:)) else { return false }
        sendText()
        return true
    }
}

/// app_compose(screen, placeholder): a compose bar along SCREEN's bottom.  Returns its handle, the COMPOSE events'.
private let appCompose: Call = { h, p, n, _, _, _ in
    guard let s = screens[h] else { return 0 }
    let ch = newHandle()
    let bar = MacComposer(handle: ch, placeholder: text(p, n))
    s.composer?.removeFromSuperview()
    s.composer = bar
    s.view.addSubview(bar)
    s.relayout()
    composers[ch] = bar
    return ch
}

/// app_table_scroll_end(table, animated): TABLE scrolled to its last row.
private let appTableScrollEnd: Call = { h, animated, _, _, _, _ in
    guard let source = tables[h] else { return 0 }
    source.scrollToEnd(animated: animated != 0)
    return 1
}

// MARK: - tables

private struct MacAction {
    let title: String
    let destructive: Bool
    let tint: NSColor?
}

private struct MacRow {
    let title: String
    let subtitle: String?
    let detail: String?
    let image: String?
    let tint: NSColor?
    let chevron: Bool
    let section: String?
    let index: Int
    let sort: String?
    let group: Int?
    let keys: [String: Term]
    let bar: Double?
    let button: String?
    let italic: Bool
    let strong: String?
    let trim: String?
    let actions: [MacAction]?
    let dot: NSColor?
    let menu: [MenuChoice]
}

/// What a table shows where it has no rows: the system's loading view, or a message with an image and a button.
private struct Placeholder {
    var loading = false
    var title: String?
    var message: String?
    var image: String?
    var button: String?
}

private enum Shown {
    case header(String)
    case row(MacRow)
    case footer
}

/// The fill behind a row that shows its share: the label's colour, faint, FRACTION of the width.
private final class ShareFill: NSView {
    var fraction = 0.0
    override func draw(_ dirtyRect: NSRect) {
        NSColor.labelColor.withAlphaComponent(0.08).setFill()
        NSRect(x: 0, y: 0, width: bounds.width * fraction, height: bounds.height).fill()
    }
}

private final class DotView: NSView {
    var color: NSColor = .systemGreen
    override var intrinsicContentSize: NSSize { NSSize(width: 8, height: 8) }
    override func draw(_ dirtyRect: NSRect) {
        color.setFill()
        NSBezierPath(ovalIn: bounds).fill()
    }
}

/// A table that chooses a row on Return, as a click does.
private final class MacTableView: NSTableView {
    var onReturn: ((Int) -> Void)?
    override func keyDown(with event: NSEvent) {
        if (event.keyCode == 36 || event.keyCode == 76), selectedRow >= 0 {
            onReturn?(selectedRow)
            return
        }
        super.keyDown(with: event)
    }
}

/// What lies over an empty table: a spinner or an image, a title, a message, a button.  Only the button takes the
/// mouse; a click or a right click anywhere else goes to the table under it.
private final class MacOverlay: FlippedView {
    let stack = NSStackView()
    var button: MacButton?

    override init(frame: NSRect) {
        super.init(frame: frame)
        stack.orientation = .vertical
        stack.alignment = .centerX
        stack.spacing = 8
        stack.translatesAutoresizingMaskIntoConstraints = false
        addSubview(stack)
        NSLayoutConstraint.activate([
            stack.centerXAnchor.constraint(equalTo: centerXAnchor),
            stack.centerYAnchor.constraint(equalTo: centerYAnchor),
            stack.widthAnchor.constraint(lessThanOrEqualTo: widthAnchor, constant: -40),
        ])
    }

    required init?(coder: NSCoder) { fatalError("not from a nib") }

    override func hitTest(_ point: NSPoint) -> NSView? {
        guard !isHidden, let b = button, let hit = super.hitTest(point) else { return nil }
        return hit === b || hit.isDescendant(of: b) ? hit : nil
    }

    func show(spinner: Bool = false, image: String? = nil, title: String? = nil, message: String? = nil,
              button: String? = nil, tap: (() -> Void)? = nil) {
        for v in stack.arrangedSubviews {
            stack.removeArrangedSubview(v)
            v.removeFromSuperview()
        }
        if spinner {
            let s = NSProgressIndicator()
            s.style = .spinning
            s.controlSize = title == nil ? .regular : .small
            s.startAnimation(nil)
            stack.addArrangedSubview(s)
        }
        if let image, let i = NSImage(systemSymbolName: image, accessibilityDescription: nil) {
            let v = NSImageView(image: i)
            v.symbolConfiguration = .init(pointSize: 40, weight: .regular)
            v.contentTintColor = .secondaryLabelColor
            stack.addArrangedSubview(v)
        }
        if let title {
            stack.addArrangedSubview(label(title, size: 17, bold: true))
        }
        if let message {
            let m = NSTextField(wrappingLabelWithString: message)
            m.font = .systemFont(ofSize: 13)
            m.textColor = .secondaryLabelColor
            m.alignment = .center
            m.preferredMaxLayoutWidth = 360
            stack.addArrangedSubview(m)
        }
        self.button = nil
        if let button {
            let b = pushButton(button, tap)
            b.keyEquivalent = ""
            stack.addArrangedSubview(b)
            self.button = b
        }
        isHidden = false
    }
}

private final class MacTableBox: FlippedView {
    weak var owner: MacTable?
    override func setFrameSize(_ newSize: NSSize) {
        super.setFrameSize(newSize)
        owner?.layoutBox()
    }
}

/// A table: its rows (in sections), their order and filter, their actions and menus, what it shows with none.
private final class MacTable: NSObject, NSTableViewDataSource, NSTableViewDelegate, NSMenuDelegate, NSSearchFieldDelegate {
    let handle: UInt
    let box = MacTableBox()
    let scroll = NSScrollView()
    let table = MacTableView()
    let overlay = MacOverlay()
    let refreshSpinner = NSProgressIndicator()
    var groups: [(title: String?, rows: [MacRow])] = []
    var shown: [Shown] = []
    /// Every row, in the order they came (a row's INDEX is its place here, what a SELECT reports).
    var all: [MacRow] = []
    var actions: [MacAction] = []
    var empty: Placeholder?
    var placeholder: Placeholder?
    var loading = false
    var loadingSince = Date()
    var refreshable = false
    var spaceMenu: [MenuChoice] = []
    var orderKey: String?
    var descending = false
    var groupsFirst = true
    private(set) var filter = ""
    var search: NSSearchField?
    let infoLabel = NSTextField(wrappingLabelWithString: "")
    var infoParts: [NSAttributedString] = []
    var optionsButton: MacButton?
    var readme: (title: String, body: NSAttributedString)?
    var lastSelected: Int?
    var marked = false

    init(handle: UInt, plain: Bool) {
        self.handle = handle
        super.init()
        let column = NSTableColumn(identifier: NSUserInterfaceItemIdentifier("row"))
        column.resizingMask = .autoresizingMask
        table.addTableColumn(column)
        table.headerView = nil
        table.style = plain ? .plain : .inset
        table.columnAutoresizingStyle = .uniformColumnAutoresizingStyle
        table.backgroundColor = .clear
        table.usesAutomaticRowHeights = false
        table.floatsGroupRows = false
        table.dataSource = self
        table.delegate = self
        table.target = self
        table.action = #selector(clicked(_:))
        table.onReturn = { [weak self] r in self?.choose(r) }
        let menu = NSMenu()
        menu.delegate = self
        menu.autoenablesItems = false
        table.menu = menu
        scroll.documentView = table
        scroll.hasVerticalScroller = true
        scroll.autohidesScrollers = true
        scroll.drawsBackground = false
        refreshSpinner.style = .spinning
        refreshSpinner.controlSize = .small
        refreshSpinner.isDisplayedWhenStopped = false
        overlay.isHidden = true
        box.owner = self
        for v in [scroll, overlay, refreshSpinner] { box.addSubview(v) }
    }

    // MARK: layout

    private var headerShown: Bool { search != nil && !(all.isEmpty && infoParts.isEmpty) }
    private var readmeWidth: CGFloat = 0

    func layoutBox() {
        let b = box.bounds
        var y: CGFloat = 0
        let inset: CGFloat = 14
        let showHeader = headerShown
        search?.isHidden = !showHeader
        optionsButton?.isHidden = !showHeader
        infoLabel.isHidden = true
        if let search, showHeader {
            let text = infoText(width: b.width - 2 * inset)
            var infoH: CGFloat = 0
            if let text {
                infoLabel.attributedStringValue = text
                infoLabel.isHidden = false
                infoH = ceil(text.boundingRect(with: CGSize(width: b.width - 2 * inset, height: 10_000),
                                               options: [.usesLineFragmentOrigin, .usesFontLeading]).height) + 14
                infoLabel.frame = CGRect(x: inset, y: 10, width: b.width - 2 * inset, height: infoH - 12)
            }
            let room: CGFloat = optionsButton == nil ? 0 : 34
            optionsButton?.frame = CGRect(x: inset - 4, y: infoH + 6, width: 30, height: 24)
            search.frame = CGRect(x: inset + room, y: infoH + 6, width: max(0, b.width - 2 * inset - room), height: 24)
            y = infoH + 38
        }
        scroll.frame = CGRect(x: 0, y: y, width: b.width, height: max(0, b.height - y))
        overlay.frame = scroll.frame
        refreshSpinner.frame = CGRect(x: (b.width - 16) / 2, y: y + 8, width: 16, height: 16)
        table.sizeLastColumnToFit()
        if readme != nil, abs(readmeWidth - b.width) > 1, case .footer? = shown.last {
            readmeWidth = b.width
            table.noteHeightOfRows(withIndexesChanged: [shown.count - 1])
        }
    }

    // MARK: the header: a few lines about the folder, the options button, the filter

    /// A search field heads the table; what is typed in it filters the rows by name, as they come too.
    func enableSearch() {
        let field = NSSearchField()
        field.placeholderString = "Filter"
        field.delegate = self
        search = field
        infoLabel.font = .systemFont(ofSize: 11)
        infoLabel.textColor = .secondaryLabelColor
        infoLabel.addGestureRecognizer(NSClickGestureRecognizer(target: self, action: #selector(infoTapped)))
        box.addSubview(infoLabel)
        box.addSubview(field)
        layoutBox()
    }

    @objc func infoTapped() { send("INFO", [.int(Int(handle))]) }

    func controlTextDidChange(_ obj: Notification) {
        guard let search, (obj.object as? NSSearchField) === search else { return }
        filter = search.stringValue
        regroup()
    }

    /// A button to the left of the filter that opens a menu: how the rows are sorted, what is shown.  A choice is
    /// an OPTIONS event with its id.
    func setOptions(_ items: Term?) {
        let b = optionsButton ?? {
            let b = barButton(symbol: "slider.horizontal.3", title: "Sort and View", nil)
            box.addSubview(b)
            optionsButton = b
            return b
        }()
        b.popMenu = barMenu(handle, items, event: "OPTIONS")
        layoutBox()
    }

    /// The info line's parts (TEXT split at its runs of four spaces), [[symbol]]s drawn as SF Symbols; wrapped a
    /// whole part at a time.
    func setInfo(_ text: String) {
        let font = NSFont.systemFont(ofSize: 11)
        infoParts = text.isEmpty ? [] : text.components(separatedBy: "    ").filter { !$0.isEmpty }.map { part in
            let out = NSMutableAttributedString()
            var rest = Substring(part)
            while let open = rest.range(of: "[["), let close = rest.range(of: "]]", range: open.upperBound..<rest.endIndex) {
                out.append(NSAttributedString(string: String(rest[..<open.lowerBound]), attributes: [.font: font]))
                let name = String(rest[open.upperBound..<close.lowerBound])
                let config = NSImage.SymbolConfiguration(pointSize: 10, weight: .regular)
                    .applying(.init(paletteColors: [.secondaryLabelColor]))
                if let image = NSImage(systemSymbolName: name, accessibilityDescription: nil)?.withSymbolConfiguration(config) {
                    let a = NSTextAttachment()
                    a.image = image
                    a.bounds = CGRect(x: 0, y: -2, width: image.size.width, height: image.size.height)
                    out.append(NSAttributedString(attachment: a))
                }
                rest = rest[close.upperBound...]
            }
            out.append(NSAttributedString(string: String(rest), attributes: [.font: font]))
            out.addAttribute(.foregroundColor, value: NSColor.secondaryLabelColor, range: NSRange(location: 0, length: out.length))
            return out
        }
        layoutBox()
        showEmpty()
    }

    private func infoText(width: CGFloat) -> NSAttributedString? {
        guard !infoParts.isEmpty else { return nil }
        let font = NSFont.systemFont(ofSize: 11)
        let gap = NSAttributedString(string: "    ", attributes: [.font: font])
        let gapWidth = gap.size().width
        let out = NSMutableAttributedString()
        var line: CGFloat = 0
        for part in infoParts {
            let w = ceil(part.size().width)
            if out.length > 0 {
                if line + gapWidth + w <= width {
                    out.append(gap)
                    line += gapWidth
                } else {
                    out.append(NSAttributedString(string: "\n", attributes: [.font: font]))
                    line = 0
                }
            }
            out.append(part)
            line += w
        }
        return out
    }

    // MARK: the rows

    private func compareKeys(_ a: Term?, _ b: Term?) -> ComparisonResult {
        switch (a, b) {
        case (.int(let x)?, .int(let y)?): return x < y ? .orderedAscending : x > y ? .orderedDescending : .orderedSame
        case (.string(let x)?, .string(let y)?): return x.localizedStandardCompare(y)
        case (nil, nil): return .orderedSame
        case (nil, _): return .orderedAscending
        case (_, nil): return .orderedDescending
        default: return .orderedSame
        }
    }

    /// By one of the rows' :KEYS, or by :SORT (as the Finder sorts names); either way round; lower :GROUPs first
    /// when GROUPSFIRST.  A tie goes by :SORT, A to Z.
    func ordered(_ rows: [MacRow]) -> [MacRow] {
        rows.sorted { a, b in
            if groupsFirst, let ga = a.group, let gb = b.group, ga != gb { return ga < gb }
            let byName = (a.sort ?? a.title).localizedStandardCompare(b.sort ?? b.title)
            if let key = orderKey {
                let c = compareKeys(a.keys[key], b.keys[key])
                if c != .orderedSame { return descending ? c == .orderedDescending : c == .orderedAscending }
                return byName == .orderedAscending
            }
            return descending ? byName == .orderedDescending : byName == .orderedAscending
        }
    }

    func setRows(_ rows: [MacRow]) {
        all = rows
        loading = false
        regroup()
    }

    func append(_ rows: [MacRow]) {
        all += rows
        regroup()
    }

    func regroup() {
        let visible = filter.isEmpty ? all : all.filter { $0.title.localizedStandardContains(filter) }
        let rows = visible.contains { $0.sort != nil || $0.group != nil } ? ordered(visible) : visible
        var out: [(title: String?, rows: [MacRow])] = []
        for row in rows {
            if let last = out.last, last.title == row.section {
                out[out.count - 1].rows.append(row)
            } else {
                out.append((row.section, [row]))
            }
        }
        groups = out
        rebuildShown()
        table.reloadData()
        if marked { mark(true) }
        showEmpty()
    }

    private var footerLoading: Bool { loading && !all.isEmpty && Date().timeIntervalSince(loadingSince) > 0.4 }
    private var wantsFooter: Bool { footerLoading || readme != nil }

    private func rebuildShown() {
        var s: [Shown] = []
        for g in groups {
            if let t = g.title { s.append(.header(t)) }
            s += g.rows.map { .row($0) }
        }
        if wantsFooter { s.append(.footer) }
        shown = s
    }

    /// While more rows are coming and some are here: a spinner and the count so far, under the last row; else the
    /// README, if there is one.
    func showFooter() {
        var has = false
        if case .footer? = shown.last { has = true }
        if has != wantsFooter {
            rebuildShown()
            table.reloadData()
            if marked { mark(true) }
        } else if has {
            let i = shown.count - 1
            table.noteHeightOfRows(withIndexesChanged: [i])
            table.reloadData(forRowIndexes: [i], columnIndexes: [0])
        }
    }

    func setReadme(_ title: String, _ text: String, markdown: Bool) {
        if text.isEmpty {
            readme = nil
        } else if markdown {
            readme = (title, MacMarkdown.render(text, base: .systemFont(ofSize: 13)))
        } else {
            readme = (title, NSAttributedString(string: text, attributes: [
                .font: NSFont.monospacedSystemFont(ofSize: 12, weight: .regular), .foregroundColor: NSColor.labelColor]))
        }
        rebuildShown()
        table.reloadData()
        if marked { mark(true) }
    }

    func showEmpty() {
        showFooter()
        if search != nil { layoutBox() }
        let none = groups.allSatisfy { $0.rows.isEmpty }
        if none, !filter.isEmpty, !all.isEmpty {
            overlay.show(image: "magnifyingglass", title: "No Results", message: "No names here have “\(filter)” in them.")
        } else if none, let p = placeholder {
            let h = handle
            overlay.show(spinner: p.loading, image: p.loading ? nil : p.image, title: p.title, message: p.message,
                         button: p.button) { send("PLACEHOLDER", [.int(Int(h))]) }
        } else if none && loading {
            overlay.show(spinner: true)
        } else if none, let e = empty {
            overlay.show(image: e.image, title: e.title, message: e.message)
        } else {
            overlay.isHidden = true
        }
    }

    func scrollToEnd(animated: Bool) {
        let n = table.numberOfRows
        guard n > 0 else { return }
        var last = n - 1
        if case .footer = shown[last], last > 0 { last -= 1 }
        guard animated else { table.scrollRowToVisible(last); return }
        let clip = scroll.contentView
        let rect = table.rect(ofRow: last)
        let y = max(0, rect.maxY - clip.bounds.height)
        animate(0.25, { clip.animator().setBoundsOrigin(NSPoint(x: 0, y: y)) }, done: {
            self.scroll.reflectScrolledClipView(clip)
        })
    }

    func beginRefresh() {
        refreshSpinner.startAnimation(nil)
        send("REFRESH", [.int(Int(handle))])
    }

    func endRefresh() {
        refreshSpinner.stopAnimation(nil)
    }

    // MARK: choosing

    @objc func clicked(_ sender: Any?) {
        let r = table.clickedRow
        guard r >= 0 else { return }
        choose(r)
    }

    func choose(_ r: Int) {
        guard r < shown.count, case .row(let row) = shown[r] else { return }
        lastSelected = row.index
        send("SELECT", [.int(Int(handle)), .int(row.index)])
        if !marked { table.deselectAll(nil) }
    }

    /// The row chosen last, shown MARKED while what it opened is beside it (a column, a preview).
    func mark(_ on: Bool) {
        marked = on
        if on, let want = lastSelected, let pos = shown.firstIndex(where: {
            if case .row(let r) = $0 { return r.index == want }
            return false
        }) {
            if table.selectedRow != pos { table.selectRowIndexes([pos], byExtendingSelection: false) }
        } else if table.selectedRow >= 0 {
            table.deselectAll(nil)
        }
    }

    // MARK: data source and delegate

    func numberOfRows(in tableView: NSTableView) -> Int { shown.count }

    func tableView(_ tableView: NSTableView, isGroupRow row: Int) -> Bool {
        if case .header = shown[row] { return true }
        return false
    }

    func tableView(_ tableView: NSTableView, shouldSelectRow row: Int) -> Bool {
        if case .row = shown[row] { return true }
        return false
    }

    func tableView(_ tableView: NSTableView, heightOfRow row: Int) -> CGFloat {
        switch shown[row] {
        case .header: return 28
        case .row(let r): return r.subtitle != nil || (r.chevron && r.detail != nil) ? 44 : 30
        case .footer: return footerHeight()
        }
    }

    private func footerHeight() -> CGFloat {
        if footerLoading { return 44 }
        guard let readme else { return 0 }
        let w = max(1, scroll.bounds.width - 56)
        let h = readme.body.boundingRect(with: CGSize(width: w, height: 1_000_000),
                                         options: [.usesLineFragmentOrigin, .usesFontLeading]).height
        return 50 + ceil(h) + 40
    }

    func tableView(_ tableView: NSTableView, viewFor tableColumn: NSTableColumn?, row: Int) -> NSView? {
        switch shown[row] {
        case .header(let title):
            let cell = NSTableCellView()
            let l = label(title, size: 11, color: .secondaryLabelColor, bold: true)
            l.translatesAutoresizingMaskIntoConstraints = false
            cell.addSubview(l)
            NSLayoutConstraint.activate([
                l.leadingAnchor.constraint(equalTo: cell.leadingAnchor, constant: 8),
                l.trailingAnchor.constraint(lessThanOrEqualTo: cell.trailingAnchor, constant: -8),
                l.bottomAnchor.constraint(equalTo: cell.bottomAnchor, constant: -4),
            ])
            return cell
        case .row(let r):
            return rowCell(r)
        case .footer:
            return footerLoading ? loadingFooter() : readmeFooter()
        }
    }

    private func loadingFooter() -> NSView {
        let cell = NSTableCellView()
        let spinner = NSProgressIndicator()
        spinner.style = .spinning
        spinner.controlSize = .small
        spinner.startAnimation(nil)
        let count = label("\(all.count.formatted()) so far", size: 11, color: .secondaryLabelColor)
        let row = NSStackView(views: [spinner, count])
        row.spacing = 8
        row.translatesAutoresizingMaskIntoConstraints = false
        cell.addSubview(row)
        NSLayoutConstraint.activate([row.centerXAnchor.constraint(equalTo: cell.centerXAnchor),
                                     row.centerYAnchor.constraint(equalTo: cell.centerYAnchor)])
        return cell
    }

    private func readmeFooter() -> NSView {
        let view = FlippedView()
        guard let readme else { return view }
        let w = scroll.bounds.width, inset: CGFloat = 28
        let rule = FillView(frame: CGRect(x: inset, y: 12, width: max(0, w - 2 * inset), height: 1))
        rule.fill = .separatorColor
        rule.autoresizingMask = [.width]
        let heading = label(readme.title, size: 11, color: .secondaryLabelColor)
        heading.frame = CGRect(x: inset, y: 24, width: max(0, w - 2 * inset), height: 16)
        heading.autoresizingMask = [.width]
        let bodyH = footerHeight() - 90
        let body = NSTextView(frame: CGRect(x: inset, y: 50, width: max(0, w - 2 * inset), height: bodyH))
        body.isEditable = false
        body.isSelectable = true
        body.drawsBackground = false
        body.textContainerInset = .zero
        body.textContainer?.lineFragmentPadding = 0
        body.textStorage?.setAttributedString(readme.body)
        body.autoresizingMask = [.width]
        for v in [rule, heading, body] { view.addSubview(v) }
        return view
    }

    /// A row as the phone's cell has it: its icon, its title (italic, or its start strong), its subtitle (a path
    /// at its end cut from the front to fit), and at its end a status dot, a button, a detail or a chevron.
    private func rowCell(_ r: MacRow) -> NSView {
        let cell = NSTableCellView()
        if let bar = r.bar {
            let fill = ShareFill(frame: cell.bounds)
            fill.fraction = bar
            fill.autoresizingMask = [.width, .height]
            cell.addSubview(fill)
        }
        var parts: [NSView] = []
        if let name = r.image, let image = NSImage(systemSymbolName: name, accessibilityDescription: nil) {
            let v = NSImageView(image: image)
            v.symbolConfiguration = .init(pointSize: 15, weight: .regular)
            v.contentTintColor = r.tint ?? accent
            v.widthAnchor.constraint(equalToConstant: 22).isActive = true
            v.setContentHuggingPriority(.required, for: .horizontal)
            parts.append(v)
        }
        let title = label(r.title)
        if r.italic { title.font = NSFontManager.shared.convert(.systemFont(ofSize: 13), toHaveTrait: .italicFontMask) }
        if let strong = r.strong, r.title.hasPrefix(strong) {
            let s = NSMutableAttributedString(string: r.title, attributes: [
                .font: NSFont.systemFont(ofSize: 11), .foregroundColor: NSColor.secondaryLabelColor])
            s.addAttributes([.font: title.font ?? .systemFont(ofSize: 13), .foregroundColor: NSColor.labelColor],
                            range: NSRange(location: 0, length: (strong as NSString).length))
            title.attributedStringValue = s
        }
        title.setContentCompressionResistancePriority(.defaultLow, for: .horizontal)
        let column = NSStackView(views: [title])
        column.orientation = .vertical
        column.alignment = .width
        column.spacing = 2
        // a chevron and a detail: the detail goes into the subtitle's line
        var subtitle = r.subtitle
        if r.chevron, let d = r.detail, r.dot == nil, r.button == nil {
            subtitle = [r.subtitle, d].compactMap { $0 }.joined(separator: " · ")
        }
        if let sub = subtitle {
            if let trim = r.trim, sub.hasSuffix(trim), sub.count > trim.count {
                let head = label(String(sub.dropLast(trim.count)), size: 11, color: .secondaryLabelColor)
                head.setContentHuggingPriority(.required, for: .horizontal)
                head.setContentCompressionResistancePriority(.defaultHigh, for: .horizontal)
                let path = label(trim, size: 11, color: .secondaryLabelColor)
                path.lineBreakMode = .byTruncatingHead
                path.setContentCompressionResistancePriority(.defaultLow - 1, for: .horizontal)
                path.setContentHuggingPriority(.defaultLow, for: .horizontal)
                let line = NSStackView(views: [head, path])
                line.orientation = .horizontal
                line.spacing = 0
                line.distribution = .fill
                column.addArrangedSubview(line)
            } else {
                let l = label(sub, size: 11, color: .secondaryLabelColor)
                l.setContentCompressionResistancePriority(.defaultLow, for: .horizontal)
                column.addArrangedSubview(l)
            }
        }
        column.setHuggingPriority(.defaultLow - 1, for: .horizontal)
        column.setContentCompressionResistancePriority(.defaultLow, for: .horizontal)
        parts.append(column)
        func chevron() -> NSView {
            let v = NSImageView(image: NSImage(systemSymbolName: "chevron.forward", accessibilityDescription: nil) ?? NSImage())
            v.symbolConfiguration = .init(pointSize: 11, weight: .semibold)
            v.contentTintColor = .tertiaryLabelColor
            v.setContentHuggingPriority(.required, for: .horizontal)
            return v
        }
        if let dot = r.dot {
            // a status dot (a server with a connection open), before the chevron if there is one
            let mark = DotView()
            mark.color = dot
            mark.setContentHuggingPriority(.required, for: .horizontal)
            parts.append(mark)
            if r.chevron { parts.append(chevron()) }
        } else if let title = r.button {
            let index = r.index, h = handle
            let b = pushButton(title) { send("SELECT", [.int(Int(h)), .int(index)]) }
            b.controlSize = .small
            b.setContentHuggingPriority(.required, for: .horizontal)
            parts.append(b)
        } else {
            if let d = r.detail, !r.chevron {
                let l = label(d, size: 12, color: .secondaryLabelColor)
                l.setContentHuggingPriority(.required, for: .horizontal)
                l.setContentCompressionResistancePriority(.defaultHigh, for: .horizontal)
                parts.append(l)
            }
            if r.chevron { parts.append(chevron()) }
        }
        let row = NSStackView(views: parts)
        row.orientation = .horizontal
        row.alignment = .centerY
        row.distribution = .fill
        row.spacing = 8
        row.translatesAutoresizingMaskIntoConstraints = false
        cell.addSubview(row)
        NSLayoutConstraint.activate([
            row.leadingAnchor.constraint(equalTo: cell.leadingAnchor, constant: 6),
            row.trailingAnchor.constraint(equalTo: cell.trailingAnchor, constant: -8),
            row.centerYAnchor.constraint(equalTo: cell.centerYAnchor),
        ])
        return cell
    }

    // MARK: actions: the trackpad's swipe, and the right click

    private func actionsOf(_ r: MacRow) -> [MacAction] { r.actions ?? actions }

    func tableView(_ tableView: NSTableView, rowActionsForRow row: Int,
                   edge: NSTableView.RowActionEdge) -> [NSTableViewRowAction] {
        guard edge == .trailing, row < shown.count, case .row(let r) = shown[row] else { return [] }
        let h = handle, index = r.index
        return actionsOf(r).enumerated().map { i, a in
            let action = NSTableViewRowAction(style: a.destructive ? .destructive : .regular, title: a.title) { _, _ in
                send("ROWACTION", [.int(Int(h)), .list([.int(i), .int(index)])])
                tableView.rowActionsVisible = false
            }
            if let tint = a.tint { action.backgroundColor = tint }
            return action
        }
    }

    /// A right click: on a row, its own menu (a MENU with (row id)) and its swipe actions (ROWACTION); off the rows,
    /// the table's own menu (MENU with (-1 id)); and Refresh, where the table refreshes.
    func menuNeedsUpdate(_ menu: NSMenu) {
        menu.removeAllItems()
        let h = handle
        let r = table.clickedRow
        if r >= 0, r < shown.count, case .row(let row) = shown[r] {
            for c in row.menu {
                menu.addItem(menuItem(c.title, image: c.image, destructive: c.destructive) {
                    send("MENU", [.int(Int(h)), .list([.int(row.index), .int(c.id)])])
                })
            }
            let acts = actionsOf(row)
            if !acts.isEmpty && !menu.items.isEmpty { menu.addItem(.separator()) }
            for (i, a) in acts.enumerated() {
                menu.addItem(menuItem(a.title, destructive: a.destructive) {
                    send("ROWACTION", [.int(Int(h)), .list([.int(i), .int(row.index)])])
                })
            }
        } else {
            for c in spaceMenu {
                menu.addItem(menuItem(c.title, image: c.image, destructive: c.destructive) {
                    send("MENU", [.int(Int(h)), .list([.int(-1), .int(c.id)])])
                })
            }
        }
        if refreshable {
            if !menu.items.isEmpty { menu.addItem(.separator()) }
            let item = menuItem("Refresh", image: "arrow.clockwise") { [weak self] in self?.beginRefresh() }
            item.keyEquivalent = "r"
            item.keyEquivalentModifierMask = .command
            menu.addItem(item)
        }
    }
}

private func makeTable(plain: Bool) -> MacTable {
    let source = MacTable(handle: newHandle(), plain: plain)
    tables[source.handle] = source
    return source
}

/// app_table(screen, plain): a table filling SCREEN -- inset, or plain when PLAIN is 1.  Its handle.
private let appTable: Call = { h, plain, _, _, _, _ in
    guard let s = screens[h] else { return 0 }
    let source = makeTable(plain: plain != 0)
    s.view.addSubview(source.box, positioned: .below, relativeTo: s.composer)
    s.tableHandles.append(source.handle)
    s.relayout()
    return source.handle
}

private func macRow(_ item: Term, _ index: Int) -> MacRow {
    let m = plist(item)
    return MacRow(title: str(m["TITLE"]) ?? "", subtitle: str(m["SUBTITLE"]), detail: str(m["DETAIL"]),
                  image: str(m["IMAGE"]), tint: color(m["TINT"]), chevron: flag(m["CHEVRON"]),
                  section: str(m["SECTION"]), index: index, sort: str(m["SORT"]),
                  group: { if case .int(let g)? = m["GROUP"] { return g }; return nil }(),
                  keys: m["KEYS"].map(plist) ?? [:],
                  bar: { if case .int(let b)? = m["BAR"] { return Double(min(1000, max(0, b))) / 1000 }; return nil }(),
                  button: str(m["BUTTON"]), italic: flag(m["ITALIC"]), strong: str(m["STRONG"]), trim: str(m["TRIM"]),
                  actions: m["ACTIONS"].map { list in (list.listItems ?? []).map(macAction) },
                  dot: color(m["DOT"]), menu: menuChoices(m["MENU"]))
}

private func macAction(_ item: Term) -> MacAction {
    let a = plist(item)
    return MacAction(title: str(a["TITLE"]) ?? "", destructive: flag(a["DESTRUCTIVE"]), tint: color(a["TINT"]))
}

/// app_table_set(table, rows): ROWS a list of plists (:TITLE and the rest, as on the phone); a row's index is its
/// place in the list.
private let appTableSet: Call = { h, p, n, _, _, _ in
    guard let source = tables[h] else { return 0 }
    let items = termBytes(p, n)?.listItems ?? []
    source.setRows(items.enumerated().map { i, item in macRow(item, i) })
    return 1
}

/// app_table_append(table, rows): ROWS after the table's own; their indices go on from there.
private let appTableAppend: Call = { h, p, n, _, _, _ in
    guard let source = tables[h] else { return 0 }
    let base = source.all.count
    let items = termBytes(p, n)?.listItems ?? []
    source.append(items.enumerated().map { i, item in macRow(item, base + i) })
    return 1
}

/// app_table_actions(table, actions): a row's actions -- :TITLE, :DESTRUCTIVE, :TINT -- each a ROWACTION with
/// (action row).
private let appTableActions: Call = { h, p, n, _, _, _ in
    guard let source = tables[h] else { return 0 }
    source.actions = (termBytes(p, n)?.listItems ?? []).map(macAction)
    return 1
}

/// app_table_refresh(table): Command-R (or Refresh in the right-click menu) is a REFRESH event;
/// app_table_refresh_end(table) puts the spinner away.
private let appTableRefresh: Call = { h, _, _, _, _, _ in
    guard let source = tables[h] else { return 0 }
    source.refreshable = true
    return 1
}

private let appTableRefreshEnd: Call = { h, _, _, _, _, _ in
    tables[h]?.endRefresh()
    return 1
}

/// app_table_empty(table, plist): what the table shows with no rows -- (:TITLE :MESSAGE :IMAGE).
private let appTableEmpty: Call = { h, p, n, _, _, _ in
    guard let source = tables[h], let t = termBytes(p, n) else { return 0 }
    let m = plist(t)
    source.empty = Placeholder(loading: false, title: str(m["TITLE"]), message: str(m["MESSAGE"]), image: str(m["IMAGE"]))
    source.showEmpty()
    return 1
}

/// app_table_loading(table, on): while ON and the table has no rows, a spinner where its empty view would be --
/// after 150 ms, so a quick load does not flash one; with rows, the count so far under them after 0.4 s.
private let appTableLoading: Call = { h, on, _, _, _, _ in
    guard let source = tables[h] else { return 0 }
    source.loading = on != 0
    if on != 0 {
        source.loadingSince = Date()
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.45) { [weak source] in
            if let source, source.loading { source.showFooter() }
        }
        if source.placeholder == nil { source.overlay.isHidden = true }
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.15) { [weak source] in
            if let source, source.loading { source.showEmpty() }
        }
    } else {
        source.showEmpty()
    }
    return 1
}

/// app_table_placeholder(table, plist): while TABLE has no rows, what it waits on or why it has nothing --
/// (:LOADING t :TITLE :MESSAGE), or (:TITLE :MESSAGE :IMAGE :BUTTON), its button a PLACEHOLDER event.  NIL: none.
private let appTablePlaceholder: Call = { h, p, n, _, _, _ in
    guard let source = tables[h] else { return 0 }
    guard let t = termBytes(p, n), !(t.listItems ?? []).isEmpty else {
        source.placeholder = nil
        source.showEmpty()
        return 1
    }
    let m = plist(t)
    source.placeholder = Placeholder(loading: flag(m["LOADING"]), title: str(m["TITLE"]), message: str(m["MESSAGE"]),
                                     image: str(m["IMAGE"]), button: str(m["BUTTON"]))
    source.showEmpty()
    return 1
}

/// app_table_order(table, plist): how TABLE's rows go -- :KEY, :DESCENDING, :GROUPS-FIRST.
private let appTableOrder: Call = { h, p, n, _, _, _ in
    guard let source = tables[h], let t = termBytes(p, n) else { return 0 }
    let m = plist(t)
    source.orderKey = str(m["KEY"])
    source.descending = flag(m["DESCENDING"])
    source.groupsFirst = flag(m["GROUPS-FIRST"])
    source.regroup()
    return 1
}

/// app_table_menu(table, items): the menu of a right click off the rows; a choice is a MENU event, (-1 id).
private let appTableMenu: Call = { h, p, n, _, _, _ in
    guard let source = tables[h] else { return 0 }
    source.spaceMenu = menuChoices(termBytes(p, n))
    return 1
}

/// app_table_info(table, text): lines about what the table shows, above its filter; a click on them is INFO.
private let appTableInfo: Call = { h, p, n, _, _, _ in
    tables[h]?.setInfo(text(p, n))
    return 1
}

/// app_table_options(table, items): a button beside the filter that opens a menu; a choice is an OPTIONS event.
private let appTableOptions: Call = { h, p, n, _, _, _ in
    tables[h]?.setOptions(termBytes(p, n))
    return 1
}

/// app_table_readme(table, term): (title text markdown) -- a README under the rows.  An empty text: none.
private let appTableReadme: Call = { h, p, n, _, _, _ in
    guard let source = tables[h], let items = termBytes(p, n)?.listItems, items.count >= 2 else { return 0 }
    var md = false
    if items.count > 2, case .int(let v) = items[2] { md = v != 0 }
    source.setReadme(str(items[0]) ?? "", str(items[1]) ?? "", markdown: md)
    return 1
}

// MARK: - the hex view

/// A hex dump: offset, the bytes in hex, the same as text -- 16 to a line where there is room, else 8.  It holds a
/// WINDOW of the file, at most a megabyte, from BASE: scrolled near either end, ONWANT asks for the part beyond
/// (the app reads it and hands it over), and the far end goes.  For a file bigger than the window, a slider at its
/// right stands for the whole file: dragged, only the part there is read.
private final class MacHexView: FlippedView, NSTableViewDataSource, NSTableViewDelegate, NSMenuDelegate {
    static let part = 65536
    static let cap = 1 << 20
    private(set) var bytes = Data()
    private(set) var base: Int64 = 0
    var total: Int64 = 0
    var onWant: ((Int, Int64) -> Void)?
    var onEdit: ((Int64, Data) -> Void)?
    private var asked = false
    private var perLine = 16
    private let font = NSFont.monospacedSystemFont(ofSize: 11.5, weight: .regular)
    let scroll = NSScrollView()
    let table = NSTableView()
    let slider = NSSlider()
    private var dragging = false
    private var lastJump: CFTimeInterval = 0
    private var jumpTo: Int64?
    private let rowHeight: CGFloat = 17
    private var observer: NSObjectProtocol?

    var big: Bool { total > Int64(MacHexView.cap) }

    override init(frame: NSRect) {
        super.init(frame: frame)
        table.addTableColumn(NSTableColumn(identifier: NSUserInterfaceItemIdentifier("hex")))
        table.headerView = nil
        table.rowHeight = rowHeight
        table.intercellSpacing = .zero
        table.selectionHighlightStyle = .none
        table.backgroundColor = .clear
        table.style = .plain
        table.columnAutoresizingStyle = .uniformColumnAutoresizingStyle
        table.dataSource = self
        table.delegate = self
        let menu = NSMenu()
        menu.delegate = self
        table.menu = menu
        scroll.documentView = table
        scroll.hasVerticalScroller = true
        scroll.drawsBackground = false
        scroll.contentView.postsBoundsChangedNotifications = true
        observer = NotificationCenter.default.addObserver(forName: NSView.boundsDidChangeNotification,
                                                          object: scroll.contentView, queue: nil) { [weak self] _ in
            self?.scrolled()
        }
        slider.isVertical = true
        slider.minValue = 0
        slider.maxValue = 1
        slider.doubleValue = 1
        slider.isContinuous = true
        slider.target = self
        slider.action = #selector(slid(_:))
        slider.isHidden = true
        addSubview(scroll)
        addSubview(slider)
    }

    required init?(coder: NSCoder) { fatalError("not from a nib") }

    deinit {
        if let observer { NotificationCenter.default.removeObserver(observer) }
    }

    override func setFrameSize(_ newSize: NSSize) {
        super.setFrameSize(newSize)
        relayout()
    }

    private func relayout() {
        let w = bounds.width, h = bounds.height
        slider.isHidden = !big
        slider.frame = CGRect(x: w - 22, y: 4, width: 20, height: max(0, h - 8))
        scroll.frame = CGRect(x: 0, y: 0, width: big ? w - 24 : w, height: h)
        table.sizeLastColumnToFit()
        // as many to a line as fit: 9 for the offset, 3 a byte, 1, 1 a byte
        let char = ("0" as NSString).size(withAttributes: [.font: font]).width
        let fit = (w - 24 - (big ? 24 : 0)) / max(1, char)
        let want = fit >= 10 + 16 * 4 ? 16 : 8
        if want != perLine {
            let top = topOffset
            perLine = want
            table.reloadData()
            scrollTo(CGFloat((top - base) / Int64(perLine)) * rowHeight)
        }
    }

    var lines: Int { (bytes.count + perLine - 1) / perLine }

    private var clipY: CGFloat { scroll.contentView.bounds.origin.y }

    private func scrollTo(_ y: CGFloat) {
        let maxY = max(0, CGFloat(lines) * rowHeight - scroll.contentView.bounds.height)
        scroll.contentView.scroll(to: NSPoint(x: 0, y: max(0, min(y, maxY))))
        scroll.reflectScrolledClipView(scroll.contentView)
    }

    func start(_ first: Data, total: Int64) {
        base = 0
        bytes = first
        self.total = total
        asked = false
        jumpTo = nil
        relayout()
        table.reloadData()
        scrollTo(0)
        placeThumb()
        DispatchQueue.main.async { [weak self] in self?.checkEdges() }
    }

    /// The app read a part of the file: MODE as asked (3: bytes from START rewritten, an edit saved).
    func received(mode: Int, start: Int64, target: Int64, _ data: Data) {
        switch mode {
        case 0:                                                  // a jump: a new window, TARGET's line on top
            guard dragging || jumpTo == target else { return }
            if jumpTo == target { jumpTo = nil }
            asked = jumpTo != nil || dragging
            if data.isEmpty, start < total { total = start }
            base = start
            bytes = data
            let row = max(0, Int(min(target, base + Int64(bytes.count) - 1) - base) / perLine)
            table.reloadData()
            scrollTo(CGFloat(row) * rowHeight)
        case 1:                                                  // the part after: onto the end
            guard jumpTo == nil, start == base + Int64(bytes.count) else { return }
            asked = false
            if data.isEmpty { total = start }
            bytes.append(data)
            var shift = 0
            if bytes.count > MacHexView.cap {                     // the start goes, whole parts of it
                let drop = (bytes.count - MacHexView.cap + MacHexView.part - 1) / MacHexView.part * MacHexView.part
                bytes = bytes.subdata(in: bytes.startIndex + drop ..< bytes.endIndex)
                base += Int64(drop)
                shift = -drop / perLine
            }
            reload(shiftingRows: shift)
        case 2:                                                  // the part before: onto the start
            guard jumpTo == nil, start + Int64(data.count) == base else { return }
            asked = false
            bytes = data + bytes
            base = start
            if bytes.count > MacHexView.cap {                     // the end goes
                let keep = MacHexView.cap / MacHexView.part * MacHexView.part
                bytes = bytes.subdata(in: bytes.startIndex ..< bytes.startIndex + keep)
            }
            reload(shiftingRows: data.count / perLine)
        case 3:                                                  // rewritten in place
            let lo = max(start, base), hi = min(start + Int64(data.count), base + Int64(bytes.count))
            guard lo < hi else { return }
            let from = bytes.startIndex + Int(lo - base)
            bytes.replaceSubrange(from ..< from + Int(hi - lo),
                                  with: data.subdata(in: data.startIndex + Int(lo - start) ..< data.startIndex + Int(hi - start)))
            reload(shiftingRows: 0)
        default: break
        }
        placeThumb()
        DispatchQueue.main.async { [weak self] in self?.checkEdges() }
    }

    private func reload(shiftingRows shift: Int) {
        let y = clipY + CGFloat(shift) * rowHeight
        table.reloadData()
        scrollTo(y)
    }

    /// The file offset of the line at the top.
    var topOffset: Int64 {
        let row = max(0, Int(clipY / rowHeight))
        return min(base + Int64(row * perLine), max(0, total - 1))
    }

    private func placeThumb() {
        guard big, !dragging else { return }
        slider.doubleValue = 1 - Double(topOffset) / Double(max(1, total))
    }

    private func scrolled() {
        placeThumb()
        checkEdges()
    }

    /// Near either end of the window, the part beyond it asked for.
    private func checkEdges() {
        guard !asked, !dragging, lines > 0 else { return }
        let visible = table.rows(in: table.visibleRect)
        let last = visible.location + visible.length
        if last >= lines - 60, base + Int64(bytes.count) < total {
            asked = true
            onWant?(1, base + Int64(bytes.count))
        } else if visible.location < 60, base > 0 {
            asked = true
            onWant?(2, base)
        }
    }

    @objc func slid(_ sender: Any?) {
        let done = NSApp.currentEvent?.type == .leftMouseUp
        dragging = !done
        scrubbed(to: CGFloat(1 - slider.doubleValue), done: done)
    }

    /// The slider dragged to FRAC of the file: there -- at once, if the window holds it; else the part there,
    /// asked for as the drag goes (at most every tenth of a second) and once more where it ends.
    private func scrubbed(to frac: CGFloat, done: Bool) {
        let target = min(Int64(Double(total) * Double(frac)), max(0, total - 1)) / 16 * 16
        slider.toolTip = String(format: "%08llx", target) + "  ·  \(Int((frac * 100).rounded()))%"
        if target >= base, target < base + Int64(bytes.count) {
            if done { jumpTo = nil; asked = false }
            scrollTo(CGFloat((target - base) / Int64(perLine)) * rowHeight)
            return
        }
        let now = CACurrentMediaTime()
        guard done || now - lastJump > 0.1 else { return }
        lastJump = now
        asked = true
        jumpTo = target
        onWant?(0, target)
    }

    /// Row ROW's file offset and bytes.
    func line(_ row: Int) -> (Int64, Data) {
        let at = min(row * perLine, bytes.count)
        return (base + Int64(at), bytes.subdata(in: bytes.startIndex + at ..< bytes.startIndex + min(at + perLine, bytes.count)))
    }

    func numberOfRows(in tableView: NSTableView) -> Int { lines }

    func tableView(_ tableView: NSTableView, viewFor tableColumn: NSTableColumn?, row: Int) -> NSView? {
        let id = NSUserInterfaceItemIdentifier("hexline")
        let cell = (tableView.makeView(withIdentifier: id, owner: nil) as? NSTableCellView) ?? {
            let c = NSTableCellView()
            c.identifier = id
            let f = NSTextField(labelWithString: "")
            f.frame = CGRect(x: 12, y: 0, width: 100, height: rowHeight)
            f.autoresizingMask = [.width]
            f.lineBreakMode = .byClipping
            c.addSubview(f)
            c.textField = f
            return c
        }()
        let (start, data) = line(row)
        var hex = ""
        var text = ""
        for (i, b) in data.enumerated() {
            hex += String(format: "%02x", b) + (i == perLine / 2 - 1 ? "  " : " ")
            text.append((0x20..<0x7f).contains(b) ? Character(UnicodeScalar(b)) : "·")
        }
        hex = hex.padding(toLength: perLine * 3 + 1, withPad: " ", startingAt: 0)
        let out = NSMutableAttributedString(string: String(format: "%08llx  ", start),
                                            attributes: [.font: font, .foregroundColor: NSColor.tertiaryLabelColor])
        out.append(NSAttributedString(string: hex, attributes: [.font: font, .foregroundColor: NSColor.labelColor]))
        out.append(NSAttributedString(string: text, attributes: [.font: font, .foregroundColor: NSColor.secondaryLabelColor]))
        cell.textField?.attributedStringValue = out
        return cell
    }

    // A right click on a line, for what can be done with it.
    func menuNeedsUpdate(_ menu: NSMenu) {
        menu.removeAllItems()
        let r = table.clickedRow
        guard r >= 0, r < lines else { return }
        let (start, data) = line(r)
        let hex = data.map { String(format: "%02x", $0) }.joined(separator: " ")
        menu.addItem(menuItem("Copy Hex", image: "doc.on.doc") { copyString(hex) })
        menu.addItem(menuItem("Copy Offset", image: "number") { copyString(String(format: "%08llx", start)) })
        menu.addItem(.separator())
        menu.addItem(menuItem("Edit Bytes…", image: "pencil") { [weak self] in self?.onEdit?(start, data) })
    }
}

private func copyString(_ s: String) {
    NSPasteboard.general.clearContents()
    NSPasteboard.general.setString(s, forType: .string)
}

// MARK: - the image viewer

/// A clip view that keeps a document smaller than itself in its middle.
private final class CentringClip: NSClipView {
    override func constrainBoundsRect(_ proposedBounds: NSRect) -> NSRect {
        var r = super.constrainBoundsRect(proposedBounds)
        guard let doc = documentView else { return r }
        if r.width > doc.frame.width { r.origin.x = (doc.frame.width - r.width) / 2 }
        if r.height > doc.frame.height { r.origin.y = (doc.frame.height - r.height) / 2 }
        return r
    }
}

/// An image to look at: fitted and centred, pinched (or double-clicked) closer, scrolled about when closer.
private final class MacImageViewer: NSScrollView {
    let imageView = NSImageView()
    private var fitted = true

    override init(frame: NSRect) {
        super.init(frame: frame)
        contentView = CentringClip()
        documentView = imageView
        imageView.imageScaling = .scaleAxesIndependently
        allowsMagnification = true
        hasVerticalScroller = true
        hasHorizontalScroller = true
        autohidesScrollers = true
        drawsBackground = false
        let double = NSClickGestureRecognizer(target: self, action: #selector(doubleClicked(_:)))
        double.numberOfClicksRequired = 2
        addGestureRecognizer(double)
    }

    required init?(coder: NSCoder) { fatalError("not from a nib") }

    var image: NSImage? {
        get { imageView.image }
        set {
            // its own pixels, a point each, as the phone shows a file's picture
            if let image = newValue, let rep = image.representations.first, rep.pixelsWide > 0, rep.pixelsHigh > 0 {
                image.size = NSSize(width: rep.pixelsWide, height: rep.pixelsHigh)
            }
            imageView.image = newValue
            magnification = 1
            imageView.frame = CGRect(origin: .zero, size: newValue?.size ?? .zero)
            fitted = true
            fit()
        }
    }

    var fitScale: CGFloat {
        guard let size = image?.size, size.width > 0, size.height > 0, bounds.width > 0 else { return 1 }
        return min(bounds.width / size.width, bounds.height / size.height, 1)
    }

    func fit() {
        let f = fitScale
        minMagnification = f
        maxMagnification = max(f * 4, 2)
        if fitted || magnification < f { magnification = f }
    }

    override func setFrameSize(_ newSize: NSSize) {
        super.setFrameSize(newSize)
        fit()
    }

    override func endGesture(with event: NSEvent) {
        super.endGesture(with: event)
        fitted = magnification <= minMagnification + 0.001
    }

    override func magnify(with event: NSEvent) {
        super.magnify(with: event)
        fitted = magnification <= minMagnification + 0.001
    }

    @objc func doubleClicked(_ g: NSClickGestureRecognizer) {
        if magnification > minMagnification + 0.001 {
            animator().magnification = minMagnification
            fitted = true
        } else {
            let p = g.location(in: imageView)
            setMagnification(min(maxMagnification, minMagnification * 2.5), centeredAt: p)
            fitted = false
        }
    }
}

// MARK: - a browser: pages under a bar that holds still

/// One screen whose bar holds still while PAGES -- tables -- slide in and out beneath it: a file browser's folders.
/// As the phone in landscape: the folder and its parent as two columns (one at the first folder), or the folder
/// beside the preview pane.  Back (the chevron, Command-[) slides the top page away with a POP for its table; on
/// the first page, it leaves the browser (a POP for the browser's screen).
private final class MacBrowser: NSObject {
    let handle: UInt
    weak var screen: MacScreen?
    var pages: [(table: UInt, view: NSView, path: [String])] = []
    let pagesView = FlippedView()
    let separator = FillView()

    // THE PREVIEW: one file, in a column beside the folder
    var previewURL: URL?
    var previewName = ""
    var previewTable: UInt?
    let pane = FillView()
    let paneRule = FillView()
    let paneName = NSTextField(labelWithString: "")
    var viewButton: MacButton!
    var shareButton: MacButton!
    var closeButton: MacButton!
    var ql: QLPreviewView?
    let textScroll = NSTextView.scrollableTextView()
    var textView: NSTextView { textScroll.documentView as! NSTextView }
    let imageViewer = MacImageViewer(frame: .zero)
    let hexView = MacHexView(frame: .zero)
    var hexPath: URL?
    var hexTotal: Int64 = 0
    let headerHeight: CGFloat = 40

    enum PreviewKind: UInt { case quickLook = 0, text = 1, image = 2, hex = 3, markdown = 4, log = 5 }

    init(handle: UInt, screen: MacScreen) {
        self.handle = handle
        self.screen = screen
        super.init()
        pagesView.wantsLayer = true
        pagesView.layer?.masksToBounds = true
        separator.fill = .separatorColor
        pagesView.addSubview(separator)
        screen.view.addSubview(pagesView)
        buildPane()
        screen.view.addSubview(pane)
    }

    var paneShown: Bool { previewURL != nil }
    var columns: Int { max(1, min(pages.count, paneShown ? 1 : 2)) }
    var width: CGFloat { pagesView.bounds.width }

    private func buildPane() {
        pane.fill = .controlBackgroundColor
        pane.isHidden = true
        pane.wantsLayer = true
        paneRule.fill = .separatorColor
        paneName.font = .boldSystemFont(ofSize: 13)
        paneName.alignment = .center
        paneName.lineBreakMode = .byTruncatingMiddle
        viewButton = barButton(symbol: "eye", title: "View As", nil)
        viewButton.isHidden = true
        shareButton = barButton(symbol: "square.and.arrow.up", title: "Share") { [weak self] in self?.sharePreview() }
        closeButton = barButton(symbol: "xmark", title: "Close") { [weak self] in self?.closePreview(byUser: true) }
        let tv = textView
        tv.isEditable = false
        tv.isSelectable = true
        tv.drawsBackground = false
        tv.textContainerInset = NSSize(width: 10, height: 12)
        textScroll.drawsBackground = false
        textScroll.isHidden = true
        imageViewer.isHidden = true
        hexView.isHidden = true
        hexView.onWant = { [weak self] mode, offset in
            guard let self else { return }
            send("HEXMORE", [.int(Int(self.handle)), .list([.int(mode), .int(Int(offset))])])
        }
        hexView.onEdit = { [weak self] offset, line in self?.editHex(offset, line) }
        for v in [textScroll, imageViewer, hexView, paneName, viewButton!, shareButton!, closeButton!, paneRule] {
            pane.addSubview(v)
        }
    }

    // MARK: layout

    func layout() {
        guard let screen else { return }
        let b = screen.view.bounds
        if paneShown {
            let listWidth = (b.width * 0.5).rounded()           // two even columns: the folder, the preview
            pagesView.frame = CGRect(x: 0, y: 0, width: listWidth, height: b.height)
            pane.frame = CGRect(x: listWidth, y: 0, width: b.width - listWidth, height: b.height)
        } else {
            pagesView.frame = b
            pane.frame = CGRect(x: b.maxX, y: 0, width: (b.width * 0.5).rounded(), height: b.height)
        }
        layoutPane()
        layoutPages()
    }

    private func layoutPane() {
        let w = pane.bounds.width, h = pane.bounds.height
        paneRule.frame = CGRect(x: 0, y: 0, width: 1, height: h)
        var x = w - 8
        for b in [closeButton!, shareButton!, viewButton!] where !b.isHidden {
            x -= 32
            b.frame = CGRect(x: x, y: (headerHeight - 28) / 2, width: 30, height: 28)
        }
        paneName.frame = CGRect(x: 12, y: (headerHeight - 18) / 2, width: max(0, x - 20), height: 18)
        let body = CGRect(x: 1, y: headerHeight, width: max(0, w - 1), height: max(0, h - headerHeight))
        for v in [textScroll, imageViewer, hexView] as [NSView] { v.frame = body }
        ql?.frame = body
    }

    /// The last COLUMNS pages side by side, the rest hidden to the left; the rule between them; the rows that
    /// opened what is beside them marked; the bar's title over them.
    func layoutPages() {
        let n = columns, w = width / CGFloat(n), h = pagesView.bounds.height
        for (i, page) in pages.enumerated() {
            let slot = i - (pages.count - n)
            page.view.frame = CGRect(x: slot >= 0 ? CGFloat(slot) * w : -w, y: 0, width: w, height: h)
            page.view.isHidden = slot < 0
            tables[page.table]?.mark(i < pages.count - 1 || page.table == previewTable)
        }
        separator.frame = CGRect(x: w, y: 0, width: 1, height: h)
        separator.isHidden = n < 2
        pagesView.addSubview(separator)                         // over the pages
        layoutTitle()
    }

    /// With columns (or the preview), each one's folder over it in the bar; else the path.
    func layoutTitle() {
        guard let screen, let nav = screen.nav, nav.stack.last === screen else { return }
        let title = nav.title
        guard columns > 1 || paneShown, !pages.isEmpty, title.window != nil else {
            title.setSegments([])
            title.subtitleSpan = nil
            return
        }
        let n = columns, w = width / CGFloat(n)
        var items: [(text: String, rect: CGRect, current: Bool)] = []
        title.subtitleSpan = nil
        for slot in 0..<n {
            let page = pages[pages.count - n + slot]
            let column = title.convert(CGRect(x: CGFloat(slot) * w + 8, y: 0, width: w - 16, height: 1), from: pagesView)
            let minX = max(column.minX, 4), maxX = min(column.maxX, title.bounds.width - 4)
            guard maxX - minX > 24 else { continue }
            items.append((page.path.last ?? "/", CGRect(x: minX, y: 0, width: maxX - minX, height: title.bounds.height),
                          slot == n - 1 && !paneShown))
            if slot == n - 1 { title.subtitleSpan = items.last?.rect }
        }
        if paneShown {
            let column = title.convert(CGRect(x: pane.frame.minX + 8, y: 0, width: pane.frame.width - 16, height: 1),
                                       from: screen.view)
            let minX = max(column.minX, 4), maxX = min(column.maxX, title.bounds.width - 4)
            if maxX - minX > 24 {
                items.append((previewName, CGRect(x: minX, y: 0, width: maxX - minX, height: title.bounds.height), true))
            }
        }
        title.setSegments(items)
    }

    // MARK: pages

    func push(_ th: UInt, _ page: NSView, path: [String]) {
        page.frame = pagesView.bounds
        let below = pages.last?.view
        pages.append((th, page, path))
        pagesView.addSubview(page)
        showTitle(path)
        guard let below, pagesView.window != nil else { layoutPages(); return }
        if columns > 1 {
            // a column more: it comes in at the right, and the columns make room
            page.frame = CGRect(x: width, y: 0, width: width / CGFloat(columns), height: pagesView.bounds.height)
            animate(0.3, { self.layoutPages() })
            return
        }
        // one at a time: it slides in over the one before, which gives way a little to the left
        below.frame = pagesView.bounds
        below.isHidden = false
        page.frame.origin.x = width
        let w = width
        animate(0.3, {
            page.animator().setFrameOrigin(.zero)
            below.animator().setFrameOrigin(NSPoint(x: -w * 0.3, y: 0))
        }, done: { self.layoutPages() })
    }

    func setPath(_ th: UInt, _ path: [String]) {
        guard let i = pages.firstIndex(where: { $0.table == th }) else { return }
        pages[i].path = path
        if i == pages.count - 1 { showTitle(path) } else { layoutTitle() }
    }

    /// Back to the page whose table is TH: those after it go, a POP for each.
    func popTo(_ th: UInt) {
        guard let i = pages.firstIndex(where: { $0.table == th }), i < pages.count - 1 else { return }
        let gone = Array(pages[(i + 1)...])
        pages.removeSubrange((i + 1)...)
        if let t = previewTable, gone.contains(where: { $0.table == t }) { previewTable = nil }
        showTitle(pages.last!.path)
        animate(0.25, {
            for page in gone { page.view.animator().alphaValue = 0 }
            self.layoutPages()
        }, done: {
            for page in gone {
                page.view.removeFromSuperview()
                tables[page.table] = nil
                send("POP", [.int(Int(page.table))])
            }
        })
    }

    func back() {
        guard pages.count > 1 else {
            closePreview(byUser: true)
            screen?.nav?.pop(animated: true)
            return
        }
        let top = pages.removeLast()
        if previewTable == top.table { previewTable = nil }
        showTitle(pages.last!.path)
        let w = width
        if columns == 1 {
            let below = pages.last!.view
            below.frame = pagesView.bounds
            below.isHidden = false
            below.setFrameOrigin(NSPoint(x: -w * 0.3, y: 0))
            animate(0.28, {
                top.view.animator().setFrameOrigin(NSPoint(x: w, y: 0))
                below.animator().setFrameOrigin(.zero)
            }, done: {
                top.view.removeFromSuperview()
                tables[top.table] = nil
                send("POP", [.int(Int(top.table))])
                self.layoutPages()
            })
            return
        }
        // a column less: it leaves to the right, and the columns spread again
        animate(0.28, {
            top.view.animator().setFrameOrigin(NSPoint(x: w, y: 0))
            top.view.animator().alphaValue = 0
            self.layoutPages()
        }, done: {
            top.view.removeFromSuperview()
            tables[top.table] = nil
            send("POP", [.int(Int(top.table))])
        })
    }

    func showTitle(_ path: [String]) {
        guard let screen else { return }
        screen.path = path
        screen.title = path.last ?? ""
        screen.barChanged()                                     // the title, the back button, the columns' titles
    }

    // MARK: the preview

    func showPreview(_ url: URL, name: String, kind: PreviewKind) {
        let opening = previewURL == nil
        previewURL = url
        previewName = name
        paneName.stringValue = name
        previewTable = pages.last?.table
        textScroll.isHidden = kind != .text && kind != .markdown && kind != .log
        imageViewer.isHidden = kind != .image
        hexView.isHidden = kind != .hex
        // a hex dump's local copy, or a file's beginning shown as text, is only part of it: nothing to share
        shareButton.isHidden = kind == .hex || url.pathExtension == "part"
        if kind == .quickLook {
            if ql == nil, let v = QLPreviewView(frame: .zero, style: .normal) {
                v.shouldCloseWithWindow = false
                pane.addSubview(v)
                ql = v
            }
            ql?.isHidden = false
            ql?.previewItem = url as NSURL
            ql?.refreshPreviewItem()
        } else {
            ql?.isHidden = true
        }
        let mono = NSFont.monospacedSystemFont(ofSize: 12, weight: .regular)
        switch kind {
        case .hex:
            hexPath = url
            hexView.start((try? Data(contentsOf: url)) ?? Data(), total: hexTotal)
        case .markdown:
            textView.textContainerInset = NSSize(width: 24, height: 18)
            textView.textStorage?.setAttributedString(MacMarkdown.render(readText(url), base: .systemFont(ofSize: 14)))
            textView.scroll(.zero)
        case .text:
            textView.textContainerInset = NSSize(width: 10, height: 12)
            textView.textStorage?.setAttributedString(NSAttributedString(string: readText(url), attributes: [
                .font: mono, .foregroundColor: NSColor.labelColor]))
            textView.scroll(.zero)
        case .log:
            // output as it comes: its end, kept in view as it grows -- unless scrolled up to read
            let clip = textScroll.contentView
            let following = opening || textView.string.isEmpty
                || clip.bounds.maxY >= textView.frame.height - 40
            textView.textContainerInset = NSSize(width: 10, height: 12)
            textView.textStorage?.setAttributedString(NSAttributedString(string: readText(url, tail: true), attributes: [
                .font: mono, .foregroundColor: NSColor.labelColor]))
            if following { textView.scrollToEndOfDocument(nil) }
        case .image:
            imageViewer.image = NSImage(contentsOfFile: url.path)
        case .quickLook:
            break
        }
        guard opening else { layoutPane(); layoutTitle(); return }
        // from past the right edge, to where it rests
        previewURL = nil
        layout()
        pane.isHidden = false
        previewURL = url
        animate(0.3, { self.layout() })
    }

    func closePreview(byUser: Bool) {
        guard previewURL != nil else { return }
        previewURL = nil
        previewTable = nil
        animate(0.25, { self.layout() }, done: {
            if self.previewURL == nil { self.pane.isHidden = true }
        })
        if byUser { send("PREVIEWED", [.int(Int(handle))]) }
    }

    /// A text file's contents to show: UTF-8 (or as near as it decodes), the first 2 MB -- or, TAIL, the last.
    func readText(_ url: URL, tail: Bool = false) -> String {
        guard let handle = try? FileHandle(forReadingFrom: url) else { return "" }
        defer { try? handle.close() }
        if tail, let end = try? handle.seekToEnd(), end > 2_000_000 { try? handle.seek(toOffset: end - 2_000_000) }
        else if tail { try? handle.seek(toOffset: 0) }
        let data = (try? handle.read(upToCount: 2_000_000)) ?? Data()
        return String(data: data, encoding: .utf8) ?? String(decoding: data, as: UTF8.self)
    }

    func hexPart(mode: Int, start: Int64, target: Int64) {
        guard let url = hexPath else { return }
        hexView.received(mode: mode, start: start, target: target, (try? Data(contentsOf: url)) ?? Data())
    }

    /// A hex line's bytes, to change and write back in place: a HEXEDIT with the offset and the text as typed.
    func editHex(_ offset: Int64, _ line: Data) {
        guard let host = hostWindow() else { return }
        let alert = NSAlert()
        alert.messageText = String(format: "Bytes at %08llx", offset)
        alert.informativeText = "\(line.count) bytes, in hex. They are written over the file's own."
        alert.window.appearance = darkAppearance
        let field = NSTextField(frame: CGRect(x: 0, y: 0, width: 380, height: 24))
        field.stringValue = line.map { String(format: "%02x", $0) }.joined(separator: " ")
        field.font = .monospacedSystemFont(ofSize: 12, weight: .regular)
        alert.accessoryView = field
        alert.addButton(withTitle: "Write").hasDestructiveAction = true
        alert.addButton(withTitle: "Cancel")
        alert.beginSheetModal(for: host) { [weak self] response in
            guard let self, response == .alertFirstButtonReturn else { return }
            send("HEXEDIT", [.int(Int(self.handle)), .list([.int(Int(offset)), .string(field.stringValue)])])
        }
        alert.window.makeFirstResponder(field)
    }

    func sharePreview() {
        guard let url = previewURL else { return }
        NSSharingServicePicker(items: [url]).show(relativeTo: shareButton.bounds, of: shareButton, preferredEdge: .minY)
    }

    /// The view button: a menu of the ways to show the file, the one shown checked and its symbol on the button.  A
    /// choice is a VIEWAS event with its index.  Fewer than two: no button.
    func setViews(_ views: [(title: String, symbol: String)], current: Int) {
        viewButton.isHidden = views.count < 2
        viewButton.image = NSImage(systemSymbolName: views.indices.contains(current) ? views[current].symbol : "eye",
                                   accessibilityDescription: "View As")
        let menu = NSMenu()
        menu.autoenablesItems = false
        menu.addItem(.sectionHeader(title: "View As"))
        let h = handle
        for (i, v) in views.enumerated() {
            menu.addItem(menuItem(v.title, image: v.symbol, on: i == current) { send("VIEWAS", [.int(Int(h)), .int(i)]) })
        }
        viewButton.popMenu = menu
        layoutPane()
    }
}

/// app_browser(): a browser screen, not shown yet: push its first page, add its bar buttons, then app_show it.
private let appBrowser: Call = { _, _, _, _, _, _ in
    let s = makeScreen("")
    browsers[s.handle] = MacBrowser(handle: s.handle, screen: s)
    return s.handle
}

/// app_browser_push(browser, path-term, plain): a new page -- a table, with a filter above it -- slid in over the
/// browser's top page, its path in the bar.  Returns the table's handle.
private let appBrowserPush: Call = { h, p, n, plain, _, _ in
    guard let b = browsers[h] else { return 0 }
    let source = makeTable(plain: plain != 0)
    source.enableSearch()
    b.push(source.handle, source.box, path: strings(termBytes(p, n)))
    return source.handle
}

/// app_browser_preview(browser, path, name, kind): the local file PATH in the preview pane -- KIND 0 Quick Look,
/// 1 text, 2 an image, 3 a hex dump, 4 Markdown, 5 a log -- headed NAME.  Its close button is a PREVIEWED.
private let appBrowserPreview: Call = { h, pp, pn, np, nn, kind in
    guard let b = browsers[h] else { return 0 }
    b.showPreview(URL(fileURLWithPath: text(pp, pn)), name: text(np, nn),
                  kind: MacBrowser.PreviewKind(rawValue: kind) ?? .quickLook)
    return 1
}

/// app_browser_preview_close(browser): the pane goes (no PREVIEWED: the app asked).
private let appBrowserPreviewClose: Call = { h, _, _, _, _, _ in
    browsers[h]?.closePreview(byUser: false)
    return 1
}

private let appBrowserPopTo: Call = { h, th, _, _, _, _ in
    browsers[h]?.popTo(th)
    return 1
}

private let appBrowserSetPath: Call = { h, th, p, n, _, _ in
    guard let b = browsers[h] else { return 0 }
    b.setPath(th, strings(termBytes(p, n)))
    return 1
}

/// app_browser_subtitle(browser, text): a line under the bar's path, smaller -- the server's name.
private let appBrowserSubtitle: Call = { h, p, n, _, _, _ in
    guard let s = screens[h], browsers[h] != nil else { return 0 }
    s.subtitle = text(p, n)
    s.barChanged()
    return 1
}

/// app_browser_preview_views(browser, term): ((title symbol) ... current).
private let appBrowserPreviewViews: Call = { h, p, n, _, _, _ in
    guard let items = termBytes(p, n)?.listItems, let last = items.last, case .int(let current) = last else { return 0 }
    let views = items.dropLast().compactMap { t -> (title: String, symbol: String)? in
        guard let v = t.listItems, v.count == 2, let title = str(v[0]), let symbol = str(v[1]) else { return nil }
        return (title, symbol)
    }
    browsers[h]?.setViews(Array(views), current: current)
    return 1
}

private let appBrowserHexTotal: Call = { h, size, _, _, _, _ in
    browsers[h]?.hexTotal = Int64(size)
    return 1
}

/// app_browser_hex_part(browser, mode, start, target): the hex preview's local file holds the part HEXMORE asked for.
private let appBrowserHexPart: Call = { h, mode, start, target, _, _ in
    browsers[h]?.hexPart(mode: Int(mode), start: Int64(start), target: Int64(target))
    return 1
}

/// app_orientation(which): a Mac is always landscape; the request is granted, as the phone's is.
private let appOrientation: Call = { _, _, _, _, _, _ in 1 }

// MARK: - local files

private let appFileStamp: Call = { p, n, _, _, _, _ in
    let attrs = try? FileManager.default.attributesOfItem(atPath: text(p, n))
    return UInt(max(0, (attrs?[.modificationDate] as? Date)?.timeIntervalSince1970 ?? 0))
}

private let appFileSize: Call = { p, n, _, _, _, _ in
    let attrs = try? FileManager.default.attributesOfItem(atPath: text(p, n))
    return UInt((attrs?[.size] as? NSNumber)?.uint64Value ?? 0)
}

private let appSetMtime: Call = { p, n, secs, _, _, _ in
    let date = Date(timeIntervalSince1970: TimeInterval(secs))
    return (try? FileManager.default.setAttributes([.modificationDate: date], ofItemAtPath: text(p, n))) == nil ? 0 : 1
}

/// app_theme(mono): 1, black and white (the label colour, white in the dark) for the bar's buttons and the rows'
/// icons; 0, the system's accent.
private let appTheme: Call = { mono, _, _, _, _, _ in
    monoTheme = mono != 0
    return 1
}

// MARK: - alerts with fields

private final class MacAlert {
    let title: String?
    let message: String?
    var actions: [(title: String, style: UInt)] = []
    var fields: [(placeholder: String, text: String, secure: Bool)] = []

    init(title: String?, message: String?) {
        self.title = title
        self.message = message
    }
}

/// app_alert(title, message, sheet): an alert, not shown yet.  On a Mac an action sheet is an alert too.
private let appAlert: Call = { tp, tn, mp, mn, _, _ in
    let h = newHandle()
    nativeAlerts[h] = MacAlert(title: tn > 0 ? text(tp, tn) : nil, message: mn > 0 ? text(mp, mn) : nil)
    return h
}

/// app_alert_action(alert, title, style): a button -- STYLE 0 default, 1 cancel, 2 destructive.  Returns its index,
/// the ALERT event's first value.
private let appAlertAction: Call = { h, p, n, style, _, _ in
    guard let alert = nativeAlerts[h] else { return 0 }
    alert.actions.append((text(p, n), style))
    return UInt(alert.actions.count - 1)
}

/// app_alert_field(alert, placeholder, text, secure): a text field, with TEXT in it.
private let appAlertField: Call = { h, pp, pn, tp, tn, secure in
    guard let alert = nativeAlerts[h] else { return 0 }
    alert.fields.append((text(pp, pn), text(tp, tn), secure != 0))
    return 1
}

/// app_alert_show(alert): the alert, as a sheet.  A button is an ALERT event with (index text ...): the button's
/// index (in the order they were added) and each field's text.  The Mac's default button is its rightmost: the
/// first added that is not a cancel -- or the cancel, where the only other choice destroys something.
private let appAlertShow: Call = { h, _, _, _, _, _ in
    guard let a = nativeAlerts[h], let host = hostWindow() else { return 0 }
    let alert = NSAlert()
    alert.window.appearance = darkAppearance
    if let title = a.title, !title.isEmpty {
        alert.messageText = title
        alert.informativeText = a.message ?? ""
    } else {
        alert.messageText = a.message ?? ""
    }
    let cancels = a.actions.indices.filter { a.actions[$0].style == 1 }
    let others = a.actions.indices.filter { a.actions[$0].style != 1 }
    let onlyDestructive = !others.isEmpty && others.allSatisfy { a.actions[$0].style == 2 }
    let order = onlyDestructive ? cancels + others : others + cancels
    for (k, i) in order.enumerated() {
        let b = alert.addButton(withTitle: a.actions[i].title)
        if a.actions[i].style == 2 { b.hasDestructiveAction = true }
        if a.actions[i].style == 1 && k > 0 { b.keyEquivalent = "\u{1b}" }
    }
    var fields: [NSTextField] = []
    if !a.fields.isEmpty {
        let stack = NSStackView()
        stack.orientation = .vertical
        stack.spacing = 8
        for f in a.fields {
            let field = f.secure ? NSSecureTextField() : NSTextField()
            field.placeholderString = f.placeholder
            field.stringValue = f.text
            field.translatesAutoresizingMaskIntoConstraints = false
            field.widthAnchor.constraint(equalToConstant: 260).isActive = true
            stack.addArrangedSubview(field)
            fields.append(field)
        }
        stack.frame = CGRect(origin: .zero, size: stack.fittingSize)
        alert.accessoryView = stack
        alert.window.initialFirstResponder = fields.first
    }
    alert.beginSheetModal(for: host) { response in
        let k = response.rawValue - NSApplication.ModalResponse.alertFirstButtonReturn.rawValue
        let index = k >= 0 && k < order.count ? order[k] : 0
        send("ALERT", [.int(Int(h)), .list([.int(index)] + fields.map { Term.string($0.stringValue) })])
        nativeAlerts[h] = nil
    }
    if let first = fields.first { alert.window.makeFirstResponder(first) }
    return 1
}

// MARK: - forms

/// A record edited in a sheet: labelled fields, buttons (a TAP each) under them, a note, Cancel and Save.  Closing
/// it is a FORM event: (1 value ...) or (0).
private final class MacForm {
    enum Kind: UInt { case text = 0, number, secure, url, toggle, choice }
    struct Field {
        let label: String
        let value: String
        let placeholder: String
        let kind: Kind
        let choices: [String]
    }
    let handle: UInt
    let title: String
    let save: String
    var fields: [Field] = []
    var buttons: [(title: String, handle: UInt)] = []
    var note: String?
    var controls: [NSControl] = []
    var sheet: MacSheet?

    init(handle: UInt, title: String, save: String) {
        self.handle = handle
        self.title = title
        self.save = save
    }

    func show() -> Bool {
        let stack = NSStackView()
        stack.orientation = .vertical
        stack.alignment = .leading
        stack.spacing = 16
        stack.edgeInsets = NSEdgeInsets(top: 20, left: 24, bottom: 20, right: 24)
        stack.addArrangedSubview(label(title, size: 15, bold: true))
        var rows: [[NSView]] = []
        controls = []
        for f in fields {
            let name = label(f.label)
            name.alignment = .right
            let control: NSControl
            switch f.kind {
            case .toggle:
                let sw = NSSwitch()
                sw.state = f.value == "1" ? .on : .off
                control = sw
            case .choice:
                let popup = NSPopUpButton(frame: .zero, pullsDown: false)
                popup.addItems(withTitles: f.choices)
                popup.selectItem(at: Int(f.value) ?? 0)
                control = popup
            default:
                let field = f.kind == .secure ? NSSecureTextField() : NSTextField()
                field.stringValue = f.value
                field.placeholderString = f.placeholder
                field.translatesAutoresizingMaskIntoConstraints = false
                // a fixed width: "at least 280" let the grid grow the fields past the sheet's right edge
                field.widthAnchor.constraint(equalToConstant: 280).isActive = true
                control = field
            }
            controls.append(control)
            rows.append([name, control])
        }
        if !rows.isEmpty {
            let grid = NSGridView(views: rows)
            grid.column(at: 0).xPlacement = .trailing
            grid.rowAlignment = .firstBaseline
            grid.rowSpacing = 10
            grid.columnSpacing = 12
            stack.addArrangedSubview(grid)
            // the sheet as wide as the grid, and its margins: a stack does not hold its own width to an arranged
            // view's, and the sheet (sized to the stack) cut the fields off at its right edge
            stack.widthAnchor.constraint(greaterThanOrEqualToConstant: grid.fittingSize.width + 48).isActive = true
        }
        if !buttons.isEmpty {
            let row = NSStackView(views: buttons.map { b in
                let h = b.handle
                return pushButton(b.title) { send("TAP", [.int(Int(h))]) }
            })
            row.spacing = 8
            stack.addArrangedSubview(row)
        }
        if let note, !note.isEmpty {
            let l = NSTextField(wrappingLabelWithString: note)
            l.font = .systemFont(ofSize: 11)
            l.textColor = .secondaryLabelColor
            l.preferredMaxLayoutWidth = 400
            stack.addArrangedSubview(l)
        }
        let cancel = pushButton("Cancel") { [weak self] in self?.close(saved: false) }
        cancel.keyEquivalent = "\u{1b}"
        let ok = pushButton(save) { [weak self] in self?.close(saved: true) }
        ok.keyEquivalent = "\r"
        let bottom = NSStackView()
        bottom.setViews([cancel, ok], in: .trailing)
        bottom.spacing = 8
        stack.addArrangedSubview(bottom)
        bottom.widthAnchor.constraint(equalTo: stack.widthAnchor, constant: -48).isActive = true
        let (content, size) = sheetContent(stack)
        guard let sheet = beginSheet(content, size: CGSize(width: max(size.width, 420), height: size.height)) else { return false }
        sheet.onCancel = { [weak self] in self?.close(saved: false) }
        self.sheet = sheet
        if let first = controls.first(where: { $0 is NSTextField }) { sheet.makeFirstResponder(first) }
        return true
    }

    func value(_ i: Int) -> String {
        switch controls[i] {
        case let sw as NSSwitch: return sw.state == .on ? "1" : "0"
        case let popup as NSPopUpButton: return String(max(0, popup.indexOfSelectedItem))
        case let field as NSTextField: return field.stringValue
        default: return fields[i].value
        }
    }

    func close(saved: Bool) {
        sheet?.makeFirstResponder(nil)                          // the field being edited, ended
        var values: [Term] = [.int(saved ? 1 : 0)]
        if saved { values += fields.indices.map { .string(value($0)) } }
        send("FORM", [.int(Int(handle)), .list(values)])
        if let sheet { endSheet(sheet) }
        forms[handle] = nil
    }
}

/// app_form(title, save): a form, not shown yet.
private let appForm: Call = { tp, tn, sp, sn, _, _ in
    let h = newHandle()
    forms[h] = MacForm(handle: h, title: text(tp, tn), save: sn > 0 ? text(sp, sn) : "Save")
    return h
}

/// app_form_field(form, plist): (:LABEL :VALUE :PLACEHOLDER :KIND :CHOICES), KIND 0 text, 1 number, 2 secure,
/// 3 URL-ish, 4 a switch ("1" or "0"), 5 a choice (the index of the one chosen).  Returns the field's index.
private let appFormField: Call = { h, p, n, _, _, _ in
    guard let form = forms[h], let t = termBytes(p, n) else { return 0 }
    let m = plist(t)
    var kindNo: UInt = 0
    if case .int(let k)? = m["KIND"] { kindNo = UInt(max(0, k)) }
    form.fields.append(.init(label: str(m["LABEL"]) ?? "", value: str(m["VALUE"]) ?? "",
                             placeholder: str(m["PLACEHOLDER"]) ?? "", kind: MacForm.Kind(rawValue: kindNo) ?? .text,
                             choices: strings(m["CHOICES"])))
    return UInt(form.fields.count - 1)
}

/// app_form_button(form, title): a button under the fields; a click is a TAP with its handle.
private let appFormButton: Call = { h, p, n, _, _, _ in
    guard let form = forms[h] else { return 0 }
    let bh = newHandle()
    form.buttons.append((text(p, n), bh))
    return bh
}

private let appFormNote: Call = { h, p, n, _, _, _ in
    forms[h]?.note = text(p, n)
    return 1
}

private let appFormShow: Call = { h, _, _, _, _, _ in
    guard let form = forms[h] else { return 0 }
    return form.show() ? 1 : 0
}

// MARK: - a Lightning invoice

/// The invoice as it is shown to be paid: the amount, the QR code a wallet scans, the bolt11 in small monospaced
/// text, Copy and Open in Wallet (a lightning: URL), and a status line Lisp updates as it polls.  Done closes it:
/// an INVOICE event.
private final class MacInvoice {
    let handle: UInt
    let bolt11: String
    let status = NSTextField(wrappingLabelWithString: "Waiting for payment...")
    var sheet: MacSheet?

    init(handle: UInt, bolt11: String) {
        self.handle = handle
        self.bolt11 = bolt11
    }

    func show(title: String, amount: String) -> Bool {
        let stack = NSStackView()
        stack.orientation = .vertical
        stack.alignment = .centerX
        stack.spacing = 16
        stack.edgeInsets = NSEdgeInsets(top: 20, left: 28, bottom: 20, right: 28)
        stack.addArrangedSubview(label(title, size: 13, color: .secondaryLabelColor, bold: true))
        stack.addArrangedSubview(label(amount, size: 26))
        let qr = NSImageView(image: MacInvoice.qrImage(bolt11.uppercased()) ?? NSImage())
        qr.imageScaling = .scaleProportionallyUpOrDown
        qr.wantsLayer = true
        qr.layer?.backgroundColor = NSColor.white.cgColor
        qr.layer?.magnificationFilter = .nearest
        qr.translatesAutoresizingMaskIntoConstraints = false
        qr.widthAnchor.constraint(equalToConstant: 260).isActive = true
        qr.heightAnchor.constraint(equalToConstant: 260).isActive = true
        stack.addArrangedSubview(qr)
        let code = NSTextField(wrappingLabelWithString: bolt11)
        code.font = .monospacedSystemFont(ofSize: 10, weight: .regular)
        code.textColor = .secondaryLabelColor
        code.alignment = .center
        code.maximumNumberOfLines = 3
        code.lineBreakMode = .byCharWrapping
        code.cell?.truncatesLastVisibleLine = true
        code.isSelectable = true
        code.preferredMaxLayoutWidth = 320
        stack.addArrangedSubview(code)
        let copy = pushButton("Copy") { [weak self] in
            guard let self else { return }
            copyString(self.bolt11)
            self.status.stringValue = "Copied"
        }
        copy.image = NSImage(systemSymbolName: "doc.on.doc", accessibilityDescription: nil)
        copy.imagePosition = .imageLeading
        let open = pushButton("Open in Wallet") { [weak self] in
            guard let self, let url = URL(string: "lightning:" + self.bolt11) else { return }
            if NSWorkspace.shared.urlForApplication(toOpen: url) == nil || !NSWorkspace.shared.open(url) {
                self.status.stringValue = "No wallet on this Mac opens lightning: links. Copy the invoice instead."
            }
        }
        open.image = NSImage(systemSymbolName: "bolt.fill", accessibilityDescription: nil)
        open.imagePosition = .imageLeading
        open.bezelColor = .controlAccentColor
        let buttons = NSStackView(views: [copy, open])
        buttons.spacing = 12
        stack.addArrangedSubview(buttons)
        status.font = .systemFont(ofSize: 12)
        status.textColor = .secondaryLabelColor
        status.alignment = .center
        status.preferredMaxLayoutWidth = 320
        stack.addArrangedSubview(status)
        let done = pushButton("Done") { [weak self] in self?.close() }
        done.keyEquivalent = "\r"
        stack.addArrangedSubview(done)
        let (content, size) = sheetContent(stack)
        guard let sheet = beginSheet(content, size: CGSize(width: max(size.width, 380), height: size.height)) else { return false }
        sheet.onCancel = { [weak self] in self?.close() }
        self.sheet = sheet
        return true
    }

    /// CoreImage's generator, its modules scaled by a whole number (no smoothing) to at least 520 pixels, shown at
    /// 260 points.
    static func qrImage(_ s: String) -> NSImage? {
        guard let f = CIFilter(name: "CIQRCodeGenerator") else { return nil }
        f.setValue(Data(s.utf8), forKey: "inputMessage")
        f.setValue("M", forKey: "inputCorrectionLevel")
        guard let raw = f.outputImage, raw.extent.width > 0 else { return nil }
        let scale = ceil(520 / raw.extent.width)
        let out = raw.transformed(by: CGAffineTransform(scaleX: scale, y: scale))
        guard let cg = CIContext().createCGImage(out, from: out.extent) else { return nil }
        return NSImage(cgImage: cg, size: NSSize(width: 260, height: 260))
    }

    func setStatus(_ text: String, paid: Bool) {
        status.stringValue = text
        if paid {
            status.textColor = .systemGreen
            status.font = .boldSystemFont(ofSize: 13)
        }
    }

    func close() {
        send("INVOICE", [.int(Int(handle))])
        if let sheet { endSheet(sheet) }
        invoices[handle] = nil
    }
}

/// app_invoice(_, plist): show a Lightning invoice -- (:TITLE :AMOUNT :BOLT11).  Returns its handle.
private let appInvoice: Call = { _, p, n, _, _, _ in
    guard let t = termBytes(p, n) else { return 0 }
    let m = plist(t)
    let h = newHandle()
    let inv = MacInvoice(handle: h, bolt11: str(m["BOLT11"]) ?? "")
    guard inv.show(title: str(m["TITLE"]) ?? "Invoice", amount: str(m["AMOUNT"]) ?? "") else { return 0 }
    invoices[h] = inv
    return h
}

/// app_invoice_status(invoice, plist): its status line -- (:TEXT :PAID).
private let appInvoiceStatus: Call = { h, p, n, _, _, _ in
    guard let inv = invoices[h], let t = termBytes(p, n) else { return 0 }
    let m = plist(t)
    var paid = false
    if case .int(let k)? = m["PAID"] { paid = k != 0 }
    inv.setStatus(str(m["TEXT"]) ?? "", paid: paid)
    return 1
}

// MARK: - pictures, small

/// app_thumbnail(path, n, maxSide, buf, cap, bg): the picture at PATH scaled by ImageIO to fit MAXSIDE pixels,
/// over BG, written to BUF as one little-endian word per pixel, #x00RRGGBB.  Returns (W << 16) | H, or 0.
private let appThumbnail: Call = { p, n, maxSide, buf, cap, bg in
    let url = URL(fileURLWithPath: text(p, n))
    guard let src = CGImageSourceCreateWithURL(url as CFURL, nil),
          let img = CGImageSourceCreateThumbnailAtIndex(src, 0, [
              kCGImageSourceCreateThumbnailFromImageAlways: true,
              kCGImageSourceCreateThumbnailWithTransform: true,
              kCGImageSourceThumbnailMaxPixelSize: Int(maxSide)] as CFDictionary)
    else { return 0 }
    let w = img.width, h = img.height
    guard w > 0, h > 0, w < 65536, h < 65536, w * h * 4 <= Int(cap),
          let out = UnsafeMutableRawPointer(bitPattern: buf) else { return 0 }
    guard let ctx = CGContext(data: out, width: w, height: h, bitsPerComponent: 8, bytesPerRow: w * 4,
                              space: CGColorSpaceCreateDeviceRGB(),
                              bitmapInfo: CGImageAlphaInfo.noneSkipFirst.rawValue | CGBitmapInfo.byteOrder32Little.rawValue)
    else { return 0 }
    ctx.setFillColor(red: CGFloat((bg >> 16) & 0xff) / 255, green: CGFloat((bg >> 8) & 0xff) / 255,
                     blue: CGFloat(bg & 0xff) / 255, alpha: 1)
    ctx.fill(CGRect(x: 0, y: 0, width: w, height: h))
    ctx.draw(img, in: CGRect(x: 0, y: 0, width: w, height: h))
    return UInt((w << 16) | h)
}

/// app_image_jpeg(path, n, maxSide, out, outN, quality): the picture scaled to fit MAXSIDE, written to OUT as a JPEG.
private let appImageJPEG: Call = { p, n, maxSide, op, on, quality in
    let url = URL(fileURLWithPath: text(p, n)), out = URL(fileURLWithPath: text(op, on))
    guard let src = CGImageSourceCreateWithURL(url as CFURL, nil),
          let img = CGImageSourceCreateThumbnailAtIndex(src, 0, [
              kCGImageSourceCreateThumbnailFromImageAlways: true,
              kCGImageSourceCreateThumbnailWithTransform: true,
              kCGImageSourceThumbnailMaxPixelSize: Int(maxSide)] as CFDictionary),
          let dst = CGImageDestinationCreateWithURL(out as CFURL, "public.jpeg" as CFString, 1, nil)
    else { return 0 }
    CGImageDestinationAddImage(dst, img, [kCGImageDestinationLossyCompressionQuality: Double(quality) / 100] as CFDictionary)
    guard CGImageDestinationFinalize(dst) else { return 0 }
    let size = (try? FileManager.default.attributesOfItem(atPath: out.path)[.size] as? Int) ?? 0
    return UInt(max(0, size))
}

// MARK: - the status bar

/// What the app is doing, along the bottom of the navigation container: a spinner while there is no measure, a
/// bar filled to the progress (forward only), or a checkmark for a result; the text ("Doing x — how far"); Cancel
/// while it works (a STATUSCANCEL), OK on a result (which clears it).
private final class MacStatusBar: FillView {
    let rule = FillView()
    let icon = NSImageView()
    let spinner = NSProgressIndicator()
    let meter = NSProgressIndicator()
    let text = NSTextField(labelWithString: "")
    let cancel = pushButton("Cancel", nil)
    let ok = pushButton("OK", nil)
    var shown = false
    var clearing: DispatchWorkItem?
    var onCancel: (() -> Void)?
    var onChange: (() -> Void)?
    private var lastFill = 0.0
    private var measured = false

    override init(frame: NSRect) {
        super.init(frame: frame)
        fill = .windowBackgroundColor
        rule.fill = .separatorColor
        spinner.style = .spinning
        spinner.controlSize = .small
        spinner.isDisplayedWhenStopped = false
        meter.style = .bar
        meter.isIndeterminate = false
        meter.minValue = 0
        meter.maxValue = 1
        meter.controlSize = .small
        icon.image = NSImage(systemSymbolName: "checkmark.circle", accessibilityDescription: "Done")
        icon.contentTintColor = .systemGreen
        text.lineBreakMode = .byTruncatingMiddle
        for b in [cancel, ok] { b.controlSize = .small }
        cancel.onClick = { [weak self] in self?.onCancel?() }
        ok.onClick = { [weak self] in
            guard let self else { return }
            self.shown = false
            self.reset()
            self.onChange?()
        }
        for v in [rule, icon, spinner, meter, text, cancel, ok] { addSubview(v) }
    }

    required init?(coder: NSCoder) { fatalError("not from a nib") }

    /// PROGRESS 0-1, or nil: no measure; DONE: a result.
    func show(_ message: String, progress: Double?, done: Bool) {
        let parts = message.components(separatedBy: " — ")
        let s = NSMutableAttributedString(string: parts[0], attributes: [
            .font: NSFont.systemFont(ofSize: 12), .foregroundColor: NSColor.labelColor])
        if parts.count > 1 {
            s.append(NSAttributedString(string: "   " + parts.dropFirst().joined(separator: " — "), attributes: [
                .font: NSFont.systemFont(ofSize: 11), .foregroundColor: NSColor.secondaryLabelColor]))
        }
        text.attributedStringValue = s
        text.toolTip = message
        icon.isHidden = !done
        cancel.isHidden = done
        ok.isHidden = !done
        measured = !done && progress != nil
        meter.isHidden = !measured
        if done {
            spinner.stopAnimation(nil)
        } else if let p = progress {
            spinner.stopAnimation(nil)
            lastFill = max(lastFill, max(0, min(1, p)))
            meter.doubleValue = lastFill
        } else {
            lastFill = 0
            spinner.startAnimation(nil)
        }
        relayout()
    }

    func reset() {
        lastFill = 0
        spinner.stopAnimation(nil)
    }

    override func setFrameSize(_ newSize: NSSize) {
        super.setFrameSize(newSize)
        relayout()
    }

    private func relayout() {
        let w = bounds.width, h = bounds.height
        rule.frame = CGRect(x: 0, y: 0, width: w, height: 1)
        var x: CGFloat = 12
        icon.frame = CGRect(x: x, y: (h - 16) / 2, width: 16, height: 16)
        spinner.frame = icon.frame
        x += 24
        if measured {
            meter.frame = CGRect(x: x, y: (h - 12) / 2, width: 140, height: 12)
            x += 152
        }
        let button = ok.isHidden ? cancel : ok
        button.frame = CGRect(x: w - 84, y: (h - 22) / 2, width: 76, height: 22)
        text.frame = CGRect(x: x, y: (h - 16) / 2, width: max(0, w - 92 - x), height: 16)
    }
}

/// app_status(nav, text, permille, cancellable): what NAV's app is doing, in its status bar: PERMILLE 0-1000 (above,
/// no measure), Cancel when CANCELLABLE (a STATUSCANCEL); not CANCELLABLE, a result.  An empty TEXT takes it away
/// -- a moment later, so a status given straight after does not make the bar go and come back.
private let appStatus: Call = { h, p, n, permille, cancellable, _ in
    guard let nav = navs[h] else { return 0 }
    let s = nav.status
    let message = text(p, n)
    s.clearing?.cancel()
    s.clearing = nil
    if message.isEmpty {
        guard s.shown else { return 1 }
        let clear = DispatchWorkItem { [weak s, weak nav] in
            guard let s, let nav else { return }
            s.clearing = nil
            s.shown = false
            s.reset()
            nav.relayout()
        }
        s.clearing = clear
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.25, execute: clear)
        return 1
    }
    let working = cancellable != 0
    s.onCancel = { send("STATUSCANCEL", [.int(Int(h))]) }
    s.show(message, progress: working && permille <= 1000 ? Double(permille) / 1000 : nil, done: !working)
    if !s.shown {
        s.shown = true
        nav.relayout()
    }
    return 1
}

// MARK: - files in and out

// A name not yet taken in DIR: "a.txt", then "a 2.txt", ...
private func freeFileName(_ name: String, in dir: URL) -> URL {
    let fm = FileManager.default
    var url = dir.appendingPathComponent(name)
    let stem = (name as NSString).deletingPathExtension
    let ext = (name as NSString).pathExtension
    var n = 2
    while fm.fileExists(atPath: url.path) {
        url = dir.appendingPathComponent(ext.isEmpty ? "\(stem) \(n)" : "\(stem) \(n).\(ext)")
        n += 1
    }
    return url
}

/// app_pick_file(): an Open panel for any file, the choice copied into the app's tmp (as the phone's picker copies
/// it) -- a PICKED event with the copy's path, or "" when cancelled.  Returns the picker's handle.
private let appPickFile: Call = { _, _, _, _, _, _ in
    guard let host = hostWindow() else { return 0 }
    let h = newHandle()
    let panel = NSOpenPanel()
    panel.canChooseFiles = true
    panel.canChooseDirectories = false
    panel.allowsMultipleSelection = false
    held[h] = panel
    panel.beginSheetModal(for: host) { response in
        var path = ""
        if response == .OK, let url = panel.url {
            let inbox = FileManager.default.temporaryDirectory.appendingPathComponent("Inbox", isDirectory: true)
            try? FileManager.default.createDirectory(at: inbox, withIntermediateDirectories: true)
            let dest = freeFileName(url.lastPathComponent, in: inbox)
            do {
                try FileManager.default.copyItem(at: url, to: dest)
                path = dest.path
            } catch {
                NSLog("app_pick_file: %@: %@", url.path, "\(error)")
            }
        }
        send("PICKED", [.int(Int(h)), .string(path)])
        held[h] = nil
    }
    return h
}

/// The share menu's delegate: a service chosen and done is SHARE 1; none chosen, or a failure, SHARE 0.
private final class FileShare: NSObject, NSSharingServicePickerDelegate, NSSharingServiceDelegate {
    let handle: UInt
    private var finished = false

    init(_ handle: UInt) {
        self.handle = handle
    }

    func sharingServicePicker(_ picker: NSSharingServicePicker, didChoose service: NSSharingService?) {
        if service == nil { finish(false) }
    }

    func sharingServicePicker(_ picker: NSSharingServicePicker, delegateFor sharingService: NSSharingService)
        -> NSSharingServiceDelegate? { self }

    func sharingService(_ sharingService: NSSharingService, didShareItems items: [Any]) { finish(true) }

    func sharingService(_ sharingService: NSSharingService, didFailToShareItems items: [Any], error: Error) {
        finish(false)
    }

    private func finish(_ done: Bool) {
        guard !finished else { return }
        finished = true
        send("SHARE", [.int(Int(handle)), .int(done ? 1 : 0)])
        held[handle] = nil
    }
}

/// app_share_file(path): the share menu for the file at PATH (AirDrop, Mail, Messages, another app).
private let appShareFile: Call = { p, n, _, _, _, _ in
    guard let host = hostWindow(), let view = host.contentView else { return 0 }
    let h = newHandle()
    let picker = NSSharingServicePicker(items: [URL(fileURLWithPath: text(p, n))])
    let delegate = FileShare(h)
    picker.delegate = delegate
    held[h] = delegate
    // from the front bar's right-hand buttons, where the share button usually is; else the window's middle
    if let nav = frontNav() ?? navs.values.first(where: { $0.window === host }), !nav.rightRow.arrangedSubviews.isEmpty {
        picker.show(relativeTo: nav.rightRow.bounds, of: nav.rightRow, preferredEdge: .minY)
    } else {
        picker.show(relativeTo: CGRect(x: view.bounds.midX, y: view.bounds.midY, width: 1, height: 1), of: view,
                    preferredEdge: .minY)
    }
    return h
}

/// app_preview(path): Quick Look for the local file at PATH, in a sheet with Share and Done.  Done (or Escape) is
/// a PREVIEWED event.  Returns its handle.
private let appPreview: Call = { p, n, _, _, _, _ in
    guard let host = hostWindow(), let ql = QLPreviewView(frame: .zero, style: .normal) else { return 0 }
    let h = newHandle()
    let url = URL(fileURLWithPath: text(p, n))
    let have = host.contentView?.bounds.size ?? CGSize(width: 800, height: 600)
    let size = CGSize(width: min(900, max(480, have.width - 80)), height: min(700, max(360, have.height - 80)))
    let content = FillView()
    content.fill = .windowBackgroundColor
    let barH: CGFloat = 44
    ql.frame = CGRect(x: 0, y: 0, width: size.width, height: size.height - barH)
    ql.autoresizingMask = [.width, .height]
    ql.previewItem = url as NSURL
    let name = label(url.lastPathComponent, bold: true)
    name.frame = CGRect(x: 16, y: size.height - barH + 13, width: size.width - 220, height: 18)
    name.autoresizingMask = [.width, .minYMargin]
    var sheet: MacSheet?
    let finish = {
        guard held[h] != nil else { return }
        held[h] = nil
        ql.close()
        if let sheet { endSheet(sheet) }
        send("PREVIEWED", [.int(Int(h))])
    }
    let share = pushButton("Share…", nil)
    share.onClick = { [weak share] in
        guard let share else { return }
        NSSharingServicePicker(items: [url]).show(relativeTo: share.bounds, of: share, preferredEdge: .minY)
    }
    share.frame = CGRect(x: size.width - 196, y: size.height - barH + 9, width: 90, height: 26)
    share.autoresizingMask = [.minXMargin, .minYMargin]
    let done = pushButton("Done") { finish() }
    done.keyEquivalent = "\r"
    done.frame = CGRect(x: size.width - 100, y: size.height - barH + 9, width: 88, height: 26)
    done.autoresizingMask = [.minXMargin, .minYMargin]
    for v in [ql, name, share, done] as [NSView] { content.addSubview(v) }
    sheet = beginSheet(content, size: size, resizable: true)
    guard let sheet else { return 0 }
    held[h] = sheet
    sheet.onCancel = { finish() }
    return h
}

/// app_utc_offset(): the local time zone's offset from UTC now, in seconds, plus 86400 (a word is unsigned).
private let appUtcOffset: Call = { _, _, _, _, _, _ in UInt(TimeZone.current.secondsFromGMT() + 86400) }

/// app_resolve(name): NAME's IPv4 address by the system's resolver (the one that knows the VPN's DNS and .local
/// names), off the main thread; a RESOLVED event brings the dotted address, or "" when there is none.
private let appResolve: Call = { p, n, _, _, _, _ in
    let name = text(p, n)
    let h = newHandle()
    DispatchQueue.global(qos: .userInitiated).async {
        var hints = addrinfo()
        hints.ai_family = AF_INET
        hints.ai_socktype = SOCK_STREAM
        var res: UnsafeMutablePointer<addrinfo>?
        var out = ""
        if getaddrinfo(name, nil, &hints, &res) == 0, let first = res {
            var addr = first.pointee.ai_addr.withMemoryRebound(to: sockaddr_in.self, capacity: 1) { $0.pointee.sin_addr }
            var buf = [CChar](repeating: 0, count: Int(INET_ADDRSTRLEN))
            if inet_ntop(AF_INET, &addr, &buf, socklen_t(INET_ADDRSTRLEN)) != nil { out = String(cString: buf) }
            freeaddrinfo(res)
        }
        DispatchQueue.main.async { send("RESOLVED", [.int(Int(h)), .string(out)]) }
    }
    return h
}

/// app_copy(text): onto the pasteboard.
private let appCopy: Call = { p, n, _, _, _, _ in
    copyString(text(p, n))
    return 1
}

/// The app's own folder: on a Mac, under Application Support -- the same one Files.swift's files_documents names
/// (~/Documents is the person's, not the app's).
private func documentsFolder() -> URL {
    let support = FileManager.default.urls(for: .applicationSupportDirectory, in: .userDomainMask)[0]
    let url = support.appendingPathComponent("org.modus-lisp.modus/Documents", isDirectory: true)
    try? FileManager.default.createDirectory(at: url, withIntermediateDirectories: true)
    return url
}

/// app_documents(buf, cap): the app's Documents folder, UTF-8 into BUF when it fits; its length.
private let appDocuments: Call = { buf, cap, _, _, _, _ in
    let bytes = Array(documentsFolder().path.utf8)
    if bytes.count <= Int(cap), let out = UnsafeMutableRawPointer(bitPattern: buf) {
        bytes.withUnsafeBytes { out.copyMemory(from: $0.baseAddress!, byteCount: bytes.count) }
    }
    return UInt(bytes.count)
}

// MARK: - Markdown

/// Markdown drawn as attributed text (the phone's Markdown.swift, in AppKit's fonts and colours): Foundation parses
/// it, and this lays out its blocks -- headings, lists, quotes, code, rules, tables a row a line.
private enum MacMarkdown {
    static func render(_ source: String, base: NSFont) -> NSAttributedString {
        let options = AttributedString.MarkdownParsingOptions(
            allowsExtendedAttributes: true, interpretedSyntax: .full, failurePolicy: .returnPartiallyParsedIfPossible)
        guard let parsed = try? AttributedString(markdown: source, options: options) else {
            return NSAttributedString(string: source, attributes: [.font: base, .foregroundColor: NSColor.labelColor])
        }
        let out = NSMutableAttributedString()
        let mono = NSFont.monospacedSystemFont(ofSize: base.pointSize * 0.88, weight: .regular)
        let codeBack = NSColor.labelColor.withAlphaComponent(0.08)
        var lastBlock: Int?
        var lastKinds: [PresentationIntent.Kind] = []
        var lastRow: Int?, lastTable: Int?

        for run in parsed.runs {
            let comps = run.presentationIntent?.components ?? []      // innermost first
            let block = comps.first?.identity
            let kinds = comps.map(\.kind)
            let row = comps.first(where: { switch $0.kind { case .tableRow, .tableHeaderRow: return true; default: return false } })?.identity
            let table = comps.first(where: { if case .table = $0.kind { return true }; return false })?.identity
            let headerRow = kinds.contains(where: { if case .tableHeaderRow = $0 { return true }; return false })

            if block != lastBlock, let r = row, r == lastRow {
                out.append(NSAttributedString(string: "   |   ", attributes: [.font: base, .foregroundColor: NSColor.tertiaryLabelColor]))
                lastBlock = block
            } else if block != lastBlock, row != nil, table != nil, table == lastTable {
                out.append(NSAttributedString(string: "\n", attributes: [.font: base]))
                lastBlock = block
                lastRow = row
            } else if block != lastBlock {
                lastRow = row
                lastTable = table
                if out.length > 0 {
                    let tight = kinds.contains(where: { if case .listItem = $0 { return true }; return false })
                        && lastKinds.contains(where: { if case .listItem = $0 { return true }; return false })
                    out.append(NSAttributedString(string: tight ? "\n" : "\n\n", attributes: [.font: base]))
                }
                if case .listItem(let ordinal)? = kinds.first(where: { if case .listItem = $0 { return true }; return false }) {
                    let ordered = kinds.contains(where: { if case .orderedList = $0 { return true }; return false })
                    out.append(NSAttributedString(string: ordered ? "\(ordinal).\t" : "•\t",
                                                  attributes: [.font: base, .foregroundColor: NSColor.secondaryLabelColor,
                                                               .paragraphStyle: paragraph(kinds, base)]))
                }
                if kinds.contains(where: { if case .thematicBreak = $0 { return true }; return false }) {
                    out.append(NSAttributedString(string: String(repeating: "\u{2500}", count: 16),
                                                  attributes: [.font: base, .foregroundColor: NSColor.separatorColor]))
                }
                lastBlock = block
                lastKinds = kinds
            }
            if kinds.contains(where: { if case .thematicBreak = $0 { return true }; return false }) { continue }

            var text = String(parsed[run.range].characters)
            var attrs: [NSAttributedString.Key: Any] = [.foregroundColor: NSColor.labelColor, .paragraphStyle: paragraph(kinds, base)]
            var font = base
            var traits: NSFontDescriptor.SymbolicTraits = headerRow ? [.bold] : []
            for k in kinds {
                switch k {
                case .header(let level):
                    let scale: CGFloat = level == 1 ? 1.7 : level == 2 ? 1.4 : level == 3 ? 1.2 : 1.05
                    font = .systemFont(ofSize: (base.pointSize * scale).rounded())
                    traits.insert(.bold)
                case .codeBlock:
                    font = mono
                    attrs[.backgroundColor] = codeBack
                    if text.hasSuffix("\n") { text.removeLast() }
                case .blockQuote:
                    attrs[.foregroundColor] = NSColor.secondaryLabelColor
                default: break
                }
            }
            if let inline = run.inlinePresentationIntent {
                if inline.contains(.stronglyEmphasized) { traits.insert(.bold) }
                if inline.contains(.emphasized) { traits.insert(.italic) }
                if inline.contains(.code) {
                    font = .monospacedSystemFont(ofSize: font.pointSize * 0.9, weight: .regular)
                    attrs[.backgroundColor] = codeBack
                }
                if inline.contains(.strikethrough) { attrs[.strikethroughStyle] = NSUnderlineStyle.single.rawValue }
            }
            if !traits.isEmpty {
                let d = font.fontDescriptor.withSymbolicTraits(font.fontDescriptor.symbolicTraits.union(traits))
                font = NSFont(descriptor: d, size: font.pointSize) ?? font
            }
            attrs[.font] = font
            if let link = run.link {
                attrs[.link] = link
                attrs[.underlineStyle] = NSUnderlineStyle.single.rawValue
            }
            if let image = run.imageURL, text.isEmpty { text = "[\(image.lastPathComponent)]" }
            out.append(NSAttributedString(string: text, attributes: attrs))
        }
        return out
    }

    /// A block's indent: a list's or a quote's, by how deep it is.
    private static func paragraph(_ kinds: [PresentationIntent.Kind], _ base: NSFont) -> NSParagraphStyle {
        let p = NSMutableParagraphStyle()
        let lists = kinds.filter { if case .orderedList = $0 { return true }; if case .unorderedList = $0 { return true }; return false }.count
        let quotes = kinds.filter { if case .blockQuote = $0 { return true }; return false }.count
        let step = (base.pointSize * 1.4).rounded()
        let indent = CGFloat(lists) * step + CGFloat(quotes) * step
        p.firstLineHeadIndent = max(0, indent - (lists > 0 ? step : 0))
        p.headIndent = indent
        p.tabStops = [NSTextTab(textAlignment: .left, location: indent, options: [:])]
        p.defaultTabInterval = step
        p.paragraphSpacing = (base.pointSize * 0.25).rounded()
        p.lineSpacing = (base.pointSize * 0.3).rounded()
        return p
    }
}

// MARK: - registration

/// The same 66 names as the phone's installNative (Native.swift).
func installNative() {
    register("app_nav_new", appNavNew)
    register("app_nav_root", appNavRoot)
    register("app_push", appPush)
    register("app_pop", appPop)
    register("app_nav_present", appNavPresent)
    register("app_nav_dismiss", appNavDismiss)
    register("app_compose", appCompose)
    register("app_table_scroll_end", appTableScrollEnd)
    register("app_path_title", appPathTitle)
    register("app_browser", appBrowser)
    register("app_browser_push", appBrowserPush)
    register("app_browser_preview", appBrowserPreview)
    register("app_browser_preview_close", appBrowserPreviewClose)
    register("app_browser_pop_to", appBrowserPopTo)
    register("app_browser_set_path", appBrowserSetPath)
    register("app_browser_subtitle", appBrowserSubtitle)
    register("app_browser_preview_views", appBrowserPreviewViews)
    register("app_browser_hex_total", appBrowserHexTotal)
    register("app_browser_hex_part", appBrowserHexPart)
    register("app_orientation", appOrientation)
    register("app_screen", appScreen)
    register("app_show", appShow)
    register("app_set_title", appSetTitle)
    register("app_bar_button", appBarButton)
    register("app_bar_menu", appBarMenu)
    register("app_bar_menu_set", appBarMenuSet)
    register("app_table_order", appTableOrder)
    register("app_table_menu", appTableMenu)
    register("app_table_info", appTableInfo)
    register("app_table_options", appTableOptions)
    register("app_file_stamp", appFileStamp)
    register("app_file_size", appFileSize)
    register("app_set_mtime", appSetMtime)
    register("app_table_readme", appTableReadme)
    register("app_theme", appTheme)
    register("app_bar_button_enable", appBarButtonEnable)
    register("app_table", appTable)
    register("app_table_set", appTableSet)
    register("app_table_actions", appTableActions)
    register("app_table_refresh", appTableRefresh)
    register("app_table_refresh_end", appTableRefreshEnd)
    register("app_table_empty", appTableEmpty)
    register("app_table_loading", appTableLoading)
    register("app_table_append", appTableAppend)
    register("app_table_placeholder", appTablePlaceholder)
    register("app_alert", appAlert)
    register("app_alert_action", appAlertAction)
    register("app_alert_field", appAlertField)
    register("app_alert_show", appAlertShow)
    register("app_form", appForm)
    register("app_form_field", appFormField)
    register("app_form_button", appFormButton)
    register("app_form_note", appFormNote)
    register("app_form_show", appFormShow)
    register("app_status", appStatus)
    register("app_pick_file", appPickFile)
    register("app_share_file", appShareFile)
    register("app_preview", appPreview)
    register("app_copy", appCopy)
    register("app_invoice", appInvoice)
    register("app_thumbnail", appThumbnail)
    register("app_image_jpeg", appImageJPEG)
    register("app_invoice_status", appInvoiceStatus)
    register("app_utc_offset", appUtcOffset)
    register("app_resolve", appResolve)
    register("app_documents", appDocuments)
}
