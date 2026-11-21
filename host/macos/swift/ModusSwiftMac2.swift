// ModusSwiftMac2.swift — the rest of the phone's components on the Mac (ModusSwiftMac.swift has the controls, lists
// and gestures).  The same names, arguments and events as host/ios/swift/ModusSwift.swift; where a Mac has no such
// thing, the nearest one it has:
//
//   the phone's sheet (modal_*)          a sheet on the modus window, under a bar with Done: Done or Escape is the
//                                        DISMISS the phone sends when its sheet is dragged away
//   tabs, a navigation stack, pages      the same contents, as views a sheet shows (NSTabView; a bar and a stack;
//                                        pages under dots, turned by a click on a dot or a drag)
//   popover, split view                  NSPopover from the source view; NSSplitView in a sheet
//   paging and zoom scroll views         a scroll view that settles on a page; magnification from 1x to 4x
//   tilt parallax                        the mouse: the view leans towards the pointer
//   gravity                              a fall, easing in, to the bottom of its superview
//   the keyboard (avoid, post)           a Mac has no on-screen keyboard: keyboard_post posts the same news
//                                        keyboard_avoid_on listens for, so a story and its test still run
//   input keys                           a row of key buttons under the field
//   document, photo, colour, font        NSOpenPanel (text, images), NSColorPanel, NSFontPanel
//   edge pan                             a drag in from the window's left edge

import AppKit
import UniformTypeIdentifiers

// Views kept beside a component, removed with it (ui_remove): input_keys_on's row.
var attached: [UInt: [NSView]] = [:]
// Where ui_add puts a view's children when they go in a container inside it (ModusSwiftMac.swift's addChild): a
// paging scroll view's document, a blur's flipped content.

var scrollDocuments: [UInt: NSView] = [:]

private let sheetColor = NSColor(white: 0.12, alpha: 1)

private func darkView(_ size: CGSize) -> FlippedView {
    let view = FlippedView(frame: CGRect(origin: .zero, size: size))
    view.wantsLayer = true
    view.layer?.backgroundColor = sheetColor.cgColor
    return view
}

// MARK: - contents shown over the window

// What modal_present, popover_present and split_present show: a modal's content, or a controller (tabs, a navigation
// stack, pages).  Not on the window until presented; its handle is in views, so ui_add adds to it.
private var contents: [UInt: NSView] = [:]
private var sheets: [UInt: NSWindow] = [:]

final class SheetWindow: NSWindow {
    var cancelled: (() -> Void)?
    override var canBecomeKey: Bool { true }
    override func cancelOperation(_ sender: Any?) { cancelled?() }
}

// A sheet on the modus window: the content under a bar with Done.  Done or Escape is a DISMISS event (the phone's
// sheet dragged away); modal_dismiss is not.
private func presentSheet(_ h: UInt, _ content: NSView, _ size: CGSize) -> UInt {
    guard let parent = rootView()?.window, sheets[h] == nil else { return 0 }
    let bar: CGFloat = 36
    let frame = CGRect(x: 0, y: 0, width: size.width, height: size.height + bar)
    let sheet = SheetWindow(contentRect: frame, styleMask: [.titled], backing: .buffered, defer: false)
    sheet.appearance = darkAppearance
    let holder = darkView(frame.size)
    let done = NSButton(title: "Done", target: nil, action: nil)
    done.bezelStyle = .rounded
    done.frame = CGRect(x: size.width - 88, y: 6, width: 80, height: 24)
    content.frame = CGRect(x: 0, y: bar, width: size.width, height: size.height)
    content.autoresizingMask = [.width, .height]
    holder.addSubview(content)
    holder.addSubview(done)
    sheet.contentView = holder
    let close = { [weak sheet] in
        guard let sheet = sheet, sheets[h] != nil else { return }
        sheets[h] = nil
        parent.endSheet(sheet)
        send("DISMISS", [.int(Int(h))])
    }
    wire(done, h) { _ in close() }
    sheet.cancelled = close
    sheets[h] = sheet
    parent.beginSheet(sheet)
    return 1
}

// modal_new(): a sheet's content, not yet shown; its children come with ui_add.
let modalNew: Call = { _, _, _, _, _, _ in
    let view = darkView(CGSize(width: 440, height: 380))
    let h = newHandle()
    views[h] = view
    contents[h] = view
    return h
}

// modal_present(handle, large): the sheet, of a phone's medium height or (large != 0) its full height.
let modalPresent: Call = { h, large, _, _, _, _ in
    guard let content = contents[h] else { return 0 }
    return presentSheet(h, content, large != 0 ? CGSize(width: 520, height: 620) : CGSize(width: 440, height: 380))
}

// modal_dismiss(handle): the sheet goes away (no event).
let modalDismiss: Call = { h, _, _, _, _, _ in
    guard let sheet = sheets[h], let parent = sheet.sheetParent else { return 0 }
    sheets[h] = nil
    parent.endSheet(sheet)
    return 0
}

// popover_present(content, source): the content in a popover from the source view.
let popoverPresent: Call = { h, source, _, _, _, _ in
    guard let content = contents[h], let anchor = views[source] else { return 0 }
    let controller = NSViewController()
    controller.view = content
    let popover = NSPopover()
    popover.contentViewController = controller
    popover.contentSize = CGSize(width: min(content.frame.width, 360), height: min(content.frame.height, 300))
    popover.behavior = .transient
    popover.appearance = NSAppearance(named: .darkAqua)
    popover.show(relativeTo: anchor.bounds, of: anchor, preferredEdge: .maxY)
    keep(h, popover)
    return 1
}

// split_present(primary, secondary): two contents side by side in a sheet; returns its handle (modal_dismiss takes it).
let splitPresent: Call = { primary, secondary, _, _, _, _ in
    guard let a = contents[primary], let b = contents[secondary] else { return 0 }
    let split = NSSplitView(frame: CGRect(x: 0, y: 0, width: 760, height: 480))
    split.isVertical = true
    split.dividerStyle = .thin
    split.addArrangedSubview(a)
    split.addArrangedSubview(b)
    split.setPosition(280, ofDividerAt: 0)
    let h = newHandle()
    views[h] = split
    contents[h] = split
    return presentSheet(h, split, split.frame.size) == 0 ? 0 : h
}

// MARK: - controllers, shown as sheets

// tab_controller_new(titles, length): tabs, one per title, each tab's content a view (tab_content).  A tab picked
// is a SELECT event with its index.  Shown with modal_present.
final class Tabs: NSObject, NSTabViewDelegate {
    let handle: UInt
    var ready = false

    init(_ handle: UInt) {
        self.handle = handle
    }

    func tabView(_ tabView: NSTabView, didSelect item: NSTabViewItem?) {
        guard ready, let item = item else { return }
        send("SELECT", [.int(Int(handle)), .int(tabView.indexOfTabViewItem(item))])
    }
}

private var tabContents: [UInt: [UInt]] = [:]

let tabControllerNew: Call = { p, n, _, _, _, _ in
    let tabView = NSTabView(frame: CGRect(x: 0, y: 0, width: 440, height: 380))
    let h = newHandle()
    let delegate = Tabs(h)
    tabView.delegate = delegate
    var pages: [UInt] = []
    for title in titlesOf(p, n) {
        let item = NSTabViewItem(identifier: title)
        item.label = title
        let page = darkView(.zero)
        item.view = page
        tabView.addTabViewItem(item)
        let ph = newHandle()
        views[ph] = page
        pages.append(ph)
    }
    delegate.ready = true
    keep(h, delegate)
    views[h] = tabView
    contents[h] = tabView
    tabContents[h] = pages
    return h
}

// tab_content(controller, index): a tab's view, to add to with ui_add.
let tabContentOf: Call = { h, i, _, _, _, _ in
    tabContents[h]?[Int(i)] ?? 0
}

// A navigation stack: the bar (ModusSwiftMac.swift's NavBar) over the top page; its back button pops.
final class NavStack: FlippedView {
    let bar = NavBar(frame: CGRect(x: 0, y: 0, width: 440, height: 44))
    var pages: [NSView] = []

    override init(frame: NSRect) {
        super.init(frame: frame)
        wantsLayer = true
        layer?.backgroundColor = sheetColor.cgColor
        bar.frame = CGRect(x: 0, y: 0, width: frame.width, height: 44)
        bar.autoresizingMask = [.width]
        addSubview(bar)
    }

    required init?(coder: NSCoder) { fatalError("not from a nib") }

    func push(_ page: NSView, _ title: String) {
        pages.last?.isHidden = true
        page.frame = CGRect(x: 0, y: 44, width: bounds.width, height: max(0, bounds.height - 44))
        page.autoresizingMask = [.width, .height]
        addSubview(page)
        pages.append(page)
        bar.items.append((title: title, right: ""))
        bar.refresh()
    }

    func pop() {
        guard pages.count > 1 else { return }
        pages.removeLast().removeFromSuperview()
        pages.last?.isHidden = false
        bar.items.removeLast()
        bar.refresh()
    }
}

private var navRoots: [UInt: UInt] = [:]

// nav_controller_new(): a navigation stack whose first page is its root (nav_root).  Shown with modal_present.
let navControllerNew: Call = { _, _, _, _, _, _ in
    let stack = NavStack(frame: CGRect(x: 0, y: 0, width: 440, height: 380))
    let h = newHandle()
    let root = darkView(.zero)
    let rh = newHandle()
    views[rh] = root
    stack.push(root, "start")
    wire(stack.bar.back, h) { _ in stack.pop() }
    views[h] = stack
    contents[h] = stack
    navRoots[h] = rh
    return h
}

// nav_root(controller): the root page's view.
let navRoot: Call = { h, _, _, _, _, _ in
    navRoots[h] ?? 0
}

// nav_push_view(controller, title, length): pushes a page titled so; returns its view's handle.
let navPushView: Call = { h, p, n, _, _, _ in
    guard let stack = views[h] as? NavStack else { return 0 }
    let page = darkView(.zero)
    let ph = newHandle()
    views[ph] = page
    stack.push(page, text(p, n))
    return ph
}

// Pages under a row of dots: a click on a dot, or a drag across, turns the page; a page shown is a PAGE event.
final class Pager: FlippedView {
    var pages: [NSView] = []
    let dots = PageDots(frame: .zero)
    var turned: ((Int) -> Void)?

    func show(_ i: Int) {
        guard i >= 0, i < pages.count else { return }
        for (j, page) in pages.enumerated() { page.isHidden = j != i }
        dots.current = i
        dots.needsDisplay = true
    }

    @objc func dragged(_ recognizer: NSPanGestureRecognizer) {
        guard recognizer.state == .ended else { return }
        let dx = recognizer.translation(in: self).x
        guard abs(dx) > 40 else { return }
        let next = dots.current + (dx < 0 ? 1 : -1)
        guard next >= 0, next < pages.count else { return }
        show(next)
        turned?(next)
    }
}

private var pageContents: [UInt: [UInt]] = [:]

// page_controller_new(count): count pages, each a view (page_content).  Shown with modal_present.
let pageControllerNew: Call = { count, _, _, _, _, _ in
    let size = CGSize(width: 440, height: 380)
    let pager = Pager(frame: CGRect(origin: .zero, size: size))
    pager.wantsLayer = true
    pager.layer?.backgroundColor = sheetColor.cgColor
    let h = newHandle()
    var handles: [UInt] = []
    for _ in 0..<Int(count) {
        let page = darkView(CGSize(width: size.width, height: size.height - 32))
        page.autoresizingMask = [.width, .height]
        pager.addSubview(page)
        pager.pages.append(page)
        let ph = newHandle()
        views[ph] = page
        handles.append(ph)
    }
    pager.dots.count = Int(count)
    let dotsWidth = CGFloat(count) * 16 + 8
    pager.dots.frame = CGRect(x: (size.width - dotsWidth) / 2, y: size.height - 28, width: dotsWidth, height: 24)
    pager.dots.autoresizingMask = [.minXMargin, .maxXMargin, .minYMargin]
    pager.addSubview(pager.dots)
    let turned = { (i: Int) in send("PAGE", [.int(Int(h)), .int(i)]) }
    pager.turned = turned
    pager.dots.picked = { i in
        pager.show(i)
        turned(i)
    }
    pager.addGestureRecognizer(NSPanGestureRecognizer(target: pager, action: #selector(Pager.dragged(_:))))
    pager.show(0)
    views[h] = pager
    contents[h] = pager
    pageContents[h] = handles
    return h
}

// page_content(controller, index): a page's view, to add to with ui_add.
let pageContent: Call = { h, i, _, _, _, _ in
    pageContents[h]?[Int(i)] ?? 0
}

// MARK: - paging and zoom

// A paging scroll view settles on the nearest page when a scroll ends, and sends PAGE with its index.
final class Paging: NSObject {
    let handle: UInt
    let width: CGFloat
    weak var scroll: NSScrollView?

    init(_ handle: UInt, _ width: CGFloat, _ scroll: NSScrollView) {
        self.handle = handle
        self.width = width
        self.scroll = scroll
    }

    @objc func ended(_ note: Notification) {
        guard let scroll = scroll, width > 0 else { return }
        let page = Int((scroll.contentView.bounds.origin.x / width).rounded())
        NSAnimationContext.runAnimationGroup { context in
            context.duration = 0.2
            scroll.contentView.animator().setBoundsOrigin(NSPoint(x: CGFloat(page) * width, y: 0))
        }
        scroll.reflectScrolledClipView(scroll.contentView)
        send("PAGE", [.int(Int(handle)), .int(page)])
    }
}

// paging_new(pages, x, y, w, h): pages as wide as the view side by side (ui_add puts each page's content in place,
// at its own x); a page that comes to rest is a PAGE event.
let pagingNew: Call = { pages, x, y, w, h, _ in
    let scroll = NSScrollView(frame: rect(x, y, w, h))
    scroll.hasHorizontalScroller = true
    scroll.autohidesScrollers = true
    scroll.drawsBackground = false
    let document = FlippedView(frame: CGRect(x: 0, y: 0, width: CGFloat(w) * CGFloat(pages), height: CGFloat(h)))
    scroll.documentView = document
    let handle = adopt(scroll)
    scrollDocuments[handle] = document
    let target = Paging(handle, CGFloat(w), scroll)
    NotificationCenter.default.addObserver(target, selector: #selector(Paging.ended(_:)),
                                           name: NSScrollView.didEndLiveScrollNotification, object: scroll)
    keep(handle, target)
    return handle
}

// The magnification when a pinch ends is a ZOOM event, in percent.
final class Zooming: NSObject {
    let handle: UInt

    init(_ handle: UInt) {
        self.handle = handle
    }

    @objc func ended(_ note: Notification) {
        guard let scroll = note.object as? NSScrollView else { return }
        send("ZOOM", [.int(Int(handle)), .int(Int(scroll.magnification * 100))])
    }
}

private var zoomContents: [UInt: UInt] = [:]

// zoom_scroll_new(x, y, w, h): a scroll view whose content (zoom_content) zooms from 1x to 4x (a trackpad's pinch).
let zoomScrollNew: Call = { x, y, w, h, _, _ in
    let scroll = NSScrollView(frame: rect(x, y, w, h))
    scroll.allowsMagnification = true
    scroll.minMagnification = 1
    scroll.maxMagnification = 4
    scroll.hasVerticalScroller = true
    scroll.hasHorizontalScroller = true
    scroll.autohidesScrollers = true
    let content = FlippedView(frame: CGRect(x: 0, y: 0, width: CGFloat(w), height: CGFloat(h)))
    content.wantsLayer = true
    content.layer?.backgroundColor = NSColor(white: 0.2, alpha: 1).cgColor
    scroll.documentView = content
    let handle = adopt(scroll)
    let ch = newHandle()
    views[ch] = content
    zoomContents[handle] = ch
    let target = Zooming(handle)
    NotificationCenter.default.addObserver(target, selector: #selector(Zooming.ended(_:)),
                                           name: NSScrollView.didEndLiveMagnifyNotification, object: scroll)
    keep(handle, target)
    return handle
}

// zoom_content(scroll): the zooming content's view, to add to with ui_add.
let zoomContent: Call = { h, _, _, _, _, _ in
    zoomContents[h] ?? 0
}

// MARK: - images and labels

// A shape drawn into an image: an oval (kind 0) or a rounded square (1), filled in rgb, size points square.
private func shapeImage(_ kind: UInt, _ rgb: UInt, _ size: CGFloat) -> NSImage {
    NSImage(size: CGSize(width: size, height: size), flipped: false) { frame in
        colorOf(rgb).setFill()
        (kind == 0 ? NSBezierPath(ovalIn: frame)
                   : NSBezierPath(roundedRect: frame, xRadius: size / 4, yRadius: size / 4)).fill()
        return true
    }
}

// drawn_image_new(kind, rgb, size, x, y): an image drawn here.
let drawnImageNew: Call = { kind, rgb, size, x, y, _ in
    let s = CGFloat(size)
    let view = NSImageView(image: shapeImage(kind, rgb, s))
    view.frame = CGRect(x: CGFloat(x), y: CGFloat(y), width: s, height: s)
    return adopt(view)
}

// file_image_new(kind, rgb, size, x, y): the same shape saved as a PNG in the temporary folder, shown from the file.
let fileImageNew: Call = { kind, rgb, size, x, y, _ in
    let s = CGFloat(size)
    let path = NSTemporaryDirectory() + "storybook-shape.png"
    if let tiff = shapeImage(kind, rgb, s).tiffRepresentation, let bitmap = NSBitmapImageRep(data: tiff),
       let png = bitmap.representation(using: .png, properties: [:]) {
        try? png.write(to: URL(fileURLWithPath: path))
    }
    let view = NSImageView(frame: CGRect(x: CGFloat(x), y: CGFloat(y), width: s, height: s))
    view.image = NSImage(contentsOfFile: path)
    return adopt(view)
}

// attributed_label_new(text, length, x, y, w, h): underlined and tinted, as attributed text.
let attributedLabelNew: Call = { p, n, x, y, w, h in
    let label = plainLabel("", rect(x, y, w, h))
    label.attributedStringValue = NSAttributedString(string: text(p, n), attributes: [
        .underlineStyle: NSUnderlineStyle.single.rawValue,
        .foregroundColor: NSColor.systemTeal,
        .font: NSFont.systemFont(ofSize: 18),
    ])
    return adopt(label)
}

// dynamic_label_new(text, length, x, y, w, h): a label in a text style (title 1), as the phone's Dynamic Type.
let dynamicLabelNew: Call = { p, n, x, y, w, h in
    let label = plainLabel(text(p, n), rect(x, y, w, h))
    label.font = .preferredFont(forTextStyle: .title1)
    return adopt(label)
}

// collection_new(rows, length, x, y, w, h): tiles in three columns under a header per section (rows as
// list_set_data's, with a SECTION each); a click on a tile is a SELECT event with its index among all the rows.
let collectionNew: Call = { p, n, x, y, w, h in
    var rows: [Row] = []
    if n > 0, let base = UnsafeRawPointer(bitPattern: p) {
        let bytes = Array(UnsafeRawBufferPointer(start: base, count: Int(n)))
        if let term = try? decodeTerm(bytes) { rows = rowsOf(term) }
    }
    var groups: [(title: String?, rows: [Row])] = []
    for row in rows {
        if let last = groups.last, last.title == row.section {
            groups[groups.count - 1].rows.append(row)
        } else {
            groups.append((title: row.section, rows: [row]))
        }
    }
    let scroll = NSScrollView(frame: rect(x, y, w, h))
    scroll.hasVerticalScroller = true
    scroll.drawsBackground = false
    let document = FlippedView(frame: CGRect(x: 0, y: 0, width: CGFloat(w), height: 0))
    scroll.documentView = document
    let handle = adopt(scroll)
    let tileWidth = (CGFloat(w) - 32) / 3
    var top: CGFloat = 4
    for group in groups {
        if let title = group.title {
            let header = NSTextField(labelWithString: title)
            header.font = .boldSystemFont(ofSize: 14)
            header.textColor = .lightGray
            header.frame = CGRect(x: 8, y: top, width: CGFloat(w) - 16, height: 22)
            document.addSubview(header)
            top += 28
        }
        for (k, row) in group.rows.enumerated() {
            let tile = NSButton(title: row.title, target: nil, action: nil)
            tile.bezelStyle = .smallSquare
            tile.frame = CGRect(x: 8 + CGFloat(k % 3) * (tileWidth + 8), y: top + CGFloat(k / 3) * 68,
                                width: tileWidth, height: 60)
            wire(tile, handle) { _ in send("SELECT", [.int(Int(handle)), .int(row.index)]) }
            document.addSubview(tile)
        }
        top += CGFloat((group.rows.count + 2) / 3) * 68 + 16
    }
    document.frame.size.height = top
    return handle
}

// MARK: - the phone's own calls

private var parallaxed: [(view: NSView, origin: CGPoint, amount: CGFloat)] = []
private var parallaxMonitor: Any?

// parallax_on(handle, amount): the phone leans the view with its tilt; here it leans towards the pointer, up to
// amount points each way.
let parallaxOn: Call = { h, amount, _, _, _, _ in
    guard let view = views[h] else { return 0 }
    parallaxed.append((view: view, origin: view.frame.origin, amount: CGFloat(amount)))
    rootView()?.window?.acceptsMouseMovedEvents = true
    if parallaxMonitor == nil {
        parallaxMonitor = NSEvent.addLocalMonitorForEvents(matching: .mouseMoved) { event in
            guard let root = rootView(), root.bounds.width > 0, root.bounds.height > 0 else { return event }
            let at = root.convert(event.locationInWindow, from: nil)
            let dx = (at.x / root.bounds.width - 0.5) * 2
            let dy = (at.y / root.bounds.height - 0.5) * 2
            parallaxed.removeAll { $0.view.superview == nil }
            for item in parallaxed {
                item.view.setFrameOrigin(CGPoint(x: item.origin.x + dx * item.amount, y: item.origin.y + dy * item.amount))
            }
            return event
        }
    }
    return 1
}

// gravity_drop(handle): the view falls, easing in as under gravity, to rest on the bottom of its superview.
let gravityDrop: Call = { h, _, _, _, _, _ in
    guard let view = views[h], let container = view.superview else { return 0 }
    let floor = container.isFlipped ? container.bounds.height - view.frame.height : 0
    let distance = abs(floor - view.frame.origin.y)
    NSAnimationContext.runAnimationGroup { context in
        context.duration = max(0.2, (2 * Double(distance) / 2500).squareRoot())   // g: 2500 points per second²
        context.timingFunction = CAMediaTimingFunction(name: .easeIn)
        view.animator().setFrameOrigin(CGPoint(x: view.frame.origin.x, y: floor))
    }
    return 1
}

// The keyboard's news: a Mac has no on-screen keyboard, so keyboard_post sends what the phone's UIKit would, and
// keyboard_avoid_on moves its view out of the way of a keyboard that high at the bottom of the window.
private let keyboardShows = Notification.Name("ModusKeyboardWillShow")
private let keyboardHides = Notification.Name("ModusKeyboardWillHide")
private var keyboardObservers: [UInt: [NSObjectProtocol]] = [:]
private var keyboardOriginals: [UInt: CGRect] = [:]

let keyboardAvoidOn: Call = { h, _, _, _, _, _ in
    guard let view = views[h] else { return 0 }
    let center = NotificationCenter.default
    let show = center.addObserver(forName: keyboardShows, object: nil, queue: .main) { note in
        guard let root = rootView(), let height = note.userInfo?["height"] as? CGFloat else { return }
        if keyboardOriginals[h] == nil { keyboardOriginals[h] = view.frame }
        guard let original = keyboardOriginals[h] else { return }
        let lift = max(0, original.maxY - (root.bounds.height - height) + 16)
        NSAnimationContext.runAnimationGroup { context in
            context.duration = 0.25
            view.animator().frame = original.offsetBy(dx: 0, dy: -lift)
        }
    }
    let hide = center.addObserver(forName: keyboardHides, object: nil, queue: .main) { _ in
        guard let original = keyboardOriginals[h] else { return }
        NSAnimationContext.runAnimationGroup { context in
            context.duration = 0.25
            view.animator().frame = original
        }
        keyboardOriginals[h] = nil
    }
    keyboardObservers[h] = [show, hide]
    return 1
}

// keyboard_post(height): a keyboard this high comes up (or, 0, goes down).
let keyboardPost: Call = { height, _, _, _, _, _ in
    NotificationCenter.default.post(name: height > 0 ? keyboardShows : keyboardHides, object: nil,
                                    userInfo: ["height": CGFloat(height)])
    return 1
}

// input_keys_on(field, keys, length): a key per title (a term: a list of strings) in a row under the field; a key
// types its title into the field (a TEXT event, as typing is).
let inputKeysOn: Call = { h, p, n, _, _, _ in
    guard let field = views[h] as? NSTextField, let parent = field.superview else { return 0 }
    let below = parent.isFlipped ? field.frame.maxY + 8 : field.frame.minY - 40
    let row = NSStackView(frame: CGRect(x: field.frame.minX, y: below, width: field.frame.width, height: 32))
    row.orientation = .horizontal
    row.distribution = .fillEqually
    row.spacing = 8
    for title in titlesOf(p, n) {
        let key = NSButton(title: title, target: nil, action: nil)
        key.bezelStyle = .rounded
        wire(key, h) { _ in
            field.stringValue += title
            send("TEXT", [.int(Int(h))])
        }
        row.addArrangedSubview(key)
    }
    parent.addSubview(row)
    attached[h, default: []].append(row)
    return 1
}

// The system's panels.  The colour panel sends COLOR (0xRRGGBB) as a colour is picked; the font panel FONT.
final class Picked: NSObject {
    let handle: UInt

    init(_ handle: UInt) {
        self.handle = handle
    }

    @objc func colorChanged(_ sender: NSColorPanel) {
        send("COLOR", [.int(Int(handle)), .int(packed(sender.color))])
    }

    @objc func changeFont(_ sender: Any?) {
        send("FONT", [.int(Int(handle))])
    }
}

// color_picker_show(): the colour panel; returns the handle its events name.
let colorPickerShow: Call = { _, _, _, _, _, _ in
    let h = newHandle()
    let target = Picked(h)
    keep(h, target)
    let panel = NSColorPanel.shared
    panel.setTarget(target)
    panel.setAction(#selector(Picked.colorChanged(_:)))
    panel.orderFront(nil)
    return h
}

// font_picker_show(): the font panel; returns the handle its events name.
let fontPickerShow: Call = { _, _, _, _, _, _ in
    let h = newHandle()
    let target = Picked(h)
    keep(h, target)
    let fonts = NSFontManager.shared
    fonts.target = target
    fonts.action = #selector(Picked.changeFont(_:))
    fonts.orderFrontFontPanel(nil)
    return h
}

private func openPanel(_ types: [UTType]) -> UInt {
    guard let window = rootView()?.window else { return 0 }
    let panel = NSOpenPanel()
    panel.allowedContentTypes = types
    panel.beginSheetModal(for: window) { _ in }
    return 1
}

// document_picker_show(): an Open panel for a text file, over the window; Cancel closes it.
let documentPickerShow: Call = { _, _, _, _, _, _ in openPanel([.text]) }

// image_picker_show(): an Open panel for an image (the phone's photo picker); Cancel closes it.
let imagePickerShow: Call = { _, _, _, _, _, _ in openPanel([.image]) }

// list_editing(list, on): each row shows a delete control (on 1) or not (0); deleting is a DELETE event.
let listEditing: Call = { h, on, _, _, _, _ in
    guard let list = lists[h] else { return 0 }
    list.editing = on != 0
    ((views[h] as? NSScrollView)?.documentView as? NSTableView)?.reloadData()
    return 0
}

// A long press's menu of choices (edit_menu_on): a choice is an EDIT event with its index.
final class EditMenu: NSObject {
    let handle: UInt
    let choices: [String]
    private var actions: [Action] = []

    init(_ handle: UInt, _ choices: [String]) {
        self.handle = handle
        self.choices = choices
    }

    @objc func pressed(_ recognizer: NSPressGestureRecognizer) {
        guard recognizer.state == .began, let view = recognizer.view else { return }
        let menu = NSMenu()
        actions = []
        for (i, title) in choices.enumerated() {
            let action = Action { _ in send("EDIT", [.int(Int(self.handle)), .int(i)]) }
            actions.append(action)
            let item = NSMenuItem(title: title, action: #selector(Action.act(_:)), keyEquivalent: "")
            item.target = action
            menu.addItem(item)
        }
        menu.popUp(positioning: nil, at: recognizer.location(in: view), in: view)
    }
}

// edit_menu_on(handle, choices, length): a long press on the view opens a menu of the choices.
let editMenuOn: Call = { h, p, n, _, _, _ in
    guard let view = views[h] else { return 0 }
    let menu = EditMenu(h, titlesOf(p, n))
    view.addGestureRecognizer(NSPressGestureRecognizer(target: menu, action: #selector(EditMenu.pressed(_:))))
    keep(h, menu)
    return 1
}

// A drag in from the window's left edge: one that starts within 24 points of it and goes 40 points in is an EDGE event.
final class EdgePan: NSObject {
    let handle: UInt
    private var fromEdge = false

    init(_ handle: UInt) {
        self.handle = handle
    }

    @objc func panned(_ recognizer: NSPanGestureRecognizer) {
        guard let view = recognizer.view else { return }
        switch recognizer.state {
        case .began:
            fromEdge = recognizer.location(in: view).x - recognizer.translation(in: view).x < 24
        case .ended:
            if fromEdge && recognizer.translation(in: view).x > 40 { send("EDGE", [.int(Int(handle))]) }
            fromEdge = false
        default:
            break
        }
    }
}

private var edgeRecognizers: [UInt: NSGestureRecognizer] = [:]

// edge_pan_on(handle): the recognizer goes on the window's content view, where an edge drag begins; the handle
// names the event.  It does not hold up the desk's own mouse events.
let edgePanOn: Call = { h, _, _, _, _, _ in
    guard let root = rootView() else { return 0 }
    let target = EdgePan(h)
    let recognizer = NSPanGestureRecognizer(target: target, action: #selector(EdgePan.panned(_:)))
    recognizer.delaysPrimaryMouseButtonEvents = false
    if let old = edgeRecognizers[h] { root.removeGestureRecognizer(old) }
    root.addGestureRecognizer(recognizer)
    edgeRecognizers[h] = recognizer
    keep(h, target)
    return 1
}
