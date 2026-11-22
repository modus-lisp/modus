// ModusSwiftMac.swift — the native components a modus app builds from Lisp, on the Mac: AppKit views made here,
// named by a handle, and the events they send back.  The Mac's half of host/ios/swift/ModusSwift.swift: the SAME
// function names (ui_*), the same arguments and the same events, so vessel's ios-swift.lisp and its storybook run
// on either.  Lisp calls these by name (host/ios/modus-swift.m, pseudo-syscall 1060); each runs on the main
// thread, where AppKit lives.
//
// Words and text: every argument is a word.  Text is a pointer and a length: UTF-8 bytes the image holds, read
// here in place.  A function returns a word: a handle, a value, 1 or 0, or nothing (0).  Structured data (a
// list's rows, a set of titles) crosses as a term (host/ios/swift/Term.swift), and so do the events.
//
// The views go into the modus window's content view, over the desk's framebuffer.  That view is FLIPPED (y grows
// down), so a frame here means what it means on the phone: x, y from the top left, in points.
//
// This file has the controls, lists and gestures; ModusSwiftMac2.swift has what the phone shows over its screen
// (sheets, a popover, a split view, and the controllers presented as sheets), the zoom and paging scroll views,
// images, styled labels, the system pickers, and the phone-only calls, each as near as a Mac comes.

import AppKit

typealias Call = @convention(c) (UInt, UInt, UInt, UInt, UInt, UInt) -> UInt

var views: [UInt: NSView] = [:]
var nextHandle: UInt = 1
// A scroll view's handle maps to the stack that holds its children (ui_add puts them there).
private var scrollContent: [UInt: NSStackView] = [:]
// A list's handle maps to its data source and delegate (an NSTableView keeps both weakly).
var lists: [UInt: ModusList] = [:]
// A text view's handle names its scroll view; this is the text view inside it.
private var textViews: [UInt: NSTextView] = [:]
// Targets and delegates AppKit holds weakly, kept here for as long as their view lives.
private var kept: [UInt: [AnyObject]] = [:]
private var events: [Term] = []

func send(_ name: String, _ args: [Term]) {
    events.append(.list([.symbol(name: name, package: "KEYWORD")] + args))
}

func text(_ p: UInt, _ n: UInt) -> String {
    guard n > 0, let base = UnsafeRawPointer(bitPattern: p) else { return "" }
    return String(decoding: UnsafeRawBufferPointer(start: base, count: Int(n)), as: UTF8.self)
}

func rect(_ x: UInt, _ y: UInt, _ w: UInt, _ h: UInt) -> CGRect {
    CGRect(x: CGFloat(x), y: CGFloat(y), width: CGFloat(w), height: CGFloat(h))
}

// The modus window's content view (host/macos/modus-ui-mac.m).
func rootView() -> NSView? {
    guard let p = modus_mac_root_view() else { return nil }
    return Unmanaged<NSView>.fromOpaque(p).takeUnretainedValue()
}

// The components are styled for a dark screen, as on the phone (white text, dark backgrounds): AppKit's controls
// take the dark appearance to match, or a button draws dark text on the dark panel behind it.
let darkAppearance = NSAppearance(named: .darkAqua)

func adopt(_ v: NSView) -> UInt {
    let handle = nextHandle
    nextHandle += 1
    if v.appearance == nil { v.appearance = darkAppearance }
    views[handle] = v
    rootView()?.addSubview(v)
    return handle
}

func newHandle() -> UInt {
    let handle = nextHandle
    nextHandle += 1
    return handle
}

func keep(_ h: UInt, _ o: AnyObject) {
    kept[h, default: []].append(o)
}

func colorOf(_ rgb: UInt) -> NSColor {
    NSColor(srgbRed: CGFloat((rgb >> 16) & 255) / 255, green: CGFloat((rgb >> 8) & 255) / 255,
            blue: CGFloat(rgb & 255) / 255, alpha: 1)
}

func packed(_ color: NSColor) -> Int {
    let c = color.usingColorSpace(.sRGB) ?? color
    return (Int((c.redComponent * 255).rounded()) << 16) | (Int((c.greenComponent * 255).rounded()) << 8)
        | Int((c.blueComponent * 255).rounded())
}

// A control's action as a closure: AppKit sends target/action, and this object is the target.
final class Action: NSObject {
    let fire: (Any?) -> Void

    init(_ fire: @escaping (Any?) -> Void) {
        self.fire = fire
    }

    @objc func act(_ sender: Any?) {
        fire(sender)
    }
}

func wire(_ control: NSControl, _ h: UInt, _ fire: @escaping (Any?) -> Void) {
    let action = Action(fire)
    control.target = action
    control.action = #selector(Action.act(_:))
    keep(h, action)
}

// A view whose y grows down, as the phone's do.
class FlippedView: NSView {
    override var isFlipped: Bool { true }
}

// The titles out of a term: a list of strings.
func titlesOf(_ p: UInt, _ n: UInt) -> [String] {
    guard n > 0, let base = UnsafeRawPointer(bitPattern: p) else { return [] }
    let bytes = Array(UnsafeRawBufferPointer(start: base, count: Int(n)))
    guard let items = (try? decodeTerm(bytes))?.listItems else { return [] }
    return items.compactMap { item -> String? in
        if case .string(let s) = item { return s }
        return nil
    }
}

func plainLabel(_ s: String, _ frame: CGRect, size: CGFloat = 18) -> NSTextField {
    let label = NSTextField(wrappingLabelWithString: s)
    label.frame = frame
    label.font = .systemFont(ofSize: size)
    label.textColor = .white
    return label
}

// MARK: - lists

// A row of a list: its title, a subtitle when it has one, its section when it has one, and its index among all the
// rows (the one a SELECT event names).
struct Row {
    let title: String
    let subtitle: String?
    let section: String?
    let index: Int
}

func rowsOf(_ data: Term) -> [Row] {
    guard let items = data.listItems else { return [] }
    return items.enumerated().map { index, item in
        var title = ""
        var subtitle: String?
        var section: String?
        let pairs = item.listItems ?? []
        var i = 0
        while i + 1 < pairs.count {
            if case .symbol(let key, _) = pairs[i], case .string(let value) = pairs[i + 1] {
                if key == "TITLE" { title = value }
                if key == "SUBTITLE" { subtitle = value }
                if key == "SECTION" { section = value }
            }
            i += 2
        }
        return Row(title: title, subtitle: subtitle, section: section, index: index)
    }
}

// The rows as the table shows them: each run of rows with the same section is headed by the section's title (a
// group row), as the phone's table has a header per section.
final class ModusList: NSObject, NSTableViewDataSource, NSTableViewDelegate {
    var shown: [(header: String?, row: Row?)] = []
    var actions: [String] = []
    var editing = false
    private var deletes: [Action] = []
    let handle: UInt

    init(handle: UInt) {
        self.handle = handle
    }

    func setRows(_ rows: [Row]) {
        shown = []
        var section: String?? = .none
        for row in rows {
            if section == nil || section! != row.section {
                section = .some(row.section)
                if let title = row.section { shown.append((header: title, row: nil)) }
            }
            shown.append((header: nil, row: row))
        }
    }

    func numberOfRows(in tableView: NSTableView) -> Int {
        shown.count
    }

    func tableView(_ tableView: NSTableView, isGroupRow row: Int) -> Bool {
        shown[row].row == nil
    }

    func tableView(_ tableView: NSTableView, heightOfRow row: Int) -> CGFloat {
        guard let r = shown[row].row else { return 26 }
        return r.subtitle == nil ? 32 : 46
    }

    func tableView(_ tableView: NSTableView, viewFor tableColumn: NSTableColumn?, row: Int) -> NSView? {
        let item = shown[row]
        guard let r = item.row else {
            let header = NSTextField(labelWithString: item.header ?? "")
            header.font = .boldSystemFont(ofSize: 12)
            header.textColor = .secondaryLabelColor
            return header
        }
        let content = rowContent(r)
        return editing ? editable(content, r, tableView) : content
    }

    // A row in editing mode (list_editing): a delete control before it; a click removes the row and sends DELETE
    // with its index, as the phone's delete control does.
    func editable(_ content: NSView, _ r: Row, _ table: NSTableView) -> NSView {
        let delete = NSButton(image: NSImage(systemSymbolName: "minus.circle.fill", accessibilityDescription: "Delete")
                                  ?? NSImage(), target: nil, action: nil)
        delete.isBordered = false
        delete.contentTintColor = .systemRed
        let action = Action { [weak self, weak table] _ in
            guard let self = self else { return }
            self.shown.removeAll { $0.row?.index == r.index }
            table?.reloadData()
            send("DELETE", [.int(Int(self.handle)), .int(r.index)])
        }
        delete.target = action
        delete.action = #selector(Action.act(_:))
        deletes.append(action)
        let row = NSStackView(views: [delete, content])
        row.orientation = .horizontal
        row.spacing = 8
        return row
    }

    func rowContent(_ r: Row) -> NSView {
        let title = NSTextField(labelWithString: r.title)
        title.font = .systemFont(ofSize: 15)
        title.textColor = .white
        guard let sub = r.subtitle else { return title }
        let subtitle = NSTextField(labelWithString: sub)
        subtitle.font = .systemFont(ofSize: 12)
        subtitle.textColor = .lightGray
        let stack = NSStackView(views: [title, subtitle])
        stack.orientation = .vertical
        stack.alignment = .leading
        stack.spacing = 2
        return stack
    }

    func tableView(_ tableView: NSTableView, shouldSelectRow row: Int) -> Bool {
        shown[row].row != nil
    }

    func tableViewSelectionDidChange(_ notification: Notification) {
        guard let table = notification.object as? NSTableView, table.selectedRow >= 0,
              let r = shown[table.selectedRow].row else { return }
        send("SELECT", [.int(Int(handle)), .int(r.index)])
        table.deselectAll(nil)
    }

    // Swipe actions (a trackpad's two-finger swipe on a row): a button for each, and a click on one sends a ROWACTION
    // event with the list's handle and the button's index.
    func tableView(_ tableView: NSTableView, rowActionsForRow row: Int,
                   edge: NSTableView.RowActionEdge) -> [NSTableViewRowAction] {
        guard edge == .trailing, !actions.isEmpty, shown[row].row != nil else { return [] }
        return actions.enumerated().map { i, title in
            NSTableViewRowAction(style: .regular, title: title) { _, _ in
                send("ROWACTION", [.int(Int(self.handle)), .int(i)])
            }
        }
    }
}

private func tableOf(_ h: UInt) -> NSTableView? {
    (views[h] as? NSScrollView)?.documentView as? NSTableView
}

// MARK: - text changes, gestures

final class TextChanges: NSObject, NSTextFieldDelegate, NSSearchFieldDelegate, NSTextViewDelegate {
    let handle: UInt

    init(_ handle: UInt) {
        self.handle = handle
    }

    func controlTextDidChange(_ obj: Notification) {
        send("TEXT", [.int(Int(handle))])
    }

    func textDidChange(_ notification: Notification) {
        send("TEXT", [.int(Int(handle))])
    }
}

// A recognizer's target: a press (long) fires when it begins, a click when it is recognized.
final class Recognized: NSObject {
    let handle: UInt
    let event: String

    init(_ handle: UInt, _ event: String) {
        self.handle = handle
        self.event = event
    }

    @objc func fired(_ recognizer: NSGestureRecognizer) {
        let due = recognizer is NSPressGestureRecognizer ? recognizer.state == .began : recognizer.state == .ended
        if due { send(event, [.int(Int(handle))]) }
    }
}

private func recognize(_ h: UInt, _ make: (Recognized) -> NSGestureRecognizer, _ event: String) -> UInt {
    guard let view = views[h] else { return 0 }
    let target = Recognized(h, event)
    view.addGestureRecognizer(make(target))
    keep(h, target)
    return 1
}

// A pan's target: a pan that ends sends PAN with the horizontal distance; a swipe is a pan that ends far enough
// along its direction (AppKit has no swipe recognizer: a trackpad's swipe is a scroll, a mouse's is a drag).
final class Panned: NSObject {
    let handle: UInt
    let swipe: Int?

    init(_ handle: UInt, swipe: Int?) {
        self.handle = handle
        self.swipe = swipe
    }

    @objc func panned(_ recognizer: NSPanGestureRecognizer) {
        guard recognizer.state == .ended, let view = recognizer.view else { return }
        let t = recognizer.translation(in: view)
        guard let want = swipe else {
            send("PAN", [.int(Int(handle)), .double(Double(t.x))])
            return
        }
        let down = view.isFlipped ? t.y : -t.y     // positive: towards the bottom of the screen
        let dir: Int
        if abs(t.x) >= abs(down) { dir = t.x < 0 ? 0 : 1 } else { dir = down < 0 ? 2 : 3 }
        if dir == want && max(abs(t.x), abs(down)) >= 40 { send("SWIPE", [.int(Int(handle)), .int(dir)]) }
    }
}

// MARK: - the components

let labelNew: Call = { p, n, x, y, w, h in
    adopt(plainLabel(text(p, n), rect(x, y, w, h)))
}

let buttonNew: Call = { p, n, x, y, w, h in
    let button = NSButton(title: text(p, n), target: nil, action: nil)
    button.bezelStyle = .rounded
    button.frame = rect(x, y, w, h)
    let handle = adopt(button)
    wire(button, handle) { _ in send("TAP", [.int(Int(handle))]) }
    return handle
}

// symbol_button_new(name, len, x, y, w, h): a button showing the SF Symbol NAME, a toggle's look: plain when off,
// filled with the accent colour when on (symbol_button_set).  A tap is a TAP event; Lisp says whether it is on.
let symbolButtonNew: Call = { p, n, x, y, w, h in
    let name = text(p, n)
    let image = NSImage(systemSymbolName: name, accessibilityDescription: name) ?? NSImage()
    let button = NSButton(image: image, target: nil, action: nil)
    button.bezelStyle = .rounded
    button.frame = rect(x, y, w, h)
    let handle = adopt(button)
    wire(button, handle) { _ in send("TAP", [.int(Int(handle))]) }
    return handle
}

let symbolButtonSet: Call = { h, on, _, _, _, _ in
    guard let button = views[h] as? NSButton else { return 0 }
    // a rounded button shows a tint on its symbol faintly at best: on is the bezel filled, as an engaged toggle is
    button.bezelColor = on != 0 ? .controlAccentColor : nil
    button.contentTintColor = on != 0 ? .white : nil
    button.state = on != 0 ? .on : .off
    return 1
}

// switch_new(on, x, y): a switch; its frame is its own size.
let switchNew: Call = { on, x, y, _, _, _ in
    let sw = NSSwitch()
    sw.state = on != 0 ? .on : .off
    sw.frame = CGRect(origin: CGPoint(x: CGFloat(x), y: CGFloat(y)), size: sw.fittingSize)
    let handle = adopt(sw)
    wire(sw, handle) { _ in send("SWITCH", [.int(Int(handle)), .int(sw.state == .on ? 1 : 0)]) }
    return handle
}

// slider_new(value, x, y, w, h): a slider from 0 to 100.
let sliderNew: Call = { value, x, y, w, h, _ in
    let slider = NSSlider(value: Double(value), minValue: 0, maxValue: 100, target: nil, action: nil)
    slider.frame = rect(x, y, w, h)
    slider.isContinuous = true
    let handle = adopt(slider)
    wire(slider, handle) { _ in send("SLIDER", [.int(Int(handle)), .double(slider.doubleValue)]) }
    return handle
}

// text_field_new(placeholder, length, x, y, w, h): a rounded text field; each change sends TEXT.
let textFieldNew: Call = { p, n, x, y, w, h in
    let field = NSTextField(frame: rect(x, y, w, h))
    field.placeholderString = text(p, n)
    field.bezelStyle = .roundedBezel
    field.font = .systemFont(ofSize: 15)
    let handle = adopt(field)
    let delegate = TextChanges(handle)
    field.delegate = delegate
    keep(handle, delegate)
    return handle
}

// text_view_new(text, length, x, y, w, h): a multi-line text view; each change sends TEXT.
let textViewNew: Call = { p, n, x, y, w, h in
    let scroll = NSTextView.scrollableTextView()
    scroll.frame = rect(x, y, w, h)
    let view = scroll.documentView as! NSTextView
    view.string = text(p, n)
    view.font = .systemFont(ofSize: 16)
    view.textColor = .white
    view.backgroundColor = NSColor(white: 0.12, alpha: 1)
    view.insertionPointColor = .white
    scroll.wantsLayer = true
    scroll.layer?.cornerRadius = 8
    let handle = adopt(scroll)
    let delegate = TextChanges(handle)
    view.delegate = delegate
    keep(handle, delegate)
    textViews[handle] = view
    return handle
}

// segmented_new(titles, length, x, y, w, h): a segmented control; choosing a segment sends SELECT with its index.
let segmentedNew: Call = { p, n, x, y, w, h in
    let control = NSSegmentedControl(labels: titlesOf(p, n), trackingMode: .selectOne, target: nil, action: nil)
    control.frame = rect(x, y, w, h)
    if control.segmentCount > 0 { control.selectedSegment = 0 }
    let handle = adopt(control)
    wire(control, handle) { _ in send("SELECT", [.int(Int(handle)), .int(control.selectedSegment)]) }
    return handle
}

// progress_new(percent, x, y, w, h): a progress bar from 0 to 100.
let progressNew: Call = { percent, x, y, w, h, _ in
    let bar = NSProgressIndicator(frame: rect(x, y, w, h))
    bar.style = .bar
    bar.isIndeterminate = false
    bar.minValue = 0
    bar.maxValue = 100
    bar.doubleValue = Double(min(percent, 100))
    return adopt(bar)
}

// spinner_new(x, y, w, h): an activity indicator, spinning until set_value stops it.
let spinnerNew: Call = { x, y, w, h, _, _ in
    let spinner = NSProgressIndicator(frame: rect(x, y, w, h))
    spinner.style = .spinning
    spinner.appearance = NSAppearance(named: .darkAqua)
    spinner.startAnimation(nil)
    return adopt(spinner)
}

// stepper_new(value, x, y): a stepper from 0 to 100; each change sends STEP with its value.
let stepperNew: Call = { value, x, y, _, _, _ in
    let stepper = NSStepper()
    stepper.minValue = 0
    stepper.maxValue = 100
    stepper.increment = 1
    stepper.valueWraps = false
    stepper.doubleValue = Double(value)
    stepper.frame = CGRect(origin: CGPoint(x: CGFloat(x), y: CGFloat(y)), size: stepper.fittingSize)
    let handle = adopt(stepper)
    wire(stepper, handle) { _ in send("STEP", [.int(Int(handle)), .double(stepper.doubleValue)]) }
    return handle
}

// A page control: a dot per page, the current one lit; a click on a dot sends PAGE with its index.
final class PageDots: FlippedView {
    var count = 0
    var current = 0
    var picked: ((Int) -> Void)?

    override func draw(_ dirtyRect: NSRect) {
        for i in 0..<count {
            (i == current ? NSColor.white : NSColor(white: 0.35, alpha: 1)).setFill()
            NSBezierPath(ovalIn: NSRect(x: CGFloat(i) * 16 + 4, y: (bounds.height - 8) / 2, width: 8, height: 8)).fill()
        }
    }

    override func mouseDown(with event: NSEvent) {
        let i = Int(convert(event.locationInWindow, from: nil).x / 16)
        guard i >= 0, i < count else { return }
        current = i
        needsDisplay = true
        picked?(i)
    }
}

let pageNew: Call = { pages, current, x, y, _, _ in
    let dots = PageDots(frame: CGRect(x: CGFloat(x), y: CGFloat(y), width: CGFloat(pages) * 16 + 8, height: 24))
    dots.count = Int(pages)
    dots.current = Int(current)
    let handle = adopt(dots)
    dots.picked = { i in send("PAGE", [.int(Int(handle)), .int(i)]) }
    return handle
}

// date_new(seconds, x, y): a date picker; a change sends DATE with the new date's seconds since 1970.
let dateNew: Call = { seconds, x, y, _, _, _ in
    let picker = NSDatePicker()
    picker.datePickerStyle = .textFieldAndStepper
    picker.datePickerElements = .yearMonthDay
    picker.dateValue = Date(timeIntervalSince1970: TimeInterval(seconds))
    picker.frame = CGRect(origin: CGPoint(x: CGFloat(x), y: CGFloat(y)), size: picker.fittingSize)
    let handle = adopt(picker)
    wire(picker, handle) { _ in send("DATE", [.int(Int(handle)), .double(picker.dateValue.timeIntervalSince1970)]) }
    return handle
}

// calendar_new(seconds, x, y, w, h): a month calendar; a day picked sends DATE.
let calendarNew: Call = { seconds, x, y, w, h, _ in
    let picker = NSDatePicker(frame: rect(x, y, w, h))
    picker.datePickerStyle = .clockAndCalendar
    picker.datePickerElements = .yearMonthDay
    picker.dateValue = Date(timeIntervalSince1970: TimeInterval(seconds))
    picker.appearance = NSAppearance(named: .darkAqua)
    // a Mac's calendar has its own size and draws from the bottom of a larger frame: its size, at the frame's top left
    picker.frame = CGRect(origin: CGPoint(x: CGFloat(x), y: CGFloat(y)), size: picker.fittingSize)
    let handle = adopt(picker)
    wire(picker, handle) { _ in send("DATE", [.int(Int(handle)), .double(picker.dateValue.timeIntervalSince1970)]) }
    return handle
}

// long_press_on(handle, ms): a press held at least ms milliseconds sends LONGPRESS.
let longPressOn: Call = { h, ms, _, _, _, _ in
    recognize(h, { target in
        let press = NSPressGestureRecognizer(target: target, action: #selector(Recognized.fired(_:)))
        press.minimumPressDuration = Double(ms) / 1000
        return press
    }, "LONGPRESS")
}

// tap_on(handle): a click on the view sends TAP, as a button's does.
let tapOn: Call = { h, _, _, _, _, _ in
    recognize(h, { NSClickGestureRecognizer(target: $0, action: #selector(Recognized.fired(_:))) }, "TAP")
}

// pan_on(handle): a drag on the view sends PAN with the horizontal distance when it ends.
let panOn: Call = { h, _, _, _, _, _ in
    guard let view = views[h] else { return 0 }
    let target = Panned(h, swipe: nil)
    view.addGestureRecognizer(NSPanGestureRecognizer(target: target, action: #selector(Panned.panned(_:))))
    keep(h, target)
    return 1
}

// swipe_on(handle, direction): a drag along the direction (0 left, 1 right, 2 up, 3 down) sends SWIPE.
let swipeOn: Call = { h, dir, _, _, _, _ in
    guard let view = views[h] else { return 0 }
    let target = Panned(h, swipe: Int(dir))
    view.addGestureRecognizer(NSPanGestureRecognizer(target: target, action: #selector(Panned.panned(_:))))
    keep(h, target)
    return 1
}

// alert_new(title, length, message, length, sheet): an alert, not yet shown.  On a Mac an action sheet is an alert too.
private var alerts: [UInt: NSAlert] = [:]

let alertNew: Call = { tp, tn, mp, mn, _, _ in
    let alert = NSAlert()
    alert.messageText = text(tp, tn)
    alert.informativeText = text(mp, mn)
    let handle = newHandle()
    alerts[handle] = alert
    return handle
}

// alert_action(handle, text, length): a button of the alert, in the order they are added.
let alertAction: Call = { h, p, n, _, _, _ in
    alerts[h]?.addButton(withTitle: text(p, n))
    return 0
}

// alert_present(handle): the alert, as a sheet on the modus window.  Choosing a button sends ALERT with its index.
let alertPresent: Call = { h, _, _, _, _, _ in
    guard let alert = alerts[h], let window = rootView()?.window else { return 0 }
    alert.beginSheetModal(for: window) { response in
        let index = response.rawValue - NSApplication.ModalResponse.alertFirstButtonReturn.rawValue
        send("ALERT", [.int(Int(h)), .int(index)])
    }
    return 1
}

// tab_bar_new(titles, length, x, y, w, h): a row of tabs; choosing one sends SELECT with its index.
let tabBarNew: Call = { p, n, x, y, w, h in
    let control = NSSegmentedControl(labels: titlesOf(p, n), trackingMode: .selectOne, target: nil, action: nil)
    control.frame = rect(x, y, w, h)
    control.segmentStyle = .automatic
    control.segmentDistribution = .fillEqually
    if control.segmentCount > 0 { control.selectedSegment = 0 }
    let handle = adopt(control)
    wire(control, handle) { _ in send("SELECT", [.int(Int(handle)), .int(control.selectedSegment)]) }
    return handle
}

// refresh_on(list): a Mac list has no pull to refresh; a list that asks for it is refreshed by send_refresh (and,
// for a person, by the story's own button).  Answers 1, as the phone does, so the story goes on.
let refreshOn: Call = { h, _, _, _, _, _ in
    tableOf(h) == nil ? 0 : 1
}

// send_refresh(list): a refresh, as pulling the list down sends it on the phone.
let sendRefresh: Call = { h, _, _, _, _, _ in
    send("REFRESH", [.int(Int(h))])
    return 0
}

// search_bar_new(placeholder, length, x, y, w, h): a search field; each change sends TEXT.
let searchBarNew: Call = { p, n, x, y, w, h in
    let field = NSSearchField(frame: rect(x, y, w, h))
    field.placeholderString = text(p, n)
    let handle = adopt(field)
    let delegate = TextChanges(handle)
    field.delegate = delegate
    keep(handle, delegate)
    return handle
}

// toolbar_new(titles, length, x, y, w, h): a bar of buttons spaced evenly; a click sends SELECT with its index.
let toolbarNew: Call = { p, n, x, y, w, h in
    let bar = NSStackView(frame: rect(x, y, w, h))
    bar.orientation = .horizontal
    bar.distribution = .equalSpacing
    bar.edgeInsets = NSEdgeInsets(top: 0, left: 16, bottom: 0, right: 16)
    bar.wantsLayer = true
    bar.layer?.backgroundColor = NSColor(white: 0.12, alpha: 1).cgColor
    let handle = adopt(bar)
    for (i, title) in titlesOf(p, n).enumerated() {
        let button = NSButton(title: title, target: nil, action: nil)
        button.bezelStyle = .recessed
        wire(button, handle) { _ in send("SELECT", [.int(Int(handle)), .int(i)]) }
        bar.addArrangedSubview(button)
    }
    return handle
}

// picker_new(titles, length, x, y, w, h): the phone's wheel is a pop-up menu here; a choice sends SELECT.
let pickerNew: Call = { p, n, x, y, w, h in
    let popup = NSPopUpButton(frame: rect(x, y, min(w, 240), 28), pullsDown: false)
    popup.addItems(withTitles: titlesOf(p, n))
    let handle = adopt(popup)
    wire(popup, handle) { _ in send("SELECT", [.int(Int(handle)), .int(popup.indexOfSelectedItem)]) }
    return handle
}

// blur_new(x, y, w, h): a dark blur of what is behind it; its children (ui_add) go inside it.
let blurNew: Call = { x, y, w, h, _, _ in
    let view = NSVisualEffectView(frame: rect(x, y, w, h))
    view.material = .hudWindow
    view.blendingMode = .withinWindow
    view.state = .active
    view.appearance = NSAppearance(named: .darkAqua)
    // the children go in a FLIPPED view inside the blur, so their frames mean what they mean on the phone (an
    // effect view is not flipped: a label at y 10 sat at its bottom, or past it)
    let content = FlippedView(frame: CGRect(x: 0, y: 0, width: CGFloat(w), height: CGFloat(h)))
    content.autoresizingMask = [.width, .height]
    view.addSubview(content)
    let handle = adopt(view)
    scrollDocuments[handle] = content
    return handle
}

// grid_new(titles, length, x, y, w, h): tiles of 100 x 60, one per title, in rows; a click sends SELECT.
let gridNew: Call = { p, n, x, y, w, h in
    let grid = FlippedView(frame: rect(x, y, w, h))
    let handle = adopt(grid)
    let per = max(1, Int((CGFloat(w) + 8) / 108))
    for (i, title) in titlesOf(p, n).enumerated() {
        let tile = NSButton(title: title, target: nil, action: nil)
        tile.bezelStyle = .smallSquare
        tile.frame = CGRect(x: CGFloat(i % per) * 108, y: CGFloat(i / per) * 68, width: 100, height: 60)
        wire(tile, handle) { _ in send("SELECT", [.int(Int(handle)), .int(i)]) }
        grid.addSubview(tile)
    }
    return handle
}

// menu_button_new(choices, length, x, y, w, h): a button, 'options', whose menu offers the choices; choosing one
// sends MENU with its index.
let menuButtonNew: Call = { p, n, x, y, w, h in
    let popup = NSPopUpButton(frame: rect(x, y, w, h), pullsDown: true)
    popup.addItem(withTitle: "options")
    popup.addItems(withTitles: titlesOf(p, n))
    let handle = adopt(popup)
    wire(popup, handle) { _ in
        let i = popup.indexOfSelectedItem - 1
        if i >= 0 { send("MENU", [.int(Int(handle)), .int(i)]) }
    }
    return handle
}

// color_well_new(rgb, x, y, w, h): a colour well; a choice sends COLOR as 0xRRGGBB.
let colorWellNew: Call = { rgb, x, y, w, h, _ in
    let well = NSColorWell(frame: rect(x, y, w, h))
    well.color = colorOf(rgb)
    let handle = adopt(well)
    wire(well, handle) { _ in send("COLOR", [.int(Int(handle)), .int(packed(well.color))]) }
    return handle
}

// A navigation bar: a back button when there is somewhere to go back to, the top item's title, and its right button.
// Going back (the button, or nav_pop) sends POP; the right button sends NAVTAP.
final class NavBar: FlippedView {
    var items: [(title: String, right: String)] = []
    let back = NSButton(title: "‹ Back", target: nil, action: nil)
    let title = NSTextField(labelWithString: "")
    let right = NSButton(title: "", target: nil, action: nil)

    override init(frame: NSRect) {
        super.init(frame: frame)
        wantsLayer = true
        layer?.backgroundColor = NSColor(white: 0.1, alpha: 1).cgColor
        title.font = .boldSystemFont(ofSize: 16)
        title.textColor = .white
        title.alignment = .center
        back.bezelStyle = .recessed
        right.bezelStyle = .recessed
        for v in [back, title, right] { addSubview(v) }
        layout()
    }

    required init?(coder: NSCoder) { fatalError("not from a nib") }

    override func layout() {
        super.layout()
        let h = bounds.height
        back.frame = CGRect(x: 8, y: (h - 24) / 2, width: 80, height: 24)
        right.frame = CGRect(x: bounds.width - 88, y: (h - 24) / 2, width: 80, height: 24)
        title.frame = CGRect(x: 96, y: (h - 22) / 2, width: max(0, bounds.width - 192), height: 22)
    }

    func refresh() {
        title.stringValue = items.last?.title ?? ""
        back.isHidden = items.count < 2
        right.title = items.last?.right ?? ""
        right.isHidden = right.title.isEmpty
    }
}

let navBarNew: Call = { x, y, w, h, _, _ in
    let bar = NavBar(frame: rect(x, y, w, h))
    let handle = adopt(bar)
    wire(bar.back, handle) { _ in
        guard bar.items.count > 1 else { return }
        bar.items.removeLast()
        bar.refresh()
        send("POP", [.int(Int(handle))])
    }
    wire(bar.right, handle) { _ in send("NAVTAP", [.int(Int(handle))]) }
    bar.refresh()
    return handle
}

// nav_push(bar, title, length, right, length): an item with the title, and a right button unless its title is empty.
let navPush: Call = { h, tp, tn, rp, rn, _ in
    guard let bar = views[h] as? NavBar else { return 0 }
    bar.items.append((title: text(tp, tn), right: text(rp, rn)))
    bar.refresh()
    return 0
}

// nav_pop(bar): pops the top item (a POP event, as the back button sends).
let navPop: Call = { h, _, _, _, _, _ in
    guard let bar = views[h] as? NavBar, bar.items.count > 1 else { return 0 }
    bar.items.removeLast()
    bar.refresh()
    send("POP", [.int(Int(h))])
    return 0
}

// context_menu_on(handle, titles, length): a right click (or control-click) opens a menu of the choices; a choice
// sends CONTEXT with its index.
let contextMenuOn: Call = { h, p, n, _, _, _ in
    guard let view = views[h] else { return 0 }
    let menu = NSMenu()
    for (i, title) in titlesOf(p, n).enumerated() {
        let action = Action { _ in send("CONTEXT", [.int(Int(h)), .int(i)]) }
        let item = NSMenuItem(title: title, action: #selector(Action.act(_:)), keyEquivalent: "")
        item.target = action
        keep(h, action)
        menu.addItem(item)
    }
    view.menu = menu
    return 1
}

// animate_to(handle, x, y, w, h, ms): the view's frame moves to the new one over ms milliseconds.
let animateTo: Call = { h, x, y, w, hh, ms in
    guard let view = views[h] else { return 0 }
    NSAnimationContext.runAnimationGroup { context in
        context.duration = Double(ms) / 1000
        view.animator().frame = rect(x, y, w, hh)
    }
    return 0
}

// fade_to(handle, percent, ms): the view's alpha moves to percent over ms milliseconds, then FADE.
let fadeTo: Call = { h, percent, ms, _, _, _ in
    guard let view = views[h] else { return 0 }
    NSAnimationContext.runAnimationGroup({ context in
        context.duration = Double(ms) / 1000
        view.animator().alphaValue = CGFloat(min(percent, 100)) / 100
    }, completionHandler: {
        send("FADE", [.int(Int(h))])
    })
    return 0
}

// shape_new(kind, x, y, w, h, rgb): a filled shape: kind 0 an oval, 1 a rounded rectangle.
let shapeNew: Call = { kind, x, y, w, h, rgb in
    let view = FlippedView(frame: rect(x, y, w, h))
    view.wantsLayer = true
    let bounds = CGRect(x: 0, y: 0, width: CGFloat(w), height: CGFloat(h))
    let shape = CAShapeLayer()
    shape.path = kind == 0 ? CGPath(ellipseIn: bounds, transform: nil)
        : CGPath(roundedRect: bounds, cornerWidth: 16, cornerHeight: 16, transform: nil)
    shape.fillColor = colorOf(rgb).cgColor
    view.layer?.addSublayer(shape)
    return adopt(view)
}

// share_present(text, length): the share menu, with the text; choosing a service (or not) sends SHARE 1 (or 0).
final class Sharing: NSObject, NSSharingServicePickerDelegate {
    let handle: UInt

    init(_ handle: UInt) {
        self.handle = handle
    }

    func sharingServicePicker(_ picker: NSSharingServicePicker, didChoose service: NSSharingService?) {
        send("SHARE", [.int(Int(handle)), .int(service == nil ? 0 : 1)])
    }
}

let sharePresent: Call = { p, n, _, _, _, _ in
    guard let root = rootView() else { return 0 }
    let handle = newHandle()
    let picker = NSSharingServicePicker(items: [text(p, n)])
    let delegate = Sharing(handle)
    picker.delegate = delegate
    keep(handle, delegate)
    let at = CGRect(x: root.bounds.midX, y: root.bounds.midY, width: 1, height: 1)
    picker.show(relativeTo: at, of: root, preferredEdge: .minY)
    return handle
}

// set_accessibility(handle, text, length): the view's accessibility label, what VoiceOver reads and a test finds.
let setAccessibility: Call = { h, p, n, _, _, _ in
    views[h]?.setAccessibilityLabel(text(p, n))
    return 0
}

// announce(text, length): VoiceOver speaks the text.
let announce: Call = { p, n, _, _, _, _ in
    NSAccessibility.post(element: NSApp as Any, notification: .announcementRequested,
                         userInfo: [.announcement: text(p, n),
                                    .priority: NSAccessibilityPriorityLevel.high.rawValue])
    return 0
}

// haptic(style): feedback on a Force Touch trackpad (0 light, 1 medium, 2 heavy, as near as a trackpad comes).
let haptic: Call = { style, _, _, _, _, _ in
    let pattern: NSHapticFeedbackManager.FeedbackPattern = style == 0 ? .generic : (style == 1 ? .alignment : .levelChange)
    NSHapticFeedbackManager.defaultPerformer.perform(pattern, performanceTime: .now)
    return 0
}

// unavailable_new(text, length, x, y, w, h): an empty state: an image and the text.
private func emptyState(_ title: String, _ detail: String, _ frame: CGRect) -> NSView {
    let image = NSImageView(image: NSImage(systemSymbolName: "tray", accessibilityDescription: nil) ?? NSImage())
    image.contentTintColor = .lightGray
    image.symbolConfiguration = .init(pointSize: 36, weight: .regular)
    let main = NSTextField(labelWithString: title)
    main.font = .boldSystemFont(ofSize: 17)
    main.textColor = .white
    let sub = NSTextField(wrappingLabelWithString: detail)
    sub.font = .systemFont(ofSize: 13)
    sub.textColor = .lightGray
    sub.alignment = .center
    let stack = NSStackView(views: [image, main, sub])
    stack.orientation = .vertical
    stack.alignment = .centerX
    stack.spacing = 8
    stack.frame = frame
    return stack
}

let unavailableNew: Call = { p, n, x, y, w, h in
    adopt(emptyState(text(p, n), "Nothing here yet.", rect(x, y, w, h)))
}

// panel_new(x, y, w, h, rgb): a plain view of one colour; w = 0 makes it the whole window.
let panelNew: Call = { x, y, w, h, rgb, _ in
    let frame = w == 0
        ? CGRect(origin: CGPoint(x: CGFloat(x), y: CGFloat(y)), size: rootView()?.bounds.size ?? .zero)
        : rect(x, y, w, h)
    let panel = FlippedView(frame: frame)
    panel.wantsLayer = true
    panel.layer?.backgroundColor = colorOf(rgb).cgColor
    if w == 0 { panel.autoresizingMask = [.width, .height] }
    return adopt(panel)
}

// stack_new(axis, spacing, x, y, w, h): a stack view (0 vertical, 1 horizontal); AppKit does the arranging.
let stackNew: Call = { axis, spacing, x, y, w, h in
    let stack = NSStackView(frame: rect(x, y, w, h))
    stack.orientation = axis == 1 ? .horizontal : .vertical
    stack.spacing = CGFloat(spacing)
    stack.alignment = axis == 1 ? .centerY : .leading
    return adopt(stack)
}

// scroll_new(x, y, w, h): a scroll view whose children stack vertically in its content (ui_add).
let scrollNew: Call = { x, y, w, h, _, _ in
    let scroll = NSScrollView(frame: rect(x, y, w, h))
    scroll.hasVerticalScroller = true
    scroll.drawsBackground = false
    let document = FlippedView()
    document.translatesAutoresizingMaskIntoConstraints = false
    scroll.documentView = document
    let content = NSStackView()
    content.orientation = .vertical
    content.alignment = .leading
    content.spacing = 12
    content.translatesAutoresizingMaskIntoConstraints = false
    document.addSubview(content)
    let clip = scroll.contentView
    NSLayoutConstraint.activate([
        document.topAnchor.constraint(equalTo: clip.topAnchor),
        document.leadingAnchor.constraint(equalTo: clip.leadingAnchor),
        document.widthAnchor.constraint(equalTo: clip.widthAnchor),
        content.topAnchor.constraint(equalTo: document.topAnchor, constant: 12),
        content.bottomAnchor.constraint(equalTo: document.bottomAnchor, constant: -12),
        content.leadingAnchor.constraint(equalTo: document.leadingAnchor, constant: 12),
        content.trailingAnchor.constraint(equalTo: document.trailingAnchor, constant: -12),
    ])
    let handle = adopt(scroll)
    scrollContent[handle] = content
    return handle
}

// image_new(name, length, x, y, w, h): an SF Symbol by its name (such as "star.fill"), in white.
let imageNew: Call = { p, n, x, y, w, h in
    let view = NSImageView(frame: rect(x, y, w, h))
    view.image = NSImage(systemSymbolName: text(p, n), accessibilityDescription: nil)
    view.contentTintColor = .white
    view.imageScaling = .scaleProportionallyUpOrDown
    view.symbolConfiguration = .init(pointSize: CGFloat(min(w, h)) * 0.8, weight: .regular)
    return adopt(view)
}

// add(parent, child): the child moves into the parent: a stack's arranged children, a scroll view's content, or a
// plain view's subviews.
let addChild: Call = { parent, child, _, _, _, _ in
    guard let c = views[child] else { return 0 }
    if let content = scrollContent[parent] {
        content.addArrangedSubview(c)
    } else if let document = scrollDocuments[parent] {
        document.addSubview(c)          // a paging scroll view's pages, each at its own x (ModusSwiftMac2.swift)
    } else if let stack = views[parent] as? NSStackView {
        stack.addArrangedSubview(c)
    } else {
        views[parent]?.addSubview(c)
    }
    return 0
}

// set_text(handle, text, length): a label's or field's text, a button's title, a text view's text.
let setText: Call = { h, p, n, _, _, _ in
    let s = text(p, n)
    if let tv = textViews[h] {
        tv.string = s
    } else if let button = views[h] as? NSButton {
        button.title = s
    } else if let field = views[h] as? NSTextField {
        field.stringValue = s
    }
    return 0
}

// set_value(handle, value): a switch (0/1), a slider (0..100), a progress bar's percent, a spinner (0 stops, 1
// starts), a stepper, a page control, a date (seconds), a picker's row, a colour well (0xRRGGBB).
let setValue: Call = { h, value, _, _, _, _ in
    let v = views[h]
    if let sw = v as? NSSwitch {
        sw.state = value != 0 ? .on : .off
    } else if let slider = v as? NSSlider {
        slider.doubleValue = Double(value)
    } else if let bar = v as? NSProgressIndicator {
        if bar.style == .spinning {
            if value != 0 { bar.startAnimation(nil) } else { bar.stopAnimation(nil) }
        } else {
            bar.doubleValue = Double(min(value, 100))
        }
    } else if let stepper = v as? NSStepper {
        stepper.doubleValue = Double(value)
    } else if let dots = v as? PageDots {
        dots.current = Int(value)
        dots.needsDisplay = true
    } else if let picker = v as? NSDatePicker {
        picker.dateValue = Date(timeIntervalSince1970: TimeInterval(value))
    } else if let popup = v as? NSPopUpButton {
        popup.selectItem(at: Int(value))
    } else if let well = v as? NSColorWell {
        well.color = colorOf(value)
    } else if let segments = v as? NSSegmentedControl {
        segments.selectedSegment = Int(value)
    }
    return 0
}

// text_value(handle, buffer, capacity): the text of a field, label or text view, as UTF-8 at the buffer if it fits;
// its length in bytes either way.
let textValue: Call = { h, buf, cap, _, _, _ in
    let s = textViews[h]?.string ?? (views[h] as? NSTextField)?.stringValue ?? ""
    let bytes = Array(s.utf8)
    if !bytes.isEmpty, bytes.count <= Int(cap), let out = UnsafeMutableRawPointer(bitPattern: buf) {
        bytes.withUnsafeBytes { out.copyMemory(from: $0.baseAddress!, byteCount: bytes.count) }
    }
    return UInt(bytes.count)
}

// list_new(x, y, w, h): an empty list; its rows come from ui_list_set_data.
let listNew: Call = { x, y, w, h, _, _ in
    let scroll = NSScrollView(frame: rect(x, y, w, h))
    scroll.hasVerticalScroller = true
    scroll.drawsBackground = false
    let table = NSTableView()
    // one column as wide as the list, and kept so: AppKit's default column is narrow, and a row's subtitle was cut
    let column = NSTableColumn(identifier: NSUserInterfaceItemIdentifier("row"))
    column.width = CGFloat(w)
    column.resizingMask = .autoresizingMask
    table.addTableColumn(column)
    table.columnAutoresizingStyle = .firstColumnOnlyAutoresizingStyle
    table.headerView = nil
    table.backgroundColor = .clear
    table.style = .plain
    table.selectionHighlightStyle = .regular
    scroll.documentView = table
    let handle = adopt(scroll)
    let source = ModusList(handle: handle)
    table.dataSource = source
    table.delegate = source
    lists[handle] = source
    return handle
}

// list_set_data(handle, bytes, length): the rows, from a term (a list of plists with :TITLE, and :SUBTITLE and
// :SECTION when they have them).
let listSetData: Call = { h, p, n, _, _, _ in
    var rows: [Row] = []
    if n > 0, let base = UnsafeRawPointer(bitPattern: p) {
        let bytes = Array(UnsafeRawBufferPointer(start: base, count: Int(n)))
        if let term = try? decodeTerm(bytes) { rows = rowsOf(term) }
    }
    lists[h]?.setRows(rows)
    tableOf(h)?.reloadData()
    return 0
}

// list_actions(handle, titles, length): the buttons a row's swipe shows; each sends ROWACTION with its index.
let listActions: Call = { h, p, n, _, _, _ in
    lists[h]?.actions = titlesOf(p, n)
    return 0
}

// set_frame(handle, x, y, w, h).
let setFrame: Call = { h, x, y, w, hh, _ in
    views[h]?.frame = rect(x, y, w, hh)
    return 0
}

// remove(handle): the view leaves the window and the handle is forgotten.
let removeView: Call = { h, _, _, _, _, _ in
    views[h]?.removeFromSuperview()
    views[h] = nil
    scrollContent[h] = nil
    lists[h] = nil
    textViews[h] = nil
    kept[h] = nil
    attached[h]?.forEach { $0.removeFromSuperview() }   // views beside it (input_keys_on's row of keys)
    attached[h] = nil
    return 0
}

// send_tap(handle): a click on a button, as the mouse would make it (for tests and automation).
let sendTap: Call = { h, _, _, _, _, _ in
    (views[h] as? NSButton)?.performClick(nil)
    return 0
}

// resign(): typing goes back to the desk: no native view keeps the keyboard.
let resign: Call = { _, _, _, _, _, _ in
    guard let root = rootView(), let window = root.window else { return 0 }
    window.makeFirstResponder(root)
    return 0
}

// The window's size in points, and the phone's safe-area top (a Mac window has none: its title bar is outside).
let screenWidth: Call = { _, _, _, _, _, _ in UInt(rootView()?.bounds.width ?? 0) }
let screenHeight: Call = { _, _, _, _, _, _ in UInt(rootView()?.bounds.height ?? 0) }
let safeAreaTop: Call = { _, _, _, _, _, _ in 0 }

// events_next(buffer, capacity): the oldest event, encoded as a term into the buffer; its length, or 0 for none.
let eventsNext: Call = { buf, cap, _, _, _, _ in
    guard let e = events.first, let out = UnsafeMutableRawPointer(bitPattern: buf) else { return 0 }
    let bytes = e.encoded()
    guard bytes.count <= Int(cap) else { return 0 }
    events.removeFirst()
    bytes.withUnsafeBytes { out.copyMemory(from: $0.baseAddress!, byteCount: bytes.count) }
    return UInt(bytes.count)
}

// MARK: - registration

func register(_ name: String, _ fn: Call) {
    let p = unsafeBitCast(fn, to: UnsafeMutableRawPointer.self)
    name.withCString { modus_swift_register($0, p) }
}

// modus-swift.m calls this, once, before the first Lisp call.  The same names as the phone's (ModusSwift.swift).
@_cdecl("modus_swift_install")
public func modusSwiftInstall() {
    // the phone's own files, built here too (host/ios/swift/: each has a macOS branch where it needs one)
    installGPU()                                   // GPU.swift: Metal, under WebGPU's shape
    installSpeech()                                // Speech.swift: the platform's voice and ear
    installBluetooth()                             // Bluetooth.swift: LE links, for a mesh
    installFiles()                                 // Files.swift: the app's folder, and the file picker
    installCamera()                                // Camera.swift: a preview, frames, photos and video
    installMicrophone()                            // Microphone.swift: its samples, for Lisp
    installNowPlaying()                            // NowPlaying.swift: Now Playing and the media keys
    installGamepad()                               // Gamepad.swift: game controllers
    installSpatial()                               // Spatial.swift: sound placed in three dimensions
    installNative()                                // NativeMac.swift: app-grade components (an app, not the desk)
    installWindow()                                // MacWindow.swift: the window's title, chrome and size
    register("ui_label_new", labelNew)
    register("ui_button_new", buttonNew)
    register("ui_symbol_button_new", symbolButtonNew)
    register("ui_symbol_button_set", symbolButtonSet)
    register("ui_switch_new", switchNew)
    register("ui_slider_new", sliderNew)
    register("ui_text_field_new", textFieldNew)
    register("ui_text_view_new", textViewNew)
    register("ui_segmented_new", segmentedNew)
    register("ui_progress_new", progressNew)
    register("ui_spinner_new", spinnerNew)
    register("ui_stepper_new", stepperNew)
    register("ui_page_new", pageNew)
    register("ui_date_new", dateNew)
    register("ui_long_press_on", longPressOn)
    register("ui_alert_new", alertNew)
    register("ui_alert_action", alertAction)
    register("ui_alert_present", alertPresent)
    register("ui_tab_bar_new", tabBarNew)
    register("ui_refresh_on", refreshOn)
    register("ui_send_refresh", sendRefresh)
    register("ui_search_bar_new", searchBarNew)
    register("ui_toolbar_new", toolbarNew)
    register("ui_picker_new", pickerNew)
    register("ui_blur_new", blurNew)
    register("ui_pan_on", panOn)
    register("ui_grid_new", gridNew)
    register("ui_menu_button_new", menuButtonNew)
    register("ui_color_well_new", colorWellNew)
    register("ui_nav_bar_new", navBarNew)
    register("ui_nav_push", navPush)
    register("ui_nav_pop", navPop)
    register("ui_context_menu_on", contextMenuOn)
    register("ui_animate_to", animateTo)
    register("ui_shape_new", shapeNew)
    register("ui_swipe_on", swipeOn)
    register("ui_share_present", sharePresent)
    register("ui_set_accessibility", setAccessibility)
    register("ui_haptic", haptic)
    register("ui_fade_to", fadeTo)
    register("ui_modal_new", modalNew)
    register("ui_modal_present", modalPresent)
    register("ui_modal_dismiss", modalDismiss)
    register("ui_list_actions", listActions)
    register("ui_paging_new", pagingNew)
    register("ui_announce", announce)
    register("ui_tab_controller_new", tabControllerNew)
    register("ui_tab_content", tabContentOf)
    register("ui_nav_controller_new", navControllerNew)
    register("ui_nav_root", navRoot)
    register("ui_nav_push_view", navPushView)
    register("ui_attributed_label_new", attributedLabelNew)
    register("ui_dynamic_label_new", dynamicLabelNew)
    register("ui_safe_area_top", safeAreaTop)
    register("ui_screen_height", screenHeight)
    register("ui_screen_width", screenWidth)
    register("ui_parallax_on", parallaxOn)
    register("ui_zoom_scroll_new", zoomScrollNew)
    register("ui_zoom_content", zoomContent)
    register("ui_drawn_image_new", drawnImageNew)
    register("ui_file_image_new", fileImageNew)
    register("ui_page_controller_new", pageControllerNew)
    register("ui_page_content", pageContent)
    register("ui_list_editing", listEditing)
    register("ui_edit_menu_on", editMenuOn)
    register("ui_popover_present", popoverPresent)
    register("ui_split_present", splitPresent)
    register("ui_gravity_drop", gravityDrop)
    register("ui_keyboard_avoid_on", keyboardAvoidOn)
    register("ui_keyboard_post", keyboardPost)
    register("ui_document_picker_show", documentPickerShow)
    register("ui_collection_new", collectionNew)
    register("ui_image_picker_show", imagePickerShow)
    register("ui_tap_on", tapOn)
    register("ui_edge_pan_on", edgePanOn)
    register("ui_calendar_new", calendarNew)
    register("ui_unavailable_new", unavailableNew)
    register("ui_color_picker_show", colorPickerShow)
    register("ui_font_picker_show", fontPickerShow)
    register("ui_input_keys_on", inputKeysOn)
    register("ui_panel_new", panelNew)
    register("ui_stack_new", stackNew)
    register("ui_scroll_new", scrollNew)
    register("ui_image_new", imageNew)
    register("ui_add", addChild)
    register("ui_set_text", setText)
    register("ui_set_value", setValue)
    register("ui_text_value", textValue)
    register("ui_list_new", listNew)
    register("ui_list_set_data", listSetData)
    register("ui_set_frame", setFrame)
    register("ui_remove", removeView)
    register("ui_send_tap", sendTap)
    register("ui_resign", resign)
    register("ui_events_next", eventsNext)
}
