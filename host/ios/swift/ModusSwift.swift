// ModusSwift.swift — the native components a modus app builds from Lisp: UIKit views made here, named by a
// handle, and the events they send back.  Lisp calls these by name (modus-swift.m, pseudo-syscall 1060); each
// runs on the main thread, where UIKit lives.
//
// Words and text: every argument is a word.  Text is a pointer and a length: UTF-8 bytes the image holds,
// read here in place.  A function returns a word: a handle, a value, 1 or 0, or nothing (0).
//
// Terms: structured data crosses as terms (Term.swift, the wire format of net/term.lisp): a pointer and a
// length of encoded bytes.  Lisp sends list data (ui_list_set_data), and Swift sends its events as terms.
//
// Events, as terms, oldest first (ui_events_next):
//   (TAP handle)   (SWITCH handle on)   (SLIDER handle value: a double)   (TEXT handle)   (SELECT handle index)
//   (STEP handle value: a double)   (PAGE handle index)   (DATE handle seconds: a double)   (LONGPRESS handle)

import UIKit

typealias Call = @convention(c) (UInt, UInt, UInt, UInt, UInt, UInt) -> UInt

private var views: [UInt: UIView] = [:]
private var nextHandle: UInt = 1
// A scroll view's handle maps to the stack that holds its children (ui_add puts them there).
private var scrollContent: [UInt: UIStackView] = [:]
// A list's handle maps to its data source (a UITableView keeps its data source weakly).
private var lists: [UInt: ModusList] = [:]
// A text view's delegate is weak in UIKit, so the view's handle keeps it here.
private var textViewDelegates: [UInt: ModusTextView] = [:]
private var events: [Term] = []

private func send(_ name: String, _ args: [Term]) {
    events.append(.list([.symbol(name: name, package: "KEYWORD")] + args))
}

private func text(_ p: UInt, _ n: UInt) -> String {
    guard n > 0, let base = UnsafeRawPointer(bitPattern: p) else { return "" }
    return String(decoding: UnsafeRawBufferPointer(start: base, count: Int(n)), as: UTF8.self)
}

private func rect(_ x: UInt, _ y: UInt, _ w: UInt, _ h: UInt) -> CGRect {
    CGRect(x: CGFloat(x), y: CGFloat(y), width: CGFloat(w), height: CGFloat(h))
}

// The views go into the key window's root view, over the app's own screen.
private func keyRoot() -> UIView? {
    let windows = UIApplication.shared.connectedScenes
        .compactMap { $0 as? UIWindowScene }
        .flatMap { $0.windows }
    return (windows.first { $0.isKeyWindow } ?? windows.first)?.rootViewController?.view
}

private func adopt(_ v: UIView) -> UInt {
    let handle = nextHandle
    nextHandle += 1
    views[handle] = v
    keyRoot()?.addSubview(v)
    return handle
}

// A row of a list: its title, and a subtitle when it has one.
struct Row {
    let title: String
    let subtitle: String?
}

// A list of rows.  Lisp sends the rows as data (ui_list_set_data: a list of plists, each with :TITLE and
// optionally :SUBTITLE); the table asks this object for its cells as it needs them, and a tap on a row is a
// SELECT event.  UIKit never calls into Lisp: the rows are data it already holds.
final class ModusList: NSObject, UITableViewDataSource, UITableViewDelegate {
    var rows: [Row] = []
    let handle: UInt

    init(handle: UInt) {
        self.handle = handle
    }

    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        rows.count
    }

    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCell(withIdentifier: "row")
            ?? UITableViewCell(style: .subtitle, reuseIdentifier: "row")
        let row = rows[indexPath.row]
        cell.textLabel?.text = row.title
        cell.textLabel?.textColor = .white
        cell.detailTextLabel?.text = row.subtitle
        cell.detailTextLabel?.textColor = .lightGray
        cell.backgroundColor = .clear
        return cell
    }

    func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        tableView.deselectRow(at: indexPath, animated: true)
        send("SELECT", [.int(Int(handle)), .int(indexPath.row)])
    }
}

// The rows out of a list term: each element a plist (:KEY value ...); keys are symbols by name.
private func rowsOf(_ data: Term) -> [Row] {
    guard let items = data.listItems else { return [] }
    return items.map { item in
        var title = ""
        var subtitle: String?
        let pairs = item.listItems ?? []
        var i = 0
        while i + 1 < pairs.count {
            if case .symbol(let key, _) = pairs[i], case .string(let value) = pairs[i + 1] {
                if key == "TITLE" { title = value }
                if key == "SUBTITLE" { subtitle = value }
            }
            i += 2
        }
        return Row(title: title, subtitle: subtitle)
    }
}

// MARK: - the components

let labelNew: Call = { p, n, x, y, w, h in
    let label = UILabel(frame: rect(x, y, w, h))
    label.text = text(p, n)
    label.font = .systemFont(ofSize: 18)
    label.textColor = .white
    label.numberOfLines = 0
    return adopt(label)
}

let buttonNew: Call = { p, n, x, y, w, h in
    let button = UIButton(type: .system)
    button.frame = rect(x, y, w, h)
    button.setTitle(text(p, n), for: .normal)
    let handle = adopt(button)
    button.addAction(UIAction { _ in send("TAP", [.int(Int(handle))]) }, for: .touchUpInside)
    return handle
}

// switch_new(on, x, y): a switch; its frame is its own size.
let switchNew: Call = { on, x, y, _, _, _ in
    let sw = UISwitch()
    sw.frame.origin = CGPoint(x: CGFloat(x), y: CGFloat(y))
    sw.isOn = on != 0
    let handle = adopt(sw)
    sw.addAction(UIAction { _ in send("SWITCH", [.int(Int(handle)), .int(sw.isOn ? 1 : 0)]) },
                 for: .valueChanged)
    return handle
}

// slider_new(value, x, y, w, h): a slider from 0 to 100.
let sliderNew: Call = { value, x, y, w, h, _ in
    let slider = UISlider(frame: rect(x, y, w, h))
    slider.minimumValue = 0
    slider.maximumValue = 100
    slider.value = Float(value)
    let handle = adopt(slider)
    slider.addAction(UIAction { _ in send("SLIDER", [.int(Int(handle)), .double(Double(slider.value))]) },
                     for: .valueChanged)
    return handle
}

// text_field_new(placeholder, length, x, y, w, h): a rounded text field.
let textFieldNew: Call = { p, n, x, y, w, h in
    let field = UITextField(frame: rect(x, y, w, h))
    field.borderStyle = .roundedRect
    field.placeholder = text(p, n)
    field.textColor = .white
    let handle = adopt(field)
    field.addAction(UIAction { _ in send("TEXT", [.int(Int(handle))]) }, for: .editingChanged)
    return handle
}

// text_view_new(text, length, x, y, w, h): a multi-line text view.  Its changes send TEXT events; a text view
// has no UIAction, so the delegate below does the sending.
final class ModusTextView: NSObject, UITextViewDelegate {
    let handle: UInt

    init(handle: UInt) {
        self.handle = handle
    }

    func textViewDidChange(_ textView: UITextView) {
        send("TEXT", [.int(Int(handle))])
    }
}

let textViewNew: Call = { p, n, x, y, w, h in
    let view = UITextView(frame: rect(x, y, w, h))
    view.text = text(p, n)
    view.font = .systemFont(ofSize: 18)
    view.textColor = .white
    view.backgroundColor = UIColor(white: 0.12, alpha: 1)
    view.layer.cornerRadius = 8
    let handle = adopt(view)
    let delegate = ModusTextView(handle: handle)
    view.delegate = delegate
    textViewDelegates[handle] = delegate
    return handle
}

// The titles out of a term: a list of strings (segmented_new's data).
private func titlesOf(_ p: UInt, _ n: UInt) -> [String] {
    guard n > 0, let base = UnsafeRawPointer(bitPattern: p) else { return [] }
    let bytes = Array(UnsafeRawBufferPointer(start: base, count: Int(n)))
    guard let items = (try? decodeTerm(bytes))?.listItems else { return [] }
    return items.compactMap { item -> String? in
        if case .string(let s) = item { return s }
        return nil
    }
}

// segmented_new(titles, length, x, y, w, h): a segmented control, one segment per title (a term: a list of
// strings).  Choosing a segment sends a SELECT event with its index.
let segmentedNew: Call = { p, n, x, y, w, h in
    let titles = titlesOf(p, n)
    let control = UISegmentedControl(items: titles)
    control.frame = rect(x, y, w, h)
    if !titles.isEmpty { control.selectedSegmentIndex = 0 }
    let handle = adopt(control)
    control.addAction(UIAction { _ in send("SELECT", [.int(Int(handle)), .int(control.selectedSegmentIndex)]) },
                      for: .valueChanged)
    return handle
}

// progress_new(percent, x, y, w, h): a progress bar, from 0 to 100 (set_value moves it).
let progressNew: Call = { percent, x, y, w, h, _ in
    let bar = UIProgressView(progressViewStyle: .default)
    bar.frame = rect(x, y, w, h)
    bar.progressTintColor = .white
    bar.trackTintColor = UIColor(white: 0.3, alpha: 1)
    bar.progress = Float(min(percent, 100)) / 100
    return adopt(bar)
}

// spinner_new(x, y, w, h): an activity indicator, spinning until set_value stops it (0) or starts it (1).
let spinnerNew: Call = { x, y, w, h, _, _ in
    let spinner = UIActivityIndicatorView(style: .medium)
    spinner.frame = rect(x, y, w, h)
    spinner.color = .white
    spinner.startAnimating()
    return adopt(spinner)
}

// stepper_new(value, x, y): a stepper from 0 to 100, one step at a time.  Each change sends a STEP event with
// its value (a double).
let stepperNew: Call = { value, x, y, _, _, _ in
    let stepper = UIStepper()
    stepper.minimumValue = 0
    stepper.maximumValue = 100
    stepper.stepValue = 1
    stepper.value = Double(value)
    stepper.frame.origin = CGPoint(x: CGFloat(x), y: CGFloat(y))
    stepper.sizeToFit()
    let handle = adopt(stepper)
    stepper.addAction(UIAction { _ in send("STEP", [.int(Int(handle)), .double(stepper.value)]) },
                      for: .valueChanged)
    return handle
}

// page_new(pages, current, x, y): a page control, a dot per page, the current one lit.  A tap on a dot sends a
// PAGE event with its index.
let pageNew: Call = { pages, current, x, y, _, _ in
    let control = UIPageControl()
    control.numberOfPages = Int(pages)
    control.currentPage = Int(current)
    control.pageIndicatorTintColor = UIColor(white: 0.35, alpha: 1)
    control.currentPageIndicatorTintColor = .white
    control.frame.origin = CGPoint(x: CGFloat(x), y: CGFloat(y))
    control.sizeToFit()
    let handle = adopt(control)
    control.addAction(UIAction { _ in send("PAGE", [.int(Int(handle)), .int(control.currentPage)]) },
                      for: .valueChanged)
    return handle
}

// date_new(seconds, x, y): a date picker showing the date (seconds since 1970), compact.  A change sends a DATE
// event with the new date's seconds (a double).
let dateNew: Call = { seconds, x, y, _, _, _ in
    let picker = UIDatePicker()
    picker.datePickerMode = .date
    picker.preferredDatePickerStyle = .compact
    picker.overrideUserInterfaceStyle = .dark
    picker.date = Date(timeIntervalSince1970: TimeInterval(seconds))
    picker.frame.origin = CGPoint(x: CGFloat(x), y: CGFloat(y))
    picker.sizeToFit()
    let handle = adopt(picker)
    picker.addAction(UIAction { _ in send("DATE", [.int(Int(handle)), .double(picker.date.timeIntervalSince1970)]) },
                     for: .valueChanged)
    return handle
}

// A long press's target: UIKit keeps a gesture's target weakly, so the view's handle keeps it here.
final class ModusGesture: NSObject {
    let handle: UInt

    init(handle: UInt) {
        self.handle = handle
    }

    @objc func fired(_ recognizer: UILongPressGestureRecognizer) {
        if recognizer.state == .began { send("LONGPRESS", [.int(Int(handle))]) }
    }
}

private var gestureTargets: [UInt: ModusGesture] = [:]

// long_press_on(handle, ms): a long press on the view, held at least ms milliseconds, sends a LONGPRESS event with
// the view's handle.  Once per press, when UIKit recognises it.
let longPressOn: Call = { h, ms, _, _, _, _ in
    guard let view = views[h] else { return 0 }
    let target = ModusGesture(handle: h)
    let recognizer = UILongPressGestureRecognizer(target: target, action: #selector(ModusGesture.fired(_:)))
    recognizer.minimumPressDuration = Double(ms) / 1000
    view.isUserInteractionEnabled = true
    view.addGestureRecognizer(recognizer)
    gestureTargets[h] = target
    return 1
}

// panel_new(x, y, w, h, rgb): a plain view of one colour; w = 0 makes it the whole screen.
let panelNew: Call = { x, y, w, h, rgb, _ in
    let frame = w == 0
        ? CGRect(origin: CGPoint(x: CGFloat(x), y: CGFloat(y)), size: keyRoot()?.bounds.size ?? .zero)
        : rect(x, y, w, h)
    let panel = UIView(frame: frame)
    panel.backgroundColor = UIColor(red: CGFloat((rgb >> 16) & 255) / 255,
                                    green: CGFloat((rgb >> 8) & 255) / 255,
                                    blue: CGFloat(rgb & 255) / 255, alpha: 1)
    return adopt(panel)
}

// stack_new(axis, spacing, x, y, w, h): a stack view, its children laid out along the axis (0 vertical,
// 1 horizontal) with the spacing between them.  Native layout: UIKit does the arranging.
let stackNew: Call = { axis, spacing, x, y, w, h in
    let stack = UIStackView(frame: rect(x, y, w, h))
    stack.axis = axis == 1 ? .horizontal : .vertical
    stack.spacing = CGFloat(spacing)
    return adopt(stack)
}

// scroll_new(x, y, w, h): a scroll view whose children stack vertically in its content (ui_add).
let scrollNew: Call = { x, y, w, h, _, _ in
    let scroll = UIScrollView(frame: rect(x, y, w, h))
    let content = UIStackView()
    content.axis = .vertical
    content.spacing = 12
    content.translatesAutoresizingMaskIntoConstraints = false
    scroll.addSubview(content)
    NSLayoutConstraint.activate([
        content.topAnchor.constraint(equalTo: scroll.contentLayoutGuide.topAnchor, constant: 12),
        content.bottomAnchor.constraint(equalTo: scroll.contentLayoutGuide.bottomAnchor, constant: -12),
        content.leadingAnchor.constraint(equalTo: scroll.contentLayoutGuide.leadingAnchor, constant: 12),
        content.trailingAnchor.constraint(equalTo: scroll.contentLayoutGuide.trailingAnchor, constant: -12),
        content.widthAnchor.constraint(equalTo: scroll.frameLayoutGuide.widthAnchor, constant: -24),
    ])
    let handle = adopt(scroll)
    scrollContent[handle] = content
    return handle
}

// image_new(name, length, x, y, w, h): an SF Symbol by its name (such as "star.fill"), in white.
let imageNew: Call = { p, n, x, y, w, h in
    let view = UIImageView(frame: rect(x, y, w, h))
    view.image = UIImage(systemName: text(p, n))
    view.tintColor = .white
    view.contentMode = .scaleAspectFit
    return adopt(view)
}

// add(parent, child): the child moves into the parent: a stack's arranged children, a scroll view's
// content, or a plain view's subviews.
let addChild: Call = { parent, child, _, _, _, _ in
    guard let c = views[child] else { return 0 }
    if let content = scrollContent[parent] {
        content.addArrangedSubview(c)
    } else if let stack = views[parent] as? UIStackView {
        stack.addArrangedSubview(c)
    } else {
        views[parent]?.addSubview(c)
    }
    return 0
}

// set_text(handle, text, length): a label's text, a button's title, or a text field's text.
let setText: Call = { h, p, n, _, _, _ in
    let s = text(p, n)
    if let label = views[h] as? UILabel {
        label.text = s
    } else if let button = views[h] as? UIButton {
        button.setTitle(s, for: .normal)
    } else if let field = views[h] as? UITextField {
        field.text = s
    } else if let view = views[h] as? UITextView {
        view.text = s
    }
    return 0
}

// set_value(handle, value): a switch's state (0 or 1), a slider's value (0..100), a progress bar's percent
// (0..100), or an activity indicator's state (0 stops it, 1 starts it).
let setValue: Call = { h, value, _, _, _, _ in
    if let sw = views[h] as? UISwitch {
        sw.isOn = value != 0
    } else if let slider = views[h] as? UISlider {
        slider.value = Float(value)
    } else if let bar = views[h] as? UIProgressView {
        bar.progress = Float(min(value, 100)) / 100
    } else if let spinner = views[h] as? UIActivityIndicatorView {
        if value != 0 { spinner.startAnimating() } else { spinner.stopAnimating() }
    } else if let stepper = views[h] as? UIStepper {
        stepper.value = Double(value)
    } else if let page = views[h] as? UIPageControl {
        page.currentPage = Int(value)
    } else if let picker = views[h] as? UIDatePicker {
        picker.date = Date(timeIntervalSince1970: TimeInterval(value))
    }
    return 0
}

// text_value(handle, buffer, capacity): the text of a text field or label, as UTF-8 at the buffer if it
// fits; the length of the text in bytes either way (0 for no text).
let textValue: Call = { h, buf, cap, _, _, _ in
    let s = (views[h] as? UITextField)?.text ?? (views[h] as? UILabel)?.text ?? (views[h] as? UITextView)?.text ?? ""
    let bytes = Array(s.utf8)
    if !bytes.isEmpty, bytes.count <= Int(cap), let out = UnsafeMutableRawPointer(bitPattern: buf) {
        bytes.withUnsafeBytes { out.copyMemory(from: $0.baseAddress!, byteCount: bytes.count) }
    }
    return UInt(bytes.count)
}

// list_new(x, y, w, h): an empty list; its rows come from ui_list_set_data.
let listNew: Call = { x, y, w, h, _, _ in
    let table = UITableView(frame: rect(x, y, w, h), style: .plain)
    table.backgroundColor = .clear
    let handle = adopt(table)
    let source = ModusList(handle: handle)
    table.dataSource = source
    table.delegate = source
    lists[handle] = source
    return handle
}

// list_set_data(handle, bytes, length): the rows, from a term (a list of plists).  The bytes are read here
// in place; a term that is not a list of plists leaves the list empty.
let listSetData: Call = { h, p, n, _, _, _ in
    guard n > 0, let base = UnsafeRawPointer(bitPattern: p) else {
        lists[h]?.rows = []
        (views[h] as? UITableView)?.reloadData()
        return 0
    }
    let bytes = Array(UnsafeRawBufferPointer(start: base, count: Int(n)))
    let rows: [Row]
    do {
        rows = rowsOf(try decodeTerm(bytes))
    } catch {
        print("list_set_data: \(error)")
        rows = []
    }
    lists[h]?.rows = rows
    (views[h] as? UITableView)?.reloadData()
    return 0
}

// set_frame(handle, x, y, w, h).
let setFrame: Call = { h, x, y, w, hh, _ in
    views[h]?.frame = rect(x, y, w, hh)
    return 0
}

// remove(handle): the view leaves the screen and the handle is forgotten.
let removeView: Call = { h, _, _, _, _, _ in
    views[h]?.removeFromSuperview()
    views[h] = nil
    scrollContent[h] = nil
    lists[h] = nil
    return 0
}

// send_tap(handle): a tap on a button, sent as the touch itself would send it (the events then come back as
// any tap does).  For tests and for automation; a user's taps need no call.
let sendTap: Call = { h, _, _, _, _, _ in
    (views[h] as? UIControl)?.sendActions(for: .touchUpInside)
    return 0
}

// resign(): the keyboard goes away.  endEditing ends editing in the whole screen, and the responder chain
// then resigns whatever is the first responder, a text field or the desk (which takes keys too).
let resign: Call = { _, _, _, _, _, _ in
    keyRoot()?.endEditing(true)
    UIApplication.shared.sendAction(#selector(UIResponder.resignFirstResponder), to: nil, from: nil, for: nil)
    return 0
}

// events_next(buffer, capacity): the oldest event, encoded as a term into the buffer; its length in bytes, or 0
// if there is none (or it does not fit, which an event never does at 1 KB).
let eventsNext: Call = { buf, cap, _, _, _, _ in
    guard let e = events.first, let out = UnsafeMutableRawPointer(bitPattern: buf) else { return 0 }
    let bytes = e.encoded()
    guard bytes.count <= Int(cap) else { return 0 }
    events.removeFirst()
    bytes.withUnsafeBytes { out.copyMemory(from: $0.baseAddress!, byteCount: bytes.count) }
    return UInt(bytes.count)
}

// MARK: - registration

private func register(_ name: String, _ fn: Call) {
    let p = unsafeBitCast(fn, to: UnsafeMutableRawPointer.self)
    name.withCString { modus_swift_register($0, p) }
}

// modus-swift.m calls this, once, before the first Lisp call.
@_cdecl("modus_swift_install")
public func modusSwiftInstall() {
    register("ui_label_new", labelNew)
    register("ui_button_new", buttonNew)
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
