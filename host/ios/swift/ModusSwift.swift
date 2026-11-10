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
//   (ALERT handle index)   (REFRESH handle)   (PAN handle dx: a double)   (MENU handle index)   (COLOR handle rgb)
//   (POP handle)   (NAVTAP handle)   (CONTEXT handle index)   (SWIPE handle direction)
//   (SHARE handle completed)   (DISMISS handle)   (FADE handle)   (ROWACTION handle index)

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
    var actions: [String] = []
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

    // Swipe actions for a row, from the actions set with list_actions: a button for each, and a tap on one sends a
    // ROWACTION event with the list's handle and the button's index (the row is the one swiped).
    func tableView(_ tableView: UITableView, trailingSwipeActionsConfigurationForRowAt indexPath: IndexPath)
        -> UISwipeActionsConfiguration? {
        guard !actions.isEmpty else { return nil }
        let items = actions.enumerated().map { i, title in
            UIContextualAction(style: .normal, title: title) { _, _, done in
                send("ROWACTION", [.int(Int(self.handle)), .int(i)])
                done(true)
            }
        }
        return UISwipeActionsConfiguration(actions: items)
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

// alert_new(title, length, message, length): an alert, not yet shown.  Its buttons come from alert_action, and
// alert_present shows it.  Choosing a button sends an ALERT event with the alert's handle and the button's index.
private var alerts: [UInt: UIAlertController] = [:]

// sheet: 1 for an action sheet (its buttons from the bottom of the screen), 0 for an alert.
let alertNew: Call = { tp, tn, mp, mn, sheet, _ in
    let alert = UIAlertController(title: text(tp, tn), message: text(mp, mn),
                                  preferredStyle: sheet != 0 ? .actionSheet : .alert)
    let handle = nextHandle
    nextHandle += 1
    alerts[handle] = alert
    return handle
}

// alert_action(handle, text, length): a button of the alert, in the order they are added.
let alertAction: Call = { h, p, n, _, _, _ in
    guard let alert = alerts[h] else { return 0 }
    let index = alert.actions.count
    alert.addAction(UIAlertAction(title: text(p, n), style: .default) { _ in
        send("ALERT", [.int(Int(h)), .int(index)])
    })
    return 0
}

// alert_present(handle): the alert is shown over the app's screen.
let alertPresent: Call = { h, _, _, _, _, _ in
    guard let alert = alerts[h], let root = keyRoot()?.window?.rootViewController else { return 0 }
    root.present(alert, animated: true)
    return 1
}

// The tab bar's delegate: a choice sends a SELECT event with the item's index (its tag).
final class ModusTabBar: NSObject, UITabBarDelegate {
    let handle: UInt

    init(handle: UInt) {
        self.handle = handle
    }

    func tabBar(_ tabBar: UITabBar, didSelect item: UITabBarItem) {
        send("SELECT", [.int(Int(handle)), .int(item.tag)])
    }
}

private var tabBarDelegates: [UInt: ModusTabBar] = [:]

// tab_bar_new(titles, length, x, y, w, h): a tab bar, an item per title (a term: a list of strings).  Choosing an
// item sends a SELECT event with its index.
let tabBarNew: Call = { p, n, x, y, w, h in
    let bar = UITabBar(frame: rect(x, y, w, h))
    bar.items = titlesOf(p, n).enumerated().map { i, title in
        UITabBarItem(title: title, image: nil, tag: i)
    }
    bar.selectedItem = bar.items?.first
    bar.tintColor = .white
    bar.barTintColor = UIColor(white: 0.12, alpha: 1)
    bar.unselectedItemTintColor = .lightGray
    let handle = adopt(bar)
    let delegate = ModusTabBar(handle: handle)
    bar.delegate = delegate
    tabBarDelegates[handle] = delegate
    return handle
}

// refresh_on(list): pulling the list down sends a REFRESH event with the list's handle.  The spinner stops when
// set_value sets the list to 0.
let refreshOn: Call = { h, _, _, _, _, _ in
    guard let table = views[h] as? UITableView else { return 0 }
    let control = UIRefreshControl()
    control.tintColor = .white
    control.addAction(UIAction { _ in send("REFRESH", [.int(Int(h))]) }, for: .valueChanged)
    table.refreshControl = control
    return 1
}

// The search bar's delegate: each change of its text sends a TEXT event (read the text with text_value).
final class ModusSearchBar: NSObject, UISearchBarDelegate {
    let handle: UInt

    init(handle: UInt) {
        self.handle = handle
    }

    func searchBar(_ searchBar: UISearchBar, textDidChange searchText: String) {
        send("TEXT", [.int(Int(handle))])
    }
}

private var searchBarDelegates: [UInt: ModusSearchBar] = [:]

// search_bar_new(placeholder, length, x, y, w, h): a search bar, its placeholder text, white on the dark screen.
let searchBarNew: Call = { p, n, x, y, w, h in
    let bar = UISearchBar(frame: rect(x, y, w, h))
    bar.placeholder = text(p, n)
    bar.searchBarStyle = .minimal
    bar.tintColor = .white
    bar.searchTextField.textColor = .white
    let handle = adopt(bar)
    let delegate = ModusSearchBar(handle: handle)
    bar.delegate = delegate
    searchBarDelegates[handle] = delegate
    return handle
}

// toolbar_new(titles, length, x, y, w, h): a toolbar with a button per title (a term: a list of strings), spaced
// evenly.  Tapping a button sends a SELECT event with its index.
let toolbarNew: Call = { p, n, x, y, w, h in
    let bar = UIToolbar(frame: rect(x, y, w, h))
    bar.tintColor = .white
    bar.barTintColor = UIColor(white: 0.12, alpha: 1)
    let handle = adopt(bar)
    var items: [UIBarButtonItem] = []
    for (i, title) in titlesOf(p, n).enumerated() {
        if i > 0 { items.append(UIBarButtonItem(systemItem: .flexibleSpace)) }
        items.append(UIBarButtonItem(primaryAction: UIAction(title: title) { _ in
            send("SELECT", [.int(Int(handle)), .int(i)])
        }))
    }
    bar.items = items
    return handle
}

// The picker's data source and delegate: one wheel of rows, the titles; a choice sends a SELECT event with its row.
final class ModusPicker: NSObject, UIPickerViewDataSource, UIPickerViewDelegate {
    let handle: UInt
    var rows: [String] = []

    init(handle: UInt) {
        self.handle = handle
    }

    func numberOfComponents(in pickerView: UIPickerView) -> Int {
        1
    }

    func pickerView(_ pickerView: UIPickerView, numberOfRowsInComponent component: Int) -> Int {
        rows.count
    }

    func pickerView(_ pickerView: UIPickerView, titleForRow row: Int, forComponent component: Int) -> String? {
        rows[row]
    }

    func pickerView(_ pickerView: UIPickerView, didSelectRow row: Int, inComponent component: Int) {
        send("SELECT", [.int(Int(handle)), .int(row)])
    }
}

private var pickers: [UInt: ModusPicker] = [:]

// picker_new(titles, length, x, y, w, h): a wheel of rows, one per title (a term: a list of strings).  Choosing a
// row sends a SELECT event with its index.
let pickerNew: Call = { p, n, x, y, w, h in
    let wheel = UIPickerView(frame: rect(x, y, w, h))
    let handle = adopt(wheel)
    let source = ModusPicker(handle: handle)
    source.rows = titlesOf(p, n)
    wheel.dataSource = source
    wheel.delegate = source
    pickers[handle] = source
    return handle
}

// blur_new(x, y, w, h): a blur of what is behind it, dark.  Its children (ui_add) go in its content view.
let blurNew: Call = { x, y, w, h, _, _ in
    let view = UIVisualEffectView(effect: UIBlurEffect(style: .systemThinMaterialDark))
    view.frame = rect(x, y, w, h)
    return adopt(view)
}

// The pan gesture's target: a pan that ends sends a PAN event with the horizontal distance dragged (a double).
final class ModusPan: NSObject {
    let handle: UInt

    init(handle: UInt) {
        self.handle = handle
    }

    @objc func panned(_ recognizer: UIPanGestureRecognizer) {
        guard recognizer.state == .ended, let view = recognizer.view else { return }
        send("PAN", [.int(Int(handle)), .double(Double(recognizer.translation(in: view).x))])
    }
}

private var panTargets: [UInt: ModusPan] = [:]

// pan_on(handle): a pan on the view, its events as above.  The view takes touches for it.
let panOn: Call = { h, _, _, _, _, _ in
    guard let view = views[h] else { return 0 }
    let target = ModusPan(handle: h)
    let recognizer = UIPanGestureRecognizer(target: target, action: #selector(ModusPan.panned(_:)))
    view.isUserInteractionEnabled = true
    view.addGestureRecognizer(recognizer)
    panTargets[h] = target
    return 1
}

// The grid's data source and delegate: one tile per title; a tap on a tile sends a SELECT event with its index.
final class ModusGrid: NSObject, UICollectionViewDataSource, UICollectionViewDelegate {
    let handle: UInt
    var items: [String] = []

    init(handle: UInt) {
        self.handle = handle
    }

    func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        items.count
    }

    func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        let cell = collectionView.dequeueReusableCell(withReuseIdentifier: "tile", for: indexPath)
        let label: UILabel
        if let existing = cell.contentView.viewWithTag(1) as? UILabel {
            label = existing
        } else {
            label = UILabel(frame: cell.contentView.bounds)
            label.tag = 1
            label.autoresizingMask = [.flexibleWidth, .flexibleHeight]
            label.textColor = .white
            label.textAlignment = .center
            cell.contentView.addSubview(label)
        }
        label.text = items[indexPath.item]
        cell.backgroundColor = UIColor(white: 0.2, alpha: 1)
        return cell
    }

    func collectionView(_ collectionView: UICollectionView, didSelectItemAt indexPath: IndexPath) {
        send("SELECT", [.int(Int(handle)), .int(indexPath.item)])
    }
}

private var grids: [UInt: ModusGrid] = [:]

// grid_new(titles, length, x, y, w, h): a collection view of tiles, one per title (a term: a list of strings).
let gridNew: Call = { p, n, x, y, w, h in
    let layout = UICollectionViewFlowLayout()
    layout.itemSize = CGSize(width: 100, height: 60)
    layout.minimumInteritemSpacing = 8
    layout.minimumLineSpacing = 8
    let view = UICollectionView(frame: rect(x, y, w, h), collectionViewLayout: layout)
    view.backgroundColor = .clear
    view.register(UICollectionViewCell.self, forCellWithReuseIdentifier: "tile")
    let handle = adopt(view)
    let source = ModusGrid(handle: handle)
    source.items = titlesOf(p, n)
    view.dataSource = source
    view.delegate = source
    grids[handle] = source
    return handle
}

// menu_button_new(choices, length, x, y, w, h): a button, 'options', whose menu offers the choices (a term: a list
// of strings).  Choosing one sends a MENU event with its index.
let menuButtonNew: Call = { p, n, x, y, w, h in
    let button = UIButton(type: .system)
    button.frame = rect(x, y, w, h)
    button.setTitle("options", for: .normal)
    button.showsMenuAsPrimaryAction = true
    let handle = adopt(button)
    let choices = titlesOf(p, n).enumerated().map { i, title in
        UIAction(title: title) { _ in send("MENU", [.int(Int(handle)), .int(i)]) }
    }
    button.menu = UIMenu(title: "", children: choices)
    return handle
}

// The colour of rgb, as 0xRRGGBB.
private func colorOf(_ rgb: UInt) -> UIColor {
    UIColor(red: CGFloat((rgb >> 16) & 255) / 255, green: CGFloat((rgb >> 8) & 255) / 255,
            blue: CGFloat(rgb & 255) / 255, alpha: 1)
}

// color_well_new(rgb, x, y, w, h): a colour well showing the colour.  Choosing a colour sends a COLOR event with the
// new colour as 0xRRGGBB.
let colorWellNew: Call = { rgb, x, y, w, h, _ in
    let well = UIColorWell(frame: rect(x, y, w, h))
    well.selectedColor = colorOf(rgb)
    let handle = adopt(well)
    well.addAction(UIAction { _ in
        var r: CGFloat = 0, g: CGFloat = 0, b: CGFloat = 0, a: CGFloat = 0
        well.selectedColor?.getRed(&r, green: &g, blue: &b, alpha: &a)
        let packed = (Int(r * 255) << 16) | (Int(g * 255) << 8) | Int(b * 255)
        send("COLOR", [.int(Int(handle)), .int(packed)])
    }, for: .valueChanged)
    return handle
}

// The navigation bar's delegate: a pop (the back button, or a pop from code) sends a POP event.
final class ModusNavBar: NSObject, UINavigationBarDelegate {
    let handle: UInt

    init(handle: UInt) {
        self.handle = handle
    }

    func navigationBar(_ navigationBar: UINavigationBar, didPop item: UINavigationItem) {
        send("POP", [.int(Int(handle))])
    }
}

private var navBarDelegates: [UInt: ModusNavBar] = [:]

// nav_bar_new(x, y, w, h): a navigation bar, dark, with no items.  nav_push pushes an item.
let navBarNew: Call = { x, y, w, h, _, _ in
    let bar = UINavigationBar(frame: rect(x, y, w, h))
    bar.barStyle = .black
    bar.tintColor = .white
    bar.titleTextAttributes = [.foregroundColor: UIColor.white]
    let handle = adopt(bar)
    let delegate = ModusNavBar(handle: handle)
    bar.delegate = delegate
    navBarDelegates[handle] = delegate
    return handle
}

// nav_push(bar, title, length, right, length): pushes an item with the title and, unless the right title is empty,
// a right button.  Tapping the right button sends a NAVTAP event with the bar's handle.
let navPush: Call = { h, tp, tn, rp, rn, _ in
    guard let bar = views[h] as? UINavigationBar else { return 0 }
    let item = UINavigationItem(title: text(tp, tn))
    let right = text(rp, rn)
    if !right.isEmpty {
        item.rightBarButtonItem = UIBarButtonItem(primaryAction: UIAction(title: right) { _ in
            send("NAVTAP", [.int(Int(h))])
        })
    }
    bar.pushItem(item, animated: false)
    return 0
}

// nav_pop(bar): pops the top item.
let navPop: Call = { h, _, _, _, _, _ in
    (views[h] as? UINavigationBar)?.popItem(animated: false)
    return 0
}

// The context menu's delegate: a long press on the view opens a menu of the choices; a choice sends a CONTEXT event
// with the view's handle and the choice's index.
final class ModusContextMenu: NSObject, UIContextMenuInteractionDelegate {
    let handle: UInt
    let choices: [String]

    init(handle: UInt, choices: [String]) {
        self.handle = handle
        self.choices = choices
    }

    func contextMenuInteraction(_ interaction: UIContextMenuInteraction,
                                configurationForMenuAtLocation location: CGPoint) -> UIContextMenuConfiguration? {
        UIContextMenuConfiguration(identifier: nil, previewProvider: nil) { _ in
            UIMenu(title: "", children: self.choices.enumerated().map { i, title in
                UIAction(title: title) { _ in send("CONTEXT", [.int(Int(self.handle)), .int(i)]) }
            })
        }
    }
}

private var contextMenus: [UInt: ModusContextMenu] = [:]

// context_menu_on(handle, titles, length): a context menu on the view, of the choices (a term: a list of strings).
let contextMenuOn: Call = { h, p, n, _, _, _ in
    guard let view = views[h] else { return 0 }
    let menu = ModusContextMenu(handle: h, choices: titlesOf(p, n))
    view.isUserInteractionEnabled = true
    view.addInteraction(UIContextMenuInteraction(delegate: menu))
    contextMenus[h] = menu
    return 1
}

// animate_to(handle, x, y, w, h, ms): the view's frame moves to the new one over ms milliseconds.
let animateTo: Call = { h, x, y, w, hh, ms in
    guard let view = views[h] else { return 0 }
    UIView.animate(withDuration: Double(ms) / 1000) {
        view.frame = rect(x, y, w, hh)
    }
    return 0
}

// shape_new(kind, x, y, w, h, rgb): a filled shape in the frame: kind 0 an oval, 1 a rounded rectangle.
let shapeNew: Call = { kind, x, y, w, h, rgb in
    let view = UIView(frame: rect(x, y, w, h))
    view.backgroundColor = .clear
    let path = kind == 0
        ? UIBezierPath(ovalIn: CGRect(x: 0, y: 0, width: CGFloat(w), height: CGFloat(h)))
        : UIBezierPath(roundedRect: CGRect(x: 0, y: 0, width: CGFloat(w), height: CGFloat(h)), cornerRadius: 16)
    let shape = CAShapeLayer()
    shape.path = path.cgPath
    shape.fillColor = colorOf(rgb).cgColor
    view.layer.addSublayer(shape)
    return adopt(view)
}

// The swipe's target: a swipe sends a SWIPE event with the view's handle and the direction (0 left, 1 right,
// 2 up, 3 down).
final class ModusSwipe: NSObject {
    let handle: UInt
    let direction: Int

    init(handle: UInt, direction: Int) {
        self.handle = handle
        self.direction = direction
    }

    @objc func swiped(_ recognizer: UISwipeGestureRecognizer) {
        send("SWIPE", [.int(Int(handle)), .int(direction)])
    }
}

private var swipeTargets: [UInt: ModusSwipe] = [:]

// swipe_on(handle, direction): a swipe in the direction (0 left, 1 right, 2 up, 3 down) on the view.
let swipeOn: Call = { h, dir, _, _, _, _ in
    guard let view = views[h] else { return 0 }
    let target = ModusSwipe(handle: h, direction: Int(dir))
    let recognizer = UISwipeGestureRecognizer(target: target, action: #selector(ModusSwipe.swiped(_:)))
    switch dir {
    case 0: recognizer.direction = .left
    case 1: recognizer.direction = .right
    case 2: recognizer.direction = .up
    default: recognizer.direction = .down
    }
    view.isUserInteractionEnabled = true
    view.addGestureRecognizer(recognizer)
    swipeTargets[h] = target
    return 1
}

// share_present(text, length): the share sheet, with the text as its item.  Its completion is a SHARE event with the
// handle and 1 (an activity was completed) or 0 (it was not).  Returns the handle.
let sharePresent: Call = { p, n, _, _, _, _ in
    guard let root = keyRoot()?.window?.rootViewController else { return 0 }
    let handle = nextHandle
    nextHandle += 1
    let sheet = UIActivityViewController(activityItems: [text(p, n)], applicationActivities: nil)
    sheet.completionWithItemsHandler = { _, completed, _, _ in
        send("SHARE", [.int(Int(handle)), .int(completed ? 1 : 0)])
    }
    root.present(sheet, animated: true)
    return handle
}

// set_accessibility(handle, text, length): the view's accessibility label, what VoiceOver reads and what a test finds.
let setAccessibility: Call = { h, p, n, _, _, _ in
    views[h]?.accessibilityLabel = text(p, n)
    return 0
}

// haptic(style): an impact feedback, 0 light, 1 medium, 2 heavy.
let haptic: Call = { style, _, _, _, _, _ in
    let weight: UIImpactFeedbackGenerator.FeedbackStyle = style == 0 ? .light : (style == 1 ? .medium : .heavy)
    UIImpactFeedbackGenerator(style: weight).impactOccurred()
    return 0
}

// fade_to(handle, percent, ms): the view's alpha moves to percent (0 to 100) over ms milliseconds.  Its completion
// is a FADE event with the handle.
let fadeTo: Call = { h, percent, ms, _, _, _ in
    guard let view = views[h] else { return 0 }
    UIView.animate(withDuration: Double(ms) / 1000, animations: {
        view.alpha = CGFloat(min(percent, 100)) / 100
    }, completion: { _ in
        send("FADE", [.int(Int(h))])
    })
    return 0
}

// A modal sheet's content: a view (adopted as the others are not: it is shown by the sheet, not the screen) and its
// controller.  Dragging the sheet down is a DISMISS event; dismissing it from code is not.
private var modalControllers: [UInt: UIViewController] = [:]

final class ModusModal: NSObject, UIAdaptivePresentationControllerDelegate {
    let handle: UInt

    init(handle: UInt) {
        self.handle = handle
    }

    func presentationControllerDidDismiss(_ presentationController: UIPresentationController) {
        send("DISMISS", [.int(Int(handle))])
    }
}

private var modalDelegates: [UInt: ModusModal] = [:]

// modal_new(): a sheet's content, not yet shown.  Its children are added with ui_add.
let modalNew: Call = { _, _, _, _, _, _ in
    let controller = UIViewController()
    controller.view.backgroundColor = UIColor(white: 0.12, alpha: 1)
    let handle = nextHandle
    nextHandle += 1
    views[handle] = controller.view
    modalControllers[handle] = controller
    return handle
}

// modal_present(handle, large): the sheet comes up over the app, at medium height, or at large (large != 0).
let modalPresent: Call = { h, large, _, _, _, _ in
    guard let controller = modalControllers[h], let root = keyRoot()?.window?.rootViewController else { return 0 }
    controller.modalPresentationStyle = .pageSheet
    controller.sheetPresentationController?.detents = large != 0 ? [.large()] : [.medium()]
    root.present(controller, animated: true)
    let delegate = ModusModal(handle: h)
    controller.presentationController?.delegate = delegate
    modalDelegates[h] = delegate
    return 1
}

// modal_dismiss(handle): the sheet goes away.
let modalDismiss: Call = { h, _, _, _, _, _ in
    modalControllers[h]?.dismiss(animated: true)
    return 0
}

// list_actions(handle, titles, length): the swipe actions of the list's rows (a term: a list of strings).
let listActions: Call = { h, p, n, _, _, _ in
    lists[h]?.actions = titlesOf(p, n)
    return 1
}

// The paging scroll view's delegate: when a page stops, a PAGE event with its index.
final class ModusPaging: NSObject, UIScrollViewDelegate {
    let handle: UInt
    var width: CGFloat = 0

    init(handle: UInt) {
        self.handle = handle
    }

    func scrollViewDidEndDecelerating(_ scrollView: UIScrollView) {
        guard width > 0 else { return }
        send("PAGE", [.int(Int(handle)), .int(Int((scrollView.contentOffset.x / width).rounded()))])
    }
}

private var pagings: [UInt: ModusPaging] = [:]

// paging_new(pages, x, y, w, h): a scroll view of pages, each as wide as the view, that snap to a page (ui_add puts
// each page's content in place, at its own x).  A page that comes to rest is a PAGE event with its index.
let pagingNew: Call = { pages, x, y, w, h, _ in
    let scroll = UIScrollView(frame: rect(x, y, w, h))
    scroll.isPagingEnabled = true
    scroll.showsHorizontalScrollIndicator = false
    scroll.contentSize = CGSize(width: CGFloat(w) * CGFloat(pages), height: CGFloat(h))
    let handle = adopt(scroll)
    let delegate = ModusPaging(handle: handle)
    delegate.width = CGFloat(w)
    scroll.delegate = delegate
    pagings[handle] = delegate
    return handle
}

// announce(text, length): VoiceOver announces the text, with no change on the screen.
let announce: Call = { p, n, _, _, _, _ in
    UIAccessibility.post(notification: .announcement, argument: text(p, n))
    return 0
}

// A tab bar controller's delegate: a tab picked sends a SELECT event with its index.
final class ModusTabs: NSObject, UITabBarControllerDelegate {
    let handle: UInt

    init(handle: UInt) {
        self.handle = handle
    }

    func tabBarController(_ tabBarController: UITabBarController, didSelect viewController: UIViewController) {
        send("SELECT", [.int(Int(handle)), .int(tabBarController.selectedIndex)])
    }
}

private var tabDelegates: [UInt: ModusTabs] = [:]
private var tabContent: [UInt: [UInt]] = [:]

// tab_controller_new(titles, length): a tab bar controller, a tab per title, each tab's content a view (tab_content).
// Shown with modal_present; a tab picked is a SELECT event with its index.  Returns the controller's handle.
let tabControllerNew: Call = { p, n, _, _, _, _ in
    let tabs = UITabBarController()
    var controllers: [UIViewController] = []
    var contents: [UInt] = []
    for (i, title) in titlesOf(p, n).enumerated() {
        let vc = UIViewController()
        vc.view.backgroundColor = UIColor(white: 0.12, alpha: 1)
        vc.tabBarItem = UITabBarItem(title: title, image: nil, tag: i)
        controllers.append(vc)
        let content = nextHandle
        nextHandle += 1
        views[content] = vc.view
        contents.append(content)
    }
    tabs.viewControllers = controllers
    let handle = nextHandle
    nextHandle += 1
    modalControllers[handle] = tabs
    tabContent[handle] = contents
    let delegate = ModusTabs(handle: handle)
    tabs.delegate = delegate
    tabDelegates[handle] = delegate
    return handle
}

// tab_content(controller, index): the view of a tab, to add to with ui_add.
let tabContentOf: Call = { h, i, _, _, _, _ in
    tabContent[h]?[Int(i)] ?? 0
}

// nav_controller_new(): a navigation controller whose root view is its first content (nav_root).  Shown with
// modal_present.
private var navContent: [UInt: [UInt]] = [:]

let navControllerNew: Call = { _, _, _, _, _, _ in
    let root = UIViewController()
    root.view.backgroundColor = UIColor(white: 0.12, alpha: 1)
    root.title = "start"
    let nav = UINavigationController(rootViewController: root)
    nav.navigationBar.barStyle = .black
    nav.navigationBar.tintColor = .white
    nav.navigationBar.titleTextAttributes = [.foregroundColor: UIColor.white]
    let rootContent = nextHandle
    nextHandle += 1
    views[rootContent] = root.view
    let handle = nextHandle
    nextHandle += 1
    modalControllers[handle] = nav
    navContent[handle] = [rootContent]
    return handle
}

// nav_root(controller): the view of the navigation controller's root.
let navRoot: Call = { h, _, _, _, _, _ in
    navContent[h]?.first ?? 0
}

// nav_push_view(controller, title, length): pushes a view titled so, and returns its handle.  Its back button reads
// 'back'.
let navPushView: Call = { h, p, n, _, _, _ in
    guard let nav = modalControllers[h] as? UINavigationController else { return 0 }
    let vc = UIViewController()
    vc.view.backgroundColor = UIColor(white: 0.12, alpha: 1)
    vc.title = text(p, n)
    vc.navigationItem.backButtonTitle = "back"
    let content = nextHandle
    nextHandle += 1
    views[content] = vc.view
    navContent[h, default: []].append(content)
    nav.pushViewController(vc, animated: false)
    return content
}

// send_refresh(list): a pull on the list, sent as the refresh control sends it (its REFRESH event then comes back).
// For tests and automation, like send_tap: a one-shot swipe in the simulator does not start a pull.
let sendRefresh: Call = { h, _, _, _, _, _ in
    (views[h] as? UITableView)?.refreshControl?.sendActions(for: .valueChanged)
    return 0
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
    } else if let effect = views[parent] as? UIVisualEffectView {
        effect.contentView.addSubview(c)
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
    } else if let bar = views[h] as? UISearchBar {
        bar.text = s
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
    } else if let table = views[h] as? UITableView {
        if value == 0 { table.refreshControl?.endRefreshing() } else { table.refreshControl?.beginRefreshing() }
    } else if let wheel = views[h] as? UIPickerView {
        wheel.selectRow(Int(value), inComponent: 0, animated: false)
    } else if let well = views[h] as? UIColorWell {
        well.selectedColor = colorOf(value)
    }
    return 0
}

// text_value(handle, buffer, capacity): the text of a text field or label, as UTF-8 at the buffer if it
// fits; the length of the text in bytes either way (0 for no text).
let textValue: Call = { h, buf, cap, _, _, _ in
    let s = (views[h] as? UITextField)?.text ?? (views[h] as? UILabel)?.text ?? (views[h] as? UITextView)?.text
        ?? (views[h] as? UISearchBar)?.text ?? ""
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
