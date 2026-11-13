// Native.swift — app-grade native components: what a modus app needs to look like an ordinary iPhone app
// rather than a window over glass's desk.  The navigation controller is the window's ROOT; screens are pushed
// onto it and say when they are popped; tables are the system's inset-grouped ones in the system's colours,
// with an icon, a detail and a disclosure per row, swipe actions, pull to refresh and an empty state; alerts take
// text fields; a form screen edits a record; a status bar at the bottom shows a transfer; files go out through
// the share sheet and come in through the document picker.  ModusSwift.swift's components stay as they are, for
// the storybook and the desk.
//
// As there, every argument is a word and text is a pointer and a length; structured data (a table's rows) comes
// as a term, and the events go back as terms through the same queue (ui_events_next).  Events whose value is
// more than one thing carry a list:
//   (TAP handle)                     a bar button, a form's button
//   (SELECT table row)               a row tapped (its index among all rows)
//   (ROWACTION table (action row))   a swipe action, and the row it was on
//   (REFRESH table)                  pulled to refresh (end it with app_table_refresh_end)
//   (POP screen)                     a screen left the navigation stack (back button or swipe)
//   (ALERT alert (index text ...))   an alert's button, and the text of each of its fields
//   (FORM form (saved value ...))    a form closed: saved 1 with its fields' values, or 0 cancelled
//   (STATUSCANCEL nav)               the status bar's Cancel
//   (PICKED picker path)             a file picked (copied into the app's tmp), or "" when cancelled
//   (SHARE handle completed)         the share sheet closed

import QuickLook
import UIKit
import UniformTypeIdentifiers

private var navs: [UInt: UINavigationController] = [:]
private var navDelegates: [UInt: NativeNav] = [:]
private var screens: [UInt: UIViewController] = [:]
private var tables: [UInt: NativeTable] = [:]
private var barButtons: [UInt: UIBarButtonItem] = [:]
private var nativeAlerts: [UInt: UIAlertController] = [:]
private var forms: [UInt: NativeForm] = [:]
private var pickers: [UInt: NativePicker] = [:]
private var statuses: [UInt: NativeStatus] = [:]

private func newHandle() -> UInt {
    let h = nextHandle
    nextHandle += 1
    return h
}

private func handleOf<T: AnyObject>(_ object: T, in table: [UInt: T]) -> UInt? {
    table.first { $0.value === object }?.key
}

private func keyWindow() -> UIWindow? {
    let windows = UIApplication.shared.connectedScenes
        .compactMap { $0 as? UIWindowScene }
        .flatMap { $0.windows }
    return windows.first { $0.isKeyWindow } ?? windows.first
}

/// What to present over: the top of whatever the window shows -- but not a screen on its way out (a form just
/// saved), which would take what was presented over it away with it.
private func presenter() -> UIViewController? {
    var top = keyWindow()?.rootViewController
    while let next = top?.presentedViewController, !next.isBeingDismissed { top = next }
    return top
}

/// A popover-style presentation (an action sheet or a share sheet on an iPad) must be anchored or UIKit raises.
private func anchor(_ vc: UIViewController, on host: UIViewController) {
    if let popover = vc.popoverPresentationController {
        popover.sourceView = host.view
        popover.sourceRect = CGRect(x: host.view.bounds.midX, y: host.view.bounds.midY, width: 1, height: 1)
        popover.permittedArrowDirections = []
    }
}

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

/// An RGB integer, or the name of a colour that follows light and dark mode: :LABEL (black, or white in the
/// dark), :SECONDARY, :TERTIARY, :GRAY, :DARKGRAY.
private func color(_ t: Term?) -> UIColor? {
    if case .symbol(let name, _)? = t {
        switch name {
        case "LABEL": return .label
        case "SECONDARY": return .secondaryLabel
        case "TERTIARY": return .tertiaryLabel
        case "GRAY": return .systemGray
        case "DARKGRAY": return UIColor { $0.userInterfaceStyle == .dark ? .systemGray2 : .darkGray }
        default: return nil
        }
    }
    guard case .int(let rgb)? = t else { return nil }
    return UIColor(red: CGFloat((rgb >> 16) & 255) / 255, green: CGFloat((rgb >> 8) & 255) / 255,
                   blue: CGFloat(rgb & 255) / 255, alpha: 1)
}

// THE THEME (app_theme): the system's blue, or MONO -- black and white, the other way round in dark mode.  The
// window's tint is what buttons, links, switches, the cursor and alerts' buttons take; red stays red.
private var monoTheme = false
private func applyTheme(_ window: UIWindow) {
    guard monoTheme else { return }
    window.tintColor = .label
}

/// app_theme(mono): 1, black and white (inverted in dark mode).  Before the first screen, so every control
/// made afterwards takes it.
let appTheme: Call = { mono, _, _, _, _, _ in
    monoTheme = mono != 0
    if monoTheme {
        UISwitch.appearance().onTintColor = .label
        UISwitch.appearance().thumbTintColor = .systemBackground     // a dark thumb on a white track, in the dark
        if let window = keyWindow() { applyTheme(window) }
    }
    return 1
}

private func termBytes(_ p: UInt, _ n: UInt) -> Term? {
    guard n > 0, let base = UnsafeRawPointer(bitPattern: p) else { return nil }
    return try? decodeTerm(Array(UnsafeRawBufferPointer(start: base, count: Int(n))))
}

// MARK: - navigation

/// Watches a navigation controller: a screen that leaves its stack (the back button, the edge swipe) is a POP.
final class NativeNav: NSObject, UINavigationControllerDelegate, UIGestureRecognizerDelegate {
    var shown: [UIViewController] = []

    /// THE SYSTEM'S BACK SWIPES (the edge's, and on iOS 26 the whole screen's).  iOS turns them off for a screen
    /// that hides the back button or puts its own in its place -- as the browser does, for its chevron -- unless
    /// their delegate says otherwise: this one allows them wherever there is a screen to go back to, except
    /// inside a browser's folders (where the browser's own swipe goes up a folder; from its first, these leave it).
    weak var nav: UINavigationController?
    func gestureRecognizerShouldBegin(_ g: UIGestureRecognizer) -> Bool {
        guard let nav, nav.viewControllers.count > 1, let top = nav.topViewController else { return false }
        if let h = handleOf(top, in: screens), let b = browsers[h] { return b.pages.count <= 1 && !b.shelfShown }
        return true
    }

    /// A path title does not travel with its page (that is a lot of motion for a few words): during a push or a
    /// pop -- the back swipe too -- the outgoing one fades where it is and the incoming one appears once the page
    /// has landed.  A swipe given up puts the old one back.
    func navigationController(_ nav: UINavigationController, willShow vc: UIViewController, animated: Bool) {
        guard animated, let coordinator = nav.transitionCoordinator else { return }
        // the label INSIDE the title view: the bar animates the title view's own alpha in a transition, over ours
        let incoming = (vc.navigationItem.titleView as? TitleBox)?.label
        let outgoing = (coordinator.viewController(forKey: .from)?.navigationItem.titleView as? TitleBox)?.label
        guard incoming != nil || outgoing != nil else { return }
        incoming?.alpha = 0
        coordinator.animate(alongsideTransition: { _ in outgoing?.alpha = 0 }, completion: { context in
            let shown = context.isCancelled ? outgoing : incoming
            UIView.animate(withDuration: 0.15) { shown?.alpha = 1 }
            if context.isCancelled { incoming?.alpha = 1 } else { outgoing?.alpha = 1 }
        })
    }

    func navigationController(_ nav: UINavigationController, didShow vc: UIViewController, animated: Bool) {
        for gone in shown where !nav.viewControllers.contains(gone) {
            if let h = handleOf(gone, in: screens) {
                if let b = browsers[h] {
                    for page in b.pages { tables[page.table] = nil }
                }
                browsers[h] = nil
                send("POP", [.int(Int(h))])
                screens[h] = nil
            }
        }
        if let h = handleOf(vc, in: screens), let b = browsers[h] {
            b.updateGestures()
        } else {
            nav.interactivePopGestureRecognizer?.isEnabled = true
            if #available(iOS 26.0, *) { nav.interactiveContentPopGestureRecognizer?.isEnabled = true }
        }
        shown = nav.viewControllers
    }
}

/// A screen's title as a view of its own (a plain title cannot be faded, see NativeNav).  A large title, where
/// the bar shows one, is drawn from the screen's title as before.
/// A title view holding a label, which is what fades (see NativeNav).  It asks for more width than any bar has,
/// so the bar gives it all the room between its buttons -- the back button and the island on the right -- and
/// the text is centred in THAT, not on the screen; where it does not fit, the label gives way as its line-break
/// mode says (a path: from the left, with an ellipsis).
final class TitleBox: UIView {
    let label = UILabel()
    /// A second line under the title, smaller: where the path is (a server's name).  Empty: none.
    let subtitle = UILabel()
    /// With column titles: the span the subtitle goes under (the folder on top's column).  Nil: the whole width.
    var subtitleSpan: CGRect? { didSet { setNeedsLayout() } }
    init(_ label: UILabel) {
        super.init(frame: CGRect(x: 0, y: 0, width: 0, height: 44))
        self.label.attributedText = label.attributedText
        self.label.textAlignment = .center
        self.label.lineBreakMode = label.lineBreakMode
        self.label.frame = bounds
        self.label.autoresizingMask = [.flexibleWidth, .flexibleHeight]
        addSubview(self.label)
        segments.frame = bounds
        segments.autoresizingMask = [.flexibleWidth, .flexibleHeight]
        addSubview(segments)
        subtitle.font = .preferredFont(forTextStyle: .caption1)
        subtitle.textColor = .secondaryLabel
        subtitle.textAlignment = .center
        subtitle.lineBreakMode = .byTruncatingMiddle
        subtitle.isHidden = true
        addSubview(subtitle)
        setContentCompressionResistancePriority(.defaultLow, for: .horizontal)
        fit()
    }

    func setSubtitle(_ text: String) {
        subtitle.text = text
        subtitle.isHidden = text.isEmpty
        setNeedsLayout()
    }

    // with a subtitle, the title (or the column titles) moves up to make room for it beneath
    override func layoutSubviews() {
        super.layoutSubviews()
        let b = bounds
        let top = subtitle.isHidden ? b : CGRect(x: 0, y: 1, width: b.width, height: b.height - 17)
        label.frame = top
        segments.frame = top
        for l in segmentLabels { l.frame.origin.y = 0; l.frame.size.height = top.height }
        let span = segments.isHidden ? nil : subtitleSpan
        subtitle.frame = CGRect(x: span?.minX ?? 0, y: b.height - 18, width: span?.width ?? b.width, height: 15)
    }

    /// Instead of the one label: a label per column, each centred over its column (RECT, in this view's
    /// coordinates), the current one in bold.  None: the one label again.
    let segments = UIView()
    private var segmentLabels: [UILabel] = []
    func setSegments(_ items: [(text: String, rect: CGRect, current: Bool)]) {
        label.isHidden = !items.isEmpty
        segments.isHidden = items.isEmpty
        while segmentLabels.count < items.count {
            let l = UILabel()
            l.textAlignment = .center
            l.lineBreakMode = .byTruncatingMiddle
            segments.addSubview(l)
            segmentLabels.append(l)
        }
        for (i, l) in segmentLabels.enumerated() {
            guard i < items.count else { l.alpha = 0; continue }
            let item = items[i]
            l.text = item.text
            l.font = item.current ? .preferredFont(forTextStyle: .headline) : .preferredFont(forTextStyle: .body)
            l.textColor = item.current ? .label : .secondaryLabel
            l.frame = CGRect(x: item.rect.minX, y: 0, width: item.rect.width, height: segments.bounds.height)
            l.alpha = 1
        }
    }
    required init?(coder: NSCoder) { fatalError() }

    override var intrinsicContentSize: CGSize { CGSize(width: 2000, height: 44) }

    func fit() {
        frame.size.width = intrinsicContentSize.width
        superview?.setNeedsLayout()
    }
}

private func titleLabel(_ title: String) -> UIView {
    let label = UILabel()
    label.text = title
    label.font = .preferredFont(forTextStyle: .headline)
    label.textAlignment = .center
    return TitleBox(label)
}

private func makeScreen(_ title: String, _ vc: UIViewController = UIViewController()) -> (UInt, UIViewController) {
    vc.view.backgroundColor = .systemGroupedBackground
    vc.title = title
    vc.navigationItem.backButtonDisplayMode = .minimal
    vc.navigationItem.titleView = titleLabel(title)
    let h = newHandle()
    screens[h] = vc
    return (h, vc)
}

/// app_nav_new(title): a navigation controller with a root screen titled TITLE, made the window's root (the app is
/// this, not a window over the desk).  Returns the navigation controller's handle; app_nav_root gives the screen's.
/// app_nav_new(title, quiet): QUIET 1 is a navigation bar that holds still -- no large titles (whose collapse
/// moves the whole bar on every push) and a back button that is a bare chevron (no title sliding across).
let appNavNew: Call = { p, n, quiet, _, _, _ in
    let (rootHandle, root) = makeScreen(text(p, n))
    let nav = UINavigationController(rootViewController: root)
    nav.navigationBar.prefersLargeTitles = quiet == 0
    let delegate = NativeNav()
    delegate.shown = [root]
    delegate.nav = nav
    nav.delegate = delegate
    nav.interactivePopGestureRecognizer?.delegate = delegate
    if #available(iOS 26.0, *) { nav.interactiveContentPopGestureRecognizer?.delegate = delegate }
    let h = newHandle()
    navs[h] = nav
    navDelegates[h] = delegate
    navRoots[h] = rootHandle
    installRoot(nav)
    return h
}

/// Make VC the window's root -- once there is a window.  The image starts beside UIKit, so an app's first call
/// can come before the scene has connected and made one (the first launch after an install, typically), and a
/// root given to no window is never seen: the app stays black.  Until then, look again every 50 ms.
private func installRoot(_ vc: UIViewController, tries: Int = 200) {
    if let window = keyWindow() {
        applyTheme(window)
        window.rootViewController = vc
        window.makeKeyAndVisible()
    } else if tries > 0 {
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.05) { installRoot(vc, tries: tries - 1) }
    }
}
private var navRoots: [UInt: UInt] = [:]

let appNavRoot: Call = { h, _, _, _, _, _ in navRoots[h] ?? 0 }

/// app_push(nav, title): a new screen pushed onto NAV; its handle.
let appPush: Call = { h, p, n, _, _, _ in
    guard let nav = navs[h] else { return 0 }
    let (sh, vc) = makeScreen(text(p, n))
    vc.navigationItem.largeTitleDisplayMode = .never
    nav.pushViewController(vc, animated: true)
    return sh
}

/// app_screen(title): a screen to push, not shown yet: fill it (its table, its bar buttons), then app_show it.
/// Built before the push, it slides in whole -- what is added to a screen while its push animates moves into
/// place inside that animation.  Returns its handle.
let appScreen: Call = { p, n, _, _, _, _ in
    let (sh, vc) = makeScreen(text(p, n))
    vc.navigationItem.largeTitleDisplayMode = .never
    return sh
}

/// app_show(nav, screen): push SCREEN (from app_screen) onto NAV, laid out first, with the status bar the screen
/// it covers has.
let appShow: Call = { h, sh, _, _, _, _ in
    guard let nav = navs[h], let vc = screens[sh] else { return 0 }
    vc.toolbarItems = nav.topViewController?.toolbarItems
    vc.view.frame = nav.view.bounds
    vc.view.layoutIfNeeded()
    nav.pushViewController(vc, animated: true)
    return 1
}

/// app_path_title(screen, term): SCREEN's title as a path -- TERM a list of strings, outermost first -- shown
/// centred between the bar's buttons, the last one in bold and the rest grey, " / " between them; when it does not
/// fit, the beginning gives way to an ellipsis, so where you are is always what shows.
let appPathTitle: Call = { h, p, n, _, _, _ in
    guard let vc = screens[h], let t = termBytes(p, n) else { return 0 }
    let parts = (t.listItems ?? []).compactMap { str($0) }
    vc.navigationItem.titleView = TitleBox(pathLabel(parts))
    vc.title = parts.last
    return 1
}

/// The path PARTS as a label: the last in bold, the rest grey, " / " between; centred, the beginning
/// giving way to an ellipsis when it does not fit.
private func pathLabel(_ parts: [String]) -> UILabel {
    let font = UIFont.preferredFont(forTextStyle: .headline)
    let out = NSMutableAttributedString()
    for (i, part) in parts.enumerated() {
        let last = i == parts.count - 1
        out.append(NSAttributedString(string: part, attributes: [
            .font: last ? font : UIFont.preferredFont(forTextStyle: .body),
            .foregroundColor: last ? UIColor.label : UIColor.secondaryLabel]))
        if !last {
            out.append(NSAttributedString(string: " / ", attributes: [
                .font: UIFont.preferredFont(forTextStyle: .body), .foregroundColor: UIColor.tertiaryLabel]))
        }
    }
    // the layout rides IN the string: a label given attributed text takes its alignment and line breaking from it
    let style = NSMutableParagraphStyle()
    style.alignment = .center
    style.lineBreakMode = .byTruncatingHead
    out.addAttribute(.paragraphStyle, value: style, range: NSRange(location: 0, length: out.length))
    let label = UILabel()
    label.attributedText = out
    return label
}

/// app_pop(nav): back one screen, as the back button does (and the screen's POP follows).
let appPop: Call = { h, _, _, _, _, _ in
    navs[h]?.popViewController(animated: true)
    return 1
}

let appSetTitle: Call = { h, p, n, _, _, _ in
    guard let vc = screens[h] else { return 0 }
    vc.title = text(p, n)
    vc.navigationItem.titleView = titleLabel(text(p, n))
    return 1
}

/// app_bar_button(screen, symbol-or-title, right): a button in SCREEN's navigation bar -- an SF Symbol when SYMBOL
/// names one, else a text button.  A tap is a TAP event with the button's handle.
let appBarButton: Call = { h, p, n, right, _, _ in
    guard let vc = screens[h] else { return 0 }
    let bh = newHandle()
    let name = text(p, n)
    let action = UIAction { _ in send("TAP", [.int(Int(bh))]) }
    let item: UIBarButtonItem
    if let image = UIImage(systemName: name) {
        item = UIBarButtonItem(image: image, primaryAction: action)
    } else {
        item = UIBarButtonItem(title: name, primaryAction: action)
    }
    barButtons[bh] = item
    if right != 0 {
        vc.navigationItem.rightBarButtonItems = (vc.navigationItem.rightBarButtonItems ?? []) + [item]
    } else {
        vc.navigationItem.leftBarButtonItems = (vc.navigationItem.leftBarButtonItems ?? []) + [item]
    }
    return bh
}

/// app_bar_menu(screen, plist): a bar button that opens a menu -- (:SYMBOL :RIGHT :ITEMS), ITEMS plists of :ID
/// :TITLE :SUBTITLE :IMAGE :ON (a checkmark) :GROUP (items of one group together, a rule between groups; the first of a
/// group may carry :HEADER).  A choice is a MENU event with the button and the item's id.
let appBarMenu: Call = { h, p, n, _, _, _ in
    guard let vc = screens[h], let t = termBytes(p, n) else { return 0 }
    let m = plist(t)
    let bh = newHandle()
    let item = UIBarButtonItem(image: UIImage(systemName: str(m["SYMBOL"]) ?? "ellipsis"),
                               menu: barMenu(bh, m["ITEMS"]))
    barButtons[bh] = item
    if flag(m["RIGHT"]) {
        vc.navigationItem.rightBarButtonItems = (vc.navigationItem.rightBarButtonItems ?? []) + [item]
    } else {
        vc.navigationItem.leftBarButtonItems = (vc.navigationItem.leftBarButtonItems ?? []) + [item]
    }
    return bh
}

/// app_bar_menu_set(button, items): the menu's items again (a checkmark moved).
let appBarMenuSet: Call = { h, p, n, _, _, _ in
    barButtons[h]?.menu = barMenu(h, termBytes(p, n))
    return 1
}

private func barMenu(_ bh: UInt, _ items: Term?) -> UIMenu {
    var groups: [(group: Int, header: String?, actions: [UIAction])] = []
    for item in items?.listItems ?? [] {
        let m = plist(item)
        guard case .int(let id)? = m["ID"] else { continue }
        var g = 0
        if case .int(let k)? = m["GROUP"] { g = k }
        let action = UIAction(title: str(m["TITLE"]) ?? "", image: str(m["IMAGE"]).flatMap { UIImage(systemName: $0) },
                              state: flag(m["ON"]) ? .on : .off) { _ in send("MENU", [.int(Int(bh)), .int(id)]) }
        action.subtitle = str(m["SUBTITLE"])
        if let last = groups.last, last.group == g {
            groups[groups.count - 1].actions.append(action)
        } else {
            groups.append((g, str(m["HEADER"]), [action]))
        }
    }
    return UIMenu(children: groups.map { UIMenu(title: $0.header ?? "", options: .displayInline, children: $0.actions) })
}

/// app_bar_button_enable(button, on)
let appBarButtonEnable: Call = { h, on, _, _, _, _ in
    barButtons[h]?.isEnabled = on != 0
    return 1
}

// MARK: - tables

struct NativeRow {
    let title: String
    let subtitle: String?
    let detail: String?
    let image: String?
    let tint: UIColor?
    let chevron: Bool
    let section: String?
    let index: Int
    let sort: String?
    let group: Int?                              // :GROUP -- shown before higher groups, when the table says so
    let keys: [String: Term]                     // :KEYS -- what else the rows can be ordered by (:DATE 1700000000)
    let bar: Double?                             // :BAR -- a share, in thousandths: a fill behind the row that wide
    let dot: UIColor?
    let menu: [(id: Int, title: String, image: String?, destructive: Bool)]

    /// Shows the same (its cell need not be redrawn).
    func looksLike(_ o: NativeRow) -> Bool {
        title == o.title && subtitle == o.subtitle && detail == o.detail && image == o.image && tint == o.tint
            && chevron == o.chevron && dot == o.dot && bar == o.bar && menu.map(\.title) == o.menu.map(\.title)
    }
}

/// The fill behind a row that shows its share: the label's colour, faint, FRACTION of the width.
final class ShareFill: UIView {
    let fraction: Double
    private let fill = UIView()
    init(_ fraction: Double) {
        self.fraction = fraction
        super.init(frame: .zero)
        fill.backgroundColor = UIColor.label.withAlphaComponent(0.08)
        addSubview(fill)
    }
    required init?(coder: NSCoder) { fatalError() }
    override func layoutSubviews() {
        super.layoutSubviews()
        fill.frame = CGRect(x: 0, y: 0, width: bounds.width * fraction, height: bounds.height)
    }
}

struct NativeAction {
    let title: String
    let destructive: Bool
    let tint: UIColor?
}

final class NativeTable: NSObject, UITableViewDataSource, UITableViewDelegate, UISearchBarDelegate,
                         UIContextMenuInteractionDelegate {
    let handle: UInt
    let table: UITableView
    var groups: [(title: String?, rows: [NativeRow])] = []
    var actions: [NativeAction] = []
    var empty: UIContentUnavailableConfiguration?
    var loading = false
    /// While set and there are no rows: what the table is waiting on, or why it has nothing (app_table_placeholder).
    var placeholder: UIContentUnavailableConfiguration?

    init(handle: UInt, table: UITableView) {
        self.handle = handle
        self.table = table
    }

    /// Every row, in the order they came (a row's INDEX is its place here, what a SELECT reports).
    var all: [NativeRow] = []

    // THE MENU OFF THE ROWS (app_table_menu): a long press on the table where there is no row -- under the last
    // one, or in an empty folder -- for what is done to the table as a whole.  A MENU event, (-1 id).  The
    // rows' own menus are the table's; this interaction answers only where they do not.
    private var spaceMenu: [(id: Int, title: String, image: String?, destructive: Bool)] = []
    private var spaceInteraction: UIContextMenuInteraction?

    func setSpaceMenu(_ items: [(id: Int, title: String, image: String?, destructive: Bool)]) {
        spaceMenu = items
        if spaceInteraction == nil {
            let i = UIContextMenuInteraction(delegate: self)
            table.addInteraction(i)
            spaceInteraction = i
        }
    }

    // nothing lifts out for it: the whole table would (the interaction's view), so the "preview" is a clear
    // point where the finger is, and the menu simply opens there
    private let spaceAnchor: UIView = {
        let v = UIView()
        v.backgroundColor = .clear
        v.isUserInteractionEnabled = false
        return v
    }()
    private func spacePreview() -> UITargetedPreview? {
        guard spaceAnchor.window != nil else { return nil }
        let p = UIPreviewParameters()
        p.backgroundColor = .clear
        p.visiblePath = UIBezierPath(rect: spaceAnchor.bounds)
        return UITargetedPreview(view: spaceAnchor, parameters: p)
    }
    func contextMenuInteraction(_ interaction: UIContextMenuInteraction, configuration: UIContextMenuConfiguration,
                                highlightPreviewForItemWithIdentifier identifier: any NSCopying) -> UITargetedPreview? {
        spacePreview()
    }
    func contextMenuInteraction(_ interaction: UIContextMenuInteraction, configuration: UIContextMenuConfiguration,
                                dismissalPreviewForItemWithIdentifier identifier: any NSCopying) -> UITargetedPreview? {
        spacePreview()
    }

    func contextMenuInteraction(_ interaction: UIContextMenuInteraction,
                                configurationForMenuAtLocation location: CGPoint) -> UIContextMenuConfiguration? {
        guard !spaceMenu.isEmpty, table.indexPathForRow(at: location) == nil else { return nil }
        if let header = table.tableHeaderView, header.frame.contains(location) { return nil }   // the filter
        spaceAnchor.frame = CGRect(x: location.x, y: location.y, width: 1, height: 1)
        if spaceAnchor.superview !== table { table.addSubview(spaceAnchor) }
        let items = spaceMenu
        return UIContextMenuConfiguration(identifier: "space" as NSString, previewProvider: nil) { [weak self] _ in
            UIMenu(children: items.map { item in
                UIAction(title: item.title, image: item.image.flatMap { UIImage(systemName: $0) },
                         attributes: item.destructive ? .destructive : []) { _ in
                    guard let self else { return }
                    send("MENU", [.int(Int(self.handle)), .list([.int(-1), .int(item.id)])])
                }
            })
        }
    }

    // THE ORDER (app_table_order): by one of the rows' :KEYS, or by :SORT (as the Finder sorts names) when
    // ORDERKEY is nil; either way round; lower :GROUPs first (folders before files) when GROUPSFIRST.  A tie
    // goes by :SORT, A to Z.
    var orderKey: String?
    var descending = false
    var groupsFirst = true

    private func compareKeys(_ a: Term?, _ b: Term?) -> ComparisonResult {
        switch (a, b) {
        case (.int(let x)?, .int(let y)?): return x < y ? .orderedAscending : x > y ? .orderedDescending : .orderedSame
        case (.string(let x)?, .string(let y)?): return x.localizedStandardCompare(y)
        case (nil, nil): return .orderedSame
        case (nil, _): return .orderedAscending          // nothing to go by (a folder's size): first, A to Z
        case (_, nil): return .orderedDescending
        default: return .orderedSame
        }
    }

    func ordered(_ rows: [NativeRow]) -> [NativeRow] {
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

    func setRows(_ rows: [NativeRow]) {
        all = rows
        loading = false
        regroup()
    }

    // ---- the filter: a search bar above the first row, out of sight until pulled down to ----

    private(set) var filter = ""
    private var searchBar: UISearchBar?
    private var searchTucked = false              // tucked away once, when there was first enough to scroll

    /// A search bar heads the table; what is typed in it filters the rows by name, as they come too.
    func enableSearch() {
        let bar = UISearchBar(frame: CGRect(x: 0, y: 0, width: table.bounds.width, height: 56))
        bar.searchBarStyle = .minimal
        bar.placeholder = "Filter"
        bar.autocapitalizationType = .none
        bar.autocorrectionType = .no
        bar.delegate = self
        searchBar = bar
        table.tableHeaderView = bar
        table.keyboardDismissMode = .onDrag
    }

    func searchBar(_ searchBar: UISearchBar, textDidChange text: String) {
        filter = text
        regroup()
    }
    func searchBarTextDidBeginEditing(_ bar: UISearchBar) { bar.setShowsCancelButton(true, animated: true) }
    func searchBarTextDidEndEditing(_ bar: UISearchBar) { bar.setShowsCancelButton(!filter.isEmpty, animated: true) }
    func searchBarSearchButtonClicked(_ bar: UISearchBar) { bar.resignFirstResponder() }
    func searchBarCancelButtonClicked(_ bar: UISearchBar) {
        bar.text = ""
        filter = ""
        bar.resignFirstResponder()
        bar.setShowsCancelButton(false, animated: true)
        regroup()
    }

    /// The search bar out of sight above the first row, once -- when the rows first fill more than the screen
    /// and nobody has scrolled yet.  Pulled down to, it is there.
    private func tuckSearch() {
        guard let bar = searchBar, !searchTucked, filter.isEmpty else { return }
        let top = -table.adjustedContentInset.top
        guard table.contentSize.height > table.bounds.height, abs(table.contentOffset.y - top) < 1 else { return }
        searchTucked = true
        table.contentOffset.y = top + bar.frame.height
    }

    // ---- the scrubber: for a list many screens long ----

    private var scrubber: Scrubber?
    private var scrubTop: NSLayoutConstraint?
    private var scrubBottom: NSLayoutConstraint?
    private var fade: DispatchWorkItem?
    /// Off while something covers the list's edge (the browser's preview shelf).
    var scrubbable = true { didSet { if !scrubbable { hideScrubber(now: true) }; updateIndicator() } }

    /// Long enough that a thumb to drag beats scrolling.
    var long: Bool { table.contentSize.height > table.bounds.height * 5 }

    func updateIndicator() {
        table.showsVerticalScrollIndicator = scrubbable && !long
    }

    private func makeScrubber() -> Scrubber {
        if let s = scrubber { return s }
        let s = Scrubber()
        s.alpha = 0
        s.isUserInteractionEnabled = false
        s.translatesAutoresizingMaskIntoConstraints = false
        table.addSubview(s)
        // pinned to the table's frame, not its content: it holds still while the rows scroll under it
        let g = table.frameLayoutGuide
        let top = s.topAnchor.constraint(equalTo: g.topAnchor)
        let bottom = s.bottomAnchor.constraint(equalTo: g.bottomAnchor)
        NSLayoutConstraint.activate([top, bottom, s.trailingAnchor.constraint(equalTo: g.trailingAnchor),
                                     s.widthAnchor.constraint(equalToConstant: 30)])
        scrubTop = top
        scrubBottom = bottom
        s.onScrub = { [weak self] frac, done in self?.scrubbed(to: frac, done: done) }
        scrubber = s
        return s
    }

    private var scrollRange: CGFloat {
        max(1, table.contentSize.height + table.adjustedContentInset.top + table.adjustedContentInset.bottom
                - table.bounds.height)
    }

    func scrollViewDidScroll(_ scrollView: UIScrollView) {
        updateIndicator()
        guard scrubbable, long else { hideScrubber(now: true); return }
        let s = makeScrubber()
        scrubTop?.constant = table.adjustedContentInset.top
        scrubBottom?.constant = -table.adjustedContentInset.bottom
        table.bringSubviewToFront(s)
        if !s.dragging {
            s.place((table.contentOffset.y + table.adjustedContentInset.top) / scrollRange)
        }
        if s.alpha < 1 {
            s.isUserInteractionEnabled = true
            UIView.animate(withDuration: 0.15) { s.alpha = 1 }
        }
        hideScrubber(now: false)
    }

    /// Out of sight: now, or once the list has been still a moment (and the thumb is not held).
    private func hideScrubber(now: Bool) {
        fade?.cancel()
        guard let s = scrubber else { return }
        if now { s.alpha = 0; s.isUserInteractionEnabled = false; return }
        let work = DispatchWorkItem { [weak self, weak s] in
            guard let s, !s.dragging else { return }
            UIView.animate(withDuration: 0.3) { s.alpha = 0 }
            s.isUserInteractionEnabled = false
            _ = self
        }
        fade = work
        DispatchQueue.main.asyncAfter(deadline: .now() + 2.5, execute: work)
    }

    private func scrubbed(to frac: CGFloat, done: Bool) {
        table.contentOffset.y = frac * scrollRange - table.adjustedContentInset.top
        // the bubble: the name at the top of the list
        let y = table.contentOffset.y + table.adjustedContentInset.top + 1
        if let ip = table.indexPathForRow(at: CGPoint(x: 1, y: max(y, 0))),
           ip.section < groups.count, ip.row < groups[ip.section].rows.count {
            scrubber?.label(groups[ip.section].rows[ip.row].title)
        }
        if done { hideScrubber(now: false) }
    }

    /// More rows, after those there are: a listing that arrives a batch at a time.  Only the batch crosses.
    func append(_ rows: [NativeRow]) {
        all += rows
        regroup()
    }

    /// The rows as shown: by their :SORT keys where they have them (as the Finder sorts -- "file 2" before
    /// "file 10"), then in sections.
    func regroup() {
        let shown = filter.isEmpty ? all : all.filter { $0.title.localizedStandardContains(filter) }
        let rows = shown.contains { $0.sort != nil || $0.group != nil } ? ordered(shown) : shown
        var out: [(title: String?, rows: [NativeRow])] = []
        for row in rows {
            if let last = out.last, last.title == row.section {
                out[out.count - 1].rows.append(row)
            } else {
                out.append((row.section, [row]))
            }
        }
        let before = groups
        groups = out
        // THE SAME ROWS IN THE SAME PLACES, some of them changed (a server's dot): those cells updated where they
        // are -- a whole reload under a context menu closing, or a swipe half open, makes a row flash
        let sameShape = before.count == out.count && zip(before, out).allSatisfy {
            $0.title == $1.title && $0.rows.count == $1.rows.count
                && zip($0.rows, $1.rows).allSatisfy { $0.title == $1.title && $0.index == $1.index }
        }
        if sameShape && !before.isEmpty {
            var changed: [IndexPath] = []
            for (s, group) in out.enumerated() {
                for (r, row) in group.rows.enumerated() where !row.looksLike(before[s].rows[r]) {
                    changed.append(IndexPath(row: r, section: s))
                }
            }
            if !changed.isEmpty { UIView.performWithoutAnimation { table.reconfigureRows(at: changed) } }
            showEmpty()
            return
        }
        defer { tuckSearch(); updateIndicator() }
        // new rows appear in place: a reload during a push would otherwise slide them in with it
        UIView.performWithoutAnimation {
            table.reloadData()
            showEmpty()
            table.layoutIfNeeded()
        }
    }

    /// While more rows are coming and some are here: a spinner and the count so far, under the last row -- once
    /// the listing has taken 0.4 s, so a quick one does not flash it.
    var loadingSince = Date()
    func showFooter() {
        guard loading, !all.isEmpty, Date().timeIntervalSince(loadingSince) > 0.4 else {
            table.tableFooterView = nil; return
        }
        let spinner = UIActivityIndicatorView(style: .medium)
        spinner.startAnimating()
        let count = UILabel()
        count.text = "\(all.count.formatted()) so far"
        count.font = .preferredFont(forTextStyle: .footnote)
        count.textColor = .secondaryLabel
        let row = UIStackView(arrangedSubviews: [spinner, count])
        row.spacing = 8
        let footer = UIView(frame: CGRect(x: 0, y: 0, width: table.bounds.width, height: 56))
        row.translatesAutoresizingMaskIntoConstraints = false
        footer.addSubview(row)
        NSLayoutConstraint.activate([row.centerXAnchor.constraint(equalTo: footer.centerXAnchor),
                                     row.centerYAnchor.constraint(equalTo: footer.centerYAnchor)])
        table.tableFooterView = footer
    }

    func showEmpty() {
        showFooter()
        // the filter only once there is something to filter: not over a placeholder, or an empty folder
        if let bar = searchBar {
            let want: UIView? = all.isEmpty ? nil : bar
            if table.tableHeaderView !== want { table.tableHeaderView = want }
        }
        let none = groups.allSatisfy { $0.rows.isEmpty }
        if none, !filter.isEmpty, !all.isEmpty {
            var c = UIContentUnavailableConfiguration.search()
            c.secondaryText = "No names here have “\(filter)” in them."
            table.backgroundView = UIContentUnavailableView(configuration: c)
        } else if none, let config = placeholder {
            if let shown = table.backgroundView as? UIContentUnavailableView {
                shown.configuration = config                  // a new step, in place: no flash
            } else {
                table.backgroundView = UIContentUnavailableView(configuration: config)
            }
        } else if none && loading {
            let spinner = UIActivityIndicatorView(style: .medium)
            spinner.startAnimating()
            table.backgroundView = spinner
        } else if none, let config = empty {
            let view = UIContentUnavailableView(configuration: config)
            table.backgroundView = view
        } else {
            table.backgroundView = nil
        }
    }

    func numberOfSections(in tableView: UITableView) -> Int { groups.count }
    func tableView(_ tableView: UITableView, titleForHeaderInSection section: Int) -> String? { groups[section].title }
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int { groups[section].rows.count }

    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let row = groups[indexPath.section].rows[indexPath.row]
        // registered (makeTable), so a row reconfigured in place gets its own cell back
        let cell = tableView.dequeueReusableCell(withIdentifier: "native", for: indexPath)
        var content = UIListContentConfiguration.subtitleCell()
        content.text = row.title
        content.secondaryText = row.subtitle
        content.secondaryTextProperties.color = .secondaryLabel
        if let name = row.image {
            content.image = UIImage(systemName: name)
            content.imageProperties.tintColor = row.tint ?? .tintColor
        }
        cell.contentConfiguration = content
        // a share of the whole (disk usage): a faint fill behind the row, that much of its width
        var bg = cell.defaultBackgroundConfiguration()
        if let bar = row.bar { bg.customView = ShareFill(bar) }
        cell.backgroundConfiguration = bg
        // the detail on the right, as Settings shows a value
        if let detail = row.detail {
            let label = UILabel()
            label.text = detail
            label.font = .preferredFont(forTextStyle: .subheadline)
            label.textColor = .secondaryLabel
            label.sizeToFit()
            cell.accessoryView = row.chevron ? nil : label
            cell.accessoryType = row.chevron ? .disclosureIndicator : .none
            if row.chevron {
                // a chevron and a detail: the detail goes into the subtitle's line instead
                var c = content
                c.secondaryText = [row.subtitle, detail].compactMap { $0 }.joined(separator: " · ")
                cell.contentConfiguration = c
            }
        } else {
            cell.accessoryView = nil
            cell.accessoryType = row.chevron ? .disclosureIndicator : .none
        }
        if let dot = row.dot {
            // a status dot (a server with a connection open), before the chevron if there is one
            let mark = UIView(frame: CGRect(x: 0, y: 0, width: 8, height: 8))
            mark.backgroundColor = dot
            mark.layer.cornerRadius = 4
            var parts: [UIView] = [mark]
            if row.chevron {
                let chevron = UIImageView(image: UIImage(systemName: "chevron.forward",
                                                         withConfiguration: UIImage.SymbolConfiguration(textStyle: .footnote, scale: .small)))
                chevron.tintColor = .tertiaryLabel
                parts.append(chevron)
            }
            let box = UIStackView(arrangedSubviews: parts)
            box.spacing = 10
            box.alignment = .center
            mark.widthAnchor.constraint(equalToConstant: 8).isActive = true
            mark.heightAnchor.constraint(equalToConstant: 8).isActive = true
            box.frame = CGRect(origin: .zero, size: box.systemLayoutSizeFitting(UIView.layoutFittingCompressedSize))
            cell.accessoryType = .none
            cell.accessoryView = box
        }
        return cell
    }

    func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        tableView.deselectRow(at: indexPath, animated: true)
        lastSelected = indexPath
        send("SELECT", [.int(Int(handle)), .int(groups[indexPath.section].rows[indexPath.row].index)])
    }

    /// The row tapped last, which a browser shows MARKED while what it opened is beside it (a column, a preview).
    var lastSelected: IndexPath?
    func mark(_ on: Bool) {
        if on, let row = lastSelected, row.section < table.numberOfSections,
           row.row < table.numberOfRows(inSection: row.section) {
            if table.indexPathForSelectedRow != row { table.selectRow(at: row, animated: false, scrollPosition: .none) }
        } else if let selected = table.indexPathForSelectedRow {
            table.deselectRow(at: selected, animated: true)
        }
    }

    func tableView(_ tableView: UITableView, trailingSwipeActionsConfigurationForRowAt indexPath: IndexPath)
        -> UISwipeActionsConfiguration? {
        guard !actions.isEmpty else { return nil }
        let row = groups[indexPath.section].rows[indexPath.row].index
        let items = actions.enumerated().map { i, a in
            let action = UIContextualAction(style: a.destructive ? .destructive : .normal, title: a.title) { _, _, done in
                send("ROWACTION", [.int(Int(self.handle)), .list([.int(i), .int(row)])])
                done(true)
            }
            if let tint = a.tint { action.backgroundColor = tint }
            return action
        }
        let config = UISwipeActionsConfiguration(actions: items)   // the first at the edge: Delete, as Mail has it
        config.performsFirstActionWithFullSwipe = false
        return config
    }

    /// A long press: the row's own menu (:MENU), its choice a MENU event with (row id).
    func tableView(_ tableView: UITableView, contextMenuConfigurationForRowAt indexPath: IndexPath,
                   point: CGPoint) -> UIContextMenuConfiguration? {
        let row = groups[indexPath.section].rows[indexPath.row]
        guard !row.menu.isEmpty else { return nil }
        return UIContextMenuConfiguration(identifier: nil, previewProvider: nil) { [weak self] _ in
            UIMenu(children: row.menu.map { item in
                UIAction(title: item.title, image: item.image.flatMap { UIImage(systemName: $0) },
                         attributes: item.destructive ? .destructive : []) { _ in
                    guard let self else { return }
                    send("MENU", [.int(Int(self.handle)), .list([.int(row.index), .int(item.id)])])
                }
            })
        }
    }

    @objc func refreshed() {
        send("REFRESH", [.int(Int(handle))])
    }
}

/// app_table(screen, plain): a table filling SCREEN -- inset grouped, or plain when PLAIN is 1.  Its handle.
/// A table and its source, registered under a new handle, not yet in any view.
private func makeTable(plain: Bool, frame: CGRect) -> (UInt, NativeTable) {
    let table = UITableView(frame: frame, style: plain ? .plain : .insetGrouped)
    table.register(UITableViewCell.self, forCellReuseIdentifier: "native")
    let th = newHandle()
    let source = NativeTable(handle: th, table: table)
    table.dataSource = source
    table.delegate = source
    tables[th] = source
    return (th, source)
}

let appTable: Call = { h, plain, _, _, _, _ in
    guard let vc = screens[h] else { return 0 }
    let (th, source) = makeTable(plain: plain != 0, frame: vc.view.bounds)
    let table = source.table
    table.translatesAutoresizingMaskIntoConstraints = false
    vc.view.addSubview(table)
    NSLayoutConstraint.activate([
        table.topAnchor.constraint(equalTo: vc.view.topAnchor),
        table.bottomAnchor.constraint(equalTo: vc.view.bottomAnchor),
        table.leadingAnchor.constraint(equalTo: vc.view.leadingAnchor),
        table.trailingAnchor.constraint(equalTo: vc.view.trailingAnchor),
    ])
    return th
}

/// A hex dump: offset, the bytes in hex, the same as text -- 16 to a line where there is room, else 8 -- each
/// line drawn as it is shown.  It holds a WINDOW of the file, at most a megabyte, from BASE: scrolled near
/// either end of it, ONWANT asks for the part beyond (the app reads that range of the file and hands it to
/// RECEIVED), and the far end goes.  Its scrubber stands for the whole file, so a drag on it goes anywhere in a
/// file of any size, and only the part there is read.
final class HexView: UITableView, UITableViewDataSource, UITableViewDelegate {
    static let part = 65536                      // what the app reads at a time
    static let cap = 1 << 20                     // what is held at once
    private(set) var bytes = Data()
    private(set) var base: Int64 = 0             // the file offset of bytes' first: a multiple of 16
    var total: Int64 = 0                         // the file's size: where the dump ends
    /// (mode, offset): 0, the part around OFFSET (a jump); 1, the part from OFFSET; 2, the part ending there.
    var onWant: ((Int, Int64) -> Void)?
    var onEdit: ((Int64, Data) -> Void)?         // a line's offset and bytes, to change in place
    private var asked = false
    private var perLine = 16
    private let font = UIFont.monospacedSystemFont(ofSize: 11.5, weight: .regular)
    let scrubber = Scrubber()
    private var lastJump: CFTimeInterval = 0
    private var jumpTo: Int64?                   // the jump asked for last, until its part comes

    init() {
        super.init(frame: .zero, style: .plain)
        dataSource = self
        delegate = self
        separatorStyle = .none
        rowHeight = 17
        estimatedRowHeight = 17
        allowsSelection = false
        register(UITableViewCell.self, forCellReuseIdentifier: "hex")
        scrubber.onScrub = { [weak self] frac, done in self?.scrubbed(to: frac, done: done) }
    }
    required init?(coder: NSCoder) { fatalError() }

    // the scrubber floats over the right edge: a sibling, so it does not scroll with the lines
    override func didMoveToSuperview() {
        super.didMoveToSuperview()
        guard let sv = superview, scrubber.superview !== sv else { return }
        scrubber.translatesAutoresizingMaskIntoConstraints = false
        sv.addSubview(scrubber)
        NSLayoutConstraint.activate([
            scrubber.topAnchor.constraint(equalTo: topAnchor),
            // above the home indicator: the PANE's safe area -- a scroll view's own guides move with its content
            scrubber.bottomAnchor.constraint(equalTo: sv.safeAreaLayoutGuide.bottomAnchor),
            scrubber.trailingAnchor.constraint(equalTo: trailingAnchor),
            scrubber.widthAnchor.constraint(equalToConstant: 30),
        ])
    }
    override var isHidden: Bool { didSet { scrubber.isHidden = isHidden || !big } }

    /// Too big for the window to be the whole of it: the scrubber stands for the file, the indicator would not.
    var big: Bool { total > Int64(HexView.cap) }

    func start(_ first: Data, total: Int64) {
        base = 0
        bytes = first
        self.total = total
        asked = false
        showsVerticalScrollIndicator = !big
        scrubber.isHidden = isHidden || !big
        reloadData()
        footer()
        setContentOffset(CGPoint(x: 0, y: -adjustedContentInset.top), animated: false)
        placeThumb()
    }

    /// The app read a part of the file: MODE as asked (3: bytes from START rewritten, an edit saved).
    func received(mode: Int, start: Int64, target: Int64, _ data: Data) {
        switch mode {
        case 0:                                                  // a jump: a new window, TARGET's line on top
            // while the scrubber is held, any part shows where it is going; after, only the last one asked for
            guard scrubber.dragging || jumpTo == target else { return }
            if jumpTo == target { jumpTo = nil }
            asked = jumpTo != nil || scrubber.dragging            // a later jump on its way: nothing else meanwhile
            if data.isEmpty, start < total { total = start }    // the end, sooner than said
            base = start
            bytes = data
            let row = max(0, Int(min(target, base + Int64(bytes.count) - 1) - base) / perLine)
            UIView.performWithoutAnimation {
                reloadData()
                layoutIfNeeded()
                setContentOffset(CGPoint(x: 0, y: CGFloat(row) * rowHeight - adjustedContentInset.top), animated: false)
            }
        case 1:                                                  // the part after: onto the end
            guard jumpTo == nil, start == base + Int64(bytes.count) else { return }   // asked of a window since left
            asked = false
            if data.isEmpty { total = start }
            bytes.append(data)
            var shift = 0
            if bytes.count > HexView.cap {                       // the start goes, whole parts of it
                let drop = (bytes.count - HexView.cap + HexView.part - 1) / HexView.part * HexView.part
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
            if bytes.count > HexView.cap {                       // the end goes
                let keep = HexView.cap / HexView.part * HexView.part
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
        footer()
        placeThumb()
    }

    /// New rows, the same lines kept where they were on the screen: SHIFT rows were added above (removed, < 0).
    private func reload(shiftingRows shift: Int) {
        let y = contentOffset.y + CGFloat(shift) * rowHeight
        UIView.performWithoutAnimation {
            reloadData()
            layoutIfNeeded()
            contentOffset.y = y
        }
    }

    var lines: Int { (bytes.count + perLine - 1) / perLine }

    override func layoutSubviews() {
        super.layoutSubviews()
        // as many to a line as fit: 9 for the offset, 3 a byte, 1, 1 a byte
        let char = ("0" as NSString).size(withAttributes: [.font: font]).width
        let fit = (bounds.width - 24 - (big ? 20 : 0)) / char
        let want = fit >= 10 + 16 * 4 ? 16 : 8
        if want != perLine {
            let top = topOffset
            perLine = want
            reloadData()
            layoutIfNeeded()
            contentOffset.y = CGFloat((top - base) / Int64(perLine)) * rowHeight - adjustedContentInset.top
        }
    }

    func footer() {
        guard base + Int64(bytes.count) < total else { tableFooterView = nil; return }
        let spinner = UIActivityIndicatorView(style: .medium)
        spinner.frame = CGRect(x: 0, y: 0, width: bounds.width, height: 44)
        spinner.startAnimating()
        tableFooterView = spinner
    }

    // ---- where in the file, and the scrubber ----

    /// The file offset of the line at the top.
    var topOffset: Int64 {
        let row = max(0, Int((contentOffset.y + adjustedContentInset.top) / rowHeight))
        return min(base + Int64(row * perLine), max(0, total - 1))
    }

    func placeThumb() {
        guard big, !scrubber.dragging else { return }
        scrubber.place(CGFloat(Double(topOffset) / Double(max(1, total))))
    }

    func scrollViewDidScroll(_ scrollView: UIScrollView) { placeThumb() }

    /// The scrubber dragged to FRAC of the file: there -- at once, if the window holds it; else the part there,
    /// asked for as the drag goes (at most every tenth of a second; the app answers only the latest) and once more
    /// where it ends.
    func scrubbed(to frac: CGFloat, done: Bool) {
        let target = min(Int64(Double(total) * Double(frac)), max(0, total - 1)) / 16 * 16
        scrubber.label(String(format: "%08llx", target) + "  ·  \(Int((frac * 100).rounded()))%")
        let line = (target - base) / Int64(perLine)
        if target >= base, target < base + Int64(bytes.count) {
            if done { jumpTo = nil; asked = false }               // ends here: a part on its way is not wanted
            contentOffset.y = min(CGFloat(line) * rowHeight - adjustedContentInset.top,
                                  max(-adjustedContentInset.top, contentSize.height - bounds.height + adjustedContentInset.bottom))
            return
        }
        let now = CACurrentMediaTime()
        guard done || now - lastJump > 0.1 else { return }
        lastJump = now
        asked = true
        jumpTo = target
        onWant?(0, target)
    }

    // ---- the lines ----

    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int { lines }

    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCell(withIdentifier: "hex", for: indexPath)
        let (start, line) = self.line(indexPath.row)
        var hex = ""
        var text = ""
        for (i, b) in line.enumerated() {
            hex += String(format: "%02x", b) + (i == perLine / 2 - 1 ? "  " : " ")
            text.append((0x20..<0x7f).contains(b) ? Character(UnicodeScalar(b)) : "·")
        }
        hex = hex.padding(toLength: perLine * 3 + 1, withPad: " ", startingAt: 0)
        let out = NSMutableAttributedString(string: String(format: "%08llx  ", start),
                                            attributes: [.font: font, .foregroundColor: UIColor.tertiaryLabel])
        out.append(NSAttributedString(string: hex, attributes: [.font: font, .foregroundColor: UIColor.label]))
        out.append(NSAttributedString(string: text, attributes: [.font: font, .foregroundColor: UIColor.secondaryLabel]))
        var c = cell.defaultContentConfiguration()
        c.attributedText = out
        c.textProperties.numberOfLines = 1
        c.directionalLayoutMargins = NSDirectionalEdgeInsets(top: 0, leading: 12, bottom: 0, trailing: 12)
        cell.contentConfiguration = c
        return cell
    }

    /// Row ROW's file offset and bytes.
    func line(_ row: Int) -> (Int64, Data) {
        let at = min(row * perLine, bytes.count)
        return (base + Int64(at), bytes.subdata(in: bytes.startIndex + at ..< bytes.startIndex + min(at + perLine, bytes.count)))
    }

    // A long press on a line, not a tap -- a tap is too easily a scroll's -- for what can be done with it.
    func tableView(_ tableView: UITableView, contextMenuConfigurationForRowAt indexPath: IndexPath,
                   point: CGPoint) -> UIContextMenuConfiguration? {
        let (start, data) = line(indexPath.row)
        let hex = data.map { String(format: "%02x", $0) }.joined(separator: " ")
        return UIContextMenuConfiguration(identifier: nil, previewProvider: nil) { _ in
            UIMenu(children: [
                UIAction(title: "Edit Bytes…", image: UIImage(systemName: "pencil")) { [weak self] _ in
                    self?.onEdit?(start, data)
                },
                UIAction(title: "Copy Hex", image: UIImage(systemName: "doc.on.doc")) { _ in
                    UIPasteboard.general.string = hex
                },
                UIAction(title: "Copy Offset", image: UIImage(systemName: "number")) { _ in
                    UIPasteboard.general.string = String(format: "%08llx", start)
                },
            ])
        }
    }

    func tableView(_ tableView: UITableView, willDisplay cell: UITableViewCell, forRowAt indexPath: IndexPath) {
        guard !asked, !scrubber.dragging else { return }
        if indexPath.row >= lines - 60, base + Int64(bytes.count) < total {
            asked = true
            onWant?(1, base + Int64(bytes.count))
        } else if indexPath.row < 60, base > 0 {
            asked = true
            onWant?(2, base)
        }
    }
}

/// A scroll bar to drag, for what is too long to scroll through: a thumb (it widens under the finger), and
/// beside it, while dragged, where that is -- an offset in a file, a name in a folder.  ONSCRUB gets the place as
/// a fraction of the whole, and DONE when the finger lifts.
final class Scrubber: UIView {
    var onScrub: ((CGFloat, Bool) -> Void)?
    private(set) var dragging = false
    private let thumb = UIView()
    private let bubble = UILabel()
    private var frac: CGFloat = 0
    private let thumbHeight: CGFloat = 44

    override init(frame: CGRect) {
        super.init(frame: frame)
        thumb.backgroundColor = .secondaryLabel
        thumb.layer.cornerCurve = .continuous
        addSubview(thumb)
        bubble.font = .monospacedDigitSystemFont(ofSize: 13, weight: .medium)
        bubble.textAlignment = .center
        bubble.lineBreakMode = .byTruncatingMiddle
        bubble.backgroundColor = .secondarySystemBackground
        bubble.layer.cornerRadius = 10
        bubble.layer.cornerCurve = .continuous
        bubble.clipsToBounds = true
        bubble.alpha = 0
        addSubview(bubble)
        addGestureRecognizer(UILongPressGestureRecognizer(target: self, action: #selector(dragged(_:))))
        (gestureRecognizers?.first as? UILongPressGestureRecognizer)?.minimumPressDuration = 0
    }
    required init?(coder: NSCoder) { fatalError() }

    // only the thumb's own strip takes touches; the bubble draws outside it
    override func point(inside point: CGPoint, with event: UIEvent?) -> Bool {
        point.x >= 0 && point.x <= bounds.width && point.y >= 0 && point.y <= bounds.height
    }

    func place(_ f: CGFloat) {
        frac = max(0, min(1, f))
        setNeedsLayout()
    }

    func label(_ text: String) { bubble.text = text }

    override func layoutSubviews() {
        super.layoutSubviews()
        let w: CGFloat = dragging ? 9 : 5
        let y = (bounds.height - thumbHeight - 8) * frac + 4
        thumb.frame = CGRect(x: bounds.width - w - 4, y: y, width: w, height: thumbHeight)
        thumb.layer.cornerRadius = w / 2
        let size = bubble.intrinsicContentSize
        let bw = min(size.width + 24, 240), bh: CGFloat = 30
        bubble.frame = CGRect(x: -bw - 6, y: max(0, min(bounds.height - bh, y + thumbHeight / 2 - bh / 2)),
                              width: bw, height: bh)
    }

    @objc func dragged(_ g: UILongPressGestureRecognizer) {
        let y = g.location(in: self).y
        let f = (y - thumbHeight / 2 - 4) / max(1, bounds.height - thumbHeight - 8)
        switch g.state {
        case .began:
            dragging = true
            place(f)
            onScrub?(frac, false)
            UIView.animate(withDuration: 0.15) { self.bubble.alpha = 1; self.layoutIfNeeded() }
        case .changed:
            place(f)
            onScrub?(frac, false)
            layoutIfNeeded()
        case .ended, .cancelled:
            onScrub?(frac, true)
            dragging = false
            UIView.animate(withDuration: 0.25) { self.bubble.alpha = 0; self.layoutIfNeeded() }
        default: break
        }
    }
}

/// An image to look at: fitted and centred, pinched (or double-tapped) closer, panned about when closer.  Ours,
/// not Quick Look's: on a device Quick Look draws out of the app's reach, where the shelf's drag cannot follow.
final class ImageViewer: UIScrollView, UIScrollViewDelegate {
    let imageView = UIImageView()
    private var fitted = true

    override init(frame: CGRect) {
        super.init(frame: frame)
        delegate = self
        showsVerticalScrollIndicator = false
        showsHorizontalScrollIndicator = false
        contentInsetAdjustmentBehavior = .never
        decelerationRate = .fast
        addSubview(imageView)
        let double = UITapGestureRecognizer(target: self, action: #selector(doubleTapped(_:)))
        double.numberOfTapsRequired = 2
        addGestureRecognizer(double)
    }
    required init?(coder: NSCoder) { fatalError() }

    var image: UIImage? {
        get { imageView.image }
        set {
            imageView.image = newValue
            zoomScale = 1
            imageView.frame = CGRect(origin: .zero, size: newValue?.size ?? .zero)
            contentSize = imageView.frame.size
            fitted = true
            setNeedsLayout()
        }
    }

    /// The scale that fits the whole image (and never enlarges a small one past its own size).
    var fitScale: CGFloat {
        guard let size = image?.size, size.width > 0, size.height > 0, bounds.width > 0 else { return 1 }
        return min(bounds.width / size.width, bounds.height / size.height, 1)
    }

    override func layoutSubviews() {
        super.layoutSubviews()
        let fit = fitScale
        minimumZoomScale = fit
        maximumZoomScale = max(fit * 4, 2)
        if fitted || zoomScale < fit { zoomScale = fit }
        centre()
    }

    /// Smaller than the view: in the middle of it, by its insets.
    func centre() {
        let w = imageView.frame.width, h = imageView.frame.height
        contentInset = UIEdgeInsets(top: max(0, (bounds.height - h) / 2), left: max(0, (bounds.width - w) / 2),
                                    bottom: 0, right: 0)
    }

    func viewForZooming(in scrollView: UIScrollView) -> UIView? { imageView }
    func scrollViewDidZoom(_ scrollView: UIScrollView) {
        fitted = zoomScale <= minimumZoomScale + 0.001
        centre()
    }

    @objc func doubleTapped(_ g: UITapGestureRecognizer) {
        if zoomScale > minimumZoomScale + 0.001 {
            setZoomScale(minimumZoomScale, animated: true)
        } else {
            let p = g.location(in: imageView), scale = min(maximumZoomScale, minimumZoomScale * 2.5)
            let size = CGSize(width: bounds.width / scale, height: bounds.height / scale)
            zoom(to: CGRect(x: p.x - size.width / 2, y: p.y - size.height / 2, width: size.width, height: size.height),
                 animated: true)
        }
    }
}

/// The browser's screen: it tells the browser when its bounds change (a rotation), so the pages and the preview
/// pane can be laid out again.
final class BrowserScreen: UIViewController {
    var onLayout: (() -> Void)?
    override func viewDidLayoutSubviews() {
        super.viewDidLayoutSubviews()
        onLayout?()
    }
}

// MARK: - a browser: pages that slide under a bar that does not

/// One screen whose navigation bar holds still while PAGES -- tables -- slide in and out beneath it: a file
/// browser's folders.  A push on this iOS moves the whole screen, bar and all; between the folders of one place
/// that is more motion than the step deserves.  Each page carries its path, which the bar's title cross-fades
/// to.  Back (the chevron, or a swipe from the left edge) slides the top page away with a POP for its table;
/// on the first page, it leaves the browser (a POP for the browser's screen, from NativeNav).
final class NativeBrowser: NSObject, QLPreviewControllerDataSource, QLPreviewControllerDelegate,
                           UIGestureRecognizerDelegate {
    let handle: UInt
    let vc: BrowserScreen
    let title = TitleBox(UILabel())
    var pages: [(table: UInt, view: UIView, path: [String])] = []
    /// The pages live here: the whole screen, or -- with a preview open -- the part the pane leaves.
    let pagesView = UIView()

    // THE PREVIEW: Quick Look for one file -- in portrait a SHELF over the list (half the screen, the list still
    // live behind it; dragged up for nearly all of it, down and away to close), in landscape a column beside it.
    // The shelf is part of this screen, not a sheet presented over it: it moves with the finger and nothing else.
    var previewURL: URL?
    var previewName = ""
    let pane = UIView()                          // the shelf / the column
    let ql = QLPreviewController()               // images, PDFs: what Quick Look draws in this process
    let textView = UITextView()                  // text and source: ours -- Quick Look draws text out of reach
    let imageViewer = ImageViewer()              // images: ours too, for the same reason (on a device)
    let hexView = HexView()                      // anything else: its bytes, a part at a time
    var hexPath: URL?                            // the local file the app appends the parts to
    let shelfHeader = UIView()                   // portrait: grabber, close, name, share
    let shelfName = UILabel()
    let floating = UIStackView()                 // landscape: share and close, in glass, over the preview
    let paneRule = UIView()
    var shelfTall = false                        // the shelf's two heights: half the screen, or nearly all of it
    var shelfDrag: CGFloat = 0                   // where a drag has it, from its resting place
    var paneShown: Bool { previewURL != nil && landscape }
    var shelfShown: Bool { previewURL != nil && !landscape }

    init(handle: UInt, vc: BrowserScreen) {
        self.handle = handle
        self.vc = vc
        super.init()
        vc.navigationItem.titleView = title
        vc.navigationItem.hidesBackButton = true
        vc.navigationItem.leftBarButtonItem = UIBarButtonItem(
            image: UIImage(systemName: "chevron.backward"), primaryAction: UIAction { [weak self] _ in self?.back() })
        let swipe = UIPanGestureRecognizer(target: self, action: #selector(swiped(_:)))
        swipe.delegate = self
        pagesView.addGestureRecognizer(swipe)
        pagesView.clipsToBounds = true
        vc.view.backgroundColor = .systemBackground        // one colour under the bar, list and pane alike
        vc.view.addSubview(pagesView)
        buildPane()
        vc.onLayout = { [weak self] in self?.layout() }
    }

    var width: CGFloat { pagesView.bounds.width }
    var landscape: Bool { vc.view.bounds.width > vc.view.bounds.height }
    /// The table whose row is shown in the preview pane (that row stays marked).
    var previewTable: UInt?
    var separators: [UIView] = []

    /// How many pages show side by side: one in portrait; in landscape the folder and its parents, as columns --
    /// three, or two beside a preview.
    /// Side by side: in landscape the folder and its parent (one at the first folder, and one beside a
    /// preview, the preview being the second); in portrait one, the pages sliding over each other.
    var columns: Int { landscape ? max(1, min(pages.count, paneShown ? 1 : 2)) : 1 }

    /// The pages where they go: the last COLUMNS of them side by side, the rest hidden to the left; the column
    /// rules between them; the rows that opened what is beside them marked; the bar's title over them.
    func layoutPages() {
        let n = columns, w = width / CGFloat(n), h = pagesView.bounds.height
        for (i, page) in pages.enumerated() {
            let slot = i - (pages.count - n)
            page.view.frame = CGRect(x: slot >= 0 ? CGFloat(slot) * w : -w, y: 0, width: w, height: h)
            page.view.isHidden = slot < 0
            tables[page.table]?.mark(i < pages.count - 1 || page.table == previewTable)
        }
        while separators.count < 1 {
            let line = UIView()
            line.backgroundColor = .separator
            pagesView.addSubview(line)
            separators.append(line)
        }
        for (k, line) in separators.enumerated() {
            line.frame = CGRect(x: CGFloat(k + 1) * w, y: 0, width: 1 / UIScreen.main.scale, height: h)
            line.alpha = k + 1 < n ? 1 : 0
            pagesView.bringSubviewToFront(line)
        }
        layoutTitle()
        layoutEdges()
    }

    // THE BAR'S FROSTED EDGE over every page that shows, not only the first.  The navigation bar softens the top
    // of the one scroll view it tracks; every other page's rows went under a clear bar, the title over the names.
    // So: the bar tracks the page on top, and -- iOS 26 -- each page that shows has a strip of its own over its
    // top edge, the width of its column, that asks for the same effect on it.
    private var edgeStrips: [UInt: UIView] = [:]
    func layoutEdges() {
        if let top = pages.last?.view as? UIScrollView, vc.contentScrollView(for: .top) !== top {
            vc.setContentScrollView(top, for: .top)
        }
        guard #available(iOS 26.0, *) else { return }
        let live = Set(pages.map(\.table))
        for (t, strip) in edgeStrips where !live.contains(t) { strip.removeFromSuperview(); edgeStrips[t] = nil }
        for page in pages {
            guard let table = page.view as? UIScrollView else { continue }
            let strip = edgeStrips[page.table] ?? {
                let v = UIView()
                v.isUserInteractionEnabled = false
                let mark = UIView()                       // something there: what the effect is shaped around
                mark.autoresizingMask = [.flexibleWidth, .flexibleHeight]
                v.addSubview(mark)
                let interaction = UIScrollEdgeElementContainerInteraction()
                interaction.scrollView = table
                interaction.edge = .top
                v.addInteraction(interaction)
                vc.view.addSubview(v)
                edgeStrips[page.table] = v
                return v
            }()
            let column = vc.view.convert(page.view.frame, from: pagesView)
            strip.frame = CGRect(x: column.minX, y: 0, width: column.width, height: vc.view.safeAreaInsets.top)
            strip.subviews.first?.frame = strip.bounds
            strip.isHidden = page.view.isHidden
            vc.view.bringSubviewToFront(strip)
        }
        if !pane.isHidden { vc.view.bringSubviewToFront(pane) }
    }

    /// The path -- or, in landscape with columns, each one's folder over it.  Not in portrait: there the bar's
    /// buttons take most of the room over the right-hand column, and the path says it better.
    func layoutTitle() {
        guard landscape, columns > 1 || paneShown, title.window != nil, !pages.isEmpty else {
            title.setSegments([]); title.subtitleSpan = nil; return
        }
        let n = columns, w = width / CGFloat(n)
        var items: [(text: String, rect: CGRect, current: Bool)] = []
        title.subtitleSpan = nil
        for slot in 0..<n {
            let page = pages[pages.count - n + slot]
            // the column's span, in the title's coordinates, cut to what the title has (the bar's buttons take the
            // ends of the first and last columns): horizontal only, the bar is not where the columns' tops are
            let column = title.convert(CGRect(x: CGFloat(slot) * w + 8, y: 0, width: w - 16, height: 1), from: pagesView)
            let minX = max(column.minX, 4), maxX = min(column.maxX, title.bounds.width - 4)
            guard maxX - minX > 24 else { continue }
            items.append((page.path.last ?? "/", CGRect(x: minX, y: 0, width: maxX - minX, height: title.bounds.height),
                          slot == n - 1 && !paneShown))
            if slot == n - 1 { title.subtitleSpan = items.last?.rect }   // the server's name, under the folder on top
        }
        if paneShown {
            let column = title.convert(CGRect(x: pane.frame.minX + 8, y: 0, width: pane.frame.width - 16, height: 1),
                                       from: vc.view)
            let minX = max(column.minX, 4), maxX = min(column.maxX, title.bounds.width - 4)
            if maxX - minX > 24 {
                items.append((previewName, CGRect(x: minX, y: 0, width: maxX - minX, height: title.bounds.height), true))
            }
        }
        title.setSegments(items)
    }

    // MARK: layout

    func layout() {
        let b = vc.view.bounds
        let top = vc.view.safeAreaInsets.top
        if paneShown {
            let listWidth = (b.width * 0.5).rounded()           // two even columns: the folder, the preview
            pagesView.frame = CGRect(x: 0, y: 0, width: listWidth, height: b.height)
            pane.frame = CGRect(x: listWidth, y: top, width: b.width - listWidth, height: b.height - top)
        } else if landscape {
            pagesView.frame = b
            pane.frame = CGRect(x: b.maxX, y: top, width: b.width * 0.5, height: b.height - top)
        } else {
            pagesView.frame = b
            // edge to edge, from the bottom of the screen
            let h = shelfHeight
            let y = shelfShown ? b.height - h + max(shelfDrag, -(b.height - h - top - shelfGap)) : b.maxY + 20
            pane.frame = CGRect(x: 0, y: y, width: b.width, height: h + max(0, -shelfDrag))
        }
        stylePane()
        // what the shelf covers, the list can scroll out from under
        let cover = shelfShown ? shelfHeight - vc.view.safeAreaInsets.bottom : 0
        for page in pages {
            // and its scroll bar goes, while the shelf is up: a bar at the edge is the preview's to drag
            if let t = page.view as? UITableView, let source = tables.values.first(where: { $0.table === t }) {
                if source.scrubbable == shelfShown { source.scrubbable = !shelfShown }
            } else {
                (page.view as? UIScrollView)?.showsVerticalScrollIndicator = !shelfShown
            }
            guard let t = page.view as? UITableView, abs(t.contentInset.bottom - cover) > 1 else { continue }
            t.contentInset.bottom = cover
            t.verticalScrollIndicatorInsets.bottom = cover
        }
        layoutPages()
    }

    let shelfGap: CGFloat = 8                    // tall, it stops this short of the bar
    var shelfHeight: CGFloat {
        let b = vc.view.bounds
        return shelfTall ? b.height - vc.view.safeAreaInsets.top - shelfGap : (b.height * 0.5).rounded()
    }

    /// Portrait: a shelf from the screen's edges and bottom -- its top corners rounded, a shadow above it, a
    /// grabber and its header.  Landscape: a column like the others, a rule beside it and its buttons floating.
    func stylePane() {
        let shelf = !landscape
        pane.layer.cornerRadius = shelf ? 28 : 0
        pane.layer.maskedCorners = [.layerMinXMinYCorner, .layerMaxXMinYCorner]
        pane.layer.cornerCurve = .continuous
        pane.layer.shadowOpacity = shelf ? 0.16 : 0
        shelfHeader.isHidden = !shelf
        floating.isHidden = shelf
        paneRule.isHidden = shelf
        qlTop?.constant = shelf ? 64 : 0
        textTop?.constant = shelf ? 64 : 0
        imageTop?.constant = shelf ? 64 : 0
        hexTop?.constant = shelf ? 64 : 0
    }
    var qlTop: NSLayoutConstraint?
    var textTop: NSLayoutConstraint?
    var imageTop: NSLayoutConstraint?
    var hexTop: NSLayoutConstraint?

    func buildPane() {
        pane.isHidden = true
        pane.backgroundColor = .systemBackground
        pane.layer.shadowColor = UIColor.black.cgColor
        pane.layer.shadowRadius = 18
        pane.layer.shadowOffset = CGSize(width: 0, height: -2)
        paneRule.backgroundColor = .separator
        paneRule.translatesAutoresizingMaskIntoConstraints = false
        ql.dataSource = self
        ql.delegate = self
        vc.addChild(ql)
        ql.view.translatesAutoresizingMaskIntoConstraints = false
        pane.addSubview(ql.view)
        textView.isEditable = false
        textView.font = .monospacedSystemFont(ofSize: 12.5, weight: .regular)
        textView.textContainerInset = UIEdgeInsets(top: 12, left: 10, bottom: 24, right: 10)
        textView.alwaysBounceVertical = true
        textView.translatesAutoresizingMaskIntoConstraints = false
        textView.isHidden = true
        pane.addSubview(textView)
        imageViewer.translatesAutoresizingMaskIntoConstraints = false
        imageViewer.isHidden = true
        pane.addSubview(imageViewer)
        hexView.translatesAutoresizingMaskIntoConstraints = false
        hexView.isHidden = true
        hexView.onWant = { [weak self] mode, offset in
            guard let self else { return }
            send("HEXMORE", [.int(Int(self.handle)), .list([.int(mode), .int(Int(offset))])])
        }
        hexView.onEdit = { [weak self] offset, line in self?.editHex(offset, line) }
        pane.addSubview(hexView)
        pane.addSubview(paneRule)
        ql.didMove(toParent: vc)
        func glass(_ symbol: String, _ act: @escaping () -> Void) -> UIButton {
            var config: UIButton.Configuration
            if #available(iOS 26.0, *) { config = .glass() } else { config = .gray() }
            config.image = UIImage(systemName: symbol)
            config.cornerStyle = .capsule
            let b = UIButton(configuration: config, primaryAction: UIAction { _ in act() })
            b.widthAnchor.constraint(equalToConstant: 44).isActive = true
            b.heightAnchor.constraint(equalToConstant: 44).isActive = true
            return b
        }
        // landscape's floating buttons
        let share = glass("square.and.arrow.up") { [weak self] in self?.sharePreview() }
        shareButton = share
        let floatingHex = glass("number.square") { [weak self] in self?.toggleHex() }
        hexToggles.append(floatingHex)
        floating.addArrangedSubview(floatingHex)
        floating.addArrangedSubview(share)
        floating.addArrangedSubview(glass("xmark") { [weak self] in self?.closePreview(byUser: true) })
        floating.spacing = 10
        floating.translatesAutoresizingMaskIntoConstraints = false
        pane.addSubview(floating)
        // portrait's header: the grabber, then close, the name, share -- the drag handle for the whole shelf
        let grabber = UIView()
        grabber.backgroundColor = .tertiaryLabel
        grabber.layer.cornerRadius = 2.5
        grabber.translatesAutoresizingMaskIntoConstraints = false
        shelfName.font = .preferredFont(forTextStyle: .headline)
        shelfName.textAlignment = .center
        shelfName.lineBreakMode = .byTruncatingMiddle
        let shelfShare = glass("square.and.arrow.up") { [weak self] in self?.sharePreview() }
        self.shelfShare = shelfShare
        // close and the hex toggle on the left, the name, share on the right
        let shelfHex = glass("number.square") { [weak self] in self?.toggleHex() }
        hexToggles.append(shelfHex)
        // and its twin on the right, empty, so the name stays in the middle
        hexTwin.widthAnchor.constraint(equalToConstant: 44).isActive = true
        let row = UIStackView(arrangedSubviews: [
            glass("xmark") { [weak self] in self?.closePreview(byUser: true) }, shelfHex, shelfName, hexTwin, shelfShare])
        row.alignment = .center
        row.spacing = 8
        row.translatesAutoresizingMaskIntoConstraints = false
        shelfHeader.translatesAutoresizingMaskIntoConstraints = false
        shelfHeader.addSubview(grabber)
        shelfHeader.addSubview(row)
        pane.addSubview(shelfHeader)
        let drag = UIPanGestureRecognizer(target: self, action: #selector(shelfDragged(_:)))
        shelfHeader.addGestureRecognizer(drag)
        contentDrag.addTarget(self, action: #selector(contentDragged(_:)))
        contentDrag.delegate = self
        pane.addGestureRecognizer(contentDrag)
        let qlTop = ql.view.topAnchor.constraint(equalTo: pane.topAnchor)
        self.qlTop = qlTop
        let textTop = textView.topAnchor.constraint(equalTo: pane.topAnchor)
        self.textTop = textTop
        let imageTop = imageViewer.topAnchor.constraint(equalTo: pane.topAnchor)
        self.imageTop = imageTop
        let hexTop = hexView.topAnchor.constraint(equalTo: pane.topAnchor)
        self.hexTop = hexTop
        NSLayoutConstraint.activate([
            qlTop, textTop, imageTop, hexTop,
            hexView.bottomAnchor.constraint(equalTo: pane.bottomAnchor),
            hexView.leadingAnchor.constraint(equalTo: pane.leadingAnchor),
            hexView.trailingAnchor.constraint(equalTo: pane.trailingAnchor),
            imageViewer.bottomAnchor.constraint(equalTo: pane.safeAreaLayoutGuide.bottomAnchor),
            imageViewer.leadingAnchor.constraint(equalTo: pane.leadingAnchor),
            imageViewer.trailingAnchor.constraint(equalTo: pane.trailingAnchor),
            textView.bottomAnchor.constraint(equalTo: pane.bottomAnchor),
            textView.leadingAnchor.constraint(equalTo: pane.leadingAnchor),
            textView.trailingAnchor.constraint(equalTo: pane.trailingAnchor),
            ql.view.bottomAnchor.constraint(equalTo: pane.bottomAnchor),
            ql.view.leadingAnchor.constraint(equalTo: pane.leadingAnchor),
            ql.view.trailingAnchor.constraint(equalTo: pane.trailingAnchor),
            paneRule.topAnchor.constraint(equalTo: pane.topAnchor),
            paneRule.bottomAnchor.constraint(equalTo: pane.bottomAnchor),
            paneRule.leadingAnchor.constraint(equalTo: pane.leadingAnchor),
            paneRule.widthAnchor.constraint(equalToConstant: 1 / UIScreen.main.scale),
            floating.topAnchor.constraint(equalTo: pane.topAnchor, constant: 10),
            floating.trailingAnchor.constraint(equalTo: pane.safeAreaLayoutGuide.trailingAnchor, constant: -10),
            shelfHeader.topAnchor.constraint(equalTo: pane.topAnchor),
            shelfHeader.leadingAnchor.constraint(equalTo: pane.leadingAnchor),
            shelfHeader.trailingAnchor.constraint(equalTo: pane.trailingAnchor),
            shelfHeader.heightAnchor.constraint(equalToConstant: 64),
            grabber.topAnchor.constraint(equalTo: shelfHeader.topAnchor, constant: 6),
            grabber.centerXAnchor.constraint(equalTo: shelfHeader.centerXAnchor),
            grabber.widthAnchor.constraint(equalToConstant: 36),
            grabber.heightAnchor.constraint(equalToConstant: 5),
            row.leadingAnchor.constraint(equalTo: shelfHeader.leadingAnchor, constant: 12),
            row.trailingAnchor.constraint(equalTo: shelfHeader.trailingAnchor, constant: -12),
            row.topAnchor.constraint(equalTo: grabber.bottomAnchor, constant: 4),
        ])
        vc.view.addSubview(pane)
    }
    var shareButton: UIView?
    var hexToggles: [UIButton] = []              // "the bytes instead": the shelf's and the floating one
    let hexTwin = UIView()

    /// The hex toggle: shown for a file that has another view (an image, text, a PDF), filled while its bytes are
    /// what is shown.  A tap is a HEXTOGGLE event; the app shows the file the other way.
    func setHexToggle(shown: Bool, on: Bool) {
        hexTwin.isHidden = !shown
        for b in hexToggles {
            b.isHidden = !shown
            b.configuration?.image = UIImage(systemName: on ? "number.square.fill" : "number.square")
        }
    }
    func toggleHex() { send("HEXTOGGLE", [.int(Int(handle))]) }
    var shelfShare: UIView?

    /// The shelf follows the finger; let go, and it springs to the nearer of its two heights -- or away, dragged
    /// down far enough or flicked.  Above its tall height it resists.
    @objc func shelfDragged(_ g: UIPanGestureRecognizer) {
        let dy = g.translation(in: vc.view).y, v = g.velocity(in: vc.view).y
        switch g.state {
        case .changed:
            shelfDrag = dy > 0 ? dy : (shelfTall ? dy / 4 : dy)
            layout()
        case .ended, .cancelled:
            releaseShelf(dy, v)
        default: break
        }
    }

    /// Let go after a drag of DY at speed V: the nearer height, or away.
    func releaseShelf(_ dy: CGFloat, _ v: CGFloat) {
        let h = shelfHeight
        if dy > h * 0.3 || v > 900 {
            if shelfTall && dy < h * 0.6 && v < 1800 { shelfTall = false } else { closePreview(byUser: true, flick: v); return }
        } else if !shelfTall && (dy < -60 || v < -700) {
            shelfTall = true
        }
        shelfDrag = 0
        UIView.animate(withDuration: 0.45, delay: 0, usingSpringWithDamping: 0.85,
                       initialSpringVelocity: max(0, min(abs(v) / 1000, 4))) { self.layout() }
    }

    // A DRAG ON THE PREVIEW ITSELF, as a sheet's content does: it scrolls the content while the content can
    // scroll; at the content's top, down moves the shelf (and up grows it, while it is not yet tall).  It runs
    // beside Quick Look's own scrolling, taking over from it -- the content held at its top -- when it moves the shelf.
    let contentDrag = UIPanGestureRecognizer()
    var contentScroll: UIScrollView?
    var takeoverAt: CGFloat?
    var startedAtTop = false
    var heldOffset: CGPoint?                     // where the content was when the shelf took over: kept there

    /// The scroll view under the finger that scrolls up and down -- not the one that pages between items.
    func verticalScrollView(at point: CGPoint) -> UIScrollView? {
        var v = pane.hitTest(point, with: nil)
        while let view = v, view !== pane {
            if let s = view as? UIScrollView, !s.isPagingEnabled { return s }
            v = view.superview
        }
        return nil
    }

    /// Nothing above to scroll back to: at the top, or content that fits and so does not scroll at all (an image
    /// smaller than the shelf, centred however Quick Look centres it).
    func atTop(_ s: UIScrollView?) -> Bool {
        guard let s else { return true }
        let room = s.bounds.height - s.adjustedContentInset.top - s.adjustedContentInset.bottom
        return s.contentSize.height <= room + 1 || s.contentOffset.y <= -s.adjustedContentInset.top + 1
    }

    @objc func contentDragged(_ g: UIPanGestureRecognizer) {
        guard shelfShown else { return }
        let dy = g.translation(in: vc.view).y
        switch g.state {
        case .began:
            contentScroll = verticalScrollView(at: g.location(in: pane))
            startedAtTop = atTop(contentScroll)
            takeoverAt = nil
        case .changed:
            // the shelf takes the drag: from the start, if the content began at its top (down moves it, up grows
            // it); or, scrolled down, once the content has come back to its top and the drag goes on down
            if takeoverAt == nil, startedAtTop ? (dy > 0 || !shelfTall) : (dy > 0 && atTop(contentScroll)) {
                takeoverAt = startedAtTop ? 0 : dy
                if let pan = contentScroll?.panGestureRecognizer {   // the content lets go: no scroll, no momentum
                    pan.isEnabled = false
                    pan.isEnabled = true
                }
                heldOffset = contentScroll?.contentOffset
            }
            guard let t0 = takeoverAt else { return }
            let d = dy - t0
            shelfDrag = d > 0 ? d : (shelfTall ? d / 4 : d)
            // the content holds still where it was -- not at a top of our reckoning, which for something
            // smaller than the shelf is not where Quick Look keeps it
            if let s = contentScroll, let held = heldOffset { s.contentOffset = held }
            layout()
        case .ended, .cancelled:
            if let t0 = takeoverAt { releaseShelf(dy - t0, g.velocity(in: vc.view).y) }
            takeoverAt = nil
            contentScroll = nil
            heldOffset = nil
        default: break
        }
    }

    func gestureRecognizer(_ g: UIGestureRecognizer, shouldRecognizeSimultaneouslyWith other: UIGestureRecognizer) -> Bool {
        g === contentDrag
    }

    // MARK: the preview

    enum PreviewKind: UInt { case quickLook = 0, text = 1, image = 2, hex = 3 }
    var hexTotal: Int64 = 0

    func showPreview(_ url: URL, name: String, kind: PreviewKind) {
        let opening = previewURL == nil
        previewURL = url
        previewName = name
        shelfName.text = name
        previewTable = pages.last?.table
        if let t = previewTable { tables[t]?.mark(true) }
        textView.isHidden = kind != .text
        imageViewer.isHidden = kind != .image
        hexView.isHidden = kind != .hex
        ql.view.isHidden = kind != .quickLook
        // a hex dump's local copy is only the part looked at: nothing to share
        shareButton?.isHidden = kind == .hex
        shelfShare?.alpha = kind == .hex ? 0 : 1
        shelfShare?.isUserInteractionEnabled = kind != .hex
        switch kind {
        case .hex:
            hexPath = url
            hexView.start((try? Data(contentsOf: url)) ?? Data(), total: hexTotal)
        case .text:
            textView.text = readText(url)
            textView.setContentOffset(CGPoint(x: 0, y: -textView.adjustedContentInset.top), animated: false)
        case .image:
            imageViewer.image = UIImage(contentsOfFile: url.path)
        case .quickLook:
            ql.reloadData()
            ql.refreshCurrentPreviewItem()
        }
        guard opening else { layoutTitle(); return }       // another file: the same shelf, new contents
        shelfTall = false
        shelfDrag = 0
        previewURL = nil
        layout()                                           // from below the screen (or past its right edge)...
        pane.isHidden = false
        previewURL = url
        UIView.animate(withDuration: 0.42, delay: 0, usingSpringWithDamping: 0.9, initialSpringVelocity: 0) {
            self.layout()                                  // ...to where it rests
        }
    }

    func closePreview(byUser: Bool, flick: CGFloat = 0) {
        guard previewURL != nil else { return }
        previewURL = nil
        previewTable = nil
        UIView.animate(withDuration: 0.3, delay: 0, usingSpringWithDamping: 1,
                       initialSpringVelocity: max(0, min(flick / 1000, 4))) {
            self.layout()
        } completion: { _ in
            if self.previewURL == nil { self.pane.isHidden = true; self.shelfDrag = 0 }
        }
        if byUser { send("PREVIEWED", [.int(Int(handle))]) }
    }

    /// A text file's contents to show: UTF-8 (Latin-1 if it is not), the first 2 MB.
    func readText(_ url: URL) -> String {
        guard let handle = try? FileHandle(forReadingFrom: url) else { return "" }
        defer { try? handle.close() }
        let data = (try? handle.read(upToCount: 2_000_000)) ?? Data()
        return String(data: data, encoding: .utf8) ?? String(decoding: data, as: UTF8.self)
    }

    /// The app read a part of the file into the hex view's local file -- for MODE, from START, TARGET where a jump
    /// was to: show it.
    func hexPart(mode: Int, start: Int64, target: Int64) {
        guard let url = hexPath else { return }
        hexView.received(mode: mode, start: start, target: target, (try? Data(contentsOf: url)) ?? Data())
    }

    /// A hex line tapped: its bytes, to change and write back in place.  The app writes them (HEXEDIT: the
    /// offset and the text as typed) and, once the server has them, shows them with app_browser_hex_more.
    func editHex(_ offset: Int64, _ line: Data) {
        let alert = UIAlertController(title: String(format: "Bytes at %08llx", offset),
                                      message: "\(line.count) bytes, in hex. They are written over the file's own.",
                                      preferredStyle: .alert)
        alert.addTextField { f in
            f.text = line.map { String(format: "%02x", $0) }.joined(separator: " ")
            f.font = .monospacedSystemFont(ofSize: 13, weight: .regular)
            f.autocapitalizationType = .none
            f.autocorrectionType = .no
            f.spellCheckingType = .no
            f.keyboardType = .asciiCapable
            f.clearButtonMode = .whileEditing
        }
        alert.addAction(UIAlertAction(title: "Cancel", style: .cancel))
        alert.addAction(UIAlertAction(title: "Write", style: .destructive) { [weak self, weak alert] _ in
            guard let self, let text = alert?.textFields?.first?.text else { return }
            send("HEXEDIT", [.int(Int(self.handle)), .list([.int(Int(offset)), .string(text)])])
        })
        vc.present(alert, animated: true)
    }

    func sharePreview() {
        guard let url = previewURL else { return }
        let sheet = UIActivityViewController(activityItems: [url], applicationActivities: nil)
        if let popover = sheet.popoverPresentationController { popover.sourceView = shareButton ?? pane }
        vc.present(sheet, animated: true)
    }

    func numberOfPreviewItems(in controller: QLPreviewController) -> Int { previewURL == nil ? 0 : 1 }
    func previewController(_ controller: QLPreviewController, previewItemAt index: Int) -> QLPreviewItem {
        (previewURL ?? URL(fileURLWithPath: "/")) as NSURL
    }
    func previewController(_ controller: QLPreviewController, editingModeFor previewItem: QLPreviewItem)
        -> QLPreviewItemEditingMode { .disabled }

    // MARK: swiping back

    /// A swipe to the right, anywhere on the list, goes back up -- following the finger in portrait, as the
    /// system's own swipe-back does; to the left is the rows' own (their actions).
    func gestureRecognizerShouldBegin(_ g: UIGestureRecognizer) -> Bool {
        if g === contentDrag {
            if !hexView.scrubber.isHidden, hexView.scrubber.bounds.contains(contentDrag.location(in: hexView.scrubber)) {
                return false
            }
            let v = contentDrag.velocity(in: vc.view)
            return shelfShown && abs(v.y) > abs(v.x)
        }
        guard let pan = g as? UIPanGestureRecognizer, pages.count > 1 else { return false }
        let v = pan.velocity(in: pagesView)
        return v.x > 0 && v.x > abs(v.y) * 1.5
    }

    /// The swipe right goes up a folder, following the finger.  Columns: the last one goes out to the right,
    /// the others spreading again.  One at a time: it slides off, the one under it coming back from the left.
    /// Let go far enough (or flick), and it goes; not, and it springs back.
    @objc func swiped(_ g: UIPanGestureRecognizer) {
        guard pages.count > 1 else { return }
        let dx = max(0, g.translation(in: pagesView).x)
        let top = pages[pages.count - 1].view, below = pages[pages.count - 2].view
        let single = columns == 1
        let home = single ? 0 : CGFloat(columns - 1) * width / CGFloat(columns)   // the top page's resting x
        switch g.state {
        case .began:
            if let t = top as? UITableView {               // the list stops scrolling: this is a swipe
                t.panGestureRecognizer.isEnabled = false
                t.panGestureRecognizer.isEnabled = true
            }
            if single {
                below.frame = pagesView.bounds
                below.frame.origin.x = -width * 0.3
                below.isHidden = false
            }
            addShadow(top)
        case .changed:
            top.frame.origin.x = home + dx
            if single { below.frame.origin.x = -width * 0.3 + dx * 0.3 }
        case .ended where dx > top.bounds.width * 0.35 || g.velocity(in: pagesView).x > 600:
            back(from: dx)
        default:                                            // not far enough: back where it was
            UIView.animate(withDuration: 0.25, delay: 0, options: [.curveEaseOut]) {
                top.frame.origin.x = home
                if single { below.frame.origin.x = -self.width * 0.3 }
            } completion: { _ in
                top.layer.shadowOpacity = 0
                self.layoutPages()
            }
        }
    }

    // MARK: pages

    func push(_ th: UInt, _ page: UIView, path: [String]) {
        page.frame = pagesView.bounds
        let below = pages.last?.view
        pages.append((th, page, path))
        pagesView.addSubview(page)
        showTitle(path)
        updateGestures()
        guard let below, vc.view.window != nil else { layoutPages(); return }   // the first, or not on screen yet
        if columns > 1 {
            // a column more: it comes in at the right, and the columns make room
            page.frame = CGRect(x: width, y: 0, width: width / CGFloat(columns), height: pagesView.bounds.height)
            UIView.animate(withDuration: 0.3, delay: 0, options: [.curveEaseOut]) { self.layoutPages() }
            return
        }
        // one at a time: it slides in over the one before, which gives way a little to the left
        below.frame = pagesView.bounds
        below.isHidden = false
        page.frame = pagesView.bounds
        page.frame.origin.x = width
        addShadow(page)
        UIView.animate(withDuration: 0.35, delay: 0, options: [.curveEaseOut]) {
            page.frame.origin.x = 0
            below.frame.origin.x = -self.width * 0.3
        } completion: { _ in
            page.layer.shadowOpacity = 0
            self.layoutPages()
        }
    }

    /// The path of the page whose table is TH: known only once it is (a server's home, after connecting).
    func setPath(_ th: UInt, _ path: [String]) {
        guard let i = pages.firstIndex(where: { $0.table == th }) else { return }
        pages[i].path = path
        if i == pages.count - 1 { showTitle(path) } else { layoutTitle() }
    }

    /// Back to the page whose table is TH: those after it go (a POP for each), as a tap in an earlier column
    /// does before it opens something there.
    func popTo(_ th: UInt) {
        guard let i = pages.firstIndex(where: { $0.table == th }), i < pages.count - 1 else { return }
        let gone = pages[(i + 1)...]
        pages.removeSubrange((i + 1)...)
        if let t = previewTable, gone.contains(where: { $0.table == t }) { previewTable = nil }
        showTitle(pages.last!.path)
        updateGestures()
        UIView.animate(withDuration: 0.25, delay: 0, options: [.curveEaseOut]) {
            for page in gone { page.view.alpha = 0 }
            self.layoutPages()
        } completion: { _ in
            for page in gone {
                page.view.removeFromSuperview()
                tables[page.table] = nil
                send("POP", [.int(Int(page.table))])
            }
        }
    }

    func back(from dx: CGFloat? = nil) {
        guard pages.count > 1 else {
            closePreview(byUser: true)                     // the sheet goes with the screen that shows it
            vc.navigationController?.popViewController(animated: true)
            return
        }
        let top = pages.removeLast()
        if previewTable == top.table { previewTable = nil }
        showTitle(pages.last!.path)
        updateGestures()
        if columns == 1 {
            // one at a time: it slides off (from where a swipe has it), the one under it back from the left
            let below = pages.last!.view
            below.frame = pagesView.bounds
            below.isHidden = false
            below.frame.origin.x = -width * 0.3 + (dx ?? 0) * 0.3
            top.view.frame.origin.x = dx ?? 0
            addShadow(top.view)
            UIView.animate(withDuration: 0.3, delay: 0, options: [.curveEaseOut]) {
                top.view.frame.origin.x = self.width
                below.frame.origin.x = 0
            } completion: { _ in
                top.view.layer.shadowOpacity = 0
                top.view.removeFromSuperview()
                tables[top.table] = nil
                send("POP", [.int(Int(top.table))])
                self.layoutPages()
            }
            return
        }
        // a column less: it leaves to the right (from where a swipe has it), and the columns spread again
        UIView.animate(withDuration: 0.3, delay: 0, options: [.curveEaseOut]) {
            top.view.frame.origin.x = self.width
            top.view.alpha = 0
            self.layoutPages()
        } completion: { _ in
            top.view.layer.shadowOpacity = 0
            top.view.removeFromSuperview()
            tables[top.table] = nil
            send("POP", [.int(Int(top.table))])
        }
    }

    /// The title changes where it is: a cross-fade, no travel.
    func showTitle(_ path: [String]) {
        let label = pathLabel(path)
        UIView.transition(with: title.label, duration: 0.2, options: .transitionCrossDissolve) {
            self.title.label.attributedText = label.attributedText
        }
        title.fit()
        vc.title = path.last
        layoutTitle()
    }

    func addShadow(_ v: UIView) {
        v.layer.shadowColor = UIColor.black.cgColor
        v.layer.shadowOpacity = 0.12
        v.layer.shadowRadius = 8
        v.layer.shadowOffset = CGSize(width: -2, height: 0)
    }

    /// The system's own swipe-back leaves the browser, so it is on only when there is no page to go back to.
    func updateGestures() {
        guard let nav = vc.navigationController else { return }
        let first = pages.count <= 1
        nav.interactivePopGestureRecognizer?.isEnabled = first
        if #available(iOS 26.0, *) { nav.interactiveContentPopGestureRecognizer?.isEnabled = first }
    }
}
private var browsers: [UInt: NativeBrowser] = [:]

/// app_browser(): a browser screen, not shown yet: push its first page, add its bar buttons, then app_show it.
/// Returns the screen's handle.
let appBrowser: Call = { _, _, _, _, _, _ in
    let screen = BrowserScreen()
    let (sh, _) = makeScreen("", screen)
    screen.navigationItem.largeTitleDisplayMode = .never
    browsers[sh] = NativeBrowser(handle: sh, vc: screen)
    return sh
}

/// app_browser_preview(browser, path, name, kind): the local file PATH -- KIND 1: as text, in a monospaced text
/// view of ours; 2: an image, in a zoomable viewer of ours; 3: a hex dump, its parts appended as they come
/// (app_browser_hex_total first, then app_browser_hex_more for each part); 0: Quick Look in BROWSER's pane -- under the
/// list in portrait, a column beside it in landscape -- headed NAME, with a share button and a close button.
/// Showing another file replaces it.  The close button is a PREVIEWED event with the browser's handle.
let appBrowserPreview: Call = { h, pp, pn, np, nn, kind in
    guard let b = browsers[h] else { return 0 }
    b.showPreview(URL(fileURLWithPath: text(pp, pn)), name: text(np, nn),
                  kind: NativeBrowser.PreviewKind(rawValue: kind) ?? .quickLook)
    return 1
}

/// app_browser_subtitle(browser, text): a line under the bar's path, smaller -- the server's name.  Empty: none.
let appBrowserSubtitle: Call = { h, p, n, _, _, _ in
    guard let b = browsers[h] else { return 0 }
    b.title.setSubtitle(text(p, n))
    return 1
}

/// app_browser_set_path(browser, table, path-term): the page whose table is TABLE has the path PATH now.
let appBrowserSetPath: Call = { h, th, p, n, _, _ in
    guard let b = browsers[h] else { return 0 }
    b.setPath(th, (termBytes(p, n)?.listItems ?? []).compactMap { str($0) })
    return 1
}

/// app_browser_preview_toggle(browser, shown, on): the preview's hex toggle -- SHOWN 1 for a file with a view
/// besides its bytes; ON 1 while the bytes are what is shown.
let appBrowserPreviewToggle: Call = { h, shown, on, _, _, _ in
    browsers[h]?.setHexToggle(shown: shown != 0, on: on != 0)
    return 1
}

/// app_browser_hex_total(browser, size): the size of the file the next hex preview shows (its parts come later).
let appBrowserHexTotal: Call = { h, size, _, _, _, _ in
    browsers[h]?.hexTotal = Int64(size)
    return 1
}

/// app_browser_hex_part(browser, mode, start, target): the hex preview's local file holds the part of the file
/// from START that its HEXMORE asked for -- MODE 0, a jump to TARGET; 1, the part after; 2, the part before; 3,
/// bytes rewritten in place (an edit, saved): show it.  A part of nothing: the file ends there.
let appBrowserHexPart: Call = { h, mode, start, target, _, _ in
    browsers[h]?.hexPart(mode: Int(mode), start: Int64(start), target: Int64(target))
    return 1
}

/// app_browser_pop_to(browser, table): back to the page whose table is TABLE -- the pages after it go, a POP for
/// each.  A tap in an earlier column, before what it opens.
let appBrowserPopTo: Call = { h, th, _, _, _, _ in
    browsers[h]?.popTo(th)
    return 1
}

/// app_browser_preview_close(browser): the pane goes (no PREVIEWED: the app asked).
let appBrowserPreviewClose: Call = { h, _, _, _, _, _ in
    browsers[h]?.closePreview(byUser: false)
    return 1
}

/// app_orientation(which): ask for portrait (0) or landscape (1) -- what turning the phone does, for a test
/// run that cannot turn it.  The app must allow it (its Info.plist's orientations).
let appOrientation: Call = { which, _, _, _, _, _ in
    guard let scene = keyWindow()?.windowScene else { return 0 }
    scene.requestGeometryUpdate(.iOS(interfaceOrientations: which == 1 ? .landscapeRight : .portrait))
    return 1
}

/// app_browser_push(browser, path-term, plain): a new page -- a table -- slid in over the browser's top page (the
/// first is simply there), its path in the bar.  Returns the table's handle (app_table_set and the rest).
let appBrowserPush: Call = { h, p, n, plain, _, _ in
    guard let b = browsers[h] else { return 0 }
    let path = (termBytes(p, n)?.listItems ?? []).compactMap { str($0) }
    let (th, source) = makeTable(plain: plain != 0, frame: b.vc.view.bounds)
    source.enableSearch()
    b.push(th, source.table, path: path)
    return th
}

/// app_table_menu(table, items): the menu of a long press off the rows (plists of :ID :TITLE :IMAGE
/// :DESTRUCTIVE); a choice is a MENU event, (-1 id).
let appTableMenu: Call = { h, p, n, _, _, _ in
    guard let source = tables[h] else { return 0 }
    source.setSpaceMenu((termBytes(p, n)?.listItems ?? []).compactMap { item in
        let a = plist(item)
        guard case .int(let id)? = a["ID"] else { return nil }
        return (id, str(a["TITLE"]) ?? "", str(a["IMAGE"]), flag(a["DESTRUCTIVE"]))
    })
    return 1
}

/// app_table_order(table, plist): how TABLE's rows go -- :KEY (a name among the rows' :KEYS, or nil for :SORT),
/// :DESCENDING, :GROUPS-FIRST.
let appTableOrder: Call = { h, p, n, _, _, _ in
    guard let source = tables[h], let t = termBytes(p, n) else { return 0 }
    let m = plist(t)
    source.orderKey = str(m["KEY"])
    source.descending = flag(m["DESCENDING"])
    source.groupsFirst = flag(m["GROUPS-FIRST"])
    source.regroup()
    return 1
}

/// app_table_set(table, rows): ROWS a term, a list of plists -- :TITLE, and any of :SUBTITLE :DETAIL :IMAGE (an SF
/// Symbol's name) :TINT (an RGB integer) :CHEVRON :SECTION :SORT (a key the rows are shown in the order of) :DOT (an RGB integer: a
/// status dot before the chevron) :MENU (its long press's menu: plists of :ID :TITLE :IMAGE :DESTRUCTIVE, a
/// choice a MENU event with (row id)).  A
/// row's index is its place in the list.
private func nativeRow(_ item: Term, _ index: Int) -> NativeRow {
    let m = plist(item)
    return NativeRow(title: str(m["TITLE"]) ?? "", subtitle: str(m["SUBTITLE"]), detail: str(m["DETAIL"]),
                     image: str(m["IMAGE"]), tint: color(m["TINT"]), chevron: flag(m["CHEVRON"]),
                     section: str(m["SECTION"]), index: index, sort: str(m["SORT"]),
                     group: { if case .int(let g)? = m["GROUP"] { return g }; return nil }(),
                     keys: m["KEYS"].map(plist) ?? [:],
                     bar: { if case .int(let b)? = m["BAR"] { return Double(min(1000, max(0, b))) / 1000 }; return nil }(),
                     dot: color(m["DOT"]),
                     menu: (m["MENU"]?.listItems ?? []).compactMap { item in
                         let a = plist(item)
                         guard case .int(let id)? = a["ID"] else { return nil }
                         return (id, str(a["TITLE"]) ?? "", str(a["IMAGE"]), flag(a["DESTRUCTIVE"]))
                     })
}

let appTableSet: Call = { h, p, n, _, _, _ in
    guard let source = tables[h] else { return 0 }
    let items = termBytes(p, n)?.listItems ?? []
    source.setRows(items.enumerated().map { i, item in nativeRow(item, i) })
    return 1
}

/// app_table_append(table, rows): ROWS (as for app_table_set) after the table's own; their indices go on from
/// there.  With app_table_loading on, the bottom of the list says how many have come so far.
let appTableAppend: Call = { h, p, n, _, _, _ in
    guard let source = tables[h] else { return 0 }
    let base = source.all.count
    let items = termBytes(p, n)?.listItems ?? []
    source.append(items.enumerated().map { i, item in nativeRow(item, base + i) })
    return 1
}

/// app_table_actions(table, actions): the swipe actions, a term -- a list of plists, :TITLE and optionally
/// :DESTRUCTIVE and :TINT.  A swipe action is a ROWACTION event with (action row).
let appTableActions: Call = { h, p, n, _, _, _ in
    guard let source = tables[h] else { return 0 }
    source.actions = (termBytes(p, n)?.listItems ?? []).map { item in
        let m = plist(item)
        return NativeAction(title: str(m["TITLE"]) ?? "", destructive: flag(m["DESTRUCTIVE"]), tint: color(m["TINT"]))
    }
    return 1
}

/// app_table_refresh(table): pull to refresh, a REFRESH event; app_table_refresh_end(table) puts the spinner away.
let appTableRefresh: Call = { h, _, _, _, _, _ in
    guard let source = tables[h] else { return 0 }
    let control = UIRefreshControl()
    control.addTarget(source, action: #selector(NativeTable.refreshed), for: .valueChanged)
    source.table.refreshControl = control
    return 1
}

let appTableRefreshEnd: Call = { h, _, _, _, _, _ in
    tables[h]?.table.refreshControl?.endRefreshing()
    return 1
}

/// app_table_empty(table, plist): what the table shows when it has no rows -- a term, (:TITLE :MESSAGE :IMAGE).
/// app_table_loading(table, on): while ON and the table has no rows, a spinner where its empty view would be --
/// after 150 ms, so a quick load does not flash one.  Rows (app_table_set) end it.
let appTableLoading: Call = { h, on, _, _, _, _ in
    guard let source = tables[h] else { return 0 }
    source.loading = on != 0
    if on != 0 {
        source.loadingSince = Date()
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.45) { [weak source] in
            if let source, source.loading { source.showFooter() }
        }
    }
    if on == 0 {
        source.showEmpty()
    } else {
        // not "empty" while it loads -- but what a placeholder says (still connecting, opening) stays up
        if source.placeholder == nil { source.table.backgroundView = nil }
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.15) { [weak source] in
            if let source, source.loading { source.showEmpty() }
        }
    }
    return 1
}

/// app_table_placeholder(table, plist): while TABLE has no rows, what it is waiting on or why it has nothing --
/// (:LOADING t :TITLE "Connecting…" :MESSAGE "…") the system's loading view, a spinner and the words; or
/// (:TITLE :MESSAGE :IMAGE :BUTTON) a message, its button a PLACEHOLDER event.  NIL takes it away.
let appTablePlaceholder: Call = { h, p, n, _, _, _ in
    guard let source = tables[h] else { return 0 }
    guard let t = termBytes(p, n), !(t.listItems ?? []).isEmpty else {
        source.placeholder = nil
        source.showEmpty()
        return 1
    }
    let m = plist(t)
    var config: UIContentUnavailableConfiguration = flag(m["LOADING"]) ? .loading() : .empty()
    config.text = str(m["TITLE"])
    config.secondaryText = str(m["MESSAGE"])
    if let name = str(m["IMAGE"]) { config.image = UIImage(systemName: name) }
    if let label = str(m["BUTTON"]) {
        var button = UIButton.Configuration.filled()
        button.title = label
        config.button = button
        config.buttonProperties.primaryAction = UIAction { _ in send("PLACEHOLDER", [.int(Int(h))]) }
    }
    source.placeholder = config
    source.showEmpty()
    return 1
}

let appTableEmpty: Call = { h, p, n, _, _, _ in
    guard let source = tables[h], let t = termBytes(p, n) else { return 0 }
    let m = plist(t)
    var config = UIContentUnavailableConfiguration.empty()
    config.text = str(m["TITLE"])
    config.secondaryText = str(m["MESSAGE"])
    if let name = str(m["IMAGE"]) { config.image = UIImage(systemName: name) }
    source.empty = config
    source.showEmpty()
    return 1
}

// MARK: - alerts with fields

/// app_alert(title, message, sheet): an alert (or, SHEET 1, an action sheet), not shown yet.
let appAlert: Call = { tp, tn, mp, mn, sheet, _ in
    let alert = UIAlertController(title: tn > 0 ? text(tp, tn) : nil, message: mn > 0 ? text(mp, mn) : nil,
                                  preferredStyle: sheet != 0 ? .actionSheet : .alert)
    let h = newHandle()
    nativeAlerts[h] = alert
    return h
}

/// app_alert_action(alert, title, style): a button -- STYLE 0 default, 1 cancel, 2 destructive.  A tap is an ALERT
/// event with (index text ...): the button's index and each field's text.
let appAlertAction: Call = { h, p, n, style, _, _ in
    guard let alert = nativeAlerts[h] else { return 0 }
    let index = alert.actions.count
    let s: UIAlertAction.Style = style == 1 ? .cancel : style == 2 ? .destructive : .default
    alert.addAction(UIAlertAction(title: text(p, n), style: s) { [weak alert] _ in
        let texts = (alert?.textFields ?? []).map { Term.string($0.text ?? "") }
        send("ALERT", [.int(Int(h)), .list([.int(index)] + texts)])
        nativeAlerts[h] = nil
    })
    return UInt(index)
}

/// app_alert_field(alert, placeholder, text, secure): a text field, with TEXT in it.
let appAlertField: Call = { h, pp, pn, tp, tn, secure in
    guard let alert = nativeAlerts[h] else { return 0 }
    let placeholder = text(pp, pn), value = text(tp, tn)
    alert.addTextField { field in
        field.placeholder = placeholder
        field.text = value
        field.isSecureTextEntry = secure != 0
        field.autocapitalizationType = .none
        field.autocorrectionType = .no
        field.clearButtonMode = .whileEditing
    }
    return 1
}

let appAlertShow: Call = { h, _, _, _, _, _ in
    guard let alert = nativeAlerts[h], let host = presenter() else { return 0 }
    anchor(alert, on: host)
    host.present(alert, animated: true)
    return 1
}

// MARK: - forms

/// A record edited on a screen of its own, presented over the app: labelled fields in one section, buttons (a
/// TAP each) in the next, Cancel and SAVE in the bar.  Closing it is a FORM event: (1 value ...) or (0).
final class NativeForm: NSObject, UITableViewDataSource, UITableViewDelegate {
    enum Kind: UInt { case text = 0, number, secure, url, toggle, choice }
    struct Field {
        let label: String; var value: String; let placeholder: String; let kind: Kind
        var choices: [String] = []                       // a choice: its value is the index of the one chosen
    }
    let handle: UInt
    var fields: [Field] = []
    var buttons: [(title: String, handle: UInt)] = []
    var note: String?
    let controller = UITableViewController(style: .insetGrouped)
    var nav: UINavigationController?

    init(handle: UInt, title: String, save: String) {
        self.handle = handle
        super.init()
        controller.title = title
        controller.tableView.dataSource = self
        controller.tableView.delegate = self
        controller.navigationItem.leftBarButtonItem = UIBarButtonItem(systemItem: .cancel, primaryAction: UIAction { _ in
            self.close(saved: false)
        })
        controller.navigationItem.rightBarButtonItem = UIBarButtonItem(title: save, primaryAction: UIAction { _ in
            self.close(saved: true)
        })
        controller.navigationItem.rightBarButtonItem?.style = .done
    }

    func close(saved: Bool) {
        controller.view.endEditing(true)
        var values: [Term] = [.int(saved ? 1 : 0)]
        if saved { values += fields.map { .string($0.value) } }
        send("FORM", [.int(Int(handle)), .list(values)])
        nav?.dismiss(animated: true)
        forms[handle] = nil
    }

    func numberOfSections(in tableView: UITableView) -> Int { buttons.isEmpty ? 1 : 2 }
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        section == 0 ? fields.count : buttons.count
    }
    func tableView(_ tableView: UITableView, titleForFooterInSection section: Int) -> String? {
        section == numberOfSections(in: tableView) - 1 ? note : nil
    }

    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = UITableViewCell(style: .default, reuseIdentifier: nil)
        if indexPath.section == 1 {
            var c = cell.defaultContentConfiguration()
            c.text = buttons[indexPath.row].title
            c.textProperties.color = .tintColor
            cell.contentConfiguration = c
            return cell
        }
        let i = indexPath.row
        let f = fields[i]
        cell.selectionStyle = .none
        let label = UILabel()
        label.text = f.label
        label.font = .preferredFont(forTextStyle: .body)
        label.setContentHuggingPriority(.required, for: .horizontal)
        let control: UIView
        if f.kind == .toggle {
            let sw = UISwitch()
            sw.isOn = f.value == "1"
            sw.addAction(UIAction { [weak self] _ in self?.fields[i].value = sw.isOn ? "1" : "0" }, for: .valueChanged)
            let spacer = UIView()                          // at the row's right edge, as Settings has it
            spacer.setContentHuggingPriority(.defaultLow, for: .horizontal)
            control = UIStackView(arrangedSubviews: [spacer, sw])
        } else if f.kind == .choice {
            // a pop-up menu, as Settings has: the choice shown, the others a tap away
            let button = UIButton(type: .system)
            let chosen = Int(f.value) ?? 0
            button.menu = UIMenu(children: f.choices.enumerated().map { k, title in
                UIAction(title: title, state: k == chosen ? .on : .off) { [weak self] _ in self?.fields[i].value = String(k) }
            })
            button.showsMenuAsPrimaryAction = true
            button.changesSelectionAsPrimaryAction = true
            button.contentHorizontalAlignment = .trailing
            let spacer = UIView()
            spacer.setContentHuggingPriority(.defaultLow, for: .horizontal)
            control = UIStackView(arrangedSubviews: [spacer, button])
        } else {
            let field = UITextField()
            field.text = f.value
            field.placeholder = f.placeholder
            field.textAlignment = .right
            field.autocapitalizationType = .none
            field.autocorrectionType = .no
            field.isSecureTextEntry = f.kind == .secure
            field.keyboardType = f.kind == .number ? .numberPad : f.kind == .url ? .URL : .default
            field.clearButtonMode = .whileEditing
            field.addAction(UIAction { [weak self] _ in self?.fields[i].value = field.text ?? "" }, for: .editingChanged)
            control = field
        }
        let row = UIStackView(arrangedSubviews: [label, control])
        row.spacing = 12
        row.alignment = .center
        row.translatesAutoresizingMaskIntoConstraints = false
        cell.contentView.addSubview(row)
        NSLayoutConstraint.activate([
            row.leadingAnchor.constraint(equalTo: cell.contentView.layoutMarginsGuide.leadingAnchor),
            row.trailingAnchor.constraint(equalTo: cell.contentView.layoutMarginsGuide.trailingAnchor),
            row.topAnchor.constraint(equalTo: cell.contentView.topAnchor, constant: 6),
            row.bottomAnchor.constraint(equalTo: cell.contentView.bottomAnchor, constant: -6),
            row.heightAnchor.constraint(greaterThanOrEqualToConstant: 32),
        ])
        return cell
    }

    func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        tableView.deselectRow(at: indexPath, animated: true)
        if indexPath.section == 1 { send("TAP", [.int(Int(buttons[indexPath.row].handle))]) }
    }
}

/// app_form(title, save): a form, not shown yet.
let appForm: Call = { tp, tn, sp, sn, _, _ in
    let h = newHandle()
    forms[h] = NativeForm(handle: h, title: text(tp, tn), save: sn > 0 ? text(sp, sn) : "Save")
    return h
}

/// app_form_field(form, plist): a field, a term -- (:LABEL :VALUE :PLACEHOLDER :KIND), KIND 0 text, 1 number,
/// 2 secure, 3 URL-ish, 4 a switch (its value "1" or "0"), 5 a choice among :CHOICES (strings; its value the
/// index of the one chosen, "0" the first).  Returns the field's index.
let appFormField: Call = { h, p, n, _, _, _ in
    guard let form = forms[h], let t = termBytes(p, n) else { return 0 }
    let m = plist(t)
    var kindNo: UInt = 0
    if case .int(let k)? = m["KIND"] { kindNo = UInt(max(0, k)) }
    form.fields.append(.init(label: str(m["LABEL"]) ?? "", value: str(m["VALUE"]) ?? "",
                             placeholder: str(m["PLACEHOLDER"]) ?? "", kind: NativeForm.Kind(rawValue: kindNo) ?? .text,
                             choices: (m["CHOICES"]?.listItems ?? []).compactMap { str($0) }))
    return UInt(form.fields.count - 1)
}

/// app_form_button(form, title): a button in the form's second section; a tap is a TAP event with its handle.
let appFormButton: Call = { h, p, n, _, _, _ in
    guard let form = forms[h] else { return 0 }
    let bh = newHandle()
    form.buttons.append((text(p, n), bh))
    return bh
}

/// app_form_note(form, text): a footnote under the form.
let appFormNote: Call = { h, p, n, _, _, _ in
    forms[h]?.note = text(p, n)
    return 1
}

let appFormShow: Call = { h, _, _, _, _, _ in
    guard let form = forms[h], let host = presenter() else { return 0 }
    let nav = UINavigationController(rootViewController: form.controller)
    form.nav = nav
    nav.isModalInPresentation = true                    // Cancel or Save, not a swipe that loses the edits
    host.present(nav, animated: true)
    return 1
}

// MARK: - the status bar: a transfer's progress, at the bottom

final class NativeStatus {
    let label = UILabel()
    let bar = UIProgressView(progressViewStyle: .default)
    let stack: UIStackView
    init() {
        label.font = .preferredFont(forTextStyle: .footnote)
        label.textColor = .secondaryLabel
        label.lineBreakMode = .byTruncatingMiddle
        stack = UIStackView(arrangedSubviews: [label, bar])
        stack.axis = .vertical
        stack.spacing = 4
        stack.frame = CGRect(x: 0, y: 0, width: 240, height: 32)
    }
}

/// app_status(nav, text, permille, cancellable): the status bar at the bottom of NAV with TEXT and a progress
/// bar at PERMILLE (0-1000; above 1000, no bar), and Cancel when CANCELLABLE.  An empty TEXT hides it.
let appStatus: Call = { h, p, n, permille, cancellable, _ in
    guard let nav = navs[h] else { return 0 }
    let s = statuses[h] ?? NativeStatus()
    statuses[h] = s
    let message = text(p, n)
    if message.isEmpty {
        s.bar.setProgress(0, animated: false)
        nav.setToolbarHidden(true, animated: true)
        return 1
    }
    s.label.text = message
    // the bar is the status line's, kept from one transfer to the next: it animates forward only, and starts
    // each transfer from where that one is -- not by sliding back from the last one's end
    let value = Float(min(permille, 1000)) / 1000
    if permille > 1000 {
        s.bar.isHidden = true
        s.bar.setProgress(0, animated: false)
    } else {
        let fresh = s.bar.isHidden || value < s.bar.progress
        s.bar.isHidden = false
        s.bar.setProgress(value, animated: !fresh)
    }
    var items: [UIBarButtonItem] = [UIBarButtonItem(customView: s.stack), .flexibleSpace()]
    if cancellable != 0 {
        items.append(UIBarButtonItem(systemItem: .cancel, primaryAction: UIAction { _ in
            send("STATUSCANCEL", [.int(Int(h))])
        }))
    }
    // the bar goes on every screen of the stack: whichever is on top shows it
    for vc in nav.viewControllers { vc.toolbarItems = items }
    s.stack.frame.size.width = max(160, nav.view.bounds.width - (cancellable != 0 ? 120 : 40))
    nav.setToolbarHidden(false, animated: true)
    return 1
}

// MARK: - files in and out

final class NativePicker: NSObject, UIDocumentPickerDelegate {
    let handle: UInt
    init(handle: UInt) { self.handle = handle }
    func documentPicker(_ controller: UIDocumentPickerViewController, didPickDocumentsAt urls: [URL]) {
        send("PICKED", [.int(Int(handle)), .string(urls.first?.path ?? "")])
        pickers[handle] = nil
    }
    func documentPickerWasCancelled(_ controller: UIDocumentPickerViewController) {
        send("PICKED", [.int(Int(handle)), .string("")])
        pickers[handle] = nil
    }
}

/// app_pick_file(): the system's document picker for any file, copied into the app (its tmp) -- a PICKED event
/// with the copy's path, or "" when cancelled.  Returns the picker's handle.
let appPickFile: Call = { _, _, _, _, _, _ in
    guard let host = presenter() else { return 0 }
    let h = newHandle()
    let delegate = NativePicker(handle: h)
    pickers[h] = delegate
    let picker = UIDocumentPickerViewController(forOpeningContentTypes: [.item], asCopy: true)
    picker.delegate = delegate
    host.present(picker, animated: true)
    return h
}

/// Quick Look, for one local file: images, text, PDF, Markdown -- what the system can show -- with its own
/// share button (Save to Files, Open In).  Closing it is a PREVIEWED event.
final class NativePreview: NSObject, QLPreviewControllerDataSource, QLPreviewControllerDelegate {
    let handle: UInt
    let url: URL
    init(handle: UInt, url: URL) { self.handle = handle; self.url = url }
    func numberOfPreviewItems(in controller: QLPreviewController) -> Int { 1 }
    func previewController(_ controller: QLPreviewController, previewItemAt index: Int) -> QLPreviewItem {
        url as NSURL
    }
    /// No markup: what is shown is a copy fetched to look at, and an edit to it would go nowhere.
    func previewController(_ controller: QLPreviewController, editingModeFor previewItem: QLPreviewItem)
        -> QLPreviewItemEditingMode { .disabled }
    func previewControllerDidDismiss(_ controller: QLPreviewController) {
        send("PREVIEWED", [.int(Int(handle))])
        previews[handle] = nil
    }
}
private var previews: [UInt: NativePreview] = [:]

/// app_preview(path): Quick Look over the app, for the local file at PATH.  Returns its handle.
let appPreview: Call = { p, n, _, _, _, _ in
    guard let host = presenter() else { return 0 }
    let h = newHandle()
    let preview = NativePreview(handle: h, url: URL(fileURLWithPath: text(p, n)))
    previews[h] = preview
    let ql = QLPreviewController()
    ql.dataSource = preview
    ql.delegate = preview
    host.present(ql, animated: true)
    return h
}

/// app_share_file(path): the share sheet for the file at PATH (Save to Files, AirDrop, open in another app).
let appShareFile: Call = { p, n, _, _, _, _ in
    guard let host = presenter() else { return 0 }
    let h = newHandle()
    let sheet = UIActivityViewController(activityItems: [URL(fileURLWithPath: text(p, n))], applicationActivities: nil)
    sheet.completionWithItemsHandler = { _, completed, _, _ in
        send("SHARE", [.int(Int(h)), .int(completed ? 1 : 0)])
    }
    anchor(sheet, on: host)
    host.present(sheet, animated: true)
    return h
}

/// app_utc_offset(): the local time zone's offset from UTC now, in seconds, plus 86400 (a word is unsigned).
let appUtcOffset: Call = { _, _, _, _, _, _ in UInt(TimeZone.current.secondsFromGMT() + 86400) }

/// app_resolve(name): NAME's IPv4 address by the system's resolver -- the one that knows the VPN's DNS, .local
/// names and the phone's own settings, none of which a query of our own would reach.  It runs off the main
/// thread; a RESOLVED event brings the dotted address, or "" when there is none.  Returns its handle.
let appResolve: Call = { p, n, _, _, _, _ in
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
let appCopy: Call = { p, n, _, _, _, _ in
    UIPasteboard.general.string = text(p, n)
    return 1
}

/// app_documents(buf, cap): the app's Documents directory -- what the Files app shows as the app's folder.
let appDocuments: Call = { buf, cap, _, _, _, _ in
    let path = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first?.path ?? ""
    let bytes = Array(path.utf8)
    if bytes.count <= Int(cap), let out = UnsafeMutableRawPointer(bitPattern: buf) {
        bytes.withUnsafeBytes { out.copyMemory(from: $0.baseAddress!, byteCount: bytes.count) }
    }
    return UInt(bytes.count)
}

func installNative() {
    register("app_nav_new", appNavNew)
    register("app_nav_root", appNavRoot)
    register("app_push", appPush)
    register("app_pop", appPop)
    register("app_path_title", appPathTitle)
    register("app_browser", appBrowser)
    register("app_browser_push", appBrowserPush)
    register("app_browser_preview", appBrowserPreview)
    register("app_browser_preview_close", appBrowserPreviewClose)
    register("app_browser_pop_to", appBrowserPopTo)
    register("app_browser_set_path", appBrowserSetPath)
    register("app_browser_subtitle", appBrowserSubtitle)
    register("app_browser_preview_toggle", appBrowserPreviewToggle)
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
    register("app_utc_offset", appUtcOffset)
    register("app_resolve", appResolve)
    register("app_documents", appDocuments)
}
