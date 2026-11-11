// modus-watch-ui.swift — the native components a modus app builds from Lisp, on watchOS: the SwiftUI twin of
// host/ios/swift/ModusSwift.swift.  The same functions under the same names (host/ios/modus-swift.m's registry), so
// Lisp's kiln-swift drives either.  Here a handle names a NODE, and SwiftUI draws the nodes over the screen's
// framebuffer (modus-watch.swift's ModusView puts NodesLayer on top of it).  Events go out as the same terms.
//
// Positions are frames, as on the phone: a node at the top of the tree sits at its frame over the screen; a stack's,
// a scroll view's and a list's children are laid out by SwiftUI.  A width of 0 asks for the control's own size.
//
// Calls run on the main thread (modus-swift.m dispatches them there), which is where SwiftUI's state lives.
//
// Events, as terms, oldest first (ui_events_next):
//   (TAP h)  (SWITCH h on)  (SLIDER h value)  (STEP h value)  (TEXT h)  (SELECT h index)  (DATE h seconds)
//   (ALERT h index)  (CROWN h value)  (DISMISS h)

import SwiftUI

typealias Call = @convention(c) (UInt, UInt, UInt, UInt, UInt, UInt) -> UInt

enum Kind {
    case label, button, toggle, slider, stepper, progress, spinner, picker, list, image, panel, vstack, hstack,
         scroll, field, gauge, date, crown, container, nav, tabs
}

final class Node: ObservableObject, Identifiable {
    let id: UInt
    let kind: Kind
    @Published var frame: CGRect
    @Published var text = ""
    @Published var value = 0.0
    @Published var on = false
    @Published var date = Date()
    @Published var titles: [String] = []
    @Published var children: [Node] = []
    @Published var path: [Int] = []          // a navigation controller's pushed pages, by index in children
    var color = Color.clear
    var spacing: CGFloat = 0
    weak var parent: Node?

    init(_ id: UInt, _ kind: Kind, _ frame: CGRect) {
        self.id = id
        self.kind = kind
        self.frame = frame
    }
}

struct AlertModel: Identifiable {
    let id: UInt
    let title: String
    let message: String
    var actions: [String]
}

final class Nodes: ObservableObject {
    @Published var roots: [Node] = []
    @Published var alert: AlertModel?
    @Published var sheet: Node?
}

let nodesModel = Nodes()
private var nodes: [UInt: Node] = [:]
private var alerts: [UInt: AlertModel] = [:]
private var nextHandle: UInt = 1
private var events: [Term] = []

private func send(_ name: String, _ args: [Term]) {
    events.append(.list([.symbol(name: name, package: "KEYWORD")] + args))
}

private func text(_ p: UInt, _ n: UInt) -> String {
    guard n > 0, let base = UnsafeRawPointer(bitPattern: p) else { return "" }
    return String(decoding: UnsafeRawBufferPointer(start: base, count: Int(n)), as: UTF8.self)
}

private func term(_ p: UInt, _ n: UInt) -> Term? {
    guard n > 0, let base = UnsafeRawPointer(bitPattern: p) else { return nil }
    return try? decodeTerm(Array(UnsafeRawBufferPointer(start: base, count: Int(n))))
}

/// The strings of a term that is a list of strings.
private func titlesOf(_ p: UInt, _ n: UInt) -> [String] {
    (term(p, n)?.listItems ?? []).compactMap { item -> String? in
        if case .string(let s) = item { return s }
        return nil
    }
}

/// The :TITLE of each plist in a term that is a list of plists (a list's rows).
private func rowTitles(_ p: UInt, _ n: UInt) -> [String] {
    (term(p, n)?.listItems ?? []).map { row in
        let pairs = row.listItems ?? []
        var i = 0
        while i + 1 < pairs.count {
            if case .symbol(let key, _) = pairs[i], key == "TITLE", case .string(let value) = pairs[i + 1] {
                return value
            }
            i += 2
        }
        return ""
    }
}

private func rect(_ x: UInt, _ y: UInt, _ w: UInt, _ h: UInt) -> CGRect {
    CGRect(x: CGFloat(x), y: CGFloat(y), width: CGFloat(w), height: CGFloat(h))
}

private func handle() -> UInt {
    let h = nextHandle
    nextHandle += 1
    return h
}

/// A node at the top of the tree, over the screen.
private func make(_ kind: Kind, _ frame: CGRect) -> Node {
    let node = Node(handle(), kind, frame)
    nodes[node.id] = node
    nodesModel.roots.append(node)
    return node
}

/// A node that is no view's child and not on the screen: a sheet's content, a page.
private func loose(_ kind: Kind) -> Node {
    let node = Node(handle(), kind, .zero)
    nodes[node.id] = node
    return node
}

private func detach(_ node: Node) {
    if let parent = node.parent {
        parent.children.removeAll { $0 === node }
        node.parent = nil
    } else {
        nodesModel.roots.removeAll { $0 === node }
    }
}

// MARK: - drawing

/// A node at its frame: its origin, and its size unless the frame gives none (then the control's own).
struct Placed: View {
    @ObservedObject var node: Node
    var body: some View {
        NodeView(node: node)
            .frame(width: node.frame.width > 0 ? node.frame.width : nil,
                   height: node.frame.height > 0 ? node.frame.height : nil, alignment: .topLeading)
            .offset(x: node.frame.minX, y: node.frame.minY)
    }
}

/// Children with frames, drawn where their frames say.
struct Absolute: View {
    @ObservedObject var node: Node
    var body: some View {
        ZStack(alignment: .topLeading) {
            node.color
            ForEach(node.children) { Placed(node: $0) }
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .topLeading)
    }
}

struct NodeView: View {
    @ObservedObject var node: Node

    var body: some View {
        switch node.kind {
        case .label:
            Text(node.text).font(.system(size: 14)).foregroundColor(.white)
                .frame(maxWidth: .infinity, alignment: .leading)
        case .button:
            Button(node.text) { send("TAP", [.int(Int(node.id))]) }
        case .toggle:
            Toggle(node.text, isOn: Binding(get: { node.on }, set: { on in
                node.on = on
                send("SWITCH", [.int(Int(node.id)), .int(on ? 1 : 0)])
            }))
        case .slider:
            Slider(value: Binding(get: { node.value }, set: { v in
                node.value = v
                send("SLIDER", [.int(Int(node.id)), .double(v)])
            }), in: 0...100, step: 10)
        case .stepper:
            Stepper(value: Binding(get: { node.value }, set: { v in
                node.value = v
                send("STEP", [.int(Int(node.id)), .double(v)])
            }), in: 0...100) { Text("\(Int(node.value))") }
        case .progress:
            ProgressView(value: node.value, total: 100)
        case .spinner:
            if node.on { ProgressView() } else { Color.clear }
        case .picker:
            Picker(node.text, selection: Binding(get: { Int(node.value) }, set: { i in
                node.value = Double(i)
                send("SELECT", [.int(Int(node.id)), .int(i)])
            })) {
                ForEach(node.titles.indices, id: \.self) { i in Text(node.titles[i]).tag(i) }
            }
            .pickerStyle(.wheel)
        case .list:
            List {
                ForEach(node.titles.indices, id: \.self) { i in
                    Button(node.titles[i]) { send("SELECT", [.int(Int(node.id)), .int(i)]) }
                }
            }
        case .image:
            Image(systemName: node.text).resizable().scaledToFit().foregroundColor(.white)
        case .panel, .container:
            Absolute(node: node)
        case .vstack:
            VStack(alignment: .leading, spacing: node.spacing) { ForEach(node.children) { NodeView(node: $0) } }
        case .hstack:
            HStack(spacing: node.spacing) { ForEach(node.children) { NodeView(node: $0) } }
        case .scroll:
            ScrollView { VStack(alignment: .leading, spacing: 8) { ForEach(node.children) { NodeView(node: $0) } } }
        case .field:
            TextField(node.titles.first ?? "", text: Binding(get: { node.text }, set: { s in
                node.text = s
                send("TEXT", [.int(Int(node.id))])
            }))
        case .gauge:
            Gauge(value: node.value, in: 0...100) { Text(node.text) }
                currentValueLabel: { Text("\(Int(node.value))") }
                .gaugeStyle(.accessoryCircular)
        case .date:
            DatePicker(node.text, selection: Binding(get: { node.date }, set: { d in
                node.date = d
                send("DATE", [.int(Int(node.id)), .double(d.timeIntervalSince1970)])
            }), displayedComponents: .date)
        case .crown:
            CrownView(node: node)
        case .nav:
            NavView(node: node)
        case .tabs:
            TabView(selection: Binding(get: { Int(node.value) }, set: { i in
                node.value = Double(i)
                send("SELECT", [.int(Int(node.id)), .int(i)])
            })) {
                ForEach(node.children.indices, id: \.self) { i in
                    Absolute(node: node.children[i]).tag(i)
                        .navigationTitle(i < node.titles.count ? node.titles[i] : "")
                }
            }
            .tabViewStyle(.verticalPage)
        }
    }
}

/// The Digital Crown, turning a value from 0 to 100: the readout follows it, and each change is a CROWN event.
struct CrownView: View {
    @ObservedObject var node: Node
    @State private var crown = 0.0
    var body: some View {
        Text("\(node.text) \(Int(node.value))")
            .font(.system(size: 14)).foregroundColor(.white)
            .frame(maxWidth: .infinity, maxHeight: .infinity)
            .focusable(true)
            .digitalCrownRotation($crown, from: 0, through: 100, by: 1, sensitivity: .medium,
                                  isContinuous: false, isHapticFeedbackEnabled: true)
            .onAppear { crown = node.value }
            .onChange(of: crown) { _, v in
                node.value = v
                send("CROWN", [.int(Int(node.id)), .double(v)])
            }
    }
}

/// A navigation stack: the first child is the root page, and each later child a page pushed on it.
struct NavView: View {
    @ObservedObject var node: Node
    var body: some View {
        NavigationStack(path: Binding(get: { node.path }, set: { node.path = $0 })) {
            Group {
                if let root = node.children.first { Absolute(node: root) } else { Color.clear }
            }
            .navigationTitle(node.titles.first ?? "")
            .navigationDestination(for: Int.self) { i in
                if i < node.children.count {
                    Absolute(node: node.children[i]).navigationTitle(i < node.titles.count ? node.titles[i] : "")
                }
            }
        }
    }
}

/// Every node at the top of the tree, over the framebuffer; and an alert or a sheet when one is up.
struct NodesLayer: View {
    @ObservedObject var model = nodesModel
    var body: some View {
        ZStack(alignment: .topLeading) {
            ForEach(model.roots) { Placed(node: $0) }
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .topLeading)
        .alert(model.alert?.title ?? "", isPresented: Binding(get: { model.alert != nil },
                                                             set: { if !$0 { model.alert = nil } }),
               presenting: model.alert) { alert in
            ForEach(alert.actions.indices, id: \.self) { i in
                Button(alert.actions[i]) {
                    send("ALERT", [.int(Int(alert.id)), .int(i)])
                    model.alert = nil
                }
            }
        } message: { alert in
            Text(alert.message)
        }
        // the binding is set only when the person closes the sheet (modal_dismiss clears it directly): a DISMISS
        .sheet(item: Binding(get: { model.sheet }, set: { new in
            if new == nil, let old = model.sheet { send("DISMISS", [.int(Int(old.id))]) }
            model.sheet = new
        })) { sheet in
            NodeView(node: sheet)
        }
    }
}

// MARK: - the calls, by the names host/ios/swift/ModusSwift.swift gives them

private func control(_ kind: Kind, _ x: UInt, _ y: UInt, _ w: UInt, _ h: UInt) -> UInt {
    make(kind, rect(x, y, w, h)).id
}

let labelNew: Call = { p, n, x, y, w, h in
    let node = make(.label, rect(x, y, w, h))
    node.text = text(p, n)
    return node.id
}

let buttonNew: Call = { p, n, x, y, w, h in
    let node = make(.button, rect(x, y, w, h))
    node.text = text(p, n)
    return node.id
}

let switchNew: Call = { on, x, y, _, _, _ in
    let node = make(.toggle, rect(x, y, 0, 0))
    node.on = on != 0
    return node.id
}

let sliderNew: Call = { value, x, y, w, h, _ in
    let node = make(.slider, rect(x, y, w, h))
    node.value = Double(value)
    return node.id
}

let stepperNew: Call = { value, x, y, _, _, _ in
    let node = make(.stepper, rect(x, y, 0, 0))
    node.value = Double(value)
    return node.id
}

let progressNew: Call = { percent, x, y, w, h, _ in
    let node = make(.progress, rect(x, y, w, h))
    node.value = Double(min(percent, 100))
    return node.id
}

let spinnerNew: Call = { x, y, w, h, _, _ in
    let node = make(.spinner, rect(x, y, w, h))
    node.on = true
    return node.id
}

let pickerNew: Call = { p, n, x, y, w, h in
    let node = make(.picker, rect(x, y, w, h))
    node.titles = titlesOf(p, n)
    return node.id
}

let textFieldNew: Call = { p, n, x, y, w, h in
    let node = make(.field, rect(x, y, w, h))
    node.titles = [text(p, n)]
    return node.id
}

let panelNew: Call = { x, y, w, h, rgb, _ in
    let node = make(.panel, w == 0 ? CGRect(x: CGFloat(x), y: CGFloat(y), width: 0, height: 0) : rect(x, y, w, h))
    node.color = Color(red: Double((rgb >> 16) & 255) / 255, green: Double((rgb >> 8) & 255) / 255,
                       blue: Double(rgb & 255) / 255)
    return node.id
}

let stackNew: Call = { axis, spacing, x, y, w, h in
    let node = make(axis == 1 ? .hstack : .vstack, rect(x, y, w, h))
    node.spacing = CGFloat(spacing)
    return node.id
}

let scrollNew: Call = { x, y, w, h, _, _ in control(.scroll, x, y, w, h) }

let imageNew: Call = { p, n, x, y, w, h in
    let node = make(.image, rect(x, y, w, h))
    node.text = text(p, n)
    return node.id
}

let listNew: Call = { x, y, w, h, _, _ in control(.list, x, y, w, h) }

let listSetData: Call = { h, p, n, _, _, _ in
    nodes[h]?.titles = rowTitles(p, n)
    return 0
}

let gaugeNew: Call = { value, x, y, w, h, _ in
    let node = make(.gauge, rect(x, y, w, h))
    node.value = Double(min(value, 100))
    return node.id
}

let crownNew: Call = { p, n, x, y, w, h in
    let node = make(.crown, rect(x, y, w, h))
    node.text = text(p, n)
    return node.id
}

let dateNew: Call = { seconds, x, y, _, _, _ in
    let node = make(.date, rect(x, y, 0, 0))
    node.date = Date(timeIntervalSince1970: TimeInterval(seconds))
    return node.id
}

let addChild: Call = { parent, child, _, _, _, _ in
    guard let p = nodes[parent], let c = nodes[child] else { return 0 }
    detach(c)
    c.parent = p
    p.children.append(c)
    return 0
}

let setText: Call = { h, p, n, _, _, _ in
    nodes[h]?.text = text(p, n)
    return 0
}

let setValue: Call = { h, value, _, _, _, _ in
    guard let node = nodes[h] else { return 0 }
    switch node.kind {
    case .toggle, .spinner: node.on = value != 0
    case .date: node.date = Date(timeIntervalSince1970: TimeInterval(value))
    default: node.value = Double(value)
    }
    return 0
}

let textValue: Call = { h, buf, cap, _, _, _ in
    let bytes = Array((nodes[h]?.text ?? "").utf8)
    if !bytes.isEmpty, bytes.count <= Int(cap), let out = UnsafeMutableRawPointer(bitPattern: buf) {
        bytes.withUnsafeBytes { out.copyMemory(from: $0.baseAddress!, byteCount: bytes.count) }
    }
    return UInt(bytes.count)
}

let setFrame: Call = { h, x, y, w, hh, _ in
    nodes[h]?.frame = rect(x, y, w, hh)
    return 0
}

let removeView: Call = { h, _, _, _, _, _ in
    if let node = nodes[h] { detach(node) }
    nodes[h] = nil
    return 0
}

let sendTap: Call = { h, _, _, _, _, _ in
    if nodes[h]?.kind == .button { send("TAP", [.int(Int(h))]) }
    return 0
}

/// The watch has no keyboard to give up: nothing to do.
let resign: Call = { _, _, _, _, _, _ in 0 }

let alertNew: Call = { tp, tn, mp, mn, _, _ in
    let h = handle()
    alerts[h] = AlertModel(id: h, title: text(tp, tn), message: text(mp, mn), actions: [])
    return h
}

let alertAction: Call = { h, p, n, _, _, _ in
    alerts[h]?.actions.append(text(p, n))
    return 0
}

let alertPresent: Call = { h, _, _, _, _, _ in
    guard let alert = alerts[h] else { return 0 }
    nodesModel.alert = alert
    return 1
}

/// A sheet's content (ui_modal): a container, shown by modal_present.
let modalNew: Call = { _, _, _, _, _, _ in loose(.container).id }

let modalPresent: Call = { h, _, _, _, _, _ in
    nodesModel.sheet = nodes[h]
    return 1
}

let modalDismiss: Call = { h, _, _, _, _, _ in
    if nodesModel.sheet?.id == h { nodesModel.sheet = nil }
    return 0
}

let navControllerNew: Call = { _, _, _, _, _, _ in
    let nav = loose(.nav)
    let root = loose(.container)
    root.parent = nav
    nav.children = [root]
    nav.titles = ["start"]
    return nav.id
}

let navRoot: Call = { h, _, _, _, _, _ in nodes[h]?.children.first?.id ?? 0 }

let navPushView: Call = { h, p, n, _, _, _ in
    guard let nav = nodes[h] else { return 0 }
    let page = loose(.container)
    page.parent = nav
    nav.children.append(page)
    nav.titles.append(text(p, n))
    nav.path.append(nav.children.count - 1)
    return page.id
}

let tabControllerNew: Call = { p, n, _, _, _, _ in
    let tabs = loose(.tabs)
    tabs.titles = titlesOf(p, n)
    tabs.children = tabs.titles.map { _ in
        let page = loose(.container)
        page.parent = tabs
        return page
    }
    return tabs.id
}

let tabContent: Call = { h, i, _, _, _, _ in
    guard let tabs = nodes[h], Int(i) < tabs.children.count else { return 0 }
    return tabs.children[Int(i)].id
}

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

@_cdecl("modus_swift_install")
public func modusSwiftInstall() {
    register("ui_label_new", labelNew)
    register("ui_button_new", buttonNew)
    register("ui_switch_new", switchNew)
    register("ui_slider_new", sliderNew)
    register("ui_stepper_new", stepperNew)
    register("ui_progress_new", progressNew)
    register("ui_spinner_new", spinnerNew)
    register("ui_picker_new", pickerNew)
    register("ui_segmented_new", pickerNew)          // a watch has no segmented control: a wheel of the same choices
    register("ui_text_field_new", textFieldNew)
    register("ui_panel_new", panelNew)
    register("ui_stack_new", stackNew)
    register("ui_scroll_new", scrollNew)
    register("ui_image_new", imageNew)
    register("ui_list_new", listNew)
    register("ui_list_set_data", listSetData)
    register("ui_gauge_new", gaugeNew)
    register("ui_crown_new", crownNew)
    register("ui_date_new", dateNew)
    register("ui_add", addChild)
    register("ui_set_text", setText)
    register("ui_set_value", setValue)
    register("ui_text_value", textValue)
    register("ui_set_frame", setFrame)
    register("ui_remove", removeView)
    register("ui_send_tap", sendTap)
    register("ui_resign", resign)
    register("ui_alert_new", alertNew)
    register("ui_alert_action", alertAction)
    register("ui_alert_present", alertPresent)
    register("ui_modal_new", modalNew)
    register("ui_modal_present", modalPresent)
    register("ui_modal_dismiss", modalDismiss)
    register("ui_nav_controller_new", navControllerNew)
    register("ui_nav_root", navRoot)
    register("ui_nav_push_view", navPushView)
    register("ui_tab_controller_new", tabControllerNew)
    register("ui_tab_content", tabContent)
    register("ui_events_next", eventsNext)
}
