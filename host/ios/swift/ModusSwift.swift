// ModusSwift.swift — the native components a modus app builds from Lisp: UIKit views made here,
// named by a handle, and the events they send back.  Lisp calls these by name (modus-swift.m,
// pseudo-syscall 1060); each runs on the main thread, where UIKit lives.
//
// Every argument is a word.  Text is a pointer and a length: UTF-8 bytes the image holds, read here
// in place.  A function returns a word: a handle, a value, 1 or 0, or nothing (0).

import UIKit

typealias Call = @convention(c) (UInt, UInt, UInt, UInt, UInt, UInt) -> UInt

private var views: [UInt: UIView] = [:]
private var nextHandle: UInt = 1
// The events, oldest first.  kind 1: a button tapped.  2: a switch changed (value 1 on, 0 off).
// 3: a slider changed (value 0..100).  4: a text field's text changed (read it with ui_text_value).
private var events: [(kind: UInt, handle: UInt, value: UInt)] = []

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
    button.addAction(UIAction { _ in events.append((kind: 1, handle: handle, value: 0)) },
                     for: .touchUpInside)
    return handle
}

// switch_new(on, x, y): a switch; its frame is its own size.
let switchNew: Call = { on, x, y, _, _, _ in
    let sw = UISwitch()
    sw.frame.origin = CGPoint(x: CGFloat(x), y: CGFloat(y))
    sw.isOn = on != 0
    let handle = adopt(sw)
    sw.addAction(UIAction { _ in events.append((kind: 2, handle: handle, value: sw.isOn ? 1 : 0)) },
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
    slider.addAction(UIAction { _ in events.append((kind: 3, handle: handle, value: UInt(slider.value))) },
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
    field.addAction(UIAction { _ in events.append((kind: 4, handle: handle, value: 0)) },
                    for: .editingChanged)
    return handle
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

// set_text(handle, text, length): a label's text, a button's title, or a text field's text.
let setText: Call = { h, p, n, _, _, _ in
    let s = text(p, n)
    if let label = views[h] as? UILabel {
        label.text = s
    } else if let button = views[h] as? UIButton {
        button.setTitle(s, for: .normal)
    } else if let field = views[h] as? UITextField {
        field.text = s
    }
    return 0
}

// set_value(handle, value): a switch's state (0 or 1) or a slider's value (0..100).
let setValue: Call = { h, value, _, _, _, _ in
    if let sw = views[h] as? UISwitch {
        sw.isOn = value != 0
    } else if let slider = views[h] as? UISlider {
        slider.value = Float(value)
    }
    return 0
}

// text_value(handle, buffer, capacity): the text of a text field or label, as UTF-8 at the buffer if
// it fits; the length of the text in bytes either way (0 for no text).
let textValue: Call = { h, buf, cap, _, _, _ in
    let s = (views[h] as? UITextField)?.text ?? (views[h] as? UILabel)?.text ?? ""
    let bytes = Array(s.utf8)
    if !bytes.isEmpty, bytes.count <= Int(cap), let out = UnsafeMutableRawPointer(bitPattern: buf) {
        bytes.withUnsafeBytes { out.copyMemory(from: $0.baseAddress!, byteCount: bytes.count) }
    }
    return UInt(bytes.count)
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
    return 0
}

// send_tap(handle): a tap on a button, sent as the touch itself would send it (the events then
// come back as any tap does).  For tests and for automation; a user's taps need no call.
let sendTap: Call = { h, _, _, _, _, _ in
    (views[h] as? UIControl)?.sendActions(for: .touchUpInside)
    return 0
}

// events_next(buffer): the oldest event, as three words (kind, handle, value) at the buffer; 1 if
// there was one, 0 if not.
let eventsNext: Call = { buf, _, _, _, _, _ in
    guard !events.isEmpty, let out = UnsafeMutableRawPointer(bitPattern: buf) else { return 0 }
    let e = events.removeFirst()
    out.storeBytes(of: e.kind, toByteOffset: 0, as: UInt.self)
    out.storeBytes(of: e.handle, toByteOffset: 8, as: UInt.self)
    out.storeBytes(of: e.value, toByteOffset: 16, as: UInt.self)
    return 1
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
    register("ui_panel_new", panelNew)
    register("ui_set_text", setText)
    register("ui_set_value", setValue)
    register("ui_text_value", textValue)
    register("ui_set_frame", setFrame)
    register("ui_remove", removeView)
    register("ui_send_tap", sendTap)
    register("ui_events_next", eventsNext)
}
