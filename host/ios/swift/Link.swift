// Link.swift — messages between a phone app and its watch app, for Lisp (WatchConnectivity): each a term in the wire
// format (Term.swift), encoded by Lisp, sent while the other side is reachable, and arriving there as an event.
// The same on the phone and on the watch (host/watch/build-watch.sh compiles this file too); the Mac answers that
// nothing is linked.  Lisp's side is vessel's link.lisp.
//
//   link_open()            start the link; the handle its events come from (the same every time); 0 if there is none
//   link_send(p, n)        send the N bytes at P (an encoded term); 1 if sent, 0 if the other side is not reachable
//   link_state()           0 no link (no paired watch, or its app not installed), 1 linked but not reachable now,
//                          2 reachable: both apps running
//
//   events: LINK (h term) -- a message that arrived; LINKSTATE (h state) -- the state changed
//
// A phone app and a watch app are linked when the watch app's bundle id is the phone app's plus a suffix and its
// WKCompanionAppBundleIdentifier names the phone app (kiln ios watch-device --app=NAME).

#if os(iOS) || os(watchOS)
import WatchConnectivity

private var linkHandle: UInt = 0
private let linkDelegate = LinkDelegate()
private var lastSendError = Date.distantPast

private func linkStateNow() -> Int {
    guard WCSession.isSupported() else { return 0 }
    let s = WCSession.default
    guard s.activationState == .activated else { return 0 }
    #if os(iOS)
    guard s.isPaired, s.isWatchAppInstalled else { return 0 }
    #endif
    return s.isReachable ? 2 : 1
}

private func stateChanged() {
    let s = WCSession.default
    #if os(iOS)
    NSLog("modus link: activated %d, paired %d, watch app installed %d, reachable %d",
          s.activationState.rawValue, s.isPaired ? 1 : 0, s.isWatchAppInstalled ? 1 : 0, s.isReachable ? 1 : 0)
    #else
    NSLog("modus link: activated %d, phone app installed %d, reachable %d",
          s.activationState.rawValue, s.isCompanionAppInstalled ? 1 : 0, s.isReachable ? 1 : 0)
    #endif
    send("LINKSTATE", [.int(Int(linkHandle)), .int(linkStateNow())])
}

private func received(_ data: Data) {
    guard let t = try? decodeTerm([UInt8](data)) else { return }
    send("LINK", [.int(Int(linkHandle)), t])
}

// WatchConnectivity calls these on a queue of its own: the events are queued on the main thread, where the calls run
final class LinkDelegate: NSObject, WCSessionDelegate {
    func session(_ session: WCSession, activationDidCompleteWith state: WCSessionActivationState, error: Error?) {
        if let error { NSLog("modus link: \(error)") }
        DispatchQueue.main.async { stateChanged() }
    }
    func sessionReachabilityDidChange(_ session: WCSession) { DispatchQueue.main.async { stateChanged() } }
    func session(_ session: WCSession, didReceiveMessageData data: Data) {
        DispatchQueue.main.async { received(data) }
    }
    func session(_ session: WCSession, didReceiveMessageData data: Data, replyHandler: @escaping (Data) -> Void) {
        replyHandler(Data())
        DispatchQueue.main.async { received(data) }
    }
    #if os(iOS)
    func sessionDidBecomeInactive(_ session: WCSession) {}
    func sessionDidDeactivate(_ session: WCSession) { session.activate() }   // another watch: link with it
    func sessionWatchStateDidChange(_ session: WCSession) { DispatchQueue.main.async { stateChanged() } }
    #endif
}

let linkOpen: Call = { _, _, _, _, _, _ in
    guard WCSession.isSupported() else { return 0 }
    if linkHandle == 0 {
        linkHandle = nextHandle
        nextHandle += 1
        WCSession.default.delegate = linkDelegate
        WCSession.default.activate()
    }
    return linkHandle
}

let linkSend: Call = { p, n, _, _, _, _ in
    guard linkStateNow() == 2, n > 0, let base = UnsafeRawPointer(bitPattern: p) else { return 0 }
    let data = Data(bytes: base, count: Int(n))
    WCSession.default.sendMessageData(data, replyHandler: nil) { error in
        // a burst of failures (the other side just went away) is said once a second, not each time
        DispatchQueue.main.async {
            if Date().timeIntervalSince(lastSendError) > 1 { NSLog("modus link: \(error)") }
            lastSendError = Date()
        }
    }
    return 1
}

let linkState: Call = { _, _, _, _, _, _ in UInt(linkStateNow()) }
#else
// a Mac: no watch to link with
let linkOpen: Call = { _, _, _, _, _, _ in 0 }
let linkSend: Call = { _, _, _, _, _, _ in 0 }
let linkState: Call = { _, _, _, _, _, _ in 0 }
#endif

func installLink() {
    register("link_open", linkOpen)
    register("link_send", linkSend)
    register("link_state", linkState)
}
