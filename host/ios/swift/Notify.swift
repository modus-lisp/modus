// Notify.swift — the system's notifications, for Lisp: ask to send them, send one (now or in a while), and hear
// when one is clicked.  The same on the Mac and the phone.  Lisp's side is vessel's notify.lisp.
//
//   notify_permission()                0 not asked, 1 allowed, 2 refused
//   notify_ask()                       ask now (the system's own question, once); a NOTIFYASK event (0 1|0)
//                                      says how it was answered
//   notify_post(title, tl, body, bl, seconds)   a notification, now (SECONDS 0) or SECONDS from now; its handle.
//                                      Clicked, a NOTIFIED event (handle)
//   notify_badge(count)                the app icon's number (0 clears it)
//
// Shown while the app is in front, too (as a banner), since a game or a desk is usually in front.

import Foundation
import UserNotifications

private var notifyPermission: UInt = 0

private final class NotifyDelegate: NSObject, UNUserNotificationCenterDelegate {
    func userNotificationCenter(_ center: UNUserNotificationCenter, willPresent notification: UNNotification,
                                withCompletionHandler done: @escaping (UNNotificationPresentationOptions) -> Void) {
        done([.banner, .sound, .list])
    }
    func userNotificationCenter(_ center: UNUserNotificationCenter, didReceive response: UNNotificationResponse,
                                withCompletionHandler done: @escaping () -> Void) {
        let h = Int(response.notification.request.identifier) ?? 0
        DispatchQueue.main.async { send("NOTIFIED", [.int(h)]) }
        done()
    }
}
private let notifyDelegate = NotifyDelegate()

// THE CENTER IS TOUCHED ONLY WHEN NOTIFICATIONS ARE FIRST USED, not when the Swift side is installed: there it raised,
// and every function registered after it was missing.
private var notifyReady = false
private func center() -> UNUserNotificationCenter {
    let c = UNUserNotificationCenter.current()
    if !notifyReady {
        notifyReady = true
        c.delegate = notifyDelegate
    }
    return c
}

let notifyPermissionCall: Call = { _, _, _, _, _, _ in
    // asked synchronously the first time, so the answer is not "not asked" merely because nothing looked yet
    if !notifyReady {
        let done = DispatchSemaphore(value: 0)
        center().getNotificationSettings { s in
            notifyPermission = switch s.authorizationStatus { case .notDetermined: 0; case .denied: 2; default: 1 }
            done.signal()
        }
        _ = done.wait(timeout: .now() + 2)
    }
    return notifyPermission
}

let notifyAsk: Call = { _, _, _, _, _, _ in
    center().requestAuthorization(options: [.alert, .sound, .badge]) { ok, error in
        if let error { NSLog("modus notify: \(error)") }
        DispatchQueue.main.async {
            notifyPermission = ok ? 1 : 2
            send("NOTIFYASK", [.int(0), .int(ok ? 1 : 0)])
        }
    }
    return 1
}

let notifyPost: Call = { tp, tl, bp, bl, seconds, _ in
    let content = UNMutableNotificationContent()
    content.title = text(tp, tl)
    content.body = text(bp, bl)
    content.sound = .default
    let h = nextHandle
    nextHandle += 1
    let trigger = seconds > 0 ? UNTimeIntervalNotificationTrigger(timeInterval: TimeInterval(seconds), repeats: false) : nil
    let request = UNNotificationRequest(identifier: String(h), content: content, trigger: trigger)
    center().add(request) { error in
        if let error { NSLog("modus notify: \(error)") }
    }
    return h
}

let notifyBadge: Call = { count, _, _, _, _, _ in
    center().setBadgeCount(Int(count)) { error in
        if let error { NSLog("modus notify: badge: \(error)") }
    }
    return 1
}

func installNotify() {
    register("notify_permission", notifyPermissionCall)
    register("notify_ask", notifyAsk)
    register("notify_post", notifyPost)
    register("notify_badge", notifyBadge)
}
