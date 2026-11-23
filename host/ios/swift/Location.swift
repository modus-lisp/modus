#if !os(tvOS)   // a TV has no location updates
// Location.swift — where the device is, for Lisp: CoreLocation, the same on the Mac and the phone.  Lisp's side is
// vessel's location.lisp.  POLLED, as the rest: ask, start, then read the latest fix when it suits.
//
//   location_permission()        0 not asked, 1 allowed, 2 refused (or Location Services off)
//   location_ask()               the system's question (once); a LOCATIONASK event (0 1|0) says how it was answered
//   location_start()             fixes begin (one is enough for most: it keeps the latest)
//   location_stop()
//   location_get(four)           the latest fix into four single-floats: latitude, longitude (degrees), its accuracy
//                                and altitude (metres); 1 if there is one, else 0

import Foundation
import CoreLocation

private final class Locator: NSObject, CLLocationManagerDelegate {
    let manager = CLLocationManager()
    var latest: CLLocation?
    var asking = false

    override init() {
        super.init()
        manager.delegate = self
        manager.desiredAccuracy = kCLLocationAccuracyHundredMeters
    }

    var permission: UInt {
        switch manager.authorizationStatus {
        case .notDetermined: return 0
        case .denied, .restricted: return 2
        default: return CLLocationManager.locationServicesEnabled() ? 1 : 2
        }
    }

    func locationManager(_ m: CLLocationManager, didUpdateLocations locations: [CLLocation]) {
        if let last = locations.last { latest = last }
    }
    func locationManager(_ m: CLLocationManager, didFailWithError error: Error) {
        NSLog("modus location: \(error)")
    }
    func locationManagerDidChangeAuthorization(_ m: CLLocationManager) {
        guard asking, m.authorizationStatus != .notDetermined else { return }
        asking = false
        send("LOCATIONASK", [.int(0), .int(permission == 1 ? 1 : 0)])
    }
}

private var locatorMade: Locator?
private func locator() -> Locator {           // made when first used, not at install (as Notify.swift's center)
    if let l = locatorMade { return l }
    let l = Locator()
    locatorMade = l
    return l
}

let locationPermission: Call = { _, _, _, _, _, _ in locator().permission }

let locationAsk: Call = { _, _, _, _, _, _ in
    let l = locator()
    if l.permission != 0 {                       // answered before: say so at once
        send("LOCATIONASK", [.int(0), .int(l.permission == 1 ? 1 : 0)])
        return 1
    }
    l.asking = true
    #if os(macOS)
    l.manager.requestAlwaysAuthorization()      // a Mac's one question
    #else
    l.manager.requestWhenInUseAuthorization()
    #endif
    return 1
}

let locationStart: Call = { _, _, _, _, _, _ in locator().manager.startUpdatingLocation(); return 1 }
let locationStop: Call = { _, _, _, _, _, _ in locator().manager.stopUpdatingLocation(); return 1 }

let locationGet: Call = { p, _, _, _, _, _ in
    guard let f = UnsafeMutablePointer<Float>(bitPattern: p), let fix = locator().latest else { return 0 }
    f[0] = Float(fix.coordinate.latitude)
    f[1] = Float(fix.coordinate.longitude)
    f[2] = Float(fix.horizontalAccuracy)
    f[3] = Float(fix.altitude)
    return 1
}

func installLocation() {
    register("location_permission", locationPermission)
    register("location_ask", locationAsk)
    register("location_start", locationStart)
    register("location_stop", locationStop)
    register("location_get", locationGet)
}
#endif
