// Heart.swift — the heart rate on a watch, for Lisp: read through a workout session (HealthKit), which is also what
// lets a watch app keep running with the wrist down.  The workout is never saved: stopping discards it.
// Needs the HealthKit entitlement, NSHealthShareUsageDescription and NSHealthUpdateUsageDescription, and the
// workout-processing background mode (host/watch/build-watch.sh writes them for a companion app).
//
//   heart_start()     ask for the heart rate (the first time, the person is asked) and start the workout
//   heart_rate()      beats a minute, the latest reading; 0 until there is one
//   heart_state()     0 off, 1 starting (asking), 2 running, 3 refused or not here
//   heart_stop()      end the workout, discarded

import HealthKit

private let store = HKHealthStore()
private var workout: HKWorkoutSession?
private var builder: HKLiveWorkoutBuilder?
private var beats = 0.0
private var heartStateNow = 0
private let heartDelegate = HeartDelegate()

final class HeartDelegate: NSObject, HKWorkoutSessionDelegate, HKLiveWorkoutBuilderDelegate {
    func workoutSession(_ s: HKWorkoutSession, didChangeTo to: HKWorkoutSessionState, from: HKWorkoutSessionState,
                        date: Date) {}
    func workoutSession(_ s: HKWorkoutSession, didFailWithError error: Error) {
        NSLog("modus heart: the workout failed: \(error)")
        DispatchQueue.main.async { heartStateNow = 3 }
    }
    func workoutBuilderDidCollectEvent(_ b: HKLiveWorkoutBuilder) {}
    func workoutBuilder(_ b: HKLiveWorkoutBuilder, didCollectDataOf types: Set<HKSampleType>) {
        let rate = HKQuantityType(.heartRate)
        guard types.contains(rate), let q = b.statistics(for: rate)?.mostRecentQuantity() else { return }
        let v = q.doubleValue(for: HKUnit.count().unitDivided(by: .minute()))
        DispatchQueue.main.async { beats = v }
    }
}

private func begin() {
    let config = HKWorkoutConfiguration()
    config.activityType = .other
    config.locationType = .indoor
    do {
        let s = try HKWorkoutSession(healthStore: store, configuration: config)
        let b = s.associatedWorkoutBuilder()
        b.dataSource = HKLiveWorkoutDataSource(healthStore: store, workoutConfiguration: config)
        s.delegate = heartDelegate
        b.delegate = heartDelegate
        workout = s
        builder = b
        let now = Date()
        s.startActivity(with: now)
        b.beginCollection(withStart: now) { ok, error in
            if let error { NSLog("modus heart: collection: \(error)") }
            DispatchQueue.main.async { heartStateNow = ok ? 2 : 3 }
        }
    } catch {
        NSLog("modus heart: no workout: \(error)")
        heartStateNow = 3
    }
}

let heartStart: Call = { _, _, _, _, _, _ in
    guard HKHealthStore.isHealthDataAvailable() else { heartStateNow = 3; return 0 }
    if workout != nil || heartStateNow == 1 { return 1 }
    heartStateNow = 1
    store.requestAuthorization(toShare: [HKObjectType.workoutType()], read: [HKQuantityType(.heartRate)]) { ok, error in
        if let error { NSLog("modus heart: authorization: \(error)") }
        DispatchQueue.main.async { if ok { begin() } else { heartStateNow = 3 } }
    }
    return 1
}

let heartRate: Call = { _, _, _, _, _, _ in UInt(max(0, beats.rounded())) }

let heartState: Call = { _, _, _, _, _, _ in UInt(heartStateNow) }

let heartStop: Call = { _, _, _, _, _, _ in
    let b = builder
    workout?.end()
    b?.endCollection(withEnd: Date()) { _, _ in b?.discardWorkout() }
    workout = nil
    builder = nil
    beats = 0
    heartStateNow = 0
    return 1
}

func installHeart() {
    register("heart_start", heartStart)
    register("heart_rate", heartRate)
    register("heart_state", heartState)
    register("heart_stop", heartStop)
}
