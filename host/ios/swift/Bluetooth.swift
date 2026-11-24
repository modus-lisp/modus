#if !os(tvOS)   // a TV cannot advertise as a peripheral
// Bluetooth.swift — Bluetooth LE links for a mesh: this phone both a central (it finds and connects to others
// offering a service) and a peripheral (it offers the service, for others to connect to), each link a number,
// its bytes handed to Lisp as they come.  What the bytes mean is Lisp's (chirp: bitchat's protocol).
//
// One service with one characteristic: a peer writes to it (with or without response), and is told of what
// this side sends by notification.  As a central this side writes to the other's characteristic and subscribes
// to it.  A link opens when the characteristic is ready both ways -- subscribed to, as a central; subscribed
// to by the other, as a peripheral.
//
// Events, as (BLE 0 value), value one of:
//   (STATE on)                       the radio is on (1) or not (0: off, unauthorised, unsupported)
//   (LINK link up max)               a link opened (up 1, with the most one write or notification carries) or
//                                    closed (up 0)
//   (DATA link bytes)                what the link's peer sent: one write or one notification
//   (LOG text)                       what happened, for a log

import CoreBluetooth
import Foundation

private func bleSend(_ items: [Term]) { send("BLE", [.int(0), .list(items)]) }
private func bleLog(_ s: String) { bleSend([.symbol(name: "LOG", package: "KEYWORD"), .string(s)]) }

final class MeshRadio: NSObject, CBCentralManagerDelegate, CBPeripheralDelegate, CBPeripheralManagerDelegate {
    let service: CBUUID
    let characteristic: CBUUID
    var central: CBCentralManager!
    var peripheral: CBPeripheralManager!
    var offered: CBMutableCharacteristic?

    // the links: to a peripheral this side connected to (its characteristic), or from a central that connected
    // to this side
    enum Link { case toPeripheral(CBPeripheral, CBCharacteristic), fromCentral(CBCentral) }
    var links: [UInt: Link] = [:]
    var nextLink: UInt = 1
    var linkOfPeripheral: [UUID: UInt] = [:]
    var linkOfCentral: [UUID: UInt] = [:]
    var connecting: [UUID: CBPeripheral] = [:]        // kept: CoreBluetooth holds a peripheral weakly
    var pending: [(Data, CBCentral)] = []             // notifications waiting for room in the queue
    let maxCentralLinks = 6

    init(service: String, characteristic: String) {
        self.service = CBUUID(string: service)
        self.characteristic = CBUUID(string: characteristic)
        super.init()
        central = CBCentralManager(delegate: self, queue: nil)
        peripheral = CBPeripheralManager(delegate: self, queue: nil)
    }

    func stop() {
        central.stopScan()
        for p in connecting.values { central.cancelPeripheralConnection(p) }
        for case .toPeripheral(let p, _) in links.values { central.cancelPeripheralConnection(p) }
        peripheral.stopAdvertising()
        peripheral.removeAllServices()
        for l in links.keys { linkDown(l) }
    }

    private func linkUp(_ link: UInt, max: Int) {
        bleSend([.symbol(name: "LINK", package: "KEYWORD"), .int(Int(link)), .int(1), .int(max)])
    }
    private func linkDown(_ link: UInt) {
        guard links.removeValue(forKey: link) != nil else { return }
        bleSend([.symbol(name: "LINK", package: "KEYWORD"), .int(Int(link)), .int(0), .int(0)])
    }
    private func data(_ link: UInt, _ d: Data) {
        bleSend([.symbol(name: "DATA", package: "KEYWORD"), .int(Int(link)), .bytes([UInt8](d))])
    }

    /// BYTES to LINK's peer: a write to its characteristic, or a notification to it.  False if there is no
    /// such link, or no room just now (a notification queued then goes when there is).
    func send(_ link: UInt, _ bytes: Data) -> Bool {
        switch links[link] {
        case .toPeripheral(let p, let c):
            let fits = bytes.count <= p.maximumWriteValueLength(for: .withoutResponse)
            p.writeValue(bytes, for: c, type: fits && c.properties.contains(.writeWithoutResponse)
                         ? .withoutResponse : .withResponse)
            return true
        case .fromCentral(let c):
            guard let ch = offered else { return false }
            if !peripheral.updateValue(bytes, for: ch, onSubscribedCentrals: [c]) { pending.append((bytes, c)) }
            return true
        case nil:
            return false
        }
    }

    // ---- the central: finding peers that offer the service ----

    func centralManagerDidUpdateState(_ m: CBCentralManager) {
        bleSend([.symbol(name: "STATE", package: "KEYWORD"), .int(m.state == .poweredOn ? 1 : 0)])
        bleLog("central \(m.state.rawValue)")
        if m.state == .poweredOn {
            m.scanForPeripherals(withServices: [service], options: [CBCentralManagerScanOptionAllowDuplicatesKey: false])
        }
    }

    func centralManager(_ m: CBCentralManager, didDiscover p: CBPeripheral, advertisementData: [String: Any],
                        rssi: NSNumber) {
        guard linkOfPeripheral[p.identifier] == nil, connecting[p.identifier] == nil else { return }
        let ours = links.values.filter { if case .toPeripheral = $0 { return true }; return false }.count
        guard ours + connecting.count < maxCentralLinks else { return }
        bleLog("found \(p.identifier.uuidString.prefix(8)) rssi \(rssi)")
        connecting[p.identifier] = p
        p.delegate = self
        m.connect(p, options: nil)
    }

    func centralManager(_ m: CBCentralManager, didConnect p: CBPeripheral) {
        p.discoverServices([service])
    }

    func centralManager(_ m: CBCentralManager, didFailToConnect p: CBPeripheral, error: Error?) {
        connecting.removeValue(forKey: p.identifier)
        bleLog("connect failed \(error?.localizedDescription ?? "")")
    }

    func centralManager(_ m: CBCentralManager, didDisconnectPeripheral p: CBPeripheral, error: Error?) {
        connecting.removeValue(forKey: p.identifier)
        keep.removeValue(forKey: p.identifier)
        if let l = linkOfPeripheral.removeValue(forKey: p.identifier) { linkDown(l) }
        // scanning again, so it is found when it comes back
        m.stopScan()
        m.scanForPeripherals(withServices: [service], options: [CBCentralManagerScanOptionAllowDuplicatesKey: false])
    }

    func peripheral(_ p: CBPeripheral, didDiscoverServices error: Error?) {
        guard let s = p.services?.first(where: { $0.uuid == service }) else { central.cancelPeripheralConnection(p); return }
        p.discoverCharacteristics([characteristic], for: s)
    }

    func peripheral(_ p: CBPeripheral, didDiscoverCharacteristicsFor s: CBService, error: Error?) {
        guard let c = s.characteristics?.first(where: { $0.uuid == characteristic }) else {
            central.cancelPeripheralConnection(p); return
        }
        p.setNotifyValue(true, for: c)
    }

    func peripheral(_ p: CBPeripheral, didUpdateNotificationStateFor c: CBCharacteristic, error: Error?) {
        guard c.isNotifying, linkOfPeripheral[p.identifier] == nil else { return }
        connecting.removeValue(forKey: p.identifier)
        let l = nextLink; nextLink += 1
        links[l] = .toPeripheral(p, c)
        linkOfPeripheral[p.identifier] = l
        keep[p.identifier] = p
        linkUp(l, max: p.maximumWriteValueLength(for: .withoutResponse))
    }
    private var keep: [UUID: CBPeripheral] = [:]      // a linked peripheral, held (CoreBluetooth does not)

    func peripheral(_ p: CBPeripheral, didUpdateValueFor c: CBCharacteristic, error: Error?) {
        guard let l = linkOfPeripheral[p.identifier], let v = c.value else { return }
        data(l, v)
    }

    // ---- the peripheral: offering the service ----

    func peripheralManagerDidUpdateState(_ m: CBPeripheralManager) {
        bleLog("peripheral \(m.state.rawValue)")
        guard m.state == .poweredOn else { return }
        let ch = CBMutableCharacteristic(type: characteristic,
                                         properties: [.read, .write, .writeWithoutResponse, .notify],
                                         value: nil, permissions: [.readable, .writeable])
        let s = CBMutableService(type: service, primary: true)
        s.characteristics = [ch]
        offered = ch
        m.removeAllServices()
        m.add(s)
    }

    func peripheralManager(_ m: CBPeripheralManager, didAdd s: CBService, error: Error?) {
        if let error { bleLog("service: \(error.localizedDescription)"); return }
        // no local name: as bitchat advertises, the service only
        m.startAdvertising([CBAdvertisementDataServiceUUIDsKey: [service]])
    }

    func peripheralManager(_ m: CBPeripheralManager, central c: CBCentral, didSubscribeTo ch: CBCharacteristic) {
        guard linkOfCentral[c.identifier] == nil else { return }
        let l = nextLink; nextLink += 1
        links[l] = .fromCentral(c)
        linkOfCentral[c.identifier] = l
        linkUp(l, max: c.maximumUpdateValueLength)
    }

    func peripheralManager(_ m: CBPeripheralManager, central c: CBCentral, didUnsubscribeFrom ch: CBCharacteristic) {
        if let l = linkOfCentral.removeValue(forKey: c.identifier) { linkDown(l) }
    }

    func peripheralManager(_ m: CBPeripheralManager, didReceiveWrite requests: [CBATTRequest]) {
        for r in requests {
            guard let v = r.value else { continue }
            // a write from a central that has not subscribed (yet): its link all the same
            let l = linkOfCentral[r.central.identifier] ?? {
                let l = nextLink; nextLink += 1
                links[l] = .fromCentral(r.central)
                linkOfCentral[r.central.identifier] = l
                linkUp(l, max: r.central.maximumUpdateValueLength)
                return l
            }()
            data(l, v)
        }
        if let first = requests.first { m.respond(to: first, withResult: .success) }
    }

    func peripheralManagerIsReady(toUpdateSubscribers m: CBPeripheralManager) {
        guard let ch = offered else { return }
        while let (d, c) = pending.first {
            if !m.updateValue(d, for: ch, onSubscribedCentrals: [c]) { return }
            pending.removeFirst()
        }
    }
}

private var radio: MeshRadio?

/// ble_start(service, characteristic): the radio on, finding and offering the service (UUID strings).
let bleStart: Call = { sp, sn, cp, cn, _, _ in
    radio?.stop()
    radio = MeshRadio(service: text(sp, sn), characteristic: text(cp, cn))
    return 1
}

/// ble_send(link, bytes, length): BYTES to LINK's peer.  1 if it went (or is queued), 0 if there is no link.
let bleSendCall: Call = { link, p, n, _, _, _ in
    guard let radio, n > 0, let base = UnsafeRawPointer(bitPattern: p) else { return 0 }
    return radio.send(link, Data(bytes: base, count: Int(n))) ? 1 : 0
}

/// ble_stop(): every link closed, the radio quiet.
let bleStop: Call = { _, _, _, _, _, _ in
    radio?.stop()
    radio = nil
    return 1
}

func installBluetooth() {
    register("ble_start", bleStart)
    register("ble_send", bleSendCall)
    register("ble_stop", bleStop)
}
#endif
