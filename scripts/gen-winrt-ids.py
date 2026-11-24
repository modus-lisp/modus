#!/usr/bin/env python3
"""gen-winrt-ids.py - the WinRT interface IDs and vtable slots net/win32-ble.lisp calls.

Usage: gen-winrt-ids.py WINMD_DIR > net/win32-winrt-ids.lisp

WINMD_DIR holds Windows.Devices.winmd, Windows.Foundation.winmd and
Windows.Storage.winmd (C:\\Windows\\System32\\WinMetadata on any Windows 10/11 or
Server 2019+; Microsoft's files, so not in this repository).  Needs dnfile (pip).

For every interface in INTERFACES it writes the IID and one constant per method,
its vtable slot: IInspectable's six methods come first, then the interface's in
metadata order -- which is the ABI order.  For every delegate in DELEGATES (a
TypedEventHandler<Sender, Args> instantiation) it computes the parameterized IID
by WinRT's rule (a SHA-1 over a signature string, as a name-based UUID); the rule
is checked first against IVector<String>'s published IID.

IIDs are written as the 16 bytes a GUID occupies in memory, as a hex string.
"""
import hashlib, os, struct, sys, uuid
import dnfile

INTERFACES = [
    "Windows.Foundation.IAsyncInfo",
    "Windows.Foundation.IAsyncOperation`1",
    "Windows.Foundation.IClosable",
    "Windows.Foundation.IDeferral",
    "Windows.Foundation.Collections.IVector`1",
    "Windows.Foundation.Collections.IVectorView`1",
    "Windows.Storage.Streams.IBuffer",
    "Windows.Storage.Streams.IBufferFactory",
    "Windows.Devices.Bluetooth.IBluetoothAdapterStatics",
    "Windows.Devices.Bluetooth.IBluetoothAdapter",
    "Windows.Devices.Bluetooth.IBluetoothLEDeviceStatics",
    "Windows.Devices.Bluetooth.IBluetoothLEDevice",
    "Windows.Devices.Bluetooth.IBluetoothLEDevice3",
    "Windows.Devices.Bluetooth.IBluetoothLEDevice4",
    "Windows.Devices.Bluetooth.IBluetoothDeviceId",
    "Windows.Devices.Radios.IRadio",
    "Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementWatcher",
    "Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementFilter",
    "Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisement",
    "Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementReceivedEventArgs",
    "Windows.Devices.Bluetooth.GenericAttributeProfile.IGattDeviceServicesResult",
    "Windows.Devices.Bluetooth.GenericAttributeProfile.IGattDeviceService3",
    "Windows.Devices.Bluetooth.GenericAttributeProfile.IGattCharacteristicsResult",
    "Windows.Devices.Bluetooth.GenericAttributeProfile.IGattCharacteristic",
    "Windows.Devices.Bluetooth.GenericAttributeProfile.IGattValueChangedEventArgs",
    "Windows.Devices.Bluetooth.GenericAttributeProfile.IGattSession",
    "Windows.Devices.Bluetooth.GenericAttributeProfile.IGattSessionStatics",
    "Windows.Devices.Bluetooth.GenericAttributeProfile.IGattServiceProviderStatics",
    "Windows.Devices.Bluetooth.GenericAttributeProfile.IGattServiceProviderResult",
    "Windows.Devices.Bluetooth.GenericAttributeProfile.IGattServiceProvider",
    "Windows.Devices.Bluetooth.GenericAttributeProfile.IGattServiceProviderAdvertisingParameters",
    "Windows.Devices.Bluetooth.GenericAttributeProfile.IGattLocalService",
    "Windows.Devices.Bluetooth.GenericAttributeProfile.IGattLocalCharacteristicParameters",
    "Windows.Devices.Bluetooth.GenericAttributeProfile.IGattLocalCharacteristicResult",
    "Windows.Devices.Bluetooth.GenericAttributeProfile.IGattLocalCharacteristic",
    "Windows.Devices.Bluetooth.GenericAttributeProfile.IGattWriteRequestedEventArgs",
    "Windows.Devices.Bluetooth.GenericAttributeProfile.IGattWriteRequest",
    "Windows.Devices.Bluetooth.GenericAttributeProfile.IGattSubscribedClient",
]

ADV = "Windows.Devices.Bluetooth.Advertisement."
GATT = "Windows.Devices.Bluetooth.GenericAttributeProfile."
DELEGATES = [  # (lisp name, sender runtime class, args runtime class or "Object")
    ("adv-received", ADV + "BluetoothLEAdvertisementWatcher", ADV + "BluetoothLEAdvertisementReceivedEventArgs"),
    ("value-changed", GATT + "GattCharacteristic", GATT + "GattValueChangedEventArgs"),
    ("connection-changed", "Windows.Devices.Bluetooth.BluetoothLEDevice", "Object"),
    ("subscribers-changed", GATT + "GattLocalCharacteristic", "Object"),
    ("write-requested", GATT + "GattLocalCharacteristic", GATT + "GattWriteRequestedEventArgs"),
]
TYPED_EVENT_HANDLER = "9de1c534-6ae1-11e0-84e1-18a905bcc53f"
PINTERFACE_NS = uuid.UUID("11f47ad5-7b73-42c0-abae-878b1e16adee")


def pinterface_iid(signature):
    """WinRT's parameterized IID: a name-based (version 5) UUID of SIGNATURE in WinRT's namespace."""
    h = hashlib.sha1(PINTERFACE_NS.bytes + signature.encode("utf-8")).digest()
    b = bytearray(h[:16])
    b[6] = (b[6] & 0x0F) | 0x50
    b[8] = (b[8] & 0x3F) | 0x80
    return uuid.UUID(bytes=bytes(b))


class Winmd:
    def __init__(self, paths):
        self.types = {}          # full name -> (pe, TypeDef row)
        for p in paths:
            pe = dnfile.dnPE(p)
            for td in pe.net.mdtables.TypeDef.rows:
                self.types[f"{td.TypeNamespace}.{td.TypeName}"] = (pe, td)

    def guid(self, name):
        pe, td = self.types[name]
        for ca in pe.net.mdtables.CustomAttribute.rows:
            if ca.Parent.row is td and self._attr_name(ca) == "GuidAttribute":
                b = bytes(ca.Value.value)
                d1, d2, d3 = struct.unpack_from("<IHH", b, 2)
                return uuid.UUID(fields=(d1, d2, d3, b[10], b[11], int.from_bytes(b[12:18], "big")))
        raise KeyError(f"{name} has no GuidAttribute")

    @staticmethod
    def _attr_name(ca):
        try:
            return ca.Type.row.Class.row.TypeName
        except Exception:
            return None

    def methods(self, name):
        """The methods in ABI order, each by its ABI name: an [Overload("X")] method is X."""
        pe, td = self.types[name]
        md = pe.net.mdtables
        out = []
        for m in td.MethodList:
            n = str(m.row.Name)
            for ca in md.CustomAttribute.rows:
                if ca.Parent.row is m.row and self._attr_name(ca) == "OverloadAttribute":
                    b = bytes(ca.Value.value)
                    k = b[2]                      # a SerString: one length byte here, then UTF-8
                    n = b[3:3 + k].decode("utf-8")
            out.append(n)
        return out

    def default_interface(self, cls):
        """The [default] interface a runtime class implements, by full name."""
        pe, td = self.types[cls]
        md = pe.net.mdtables
        for ii in md.InterfaceImpl.rows:
            if ii.Class.row is not td:
                continue
            for ca in md.CustomAttribute.rows:
                if ca.Parent.row is ii and self._attr_name(ca) == "DefaultAttribute":
                    t = ii.Interface.row
                    return f"{t.TypeNamespace}.{t.TypeName}"
        raise KeyError(f"{cls}: no default interface")

    def signature(self, t):
        if t == "Object":
            return "cinterface(IInspectable)"
        if t in self.types and self.types[t][1].Flags.tdInterface:
            return "{" + str(self.guid(t)) + "}"
        return f"rc({t};{self.signature(self.default_interface(t))})"


def lisp_name(full):
    n = full.split(".")[-1].replace("`1", "")
    return n.lower()


def guid_hex(g):
    return g.bytes_le.hex()


def main():
    d = sys.argv[1]
    wm = Winmd([os.path.join(d, f) for f in
                ("Windows.Foundation.winmd", "Windows.Devices.winmd", "Windows.Storage.winmd")])
    check = pinterface_iid("pinterface({913337e9-11a1-4345-a3a2-4e7f956e222d};string)")
    if str(check) != "98b9acc1-4b56-532e-ac73-03d5291cca90":
        sys.exit(f"pinterface IID rule is wrong: IVector<String> came out {check}")
    out = sys.stdout
    out.write(";;;; net/win32-winrt-ids.lisp -- GENERATED by scripts/gen-winrt-ids.py from Windows' own\n")
    out.write(";;;; metadata (build 20348's WinMetadata).  Do not edit: regenerate.\n")
    out.write(";;;; +WRT-IID-name+ is the interface's IID as the 16 bytes it occupies in memory (hex);\n")
    out.write(";;;; +WRT-name-method+ is a method's vtable slot.  Delegate IIDs are WinRT's\n")
    out.write(";;;; parameterized ones, checked against IVector<String>'s published IID.\n\n")
    for name in INTERFACES:
        n = lisp_name(name)
        if "`" not in name:
            out.write(f"(defconstant +wrt-iid-{n}+ \"{guid_hex(wm.guid(name))}\")   ; {wm.guid(name)}\n")
        ms = wm.methods(name)
        if len(set(m.lower() for m in ms)) != len(ms):
            sys.exit(f"{name}: two methods with one ABI name: {ms}")
        for k, m in enumerate(ms):
            out.write(f"(defconstant +wrt-{n}-{m.lower()}+ {6 + k})\n")
        out.write("\n")
    for lname, sender, args in DELEGATES:
        sig = f"pinterface({{{TYPED_EVENT_HANDLER}}};{wm.signature(sender)};{wm.signature(args)})"
        g = pinterface_iid(sig)
        out.write(f";; {sig}\n(defconstant +wrt-iid-handler-{lname}+ \"{guid_hex(g)}\")   ; {g}\n")


if __name__ == "__main__":
    main()
