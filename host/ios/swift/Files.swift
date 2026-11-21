// Files.swift — the phone's files for the image: the app's Documents folder (what the Files app shows as
// On My iPhone › the app, since Info.plist has UIFileSharingEnabled), and the system's document picker, which
// reaches iCloud Drive and every other file provider.
//
//   files_documents(buf, cap)  the Documents folder's path, UTF-8, into buf; returns its length (copied only
//                              when it fits in cap)
//   files_pick(dir, length)    the document picker, over the app: what is chosen is copied into the folder at
//                              dir (iCloud Drive downloads it first).  Returns 1 when the picker came up.  The
//                              image is not called back: the copies land in the folder, which the media
//                              library re-reads as it draws.
//   files_copy(src, n, dst, m) the file or folder at src copied to dst, when nothing is there yet: the
//                              bundle's sample media seeded into Documents.  Returns 1 if it copied.

#if os(macOS)
import AppKit          // the macOS window host builds this file too (host/macos/build-macos.sh gui)
#else
import UIKit
#endif
import UniformTypeIdentifiers

private func filesCopyOut(_ s: String, _ buf: UInt, _ cap: UInt) -> UInt {
    let bytes = Array(s.utf8)
    if !bytes.isEmpty, bytes.count <= Int(cap), let out = UnsafeMutableRawPointer(bitPattern: buf) {
        bytes.withUnsafeBytes { out.copyMemory(from: $0.baseAddress!, byteCount: bytes.count) }
    }
    return UInt(bytes.count)
}

// On a Mac the app has no Documents of its own: ~/Documents is the person's, and the media library seeds its
// samples into this folder.  So the app's own folder, under Application Support.
private func documentsURL() -> URL {
    #if os(macOS)
    let support = FileManager.default.urls(for: .applicationSupportDirectory, in: .userDomainMask)[0]
    let url = support.appendingPathComponent("org.modus-lisp.modus/Documents", isDirectory: true)
    try? FileManager.default.createDirectory(at: url, withIntermediateDirectories: true)
    return url
    #else
    FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0]
    #endif
}

// A name not yet taken in DIR: "song.mp3", then "song 2.mp3", "song 3.mp3", ...
private func freeName(_ name: String, in dir: URL) -> URL {
    let fm = FileManager.default
    var url = dir.appendingPathComponent(name)
    let stem = (name as NSString).deletingPathExtension
    let ext = (name as NSString).pathExtension
    var n = 2
    while fm.fileExists(atPath: url.path) {
        url = dir.appendingPathComponent(ext.isEmpty ? "\(stem) \(n)" : "\(stem) \(n).\(ext)")
        n += 1
    }
    return url
}

let filesDocuments: Call = { buf, cap, _, _, _, _ in filesCopyOut(documentsURL().path, buf, cap) }

#if os(macOS)
// The Mac's picker: an Open panel over the window.  What is chosen is COPIED into the folder (the files are the
// person's own, not the picker's copies as on the phone).
let filesPick: Call = { p, n, _, _, _, _ in
    guard let root = rootView(), let window = root.window else { return 0 }
    let into = n > 0 ? URL(fileURLWithPath: text(p, n), isDirectory: true) : documentsURL()
    let panel = NSOpenPanel()
    panel.allowedContentTypes = [.audio, .movie, .image, .pdf]
    panel.allowsMultipleSelection = true
    panel.beginSheetModal(for: window) { response in
        guard response == .OK else { return }
        let fm = FileManager.default
        try? fm.createDirectory(at: into, withIntermediateDirectories: true)
        for url in panel.urls {
            do { try fm.copyItem(at: url, to: freeName(url.lastPathComponent, in: into)) } catch {
                NSLog("files_pick: %@: %@", url.path, "\(error)")
            }
        }
    }
    return 1
}
#else
private final class Picked: NSObject, UIDocumentPickerDelegate {
    let into: URL
    init(into: URL) { self.into = into }

    func documentPicker(_ controller: UIDocumentPickerViewController, didPickDocumentsAt urls: [URL]) {
        let fm = FileManager.default
        try? fm.createDirectory(at: into, withIntermediateDirectories: true)
        for url in urls {
            // asCopy: each URL is the picker's own copy in our tmp/…-Inbox, ours to move
            let scoped = url.startAccessingSecurityScopedResource()
            defer { if scoped { url.stopAccessingSecurityScopedResource() } }
            let to = freeName(url.lastPathComponent, in: into)
            do { try fm.moveItem(at: url, to: to) } catch {
                do { try fm.copyItem(at: url, to: to) } catch { NSLog("files_pick: %@: %@", url.path, "\(error)") }
            }
        }
        picking = nil
    }

    func documentPickerWasCancelled(_ controller: UIDocumentPickerViewController) { picking = nil }
}

private var picking: Picked?       // the delegate, held while the picker is up (the picker's reference is weak)

let filesPick: Call = { p, n, _, _, _, _ in
    let windows = UIApplication.shared.connectedScenes.compactMap { $0 as? UIWindowScene }.flatMap { $0.windows }
    guard var top = (windows.first { $0.isKeyWindow } ?? windows.first)?.rootViewController else { return 0 }
    while let shown = top.presentedViewController { top = shown }
    if picking != nil { return 0 }                    // one picker at a time
    let into = n > 0 ? URL(fileURLWithPath: text(p, n), isDirectory: true) : documentsURL()
    let types: [UTType] = [.audio, .movie, .image, .pdf]
    let picker = UIDocumentPickerViewController(forOpeningContentTypes: types, asCopy: true)
    picker.allowsMultipleSelection = true
    let delegate = Picked(into: into)
    picking = delegate
    picker.delegate = delegate
    top.present(picker, animated: true)
    return 1
}
#endif

let filesCopy: Call = { sp, sn, dp, dn, _, _ in
    let fm = FileManager.default
    let dst = text(dp, dn)
    if fm.fileExists(atPath: dst) { return 0 }
    do { try fm.copyItem(atPath: text(sp, sn), toPath: dst); return 1 } catch {
        NSLog("files_copy: %@", "\(error)"); return 0
    }
}

func installFiles() {
    register("files_copy", filesCopy)
    register("files_documents", filesDocuments)
    register("files_pick", filesPick)
}
