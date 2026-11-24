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
//   files_choose_folder(k, n)  a folder chooser: a handle, and a FOLDER event from it, (h path), when a folder is
//                              chosen -- the folder itself, opened where it is, not copied.  On a Mac, the Open
//                              panel over the window.  On the phone, the document picker for folders (iCloud
//                              Drive and every other provider), and the choice is KEPT under the key k (n bytes;
//                              none: not kept) as a security-scoped bookmark, for files_folder_path to reopen.
//   files_folder_path(k, n, buf, cap)  the phone: the folder kept under key k, opened for this run (its access
//                              begun, never ended), its path into buf as files_documents does -- 0 when nothing
//                              is kept or it can no longer be reached.  0 on a Mac.

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
    #elseif os(tvOS)
    // a TV's Documents cannot be written: the shim moved HOME to Library/Caches and made Documents there
    URL(fileURLWithPath: ProcessInfo.processInfo.environment["HOME"] ?? NSTemporaryDirectory(), isDirectory: true)
        .appendingPathComponent("Documents", isDirectory: true)
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

let filesChooseFolder: Call = { _, _, _, _, _, _ in
    guard let root = rootView(), let window = root.window else { return 0 }
    let handle = nextHandle
    nextHandle += 1
    let panel = NSOpenPanel()
    panel.canChooseDirectories = true
    panel.canChooseFiles = false
    panel.allowsMultipleSelection = false
    panel.prompt = "Open"
    panel.beginSheetModal(for: window) { response in
        guard response == .OK, let url = panel.url else { return }
        send("FOLDER", [.int(Int(handle)), .string(url.path)])
    }
    return handle
}
#elseif os(iOS)
// A folder of the person's, chosen once and kept: the document picker for folders hands back a URL that is the
// app's only while it is "accessing" it, so what is kept is its security-scoped bookmark (UserDefaults, under
// the caller's key), and FILES_FOLDER_PATH resolves it again on a later run.  Writing into it is then ordinary
// file I/O -- the image's own open and write -- for as long as the app runs.
private final class FolderPicked: NSObject, UIDocumentPickerDelegate {
    let handle: UInt
    let key: String
    init(handle: UInt, key: String) { self.handle = handle; self.key = key }

    func documentPicker(_ controller: UIDocumentPickerViewController, didPickDocumentsAt urls: [URL]) {
        folderPicking = nil
        guard let url = urls.first else { return }
        _ = url.startAccessingSecurityScopedResource()          // kept open: the app writes there from now on
        if !key.isEmpty, let mark = try? url.bookmarkData(options: [], includingResourceValuesForKeys: nil,
                                                          relativeTo: nil) {
            UserDefaults.standard.set(mark, forKey: "folder." + key)
        }
        send("FOLDER", [.int(Int(handle)), .string(url.path)])
    }

    func documentPickerWasCancelled(_ controller: UIDocumentPickerViewController) {
        folderPicking = nil
        send("FOLDER", [.int(Int(handle)), .string("")])          // "" : nothing was chosen
    }
}

private var folderPicking: FolderPicked?

let filesChooseFolder: Call = { k, n, _, _, _, _ in
    let windows = UIApplication.shared.connectedScenes.compactMap { $0 as? UIWindowScene }.flatMap { $0.windows }
    guard var top = (windows.first { $0.isKeyWindow } ?? windows.first)?.rootViewController else { return 0 }
    while let shown = top.presentedViewController { top = shown }
    if folderPicking != nil || picking != nil { return 0 }
    let handle = nextHandle
    nextHandle += 1
    let picker = UIDocumentPickerViewController(forOpeningContentTypes: [.folder])
    picker.allowsMultipleSelection = false
    let delegate = FolderPicked(handle: handle, key: n > 0 ? text(k, n) : "")
    folderPicking = delegate
    picker.delegate = delegate
    top.present(picker, animated: true)
    return handle
}

private var foldersOpen: [String: URL] = [:]       // key -> the URL whose access this run has begun

let filesFolderPath: Call = { k, n, buf, cap, _, _ in
    let key = text(k, n)
    if let open = foldersOpen[key] { return filesCopyOut(open.path, buf, cap) }
    guard let mark = UserDefaults.standard.data(forKey: "folder." + key) else { return 0 }
    var stale = false
    guard let url = try? URL(resolvingBookmarkData: mark, options: [], relativeTo: nil, bookmarkDataIsStale: &stale),
          url.startAccessingSecurityScopedResource() else { return 0 }
    if stale, let fresh = try? url.bookmarkData(options: [], includingResourceValuesForKeys: nil, relativeTo: nil) {
        UserDefaults.standard.set(fresh, forKey: "folder." + key)
    }
    foldersOpen[key] = url
    return filesCopyOut(url.path, buf, cap)
}

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
    #if !os(tvOS)   // a TV has no document picker: Documents alone
    register("files_pick", filesPick)
    register("files_choose_folder", filesChooseFolder)
    #endif
    #if os(iOS)
    register("files_folder_path", filesFolderPath)
    #endif
}
