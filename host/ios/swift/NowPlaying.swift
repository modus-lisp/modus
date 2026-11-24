// NowPlaying.swift — the system's Now Playing and the media keys, for Lisp: what is playing (title, artist, album,
// length, where it is, artwork) shown where the system shows it (a Mac's Control Center and menu bar, the phone's
// lock screen), and the commands sent back -- a Mac keyboard's play/pause, next and previous keys, headphone
// buttons, Control Center's controls -- as events.  The same on the phone and on the Mac.  Lisp's side is vessel's
// now-playing.lisp.
//
//   np_enable()                 take the commands; the handle their events come from (the same every time)
//   np_text(field, p, n)        0 title, 1 artist, 2 album: the UTF-8 text at P ("" removes it)
//   np_times(dur, pos, playing) length and position in milliseconds; 1 playing, 0 paused (the system moves the
//                               position on while playing); a length of 0 is a live stream (a radio station)
//   np_artwork(p, n)            the image file at path P ("" removes it)
//   np_artwork_data(p, n)       the image whose bytes (JPEG or PNG) are at P, N long: a track's own cover, read out
//                               of the file (0 bytes removes it)
//   np_clear()                  nothing is playing
//
//   events: MEDIA (h "play" | "pause" | "toggle" | "stop" | "next" | "previous"), SEEK (h milliseconds)

import MediaPlayer
#if os(iOS) || os(tvOS)
import AVFoundation
import UIKit
#else
import AppKit
#endif

private var mediaHandle: UInt = 0
private var info: [String: Any] = [:]

private func publish(playing: Bool? = nil) {
    let center = MPNowPlayingInfoCenter.default()
    center.nowPlayingInfo = info.isEmpty ? nil : info
    #if os(macOS)
    // a Mac decides which app the media keys go to by its playback state
    if let playing { center.playbackState = playing ? .playing : .paused }
    #endif
}

private func installCommands() {
    let c = MPRemoteCommandCenter.shared()
    func on(_ command: MPRemoteCommand, _ name: String) {
        command.isEnabled = true
        command.addTarget { _ in
            send("MEDIA", [.int(Int(mediaHandle)), .string(name)])
            return .success
        }
    }
    on(c.playCommand, "play")
    on(c.pauseCommand, "pause")
    on(c.togglePlayPauseCommand, "toggle")
    on(c.stopCommand, "stop")
    on(c.nextTrackCommand, "next")
    on(c.previousTrackCommand, "previous")
    c.changePlaybackPositionCommand.isEnabled = true
    c.changePlaybackPositionCommand.addTarget { e in
        guard let e = e as? MPChangePlaybackPositionCommandEvent else { return .commandFailed }
        send("SEEK", [.int(Int(mediaHandle)), .int(Int(e.positionTime * 1000))])
        return .success
    }
    #if os(iOS) || os(tvOS)
    // the phone shows an app in Now Playing only while its audio session is playback, and not mixed with others
    try? AVAudioSession.sharedInstance().setCategory(.playback, mode: .default, options: [])
    try? AVAudioSession.sharedInstance().setActive(true)
    #endif
}

let npEnable: Call = { _, _, _, _, _, _ in
    if mediaHandle == 0 {
        mediaHandle = nextHandle
        nextHandle += 1
        installCommands()
    }
    return mediaHandle
}

let npText: Call = { field, p, n, _, _, _ in
    let key: String
    switch field {
    case 0: key = MPMediaItemPropertyTitle
    case 1: key = MPMediaItemPropertyArtist
    case 2: key = MPMediaItemPropertyAlbumTitle
    default: return 0
    }
    let s = text(p, n)
    info[key] = s.isEmpty ? nil : s
    publish()
    return 1
}

let npTimes: Call = { dur, pos, playing, _, _, _ in
    // no length: a live stream (a radio station), shown as live rather than as a track of no length
    info[MPMediaItemPropertyPlaybackDuration] = dur == 0 ? nil : Double(dur) / 1000
    info[MPNowPlayingInfoPropertyIsLiveStream] = dur == 0
    info[MPNowPlayingInfoPropertyElapsedPlaybackTime] = Double(pos) / 1000
    info[MPNowPlayingInfoPropertyPlaybackRate] = playing != 0 ? 1.0 : 0.0
    publish(playing: playing != 0)
    return 1
}

let npArtwork: Call = { p, n, _, _, _, _ in
    let path = text(p, n)
    #if os(iOS) || os(tvOS)
    let image = path.isEmpty ? nil : UIImage(contentsOfFile: path)
    #else
    let image = path.isEmpty ? nil : NSImage(contentsOfFile: path)
    #endif
    if let image {
        info[MPMediaItemPropertyArtwork] = MPMediaItemArtwork(boundsSize: image.size) { _ in image }
    } else {
        info[MPMediaItemPropertyArtwork] = nil
    }
    publish()
    return image != nil || path.isEmpty ? 1 : 0
}

let npArtworkData: Call = { p, n, _, _, _, _ in
    var ok = true
    if n > 0, let bytes = UnsafeRawPointer(bitPattern: p) {
        let data = Data(bytes: bytes, count: Int(n))
        #if os(iOS) || os(tvOS)
        let image = UIImage(data: data)
        #else
        let image = NSImage(data: data)
        #endif
        if let image {
            info[MPMediaItemPropertyArtwork] = MPMediaItemArtwork(boundsSize: image.size) { _ in image }
        } else {
            info[MPMediaItemPropertyArtwork] = nil
            ok = false
        }
    } else {
        info[MPMediaItemPropertyArtwork] = nil
    }
    publish()
    return ok ? 1 : 0
}

let npClear: Call = { _, _, _, _, _, _ in
    info = [:]
    MPNowPlayingInfoCenter.default().nowPlayingInfo = nil
    #if os(macOS)
    MPNowPlayingInfoCenter.default().playbackState = .stopped
    #endif
    return 1
}

func installNowPlaying() {
    register("np_enable", npEnable)
    register("np_text", npText)
    register("np_times", npTimes)
    register("np_artwork", npArtwork)
    register("np_artwork_data", npArtworkData)
    register("np_clear", npClear)
}
