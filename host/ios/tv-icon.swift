// tv-icon.swift ICON.png OUT.xcassets -- an Apple TV's icon and Top Shelf, made from the app's square icon.
//
// A TV's icon is a stack of layers that the system tilts against each other under the remote's thumb (the
// parallax), in a "brand assets" set beside the Top Shelf's banner.  From one square picture: the BACK layer is the
// picture blown up past the edges and blurred, a field of its own colours; the FRONT is the picture itself, its
// corners rounded as iOS rounds it, on clear.  The Top Shelf banners are the two flattened, the picture smaller.
// Run by build-ios.sh (xcrun swift) before actool compiles the catalog.
import CoreGraphics
import CoreImage
import Foundation
import ImageIO
import UniformTypeIdentifiers

let args = CommandLine.arguments
guard args.count == 3,
      let src = CGImageSourceCreateWithURL(URL(fileURLWithPath: args[1]) as CFURL, nil),
      let icon = CGImageSourceCreateImageAtIndex(src, 0, nil) else {
    FileHandle.standardError.write("usage: tv-icon.swift ICON.png OUT.xcassets\n".data(using: .utf8)!)
    exit(2)
}
let out = URL(fileURLWithPath: args[2])
let space = CGColorSpace(name: CGColorSpace.sRGB)!
let ci = CIContext(options: [.workingColorSpace: space])

func canvas(_ w: Int, _ h: Int) -> CGContext {
    CGContext(data: nil, width: w, height: h, bitsPerComponent: 8, bytesPerRow: 0, space: space,
              bitmapInfo: CGImageAlphaInfo.premultipliedLast.rawValue)!
}

// The back: the icon scaled to cover W x H (and a margin, so the blur has no dark rim), blurred, a little darker.
func back(_ w: Int, _ h: Int) -> CGImage {
    let side = CGFloat(max(w, h)) * 1.3
    let s = side / CGFloat(icon.width)
    var im = CIImage(cgImage: icon).transformed(by: CGAffineTransform(scaleX: s, y: s))
    im = im.transformed(by: CGAffineTransform(translationX: (CGFloat(w) - side) / 2, y: (CGFloat(h) - side) / 2))
    im = im.clampedToExtent()
        .applyingGaussianBlur(sigma: Double(max(w, h)) / 10)
        .applyingFilter("CIColorControls", parameters: [kCIInputBrightnessKey: -0.12, kCIInputSaturationKey: 1.1])
    return ci.createCGImage(im, from: CGRect(x: 0, y: 0, width: w, height: h))!
}

// The icon, SIDE points square with its corners rounded, at the middle of a clear W x H.
func drawIcon(_ c: CGContext, _ w: Int, _ h: Int, side: CGFloat) {
    let r = CGRect(x: (CGFloat(w) - side) / 2, y: (CGFloat(h) - side) / 2, width: side, height: side)
    c.saveGState()
    c.setShadow(offset: CGSize(width: 0, height: -side / 40), blur: side / 15,
                color: CGColor(srgbRed: 0, green: 0, blue: 0, alpha: 0.45))
    c.beginTransparencyLayer(auxiliaryInfo: nil)
    c.addPath(CGPath(roundedRect: r, cornerWidth: side * 0.2237, cornerHeight: side * 0.2237, transform: nil))
    c.clip()
    c.interpolationQuality = .high
    c.draw(icon, in: r)
    c.endTransparencyLayer()
    c.restoreGState()
}
func front(_ w: Int, _ h: Int) -> CGImage {
    let c = canvas(w, h)
    drawIcon(c, w, h, side: CGFloat(h) * 0.78)
    return c.makeImage()!
}
func shelf(_ w: Int, _ h: Int) -> CGImage {
    let c = canvas(w, h)
    c.draw(back(w, h), in: CGRect(x: 0, y: 0, width: w, height: h))
    drawIcon(c, w, h, side: CGFloat(h) * 0.6)
    return c.makeImage()!
}

func write(_ im: CGImage, _ url: URL, opaque: Bool) {
    var im = im
    if opaque {                                      // the back and the banners have no alpha: the TV wants them so
        let c = CGContext(data: nil, width: im.width, height: im.height, bitsPerComponent: 8, bytesPerRow: 0,
                          space: space, bitmapInfo: CGImageAlphaInfo.noneSkipLast.rawValue)!
        c.draw(im, in: CGRect(x: 0, y: 0, width: im.width, height: im.height))
        im = c.makeImage()!
    }
    let d = CGImageDestinationCreateWithURL(url as CFURL, UTType.png.identifier as CFString, 1, nil)!
    CGImageDestinationAddImage(d, im, nil)
    guard CGImageDestinationFinalize(d) else { fatalError("could not write \(url.path)") }
}
func json(_ obj: Any, _ dir: URL) {
    try! FileManager.default.createDirectory(at: dir, withIntermediateDirectories: true)
    let d = try! JSONSerialization.data(withJSONObject: obj, options: [.prettyPrinted, .sortedKeys])
    try! d.write(to: dir.appendingPathComponent("Contents.json"))
}
let info = ["author": "xcode", "version": 1] as [String: Any]

// An image set of SIZE (w x h points) at the scales given, each made by MAKE.
func imageset(_ dir: URL, _ w: Int, _ h: Int, scales: [Int], opaque: Bool, _ make: (Int, Int) -> CGImage) {
    var images: [[String: Any]] = []
    for s in scales {
        let name = "image@\(s)x.png"
        images.append(["filename": name, "idiom": "tv", "scale": "\(s)x"])
        json(["images": images, "info": info], dir)
        write(make(w * s, h * s), dir.appendingPathComponent(name), opaque: opaque)
    }
}
func stack(_ name: String, _ w: Int, _ h: Int, scales: [Int]) {
    let dir = brand.appendingPathComponent("\(name).imagestack")
    json(["layers": [["filename": "Front.imagestacklayer"], ["filename": "Back.imagestacklayer"]], "info": info], dir)
    for (layer, opaque, make) in [("Front", false, front), ("Back", true, back)] as [(String, Bool, (Int, Int) -> CGImage)] {
        let l = dir.appendingPathComponent("\(layer).imagestacklayer")
        json(["info": info], l)
        imageset(l.appendingPathComponent("Content.imageset"), w, h, scales: scales, opaque: opaque, make)
    }
}

json(["info": info], out)
let brand = out.appendingPathComponent("App Icon & Top Shelf Image.brandassets")
json(["assets": [
    ["filename": "App Icon - App Store.imagestack", "idiom": "tv", "role": "primary-app-icon", "size": "1280x768"],
    ["filename": "App Icon.imagestack", "idiom": "tv", "role": "primary-app-icon", "size": "400x240"],
    ["filename": "Top Shelf Image Wide.imageset", "idiom": "tv", "role": "top-shelf-image-wide", "size": "2320x720"],
    ["filename": "Top Shelf Image.imageset", "idiom": "tv", "role": "top-shelf-image", "size": "1920x720"],
], "info": info], brand)
stack("App Icon", 400, 240, scales: [1, 2])
stack("App Icon - App Store", 1280, 768, scales: [1])
imageset(brand.appendingPathComponent("Top Shelf Image.imageset"), 1920, 720, scales: [1, 2], opaque: true, shelf)
imageset(brand.appendingPathComponent("Top Shelf Image Wide.imageset"), 2320, 720, scales: [1, 2], opaque: true, shelf)
