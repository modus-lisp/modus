// main.swift -- the Swift half of test/term-format-swift.sh.  Reads the samples the image encoded (hex, one
// per line), decodes each, and checks that it is the sample and re-encodes to the same bytes.  Then writes
// Swift's own encodings of the samples, for the image to decode (test/term-swift-check.lisp).
//
//   swiftc -o term-check host/ios/swift/Term.swift test/term-swift/main.swift
//   term-check /tmp/term-lisp.hex /tmp/term-swift.hex

import Foundation

// The samples, in the order of test/term-samples.lisp.
let samples: [Term] = [
    .null, .int(0), .int(42), .int(-7),
    .string("hello"), .string("café"), .string("日本"), .string("🙂"), .string(""),
    .double(2.5), .double(-0.25), .double(0.0),
    .symbol(name: "TAP", package: "KEYWORD"),
    .list([
        .symbol(name: "ID", package: "KEYWORD"), .string("a1"),
        .symbol(name: "TITLE", package: "KEYWORD"), .string("Apples"),
        .symbol(name: "COUNT", package: "KEYWORD"), .int(3),
        .symbol(name: "WEIGHT", package: "KEYWORD"), .double(2.5),
        .symbol(name: "KIND", package: "KEYWORD"), .symbol(name: "FRUIT", package: "KEYWORD"),
    ]),
    .list([.string("café"), .string("日本"), .string("🙂"), .double(1.25)]),
]

func hex(_ bytes: [UInt8]) -> String {
    bytes.map { String(format: "%02x", $0) }.joined()
}

func unhex(_ s: String) -> [UInt8] {
    var out: [UInt8] = []
    var it = s.makeIterator()
    while let a = it.next(), let b = it.next() {
        out.append(UInt8(String([a, b]), radix: 16) ?? 0)
    }
    return out
}

let args = CommandLine.arguments
guard args.count == 3 else {
    print("usage: term-check LISP-HEX OUT-HEX")
    exit(2)
}

let lines = try String(contentsOfFile: args[1], encoding: .utf8)
    .split(separator: "\n", omittingEmptySubsequences: true)
    .map(String.init)

guard lines.count == samples.count else {
    print("FAIL: \(lines.count) lines for \(samples.count) samples")
    exit(1)
}

var failures = 0
for (i, line) in lines.enumerated() {
    let bytes = unhex(line)
    do {
        let decoded = try decodeTerm(bytes)
        if decoded == samples[i] && decoded.encoded() == bytes {
            print("PASS \(i)")
        } else {
            failures += 1
            print("FAIL \(i): decoded \(decoded)")
        }
    } catch {
        failures += 1
        print("FAIL \(i): \(error)")
    }
}

let out = samples.map { hex($0.encoded()) }.joined(separator: "\n") + "\n"
try out.write(toFile: args[2], atomically: true, encoding: .utf8)
print("\(samples.count - failures) of \(samples.count) decoded in Swift to their samples, and re-encoded identically")
exit(failures == 0 ? 0 : 1)
