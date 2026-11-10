// Term.swift — the wire format of net/actors.lisp (term-encode, term-decode-step), in Swift: the bridge
// speaks the same bytes as the image's actor messages.  A term is a tree:
//
//   0 nil             1 fixnum (u64)           2 cons (car, then cdr)
//   3 string, 8-bit   4 byte vector            5 bignum (not decoded here)
//   6 symbol (name, then package)              7 string, UTF-8
//   8 double-float (IEEE bits: the low u32, then the high u32)
//
// Every count and length is a little-endian u32; a fixnum is a little-endian u64.  Lisp's NIL is tag 0,
// and a fixnum zero is tag 1: the two are different terms here too.

import Foundation

indirect enum Term: Equatable {
    case null
    case int(Int)
    case double(Double)
    case string(String)
    case symbol(name: String, package: String)
    case cons(Term, Term)
    case bytes([UInt8])
}

enum TermError: Error, Equatable {
    case truncated
    case badTag(UInt8)
    case unsupported(String)
}

// A proper list (a cons chain ending in null), as an array; nil if the term is not one.
extension Term {
    static func list(_ items: [Term]) -> Term {
        items.reversed().reduce(Term.null) { .cons($1, $0) }
    }

    var listItems: [Term]? {
        var out: [Term] = []
        var cur = self
        while true {
            switch cur {
            case .null: return out
            case .cons(let a, let d): out.append(a); cur = d
            default: return nil
            }
        }
    }
}

// MARK: - encoding

extension Term {
    func encode(into out: inout [UInt8]) {
        switch self {
        case .null:
            out.append(0)
        case .int(let v):
            out.append(1)
            appendU64(&out, UInt64(bitPattern: Int64(v)))
        case .double(let d):
            out.append(8)
            let bits = d.bitPattern
            appendU32(&out, UInt32(truncatingIfNeeded: bits))
            appendU32(&out, UInt32(truncatingIfNeeded: bits >> 32))
        case .string(let s):
            let u = Array(s.utf8)
            out.append(7)
            appendU32(&out, UInt32(u.count))
            out += u
        case .symbol(let name, let package):
            let n = Array(name.utf8), p = Array(package.utf8)
            out.append(6)
            appendU32(&out, UInt32(n.count))
            out += n
            appendU32(&out, UInt32(p.count))
            out += p
        case .cons(let a, let d):
            out.append(2)
            a.encode(into: &out)
            d.encode(into: &out)
        case .bytes(let b):
            out.append(4)
            appendU32(&out, UInt32(b.count))
            out += b
        }
    }

    func encoded() -> [UInt8] {
        var out: [UInt8] = []
        encode(into: &out)
        return out
    }
}

private func appendU32(_ out: inout [UInt8], _ v: UInt32) {
    for i in 0..<4 { out.append(UInt8(truncatingIfNeeded: v >> (8 * UInt32(i)))) }
}

private func appendU64(_ out: inout [UInt8], _ v: UInt64) {
    for i in 0..<8 { out.append(UInt8(truncatingIfNeeded: v >> (8 * UInt64(i)))) }
}

// MARK: - decoding

struct TermReader {
    let bytes: [UInt8]
    private(set) var pos = 0

    init(_ bytes: [UInt8]) {
        self.bytes = bytes
    }

    var atEnd: Bool { pos >= bytes.count }

    private mutating func byte() throws -> UInt8 {
        guard pos < bytes.count else { throw TermError.truncated }
        defer { pos += 1 }
        return bytes[pos]
    }

    private mutating func u32() throws -> UInt32 {
        var v: UInt32 = 0
        for i in 0..<4 { v |= UInt32(try byte()) << (8 * UInt32(i)) }
        return v
    }

    private mutating func u64() throws -> UInt64 {
        var v: UInt64 = 0
        for i in 0..<8 { v |= UInt64(try byte()) << (8 * UInt64(i)) }
        return v
    }

    private mutating func take(_ n: Int) throws -> [UInt8] {
        guard n >= 0, pos + n <= bytes.count else { throw TermError.truncated }
        defer { pos += n }
        return Array(bytes[pos..<(pos + n)])
    }

    // One term, from the current position.
    mutating func term() throws -> Term {
        let tag = try byte()
        switch tag {
        case 0:
            return .null
        case 1:
            return .int(Int(Int64(bitPattern: try u64())))
        case 2:
            let a = try term()
            let d = try term()
            return .cons(a, d)
        case 3:   // the old 8-bit string: one character per byte, Latin-1
            let n = try u32()
            let s = try take(Int(n)).map { Character(Unicode.Scalar($0)) }
            return .string(String(s))
        case 4:
            let n = try u32()
            return .bytes(try take(Int(n)))
        case 6:
            let nlen = try u32()
            let name = String(decoding: try take(Int(nlen)), as: UTF8.self)
            let plen = try u32()
            let package = String(decoding: try take(Int(plen)), as: UTF8.self)
            return .symbol(name: name, package: package)
        case 7:
            let n = try u32()
            return .string(String(decoding: try take(Int(n)), as: UTF8.self))
        case 8:
            let lo = try u32()
            let hi = try u32()
            return .double(Double(bitPattern: UInt64(hi) << 32 | UInt64(lo)))
        case 5:
            throw TermError.unsupported("bignum")
        default:
            throw TermError.badTag(tag)
        }
    }
}

// Decode one term from the bytes.
func decodeTerm(_ bytes: [UInt8]) throws -> Term {
    var r = TermReader(bytes)
    return try r.term()
}
