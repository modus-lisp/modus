// Markdown.swift -- Markdown drawn as attributed text: Foundation parses it (headings, lists, quotes, code,
// emphasis, links -- CommonMark and GitHub's tables and strikethrough), and this lays out its blocks, which
// AttributedString leaves to whoever shows it.  For a preview, and a folder's README under its list.

import UIKit

enum Markdown {
    static func render(_ source: String, base: UIFont = .preferredFont(forTextStyle: .body)) -> NSAttributedString {
        let options = AttributedString.MarkdownParsingOptions(
            allowsExtendedAttributes: true, interpretedSyntax: .full, failurePolicy: .returnPartiallyParsedIfPossible)
        guard let parsed = try? AttributedString(markdown: source, options: options) else {
            return NSAttributedString(string: source, attributes: [.font: base, .foregroundColor: UIColor.label])
        }
        let out = NSMutableAttributedString()
        let mono = UIFont.monospacedSystemFont(ofSize: base.pointSize * 0.88, weight: .regular)
        var lastBlock: Int?
        var lastKinds: [PresentationIntent.Kind] = []
        var lastRow: Int?, lastTable: Int?

        for run in parsed.runs {
            let comps = run.presentationIntent?.components ?? []      // innermost first
            let block = comps.first?.identity
            let kinds = comps.map(\.kind)
            // a table: its cells on one line, a row a line
            let row = comps.first(where: { switch $0.kind { case .tableRow, .tableHeaderRow: return true; default: return false } })?.identity
            let table = comps.first(where: { if case .table = $0.kind { return true }; return false })?.identity
            let headerRow = kinds.contains(where: { if case .tableHeaderRow = $0 { return true }; return false })

            // a new block: the break before it, and a list item's marker
            if block != lastBlock, let r = row, r == lastRow {
                out.append(NSAttributedString(string: "   |   ", attributes: [.font: base, .foregroundColor: UIColor.tertiaryLabel]))
                lastBlock = block
            } else if block != lastBlock, row != nil, table != nil, table == lastTable {
                out.append(NSAttributedString(string: "\n", attributes: [.font: base]))
                lastBlock = block
                lastRow = row
            } else if block != lastBlock {
                lastRow = row
                lastTable = table
                if out.length > 0 {
                    let tight = kinds.contains(where: { if case .listItem = $0 { return true }; return false })
                        && lastKinds.contains(where: { if case .listItem = $0 { return true }; return false })
                    out.append(NSAttributedString(string: tight ? "\n" : "\n\n", attributes: [.font: base]))
                }
                if case .listItem(let ordinal)? = kinds.first(where: { if case .listItem = $0 { return true }; return false }) {
                    let ordered = kinds.contains(where: { if case .orderedList = $0 { return true }; return false })
                    out.append(NSAttributedString(string: ordered ? "\(ordinal).\t" : "•\t",
                                                  attributes: [.font: base, .foregroundColor: UIColor.secondaryLabel,
                                                               .paragraphStyle: paragraph(kinds, base)]))
                }
                if kinds.contains(where: { if case .thematicBreak = $0 { return true }; return false }) {
                    // a rule: a short line, in the separators' grey
                    out.append(NSAttributedString(string: String(repeating: "\u{2500}", count: 16),
                                                  attributes: [.font: base, .foregroundColor: UIColor.separator]))
                }
                lastBlock = block
                lastKinds = kinds
            }
            if kinds.contains(where: { if case .thematicBreak = $0 { return true }; return false }) { continue }

            var text = String(parsed[run.range].characters)
            var attrs: [NSAttributedString.Key: Any] = [.foregroundColor: UIColor.label, .paragraphStyle: paragraph(kinds, base)]
            var font = base
            var traits: UIFontDescriptor.SymbolicTraits = headerRow ? [.traitBold] : []
            for k in kinds {
                switch k {
                case .header(let level):
                    let style: UIFont.TextStyle = level == 1 ? .title1 : level == 2 ? .title2 : level == 3 ? .title3 : .headline
                    font = .preferredFont(forTextStyle: style)
                    traits.insert(.traitBold)
                case .codeBlock:
                    font = mono
                    attrs[.backgroundColor] = UIColor.secondarySystemBackground
                    if text.hasSuffix("\n") { text.removeLast() }
                case .blockQuote:
                    attrs[.foregroundColor] = UIColor.secondaryLabel
                default: break
                }
            }
            if let inline = run.inlinePresentationIntent {
                if inline.contains(.stronglyEmphasized) { traits.insert(.traitBold) }
                if inline.contains(.emphasized) { traits.insert(.traitItalic) }
                if inline.contains(.code) {
                    font = .monospacedSystemFont(ofSize: font.pointSize * 0.9, weight: .regular)
                    attrs[.backgroundColor] = UIColor.secondarySystemBackground
                }
                if inline.contains(.strikethrough) { attrs[.strikethroughStyle] = NSUnderlineStyle.single.rawValue }
            }
            if !traits.isEmpty, let d = font.fontDescriptor.withSymbolicTraits(font.fontDescriptor.symbolicTraits.union(traits)) {
                font = UIFont(descriptor: d, size: font.pointSize)
            }
            attrs[.font] = font
            if let link = run.link {
                attrs[.link] = link
                attrs[.underlineStyle] = NSUnderlineStyle.single.rawValue
            }
            if let image = run.imageURL, text.isEmpty { text = "[\(image.lastPathComponent)]" }
            out.append(NSAttributedString(string: text, attributes: attrs))
        }
        return out
    }

    /// A block's indent: a list's or a quote's, by how deep it is.
    private static func paragraph(_ kinds: [PresentationIntent.Kind], _ base: UIFont) -> NSParagraphStyle {
        let p = NSMutableParagraphStyle()
        let lists = kinds.filter { if case .orderedList = $0 { return true }; if case .unorderedList = $0 { return true }; return false }.count
        let quotes = kinds.filter { if case .blockQuote = $0 { return true }; return false }.count
        // indents in proportion to the type, as GitHub's are: a list level, a quote level
        let step = (base.pointSize * 1.4).rounded()
        let indent = CGFloat(lists) * step + CGFloat(quotes) * step
        p.firstLineHeadIndent = max(0, indent - (lists > 0 ? step : 0))
        p.headIndent = indent
        p.tabStops = [NSTextTab(textAlignment: .left, location: indent)]
        p.defaultTabInterval = step
        p.paragraphSpacing = (base.pointSize * 0.25).rounded()
        p.lineSpacing = (base.pointSize * 0.3).rounded()
        return p
    }
}
