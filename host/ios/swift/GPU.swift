// GPU.swift — the GPU, for Lisp: Metal, behind calls shaped like WebGPU's, so the same Lisp drives a browser's
// WebGPU through the web build's bridge (the same names, answered in JavaScript).  Lisp's side is vessel's
// gpu.lisp (package KILN-GPU).
//
//   gpu_view_new(x, y, w, h)                 a view whose layer the GPU draws (a CAMetalLayer); a UI handle
//   gpu_shader_new(src, len)                 a shader module from source (Metal Shading Language here)
//   gpu_pipeline_new(shader, vname, vlen, fname, flen)   a render pipeline: vertex + fragment function, blending
//   gpu_buffer_new(data, len)                a buffer holding a copy of the bytes
//   gpu_buffer_write(buffer, offset, data, len)
//   gpu_pass_begin(view, rgb)                a frame's render pass, cleared to RGB; 0 when the view is gone
//   gpu_set_pipeline(pass, pipeline)
//   gpu_set_buffer(pass, buffer, index, stage)   stage 0 vertex, 1 fragment, 2 both
//   gpu_draw(pass, primitive, start, count)  primitive 0 triangles, 1 triangle strip, 2 lines, 3 points
//   gpu_pass_end(pass)                       encode, present, commit: the GPU runs it while Lisp goes on
//
// For 3D: every view has a DEPTH buffer (32-bit float, cleared to 1 each pass, sized with the drawable), every
// pipeline writes it, and a pass starts with the depth test on (less-or-equal, so 2D drawn at one depth still
// layers in order) and no face culled.
//   gpu_view_new(0, 0, 0, 0)                 a view filling the window, following it as it resizes
//   gpu_view_size(view)                      its drawable's size in pixels: width * 65536 + height
//   gpu_set_depth(pass, mode)                0 no test and no write, 1 test and write, 2 test only (see-through)
//   gpu_set_cull(pass, mode)                 0 none, 1 back faces, 2 front faces (counter-clockwise is the front)
//   gpu_set_bytes(pass, data, len, index, stage)   up to 4 KB of constants for this draw -- a model matrix --
//                                            copied now, so the next draw's do not race the GPU
//   gpu_texture_new(rgba, width, height)     an RGBA8 texture from WIDTH x HEIGHT pixels, mipmapped
//   gpu_set_texture(pass, texture, index, stage)   and a sampler at the same index: linear, mipmapped, repeating
//   gpu_draw_indexed(pass, primitive, indices, count, first, wide)   COUNT indices from FIRST in the buffer
//                                            INDICES: 16-bit, or 32-bit when WIDE is 1
//
// Data is bytes in the image's memory, read in place: a single-float vector's payload is IEEE singles already.

#if os(macOS)
import AppKit          // the macOS window host builds this file too (host/macos/build-macos.sh gui)
#else
import UIKit
#endif
import Metal
import QuartzCore

private let device = MTLCreateSystemDefaultDevice()
private let queue = device?.makeCommandQueue()       // globals are lazy already
private var objects: [UInt: AnyObject] = [:]

/// A handle for OBJECT, from the components' own counter, so a GPU view is also a UI handle.
private func keep(_ object: AnyObject) -> UInt {
    let h = nextHandle
    nextHandle += 1
    objects[h] = object
    return h
}

private func bytes(_ p: UInt, _ n: UInt) -> UnsafeRawPointer? {
    n > 0 ? UnsafeRawPointer(bitPattern: p) : nil
}

/// A view the GPU draws: its layer is a CAMetalLayer, sized in pixels to the view.
#if os(macOS)
final class GPUView: NSView {
    var metalLayer: CAMetalLayer { layer as! CAMetalLayer }
    var depth: MTLTexture?

    override init(frame: CGRect) {
        super.init(frame: frame)
        wantsLayer = true                    // the layer comes from makeBackingLayer
    }
    required init?(coder: NSCoder) { fatalError("no coder") }

    override func makeBackingLayer() -> CALayer {
        let layer = CAMetalLayer()
        layer.device = device
        layer.pixelFormat = .bgra8Unorm
        layer.framebufferOnly = true
        return layer
    }

    override var isFlipped: Bool { true }
    override func hitTest(_ point: NSPoint) -> NSView? { nil }   // the phone's isUserInteractionEnabled = false

    private func fit() {
        let scale = window?.backingScaleFactor ?? NSScreen.main?.backingScaleFactor ?? 2
        metalLayer.contentsScale = scale
        metalLayer.drawableSize = CGSize(width: bounds.width * scale, height: bounds.height * scale)
    }
    override func setFrameSize(_ newSize: NSSize) { super.setFrameSize(newSize); fit() }
    override func viewDidMoveToWindow() { super.viewDidMoveToWindow(); fit() }
}
#else
final class GPUView: UIView {
    override class var layerClass: AnyClass { CAMetalLayer.self }
    var metalLayer: CAMetalLayer { layer as! CAMetalLayer }
    var depth: MTLTexture?

    override init(frame: CGRect) {
        super.init(frame: frame)
        metalLayer.device = device
        metalLayer.pixelFormat = .bgra8Unorm
        metalLayer.framebufferOnly = true
        isUserInteractionEnabled = false
    }
    required init?(coder: NSCoder) { fatalError("no coder") }

    override func layoutSubviews() {
        super.layoutSubviews()
        let scale = window?.screen.scale ?? UIScreen.main.scale
        metalLayer.contentsScale = scale
        metalLayer.drawableSize = CGSize(width: bounds.width * scale, height: bounds.height * scale)
    }
}
#endif

/// The depth states a pass can use, by mode: 0 none, 1 test and write, 2 test only.
private func depthState(_ compare: MTLCompareFunction, _ write: Bool) -> MTLDepthStencilState? {
    let d = MTLDepthStencilDescriptor()
    d.depthCompareFunction = compare
    d.isDepthWriteEnabled = write
    return device?.makeDepthStencilState(descriptor: d)
}
private let depthStates: [MTLDepthStencilState?] = [depthState(.always, false), depthState(.lessEqual, true),
                                                    depthState(.lessEqual, false)]

/// The one sampler textures are read with: linear, mipmapped, repeating, anisotropic.
private let sampler: MTLSamplerState? = {
    let d = MTLSamplerDescriptor()
    d.minFilter = .linear
    d.magFilter = .linear
    d.mipFilter = .linear
    d.sAddressMode = .repeat
    d.tAddressMode = .repeat
    d.maxAnisotropy = 8
    return device?.makeSamplerState(descriptor: d)
}()

/// A frame in progress: its drawable, command buffer and encoder.
private final class Pass {
    let drawable: CAMetalDrawable
    let commands: MTLCommandBuffer
    let encoder: MTLRenderCommandEncoder
    init(_ drawable: CAMetalDrawable, _ commands: MTLCommandBuffer, _ encoder: MTLRenderCommandEncoder) {
        self.drawable = drawable
        self.commands = commands
        self.encoder = encoder
    }
}

private var gpuViews: [UInt: GPUView] = [:]

let gpuViewNew: Call = { x, y, w, h, _, _ in
    let fill = w == 0 && h == 0
    let view = GPUView(frame: rect(x, y, max(w, 1), max(h, 1)))
    let handle = adopt(view)
    if fill, let parent = view.superview {        // the whole window, and following it
        view.frame = parent.bounds
        #if os(macOS)
        view.autoresizingMask = [.width, .height]
        #else
        view.autoresizingMask = [.flexibleWidth, .flexibleHeight]
        #endif
    }
    gpuViews[handle] = view
    return handle
}

let gpuViewSize: Call = { v, _, _, _, _, _ in
    guard let view = gpuViews[v] else { return 0 }
    let size = view.metalLayer.drawableSize
    return UInt(max(0, size.width)) * 65536 + UInt(max(0, size.height))
}

let gpuShaderNew: Call = { p, n, _, _, _, _ in
    guard let device else { return 0 }
    do {
        return keep(try device.makeLibrary(source: text(p, n), options: nil))
    } catch {
        NSLog("modus gpu: the shader does not compile: \(error)")
        return 0
    }
}

let gpuPipelineNew: Call = { shader, vp, vn, fp, fn, _ in
    guard let device, let library = objects[shader] as? MTLLibrary,
          let vertex = library.makeFunction(name: text(vp, vn)),
          let fragment = library.makeFunction(name: text(fp, fn)) else { return 0 }
    let d = MTLRenderPipelineDescriptor()
    d.vertexFunction = vertex
    d.fragmentFunction = fragment
    let colour = d.colorAttachments[0]!
    colour.pixelFormat = .bgra8Unorm
    colour.isBlendingEnabled = true                              // straight alpha, over
    colour.sourceRGBBlendFactor = .sourceAlpha
    colour.destinationRGBBlendFactor = .oneMinusSourceAlpha
    colour.sourceAlphaBlendFactor = .one
    colour.destinationAlphaBlendFactor = .oneMinusSourceAlpha
    d.depthAttachmentPixelFormat = .depth32Float               // every view has one (gpu_pass_begin)
    do {
        return keep(try device.makeRenderPipelineState(descriptor: d))
    } catch {
        NSLog("modus gpu: the pipeline does not build: \(error)")
        return 0
    }
}

let gpuBufferNew: Call = { p, n, _, _, _, _ in
    guard let device, n > 0 else { return 0 }
    let buffer = bytes(p, n).flatMap { device.makeBuffer(bytes: $0, length: Int(n), options: .storageModeShared) }
        ?? device.makeBuffer(length: Int(n), options: .storageModeShared)
    return buffer.map { keep($0) } ?? 0
}

let gpuBufferWrite: Call = { h, offset, p, n, _, _ in
    guard let buffer = objects[h] as? MTLBuffer, let src = bytes(p, n),
          Int(offset) + Int(n) <= buffer.length else { return 0 }
    buffer.contents().advanced(by: Int(offset)).copyMemory(from: src, byteCount: Int(n))
    return 1
}

let gpuPassBegin: Call = { v, rgb, _, _, _, _ in
    guard let view = gpuViews[v], view.superview != nil, view.bounds.width > 0,
          let drawable = view.metalLayer.nextDrawable(), let commands = queue?.makeCommandBuffer() else { return 0 }
    let d = MTLRenderPassDescriptor()
    let colour = d.colorAttachments[0]!
    colour.texture = drawable.texture
    colour.loadAction = .clear
    colour.storeAction = .store
    colour.clearColor = MTLClearColor(red: Double((rgb >> 16) & 255) / 255, green: Double((rgb >> 8) & 255) / 255,
                                      blue: Double(rgb & 255) / 255, alpha: 1)
    // THE DEPTH BUFFER, made again when the drawable's size changes (the window resized)
    let w = drawable.texture.width, h = drawable.texture.height
    if view.depth == nil || view.depth!.width != w || view.depth!.height != h {
        let td = MTLTextureDescriptor.texture2DDescriptor(pixelFormat: .depth32Float, width: w, height: h, mipmapped: false)
        td.usage = .renderTarget
        td.storageMode = .private
        view.depth = device?.makeTexture(descriptor: td)
    }
    d.depthAttachment.texture = view.depth
    d.depthAttachment.loadAction = .clear
    d.depthAttachment.clearDepth = 1
    d.depthAttachment.storeAction = .dontCare
    guard let encoder = commands.makeRenderCommandEncoder(descriptor: d) else { return 0 }
    if let state = depthStates[1] { encoder.setDepthStencilState(state) }
    // COUNTER-CLOCKWISE IS THE FRONT, as Lisp's geometry is wound (OpenGL's way)
    encoder.setFrontFacing(.counterClockwise)
    encoder.setCullMode(.none)
    return keep(Pass(drawable, commands, encoder))
}

let gpuSetPipeline: Call = { p, pipe, _, _, _, _ in
    guard let pass = objects[p] as? Pass, let pipeline = objects[pipe] as? MTLRenderPipelineState else { return 0 }
    pass.encoder.setRenderPipelineState(pipeline)
    return 1
}

let gpuSetBuffer: Call = { p, b, index, stage, _, _ in
    guard let pass = objects[p] as? Pass, let buffer = objects[b] as? MTLBuffer else { return 0 }
    if stage != 1 { pass.encoder.setVertexBuffer(buffer, offset: 0, index: Int(index)) }
    if stage != 0 { pass.encoder.setFragmentBuffer(buffer, offset: 0, index: Int(index)) }
    return 1
}

let gpuDraw: Call = { p, primitive, start, count, _, _ in
    guard let pass = objects[p] as? Pass else { return 0 }
    let kind: MTLPrimitiveType = [.triangle, .triangleStrip, .line, .point][Int(min(primitive, 3))]
    pass.encoder.drawPrimitives(type: kind, vertexStart: Int(start), vertexCount: Int(count))
    return 1
}

let gpuSetDepth: Call = { p, mode, _, _, _, _ in
    guard let pass = objects[p] as? Pass, mode < 3, let state = depthStates[Int(mode)] else { return 0 }
    pass.encoder.setDepthStencilState(state)
    return 1
}

let gpuSetCull: Call = { p, mode, _, _, _, _ in
    guard let pass = objects[p] as? Pass else { return 0 }
    pass.encoder.setCullMode([.none, .back, .front][Int(min(mode, 2))])
    return 1
}

let gpuSetBytes: Call = { p, data, n, index, stage, _ in
    guard let pass = objects[p] as? Pass, let src = bytes(data, n), n <= 4096 else { return 0 }
    if stage != 1 { pass.encoder.setVertexBytes(src, length: Int(n), index: Int(index)) }
    if stage != 0 { pass.encoder.setFragmentBytes(src, length: Int(n), index: Int(index)) }
    return 1
}

let gpuTextureNew: Call = { p, w, h, _, _, _ in
    guard let device, let src = bytes(p, w * h * 4), w > 0, h > 0 else { return 0 }
    let d = MTLTextureDescriptor.texture2DDescriptor(pixelFormat: .rgba8Unorm, width: Int(w), height: Int(h),
                                                     mipmapped: true)
    d.usage = .shaderRead
    guard let texture = device.makeTexture(descriptor: d) else { return 0 }
    texture.replace(region: MTLRegionMake2D(0, 0, Int(w), Int(h)), mipmapLevel: 0, withBytes: src,
                    bytesPerRow: Int(w) * 4)
    // the smaller levels, made by the GPU: a textured floor seen at a slant does not shimmer
    if let commands = queue?.makeCommandBuffer(), let blit = commands.makeBlitCommandEncoder() {
        blit.generateMipmaps(for: texture)
        blit.endEncoding()
        commands.commit()
    }
    return keep(texture)
}

let gpuSetTexture: Call = { p, t, index, stage, _, _ in
    guard let pass = objects[p] as? Pass, let texture = objects[t] as? MTLTexture else { return 0 }
    if stage != 1 {
        pass.encoder.setVertexTexture(texture, index: Int(index))
        pass.encoder.setVertexSamplerState(sampler, index: Int(index))
    }
    if stage != 0 {
        pass.encoder.setFragmentTexture(texture, index: Int(index))
        pass.encoder.setFragmentSamplerState(sampler, index: Int(index))
    }
    return 1
}

let gpuDrawIndexed: Call = { p, primitive, ib, count, first, wide in
    guard let pass = objects[p] as? Pass, let indices = objects[ib] as? MTLBuffer else { return 0 }
    let kind: MTLPrimitiveType = [.triangle, .triangleStrip, .line, .point][Int(min(primitive, 3))]
    let size = wide != 0 ? 4 : 2
    guard (Int(first) + Int(count)) * size <= indices.length else { return 0 }
    pass.encoder.drawIndexedPrimitives(type: kind, indexCount: Int(count),
                                       indexType: wide != 0 ? .uint32 : .uint16,
                                       indexBuffer: indices, indexBufferOffset: Int(first) * size)
    return 1
}

let gpuPassEnd: Call = { p, _, _, _, _, _ in
    guard let pass = objects.removeValue(forKey: p) as? Pass else { return 0 }
    pass.encoder.endEncoding()
    pass.commands.present(pass.drawable)
    pass.commands.commit()
    return 1
}

func installGPU() {
    register("gpu_view_new", gpuViewNew)
    register("gpu_shader_new", gpuShaderNew)
    register("gpu_pipeline_new", gpuPipelineNew)
    register("gpu_buffer_new", gpuBufferNew)
    register("gpu_buffer_write", gpuBufferWrite)
    register("gpu_pass_begin", gpuPassBegin)
    register("gpu_set_pipeline", gpuSetPipeline)
    register("gpu_set_buffer", gpuSetBuffer)
    register("gpu_draw", gpuDraw)
    register("gpu_pass_end", gpuPassEnd)
    register("gpu_view_size", gpuViewSize)
    register("gpu_set_depth", gpuSetDepth)
    register("gpu_set_cull", gpuSetCull)
    register("gpu_set_bytes", gpuSetBytes)
    register("gpu_texture_new", gpuTextureNew)
    register("gpu_set_texture", gpuSetTexture)
    register("gpu_draw_indexed", gpuDrawIndexed)
}
