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
// Data is bytes in the image's memory, read in place: a single-float vector's payload is IEEE singles already.

import UIKit
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
final class GPUView: UIView {
    override class var layerClass: AnyClass { CAMetalLayer.self }
    var metalLayer: CAMetalLayer { layer as! CAMetalLayer }

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
    let view = GPUView(frame: rect(x, y, w, h))
    let handle = adopt(view)
    gpuViews[handle] = view
    return handle
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
    guard let encoder = commands.makeRenderCommandEncoder(descriptor: d) else { return 0 }
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
}
