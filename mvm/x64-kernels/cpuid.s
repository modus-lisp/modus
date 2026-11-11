# KERNEL kind 5: CPUID leaf 1's ECX (feature bits) in rax.  rbx is not among KERNEL's
# saved registers, so it is saved here.
	push %rbx
	mov $1,%eax
	xor %ecx,%ecx
	cpuid
	mov %ecx,%eax
	pop %rbx
