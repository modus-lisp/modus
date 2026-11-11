# KERNEL kind 3: AES counter mode.  In: rsi = KS (tagged octet vector: J0[16] then the
# round keys, 16*(R+1) bytes), rax = IN, rdx = OUT (tagged octet vectors), rcx = tagged
# (BYTES<<4 | R).  OUT[k] = IN[k] ^ AES(J0+1+floor(k/16))[k mod 16]; the counter is the
# big-endian low 32 bits, wrapping (GCM's inc32).  xmm0-xmm2 saved; rax = 0.
	sub $64,%rsp
	movdqu %xmm0,(%rsp)
	movdqu %xmm1,16(%rsp)
	movdqu %xmm2,32(%rsp)
	sar $1,%rcx
	mov %rcx,%r8
	and $15,%r8
	shr $4,%rcx
	add $7,%rsi
	add $7,%rax
	add $7,%rdx
	movdqu (%rsi),%xmm0
	mov 12(%rsi),%r9d
	bswap %r9d
	test %rcx,%rcx
	jz done
block:
	inc %r9d
	mov %r9d,%r10d
	bswap %r10d
	movdqa %xmm0,%xmm1
	pinsrd $3,%r10d,%xmm1
	movdqu 16(%rsi),%xmm2
	pxor %xmm2,%xmm1
	lea 32(%rsi),%r11
	mov %r8,%r10
	dec %r10
round:
	movdqu (%r11),%xmm2
	aesenc %xmm2,%xmm1
	add $16,%r11
	dec %r10
	jnz round
	movdqu (%r11),%xmm2
	aesenclast %xmm2,%xmm1
	cmp $16,%rcx
	jb tail
	movdqu (%rax),%xmm2
	pxor %xmm2,%xmm1
	movdqu %xmm1,(%rdx)
	add $16,%rax
	add $16,%rdx
	sub $16,%rcx
	jnz block
	jmp done
tail:
	movdqu %xmm1,48(%rsp)
	xor %r11,%r11
tbyte:
	movzbl (%rax,%r11),%r10d
	xorb 48(%rsp,%r11),%r10b
	mov %r10b,(%rdx,%r11)
	inc %r11
	cmp %rcx,%r11
	jb tbyte
done:
	movdqu (%rsp),%xmm0
	movdqu 16(%rsp),%xmm1
	movdqu 32(%rsp),%xmm2
	add $64,%rsp
	xor %eax,%eax
