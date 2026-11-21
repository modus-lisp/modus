; Build: nasm -f win64 fsbase.asm -o fsbase.obj && ld -m i386pep --subsystem console -e start -o fsbase.exe fsbase.obj KERNEL32.DLL
; (any kernel32.dll does as the import source; Wine's is in its x86_64-windows dir).
; Run in the guest: EXTRA_CMD='fsbase.exe & call echo %%errorlevel%% > FSBASE.TXT' test/win64/run-vm.sh BASE.qcow2 modus.exe fsbase.exe
; Measured 2026-11-21: Wine 0; Windows Server 2022 (20348) under KVM 20 -- WRFSBASE executes and
; fs: loads use the base, but the first context switch loses it.  So the per-thread
; window cannot be an FS base on Windows.
; fsbase.asm - can a Windows x64 user process set its own FS base, and keep it?
; Exit code: 0 yes; 10 no FSGSBASE in CPUID; 20 the base was lost across
; Sleep (a real context switch); 21 lost during a busy spin (interrupts only);
; 22 an fs: load did not read through the base.  If CR4.FSGSBASE is off the
; first RDFSBASE faults and the process dies with 0xC000001D (-1073741795).
bits 64
default rel
extern __imp_ExitProcess
extern __imp_Sleep
global start
section .data
marker: dq 0x0123456789ABCDEF
section .text
start:
  sub rsp, 0x28
  mov eax, 7
  xor ecx, ecx
  cpuid
  test ebx, 1
  jnz .have
  mov ecx, 10
  jmp .exit0
.have:
  rdfsbase r12                  ; the original base, put back at exit
  lea rbx, [marker]
  wrfsbase rbx
  mov rax, [fs:0]
  cmp rax, [rbx]
  jne .f22
  mov esi, 200                  ; 200 x Sleep(1): the thread is switched out each time
.sl:
  mov ecx, 1
  call [rel __imp_Sleep]
  rdfsbase rax
  cmp rax, rbx
  jne .f20
  dec esi
  jnz .sl
  rdtsc                         ; ~2^31 cycles of spinning: timer interrupts only
  shl rdx, 32
  or rax, rdx
  lea rdi, [rax + 0x7FFFFFFF]
.bl:
  rdfsbase rax
  cmp rax, rbx
  jne .f21
  rdtsc
  shl rdx, 32
  or rax, rdx
  cmp rax, rdi
  jb .bl
  mov rax, [fs:0]
  cmp rax, [rbx]
  jne .f22
  xor ecx, ecx
  jmp .exit
.f20: mov ecx, 20
  jmp .exit
.f21: mov ecx, 21
  jmp .exit
.f22: mov ecx, 22
.exit:
  wrfsbase r12
.exit0:
  call [rel __imp_ExitProcess]
