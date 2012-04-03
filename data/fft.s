	.file	"fft.c"
	.text
	.p2align 4,,15
.globl TouchArray
	.type	TouchArray, @function
TouchArray:
.LFB15:
	pushq	%r15
.LCFI0:
	movq	%rdx, %r10
	xorpd	%xmm1, %xmm1
	pushq	%r14
.LCFI1:
	pushq	%r13
.LCFI2:
	pushq	%r12
.LCFI3:
	pushq	%rbp
.LCFI4:
	pushq	%rbx
.LCFI5:
	movq	%rdi, -16(%rsp)
	movq	%r8, %rdi
	movq	rootN(%rip), %r8
	movq	%r9, -24(%rsp)
	leaq	-2(%r8,%r8), %rdx
	testq	%rdx, %rdx
	jle	.L3
	xorpd	%xmm1, %xmm1
	xorl	%eax, %eax
	.p2align 4,,10
	.p2align 3
.L4:
	addsd	(%rcx,%rax,8), %xmm1
	addq	$1, %rax
	cmpq	%rdx, %rax
	jl	.L4
.L3:
	cmpq	-24(%rsp), %rdi
	jge	.L5
	movq	%r8, %rax
	addq	pad_length(%rip), %rax
	movq	%rdi, %rbp
	xorl	%ebx, %ebx
	movq	%rax, %r9
	imulq	%rdi, %rax
	salq	$4, %r9
	leaq	1(%rax,%rax), %rdx
	salq	$4, %rax
	movq	%rax, -8(%rsp)
	leaq	(%r10,%rax), %r14
	leaq	(%rsi,%rax), %r12
	salq	$3, %rdx
	addq	$8, %rax
	leaq	(%r10,%rdx), %r15
	leaq	(%rsi,%rdx), %r13
	movq	%rax, -32(%rsp)
	.p2align 4,,10
	.p2align 3
.L8:
	testq	%r8, %r8
	jle	.L6
	movq	-8(%rsp), %rdx
	movq	-16(%rsp), %rdi
	movq	%r12, %r11
	movq	-16(%rsp), %rsi
	movq	%r13, %r10
	movq	%r14, %rcx
	leaq	(%rbx,%rdx), %rax
	movq	%r15, %rdx
	addq	%rax, %rdi
	movq	-32(%rsp), %rax
	addq	%rbx, %rax
	addq	%rax, %rsi
	xorl	%eax, %eax
	.p2align 4,,10
	.p2align 3
.L7:
	movsd	(%rdi), %xmm0
	addq	$1, %rax
	addq	$16, %rdi
	addsd	(%rsi), %xmm0
	addq	$16, %rsi
	addsd	(%r11), %xmm0
	addq	$16, %r11
	addsd	(%r10), %xmm0
	addq	$16, %r10
	addsd	(%rcx), %xmm0
	addq	$16, %rcx
	addsd	(%rdx), %xmm0
	addq	$16, %rdx
	cmpq	%rax, %r8
	addsd	%xmm0, %xmm1
	jg	.L7
.L6:
	addq	$1, %rbp
	addq	%r9, %r15
	addq	%r9, %rbx
	addq	%r9, %r14
	addq	%r9, %r13
	addq	%r9, %r12
	cmpq	%rbp, -24(%rsp)
	jg	.L8
.L5:
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	movapd	%xmm1, %xmm0
	popq	%r15
	ret
.LFE15:
	.size	TouchArray, .-TouchArray
	.p2align 4,,15
.globl CheckSum
	.type	CheckSum, @function
CheckSum:
.LFB16:
	movq	rootN(%rip), %rsi
	xorpd	%xmm1, %xmm1
	testq	%rsi, %rsi
	jle	.L16
	movq	%rsi, %r9
	addq	pad_length(%rip), %r9
	xorl	%r8d, %r8d
	xorpd	%xmm1, %xmm1
	salq	$4, %r9
	.p2align 4,,10
	.p2align 3
.L18:
	leaq	8(%rdi), %rcx
	movq	%rdi, %rdx
	xorl	%eax, %eax
	.p2align 4,,10
	.p2align 3
.L17:
	movsd	(%rdx), %xmm0
	addq	$1, %rax
	addq	$16, %rdx
	addsd	(%rcx), %xmm0
	addq	$16, %rcx
	cmpq	%rax, %rsi
	addsd	%xmm0, %xmm1
	jg	.L17
	addq	$1, %r8
	addq	%r9, %rdi
	cmpq	%r8, %rsi
	jg	.L18
.L16:
	movapd	%xmm1, %xmm0
	ret
.LFE16:
	.size	CheckSum, .-CheckSum
	.p2align 4,,15
.globl BitReverse
	.type	BitReverse, @function
BitReverse:
.LFB20:
	xorl	%ecx, %ecx
	testq	%rdi, %rdi
	jle	.L24
	xorl	%edx, %edx
	xorl	%ecx, %ecx
	.p2align 4,,10
	.p2align 3
.L25:
	movq	%rsi, %rax
	addq	$1, %rdx
	sarq	%rsi
	andl	$1, %eax
	cmpq	%rdx, %rdi
	leaq	(%rax,%rcx,2), %rcx
	jg	.L25
.L24:
	movq	%rcx, %rax
	ret
.LFE20:
	.size	BitReverse, .-BitReverse
	.p2align 4,,15
.globl TwiddleOneCol
	.type	TwiddleOneCol, @function
TwiddleOneCol:
.LFB22:
	testq	%rsi, %rsi
	movq	%r9, %rax
	jle	.L31
	addq	%rsi, %rax
	cvtsi2sdq	%rdi, %xmm6
	imulq	%rdx, %rax
	xorl	%edi, %edi
	movq	%rax, %rdx
	leaq	1(%rax,%rax), %rax
	salq	$4, %rdx
	leaq	(%rcx,%rax,8), %rax
	leaq	(%rcx,%rdx), %r10
	movq	%r8, %rdx
	.p2align 4,,10
	.p2align 3
.L30:
	movapd	%xmm6, %xmm3
	addq	$1, %rdi
	movsd	(%r10), %xmm2
	addq	$16, %r10
	mulsd	(%rax), %xmm3
	addq	$16, %rax
	movsd	(%rdx), %xmm5
	movapd	%xmm2, %xmm0
	movsd	8(%rdx), %xmm4
	mulsd	%xmm5, %xmm0
	mulsd	%xmm4, %xmm2
	movapd	%xmm3, %xmm1
	mulsd	%xmm5, %xmm3
	mulsd	%xmm4, %xmm1
	addsd	%xmm3, %xmm2
	subsd	%xmm1, %xmm0
	movsd	%xmm2, 8(%rdx)
	movsd	%xmm0, (%rdx)
	addq	$16, %rdx
	cmpq	%rdi, %rsi
	jg	.L30
.L31:
	rep
	ret
.LFE22:
	.size	TwiddleOneCol, .-TwiddleOneCol
	.p2align 4,,15
.globl Scale
	.type	Scale, @function
Scale:
.LFB23:
	testq	%rdi, %rdi
	jle	.L36
	cvtsi2sdq	%rsi, %xmm1
	xorl	%eax, %eax
	.p2align 4,,10
	.p2align 3
.L35:
	movsd	(%rdx), %xmm0
	addq	$1, %rax
	divsd	%xmm1, %xmm0
	movsd	%xmm0, (%rdx)
	movsd	8(%rdx), %xmm0
	divsd	%xmm1, %xmm0
	movsd	%xmm0, 8(%rdx)
	addq	$16, %rdx
	cmpq	%rax, %rdi
	jg	.L35
.L36:
	rep
	ret
.LFE23:
	.size	Scale, .-Scale
	.p2align 4,,15
.globl Transpose
	.type	Transpose, @function
Transpose:
.LFB24:
	pushq	%r15
.LCFI6:
	movq	%rdi, %r10
	pushq	%r14
.LCFI7:
	pushq	%r13
.LCFI8:
	pushq	%r12
.LCFI9:
	pushq	%rbp
.LCFI10:
	pushq	%rbx
.LCFI11:
	subq	$296, %rsp
.LCFI12:
	movq	%rsi, -56(%rsp)
	movq	%r9, %rsi
	movq	%rcx, -72(%rsp)
	subq	%r8, %rsi
	movq	num_cache_lines(%rip), %r9
	movq	%rdx, -64(%rsp)
	leaq	(%rsi,%rsi), %rcx
	movq	%r8, -80(%rsp)
	movq	%rcx, %rdx
	movq	%rcx, %rax
	sarq	$63, %rdx
	idivq	%r9
	movq	%rsi, %rdx
	movq	%rax, %r11
	movq	%r9, %rax
	imulq	%r11, %rax
	leaq	1(%r11), %rdi
	cmpq	%rax, %rcx
	movq	%rsi, %rax
	cmovne	%rdi, %r11
	sarq	$63, %rdx
	idivq	%r11
	movq	P(%rip), %rdx
	movq	%r11, -24(%rsp)
	movq	%rdx, -48(%rsp)
	movq	%r10, %rdx
	sarq	$63, %rdx
	movq	%rax, %r8
	movq	%r10, %rax
	addq	352(%rsp), %r10
	idivq	-48(%rsp)
	movq	-72(%rsp), %rdx
	movq	%r10, -16(%rsp)
	addq	$1, %rdx
	cmpq	%rdx, -48(%rsp)
	movq	%rdx, -32(%rsp)
	movq	%rax, -88(%rsp)
	jle	.L40
	movq	%r10, %rcx
	movq	%rax, %rdx
	movq	-88(%rsp), %rbx
	imulq	-32(%rsp), %rdx
	imulq	%rax, %rcx
	salq	$4, %rbx
	movq	%rbx, 72(%rsp)
	movq	%r8, %rbx
	salq	$4, %rbx
	salq	$4, %rcx
	movq	%rbx, -112(%rsp)
	movq	%rcx, 64(%rsp)
	movq	%r10, %rcx
	imulq	%rdx, %rcx
	addq	-80(%rsp), %rcx
	movq	%rcx, %rax
	leaq	1(%rcx,%rcx), %rcx
	salq	$4, %rax
	addq	-56(%rsp), %rax
	movq	%rax, 272(%rsp)
	movq	%r10, %rax
	imulq	-80(%rsp), %rax
	addq	%rdx, %rax
	movq	%rax, %rdx
	leaq	1(%rax,%rax), %rax
	salq	$4, %rdx
	addq	-64(%rsp), %rdx
	movq	%rdx, 248(%rsp)
	movq	-56(%rsp), %rdx
	leaq	(%rdx,%rcx,8), %rcx
	movq	%rcx, 80(%rsp)
	movq	-64(%rsp), %rcx
	leaq	(%rcx,%rax,8), %rax
	movq	%rax, 288(%rsp)
	movq	%r10, %rax
	salq	$4, %r10
	imulq	%r8, %rax
	salq	$4, %rax
	movq	%rax, -120(%rsp)
.L48:
	cmpq	$0, -24(%rsp)
	jle	.L41
	movq	288(%rsp), %rdx
	movq	80(%rsp), %rcx
	movq	248(%rsp), %rbx
	movq	272(%rsp), %rax
	movq	$0, -40(%rsp)
	movq	%rdx, 264(%rsp)
	movq	%rcx, 192(%rsp)
	movq	%rbx, 56(%rsp)
	movq	%rax, 280(%rsp)
.L42:
	movq	280(%rsp), %rcx
	movq	56(%rsp), %rbx
	xorl	%r14d, %r14d
	movq	192(%rsp), %rax
	movq	264(%rsp), %r15
	movq	%rcx, 256(%rsp)
	movq	%rbx, 224(%rsp)
	movq	%rax, 48(%rsp)
.L47:
	testq	%r8, %r8
	jle	.L46
	movq	48(%rsp), %r12
	movq	224(%rsp), %rbp
	movq	%r15, %r13
	movq	256(%rsp), %rbx
	xorl	%r11d, %r11d
	.p2align 4,,10
	.p2align 3
.L44:
	movq	%rbx, %r9
	movq	%rbp, %rdi
	movq	%r12, %rsi
	movq	%r13, %rcx
	xorl	%edx, %edx
	.p2align 4,,10
	.p2align 3
.L43:
	movq	(%r9), %rax
	addq	$1, %rdx
	addq	$16, %r9
	movq	%rax, (%rdi)
	movq	(%rsi), %rax
	addq	%r10, %rdi
	addq	$16, %rsi
	movq	%rax, (%rcx)
	addq	%r10, %rcx
	cmpq	%rdx, %r8
	jg	.L43
	addq	$1, %r11
	addq	$16, %r13
	addq	%r10, %r12
	addq	$16, %rbp
	addq	%r10, %rbx
	cmpq	%r11, %r8
	jg	.L44
.L46:
	movq	-120(%rsp), %rcx
	movq	-112(%rsp), %rdx
	addq	$1, %r14
	addq	%rdx, 256(%rsp)
	addq	%rcx, 224(%rsp)
	addq	%rdx, 48(%rsp)
	addq	%rcx, %r15
	cmpq	%r14, -24(%rsp)
	jg	.L47
	addq	$1, -40(%rsp)
	movq	-112(%rsp), %rbx
	movq	-120(%rsp), %rax
	addq	%rbx, 264(%rsp)
	addq	%rax, 192(%rsp)
	movq	-40(%rsp), %rdx
	addq	%rbx, 56(%rsp)
	addq	%rax, 280(%rsp)
	cmpq	%rdx, -24(%rsp)
	jg	.L42
.L41:
	addq	$1, -32(%rsp)
	movq	64(%rsp), %rdx
	movq	72(%rsp), %rcx
	addq	%rdx, 272(%rsp)
	addq	%rcx, 248(%rsp)
	movq	-32(%rsp), %rbx
	addq	%rdx, 80(%rsp)
	addq	%rcx, 288(%rsp)
	cmpq	%rbx, -48(%rsp)
	jg	.L48
.L40:
	cmpq	$0, -72(%rsp)
	jle	.L49
	movq	-16(%rsp), %rax
	movq	-88(%rsp), %rdx
	imulq	-88(%rsp), %rax
	movq	-80(%rsp), %rcx
	movq	-56(%rsp), %rbx
	salq	$4, %rdx
	movq	-16(%rsp), %r10
	movq	$0, 104(%rsp)
	movq	%rdx, 40(%rsp)
	movq	-16(%rsp), %rdx
	imulq	-80(%rsp), %rdx
	salq	$4, %r10
	salq	$4, %rax
	movq	%rax, 32(%rsp)
	movq	-80(%rsp), %rax
	salq	$4, %rax
	addq	-56(%rsp), %rax
	movq	%rax, 216(%rsp)
	movq	%rdx, %rax
	leaq	1(%rdx,%rdx), %rdx
	salq	$4, %rax
	addq	-64(%rsp), %rax
	movq	%rax, 208(%rsp)
	leaq	1(%rcx,%rcx), %rax
	movq	-16(%rsp), %rcx
	leaq	(%rbx,%rax,8), %rax
	imulq	%r8, %rcx
	movq	%rax, 152(%rsp)
	movq	-64(%rsp), %rax
	leaq	(%rax,%rdx,8), %rdx
	salq	$4, %rcx
	movq	%rcx, -104(%rsp)
	movq	%rdx, 240(%rsp)
	movq	%r8, %rdx
	salq	$4, %rdx
	movq	%rdx, -96(%rsp)
.L57:
	cmpq	$0, -24(%rsp)
	jle	.L50
	movq	240(%rsp), %rbx
	movq	152(%rsp), %rax
	movq	208(%rsp), %rdx
	movq	216(%rsp), %rcx
	movq	$0, 96(%rsp)
	movq	%rbx, 200(%rsp)
	movq	%rax, 160(%rsp)
	movq	%rdx, 24(%rsp)
	movq	%rcx, 232(%rsp)
.L51:
	movq	232(%rsp), %rax
	movq	24(%rsp), %rdx
	xorl	%r14d, %r14d
	movq	160(%rsp), %rcx
	movq	200(%rsp), %r15
	movq	%rax, 184(%rsp)
	movq	%rdx, 136(%rsp)
	movq	%rcx, 16(%rsp)
.L56:
	testq	%r8, %r8
	jle	.L55
	movq	16(%rsp), %r12
	movq	136(%rsp), %rbp
	movq	%r15, %r13
	movq	184(%rsp), %rbx
	xorl	%r11d, %r11d
	.p2align 4,,10
	.p2align 3
.L53:
	movq	%rbx, %r9
	movq	%rbp, %rdi
	movq	%r12, %rsi
	movq	%r13, %rcx
	xorl	%edx, %edx
	.p2align 4,,10
	.p2align 3
.L52:
	movq	(%r9), %rax
	addq	$1, %rdx
	addq	$16, %r9
	movq	%rax, (%rdi)
	movq	(%rsi), %rax
	addq	%r10, %rdi
	addq	$16, %rsi
	movq	%rax, (%rcx)
	addq	%r10, %rcx
	cmpq	%rdx, %r8
	jg	.L52
	addq	$1, %r11
	addq	$16, %r13
	addq	%r10, %r12
	addq	$16, %rbp
	addq	%r10, %rbx
	cmpq	%r11, %r8
	jg	.L53
.L55:
	movq	-104(%rsp), %rax
	movq	-96(%rsp), %rbx
	addq	$1, %r14
	addq	%rbx, 184(%rsp)
	addq	%rax, 136(%rsp)
	addq	%rbx, 16(%rsp)
	addq	%rax, %r15
	cmpq	%r14, -24(%rsp)
	jg	.L56
	addq	$1, 96(%rsp)
	movq	-96(%rsp), %rdx
	movq	-104(%rsp), %rcx
	addq	%rdx, 200(%rsp)
	addq	%rcx, 160(%rsp)
	movq	96(%rsp), %rbx
	addq	%rdx, 24(%rsp)
	addq	%rcx, 232(%rsp)
	cmpq	%rbx, -24(%rsp)
	jg	.L51
.L50:
	addq	$1, 104(%rsp)
	movq	32(%rsp), %rbx
	movq	40(%rsp), %rax
	addq	%rbx, 216(%rsp)
	addq	%rax, 208(%rsp)
	movq	104(%rsp), %rdx
	addq	%rbx, 152(%rsp)
	addq	%rax, 240(%rsp)
	cmpq	%rdx, -72(%rsp)
	jg	.L57
.L49:
	cmpq	$0, -24(%rsp)
	jle	.L65
	movq	%r8, %rcx
	movq	-72(%rsp), %rax
	movq	-64(%rsp), %rbx
	salq	$4, %rcx
	movq	-16(%rsp), %r14
	movq	$0, 112(%rsp)
	imulq	-88(%rsp), %rax
	movq	%rcx, 8(%rsp)
	movq	-16(%rsp), %rcx
	imulq	-80(%rsp), %rcx
	imulq	%r8, %r14
	salq	$4, %r14
	addq	%rax, %rcx
	imulq	-16(%rsp), %rax
	leaq	1(%rcx,%rcx), %rdx
	salq	$4, %rcx
	addq	-64(%rsp), %rcx
	leaq	(%rbx,%rdx,8), %rdx
	movq	-56(%rsp), %rbx
	addq	-80(%rsp), %rax
	movq	%rdx, 168(%rsp)
	movq	%rcx, (%rsp)
	movq	%rbx, %r15
	leaq	1(%rax,%rax), %rdx
	salq	$4, %rax
	addq	%rax, %r15
	leaq	(%rbx,%rdx,8), %rdx
	movq	-16(%rsp), %rbx
	movq	%rdx, 120(%rsp)
	salq	$4, %rbx
.L59:
	movq	(%rsp), %rdx
	movq	120(%rsp), %rcx
	movq	168(%rsp), %rax
	movq	%r15, 144(%rsp)
	movq	$0, 128(%rsp)
	movq	%rdx, 88(%rsp)
	movq	%rcx, -8(%rsp)
	movq	%rax, 176(%rsp)
.L64:
	testq	%r8, %r8
	jle	.L63
	movq	176(%rsp), %r10
	movq	-8(%rsp), %rsi
	xorl	%ebp, %ebp
	movq	88(%rsp), %r13
	movq	144(%rsp), %r12
.L61:
	movq	%r12, %rcx
	movq	%r13, %rdx
	movq	%rsi, %r11
	movq	%r10, %r9
	xorl	%edi, %edi
	.p2align 4,,10
	.p2align 3
.L60:
	movq	(%rcx), %rax
	addq	$1, %rdi
	addq	$16, %rcx
	movq	%rax, (%rdx)
	movq	(%r11), %rax
	addq	%rbx, %rdx
	addq	$16, %r11
	movq	%rax, (%r9)
	addq	%rbx, %r9
	cmpq	%rdi, %r8
	jg	.L60
	addq	$1, %rbp
	addq	$16, %r10
	addq	%rbx, %rsi
	addq	$16, %r13
	addq	%rbx, %r12
	cmpq	%rbp, %r8
	jg	.L61
.L63:
	addq	$1, 128(%rsp)
	movq	8(%rsp), %rax
	addq	%r14, 88(%rsp)
	movq	128(%rsp), %rdx
	addq	%rax, 144(%rsp)
	addq	%rax, -8(%rsp)
	addq	%r14, 176(%rsp)
	cmpq	%rdx, -24(%rsp)
	jg	.L64
	addq	$1, 112(%rsp)
	movq	8(%rsp), %rcx
	addq	%r14, %r15
	addq	%rcx, 168(%rsp)
	movq	112(%rsp), %rax
	addq	%r14, 120(%rsp)
	addq	%rcx, (%rsp)
	cmpq	%rax, -24(%rsp)
	jg	.L59
.L65:
	addq	$296, %rsp
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
.LFE24:
	.size	Transpose, .-Transpose
	.p2align 4,,15
.globl CopyColumn
	.type	CopyColumn, @function
CopyColumn:
.LFB25:
	testq	%rdi, %rdi
	jle	.L77
	xorl	%r8d, %r8d
	xorl	%ecx, %ecx
	.p2align 4,,10
	.p2align 3
.L76:
	movq	(%rsi,%rcx), %rax
	addq	$1, %r8
	movq	%rax, (%rdx,%rcx)
	movq	8(%rsi,%rcx), %rax
	movq	%rax, 8(%rdx,%rcx)
	addq	$16, %rcx
	cmpq	%r8, %rdi
	jg	.L76
.L77:
	rep
	ret
.LFE25:
	.size	CopyColumn, .-CopyColumn
	.p2align 4,,15
.globl Reverse
	.type	Reverse, @function
Reverse:
.LFB26:
	testq	%rdi, %rdi
	movq	%rdx, %r11
	jle	.L85
	movq	%rdx, %r9
	xorl	%ecx, %ecx
	xorl	%r10d, %r10d
	.p2align 4,,10
	.p2align 3
.L81:
	xorl	%r8d, %r8d
	xorl	%edx, %edx
	testq	%rsi, %rsi
	jle	.L83
	.p2align 4,,10
	.p2align 3
.L86:
	movq	%rcx, %rax
	addq	$1, %rdx
	sarq	%rcx
	andl	$1, %eax
	cmpq	%rdx, %rsi
	leaq	(%rax,%r8,2), %r8
	jg	.L86
	cmpq	%r8, %r10
	jge	.L83
	movq	%r8, %rax
	movq	(%r9), %rdx
	salq	$4, %rax
	leaq	(%r11,%rax), %rax
	movq	(%rax), %rcx
	movq	%rdx, (%rax)
	leaq	1(%r8,%r8), %rax
	movq	8(%r9), %rdx
	leaq	(%r11,%rax,8), %rax
	movq	%rcx, (%r9)
	movq	(%rax), %rcx
	movq	%rdx, (%rax)
	movq	%rcx, 8(%r9)
.L83:
	leaq	1(%r10), %rcx
	addq	$16, %r9
	cmpq	%rcx, %rdi
	jle	.L85
	movq	%rcx, %r10
	jmp	.L81
.L85:
	rep
	ret
.LFE26:
	.size	Reverse, .-Reverse
	.p2align 4,,15
.globl FFT1DOnce
	.type	FFT1DOnce, @function
FFT1DOnce:
.LFB27:
	pushq	%r15
.LCFI13:
	movq	%rcx, %r15
	pushq	%r14
.LCFI14:
	movq	%rsi, %r14
	pushq	%r13
.LCFI15:
	movq	%r8, %r13
	pushq	%r12
.LCFI16:
	movq	%rdx, %r12
	movq	%r8, %rdx
	pushq	%rbp
.LCFI17:
	movq	%rdi, %rbp
	movq	%r12, %rdi
	pushq	%rbx
.LCFI18:
	call	Reverse
	testq	%r14, %r14
	jle	.L95
	movl	$1, %ebx
.L94:
	movl	%ebx, %ecx
	movl	$1, %eax
	movq	%r12, %rdx
	sall	%cl, %eax
	sarq	$63, %rdx
	movslq	%eax,%rcx
	movq	%r12, %rax
	idivq	%rcx
	movq	%rax, %r11
	movq	%rcx, %rax
	shrq	$63, %rax
	leaq	(%rax,%rcx), %rdi
	sarq	%rdi
	movq	%rdi, %rax
	salq	$4, %rax
	testq	%r11, %r11
	leaq	-16(%rax,%r15), %r8
	jle	.L90
	movq	%rdi, %rax
	movq	%rcx, %r10
	xorl	%r9d, %r9d
	salq	$4, %rax
	salq	$4, %r10
	movq	%r13, %rcx
	leaq	(%r13,%rax), %rsi
	.p2align 4,,10
	.p2align 3
.L93:
	testq	%rdi, %rdi
	jle	.L91
	cvtsi2sdq	%rbp, %xmm6
	xorl	%edx, %edx
	xorl	%eax, %eax
	.p2align 4,,10
	.p2align 3
.L92:
	movapd	%xmm6, %xmm1
	addq	$1, %rdx
	movsd	(%r8,%rax), %xmm2
	mulsd	8(%r8,%rax), %xmm1
	movsd	8(%rsi,%rax), %xmm3
	movapd	%xmm2, %xmm4
	movsd	(%rsi,%rax), %xmm5
	mulsd	%xmm3, %xmm2
	mulsd	%xmm5, %xmm4
	movapd	%xmm1, %xmm0
	mulsd	%xmm5, %xmm1
	mulsd	%xmm3, %xmm0
	movsd	8(%rcx,%rax), %xmm3
	addsd	%xmm1, %xmm2
	movsd	(%rcx,%rax), %xmm1
	subsd	%xmm0, %xmm4
	movapd	%xmm1, %xmm0
	subsd	%xmm4, %xmm0
	addsd	%xmm4, %xmm1
	movsd	%xmm0, (%rsi,%rax)
	movapd	%xmm3, %xmm0
	addsd	%xmm2, %xmm3
	subsd	%xmm2, %xmm0
	movsd	%xmm0, 8(%rsi,%rax)
	movsd	%xmm1, (%rcx,%rax)
	movsd	%xmm3, 8(%rcx,%rax)
	addq	$16, %rax
	cmpq	%rdx, %rdi
	jg	.L92
.L91:
	addq	$1, %r9
	addq	%r10, %rsi
	addq	%r10, %rcx
	cmpq	%r9, %r11
	jg	.L93
.L90:
	addq	$1, %rbx
	cmpq	%rbx, %r14
	jge	.L94
.L95:
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
.LFE27:
	.size	FFT1DOnce, .-FFT1DOnce
	.p2align 4,,15
.globl log_2
	.type	log_2, @function
log_2:
.LFB30:
	cmpq	$1, %rdi
	jle	.L100
	movl	$1, %edx
	xorl	%eax, %eax
	.p2align 4,,10
	.p2align 3
.L102:
	addq	%rdx, %rdx
	addq	$1, %rax
	cmpq	%rdx, %rdi
	jle	.L101
	cmpq	$50, %rax
	jne	.L102
.L103:
	movq	$-1, %rax
	ret
.L100:
	movl	$1, %edx
	xorl	%eax, %eax
.L101:
	cmpq	%rdx, %rdi
	.p2align 4,,2
	.p2align 3
	jne	.L103
	rep
	ret
.LFE30:
	.size	log_2, .-log_2
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC1:
	.string	"ERROR: %s\n"
	.text
	.p2align 4,,15
.globl printerr
	.type	printerr, @function
printerr:
.LFB29:
	movq	%rdi, %rdx
	movq	stderr(%rip), %rdi
	movl	$.LC1, %esi
	xorl	%eax, %eax
	jmp	fprintf
.LFE29:
	.size	printerr, .-printerr
	.section	.rodata.str1.1
.LC2:
	.string	" %4.2f %4.2f"
	.text
	.p2align 4,,15
.globl PrintArray
	.type	PrintArray, @function
PrintArray:
.LFB28:
	pushq	%r15
.LCFI19:
	movq	%rsi, %r15
	pushq	%r14
.LCFI20:
	pushq	%r13
.LCFI21:
	pushq	%r12
.LCFI22:
	pushq	%rbp
.LCFI23:
	pushq	%rbx
.LCFI24:
	subq	$8, %rsp
.LCFI25:
	movq	rootN(%rip), %rax
	testq	%rax, %rax
	jle	.L111
	leaq	-1(%rdi), %r14
	xorl	%r13d, %r13d
	.p2align 4,,10
	.p2align 3
.L116:
	movq	%rax, %rdx
	addq	pad_length(%rip), %rdx
	imulq	%r13, %rdx
	cmpq	$0, rootN(%rip)
	jle	.L112
	leaq	1(%rdx,%rdx), %rax
	salq	$4, %rdx
	xorl	%ebx, %ebx
	leaq	(%r15,%rdx), %rbp
	leaq	(%r15,%rax,8), %r12
	jmp	.L115
	.p2align 4,,10
	.p2align 3
.L114:
	addq	$1, %rbx
	addq	$16, %r12
	addq	$16, %rbp
	cmpq	%rbx, rootN(%rip)
	jle	.L120
.L115:
	movsd	(%r12), %xmm1
	movl	$.LC2, %edi
	movsd	(%rbp), %xmm0
	movl	$2, %eax
	call	printf
	movq	%r13, %rax
	imulq	rootN(%rip), %rax
	leaq	(%rbx,%rax), %rax
	cmpq	%rax, %r14
	je	.L113
	movl	$44, %edi
	call	putchar
.L113:
	movq	%r13, %rax
	imulq	rootN(%rip), %rax
	leaq	1(%rbx,%rax), %rax
	testb	$7, %al
	jne	.L114
	movl	$10, %edi
	call	putchar
	jmp	.L114
	.p2align 4,,10
	.p2align 3
.L120:
	movq	rootN(%rip), %rax
.L112:
	addq	$1, %r13
	cmpq	%r13, %rax
	jg	.L116
.L111:
	movl	$10, %edi
	call	putchar
	addq	$8, %rsp
	movl	$10, %edi
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	putchar
.LFE28:
	.size	PrintArray, .-PrintArray
	.section	.rodata.str1.8,"aMS",@progbits,1
	.align 8
.LC3:
	.string	"Error while trying to get lock in barrier."
	.text
	.p2align 4,,15
.globl FFT1D
	.type	FFT1D, @function
FFT1D:
.LFB21:
	pushq	%r15
.LCFI26:
	pushq	%r14
.LCFI27:
	pushq	%r13
.LCFI28:
	movq	%rdi, %r13
	pushq	%r12
.LCFI29:
	pushq	%rbp
.LCFI30:
	movq	%rsi, %rbp
	pushq	%rbx
.LCFI31:
	subq	$104, %rsp
.LCFI32:
	movq	Global(%rip), %rdi
	movq	%rdx, 48(%rsp)
	movq	%rcx, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%r9, 24(%rsp)
	addq	$48, %rdi
	call	pthread_mutex_lock
	testl	%eax, %eax
	jne	.L171
	movq	Global(%rip), %rdx
	movq	136(%rdx), %rax
	movq	144(%rdx), %rbx
	addq	$1, %rax
	cmpq	P(%rip), %rax
	movq	%rax, 136(%rdx)
	je	.L123
	leaq	88(%rsp), %rsi
	movl	$1, %edi
	call	pthread_setcancelstate
	.p2align 4,,10
	.p2align 3
.L124:
	movq	Global(%rip), %rax
	cmpq	%rbx, 144(%rax)
	jne	.L125
	leaq	48(%rax), %rsi
	leaq	88(%rax), %rdi
	call	pthread_cond_wait
	testl	%eax, %eax
	je	.L124
.L125:
	movl	88(%rsp), %edi
	leaq	92(%rsp), %rsi
	call	pthread_setcancelstate
.L127:
	movq	%rbp, %rax
	movq	Global(%rip), %rdi
	shrq	$63, %rax
	leaq	(%rax,%rbp), %r15
	movl	$1, %eax
	addq	$48, %rdi
	sarq	%r15
	movl	%r15d, %ecx
	sall	%cl, %eax
	movslq	%eax,%r14
	call	pthread_mutex_unlock
	cmpq	$0, 168(%rsp)
	sete	%dl
	cmpq	$0, 216(%rsp)
	setne	%al
	orb	%al, %dl
	movb	%dl, 63(%rsp)
	jne	.L172
	movq	200(%rsp), %rcx
	movq	192(%rsp), %r9
	movq	%r14, %rdi
	movq	184(%rsp), %r8
	movq	32(%rsp), %rdx
	movq	40(%rsp), %rsi
	movq	%rcx, (%rsp)
	movq	168(%rsp), %rcx
	call	Transpose
.L129:
	movq	192(%rsp), %rax
	cmpq	%rax, 184(%rsp)
	jge	.L130
	movq	200(%rsp), %rdx
	movq	184(%rsp), %rbx
	addq	%r14, %rdx
	leaq	(%rdx,%rdx), %rax
	movq	%rdx, %rbp
	movq	32(%rsp), %rdx
	imulq	184(%rsp), %rax
	salq	$4, %rbp
	leaq	(%rdx,%rax,8), %r12
	.p2align 4,,10
	.p2align 3
.L131:
	movq	24(%rsp), %rcx
	movq	%r12, %r8
	movq	%r14, %rdx
	movq	%r15, %rsi
	movq	%r13, %rdi
	call	FFT1DOnce
	movq	200(%rsp), %r9
	movq	160(%rsp), %rcx
	movq	%r12, %r8
	movq	%rbx, %rdx
	movq	%r14, %rsi
	movq	%r13, %rdi
	addq	$1, %rbx
	addq	%rbp, %r12
	call	TwiddleOneCol
	cmpq	%rbx, 192(%rsp)
	jg	.L131
.L130:
	movq	Global(%rip), %rdi
	addq	$48, %rdi
	call	pthread_mutex_lock
	testl	%eax, %eax
	jne	.L171
	movq	Global(%rip), %rdx
	movq	136(%rdx), %rax
	movq	144(%rdx), %rbx
	addq	$1, %rax
	cmpq	P(%rip), %rax
	movq	%rax, 136(%rdx)
	je	.L133
	leaq	92(%rsp), %rsi
	movl	$1, %edi
	call	pthread_setcancelstate
	.p2align 4,,10
	.p2align 3
.L134:
	movq	Global(%rip), %rax
	cmpq	%rbx, 144(%rax)
	jne	.L135
	leaq	48(%rax), %rsi
	leaq	88(%rax), %rdi
	call	pthread_cond_wait
	testl	%eax, %eax
	je	.L134
.L135:
	movl	92(%rsp), %edi
	leaq	88(%rsp), %rsi
	call	pthread_setcancelstate
.L137:
	movq	Global(%rip), %rdi
	addq	$48, %rdi
	call	pthread_mutex_unlock
	cmpb	$0, 63(%rsp)
	jne	.L173
	movq	200(%rsp), %rcx
	movq	192(%rsp), %r9
	movq	%r14, %rdi
	movq	184(%rsp), %r8
	movq	40(%rsp), %rdx
	movq	32(%rsp), %rsi
	movq	%rcx, (%rsp)
	movq	168(%rsp), %rcx
	call	Transpose
.L139:
	movq	192(%rsp), %rax
	cmpq	%rax, 184(%rsp)
	jge	.L140
	movq	200(%rsp), %rdx
	movq	184(%rsp), %rbx
	addq	%r14, %rdx
	leaq	(%rdx,%rdx), %rax
	movq	%rdx, %r12
	movq	40(%rsp), %rdx
	imulq	184(%rsp), %rax
	salq	$4, %r12
	leaq	(%rdx,%rax,8), %rbp
	jmp	.L142
	.p2align 4,,10
	.p2align 3
.L141:
	addq	$1, %rbx
	addq	%r12, %rbp
	cmpq	%rbx, 192(%rsp)
	jle	.L140
.L142:
	movq	24(%rsp), %rcx
	movq	%rbp, %r8
	movq	%r14, %rdx
	movq	%r15, %rsi
	movq	%r13, %rdi
	call	FFT1DOnce
	cmpq	$-1, %r13
	jne	.L141
	movq	48(%rsp), %rsi
	movq	%rbp, %rdx
	movq	%r14, %rdi
	call	Scale
	jmp	.L141
	.p2align 4,,10
	.p2align 3
.L140:
	movq	Global(%rip), %rdi
	addq	$48, %rdi
	call	pthread_mutex_lock
	testl	%eax, %eax
	jne	.L171
	movq	Global(%rip), %rdx
	movq	136(%rdx), %rax
	movq	144(%rdx), %rbx
	addq	$1, %rax
	cmpq	P(%rip), %rax
	movq	%rax, 136(%rdx)
	je	.L144
	leaq	88(%rsp), %rsi
	movl	$1, %edi
	call	pthread_setcancelstate
	.p2align 4,,10
	.p2align 3
.L145:
	movq	Global(%rip), %rax
	cmpq	%rbx, 144(%rax)
	jne	.L146
	leaq	48(%rax), %rsi
	leaq	88(%rax), %rdi
	call	pthread_cond_wait
	testl	%eax, %eax
	je	.L145
.L146:
	movl	88(%rsp), %edi
	leaq	92(%rsp), %rsi
	call	pthread_setcancelstate
.L148:
	movq	Global(%rip), %rdi
	addq	$48, %rdi
	call	pthread_mutex_unlock
	cmpb	$0, 63(%rsp)
	jne	.L174
	movq	200(%rsp), %rcx
	movq	192(%rsp), %r9
	movq	%r14, %rdi
	movq	184(%rsp), %r8
	movq	32(%rsp), %rdx
	movq	40(%rsp), %rsi
	movq	%rcx, (%rsp)
	movq	168(%rsp), %rcx
	call	Transpose
.L150:
	movq	Global(%rip), %rdi
	addq	$48, %rdi
	call	pthread_mutex_lock
	testl	%eax, %eax
	jne	.L171
	movq	Global(%rip), %rdx
	movq	136(%rdx), %rax
	movq	144(%rdx), %rbx
	addq	$1, %rax
	cmpq	P(%rip), %rax
	movq	%rax, 136(%rdx)
	je	.L152
	leaq	92(%rsp), %rsi
	movl	$1, %edi
	call	pthread_setcancelstate
	.p2align 4,,10
	.p2align 3
.L153:
	movq	Global(%rip), %rax
	cmpq	%rbx, 144(%rax)
	jne	.L154
	leaq	48(%rax), %rsi
	leaq	88(%rax), %rdi
	call	pthread_cond_wait
	testl	%eax, %eax
	je	.L153
.L154:
	movl	92(%rsp), %edi
	leaq	88(%rsp), %rsi
	call	pthread_setcancelstate
.L156:
	movq	Global(%rip), %rdi
	addq	$48, %rdi
	call	pthread_mutex_unlock
	cmpq	$0, 208(%rsp)
	je	.L175
.L157:
	movq	192(%rsp), %rax
	cmpq	%rax, 184(%rsp)
	jge	.L158
	movq	200(%rsp), %rdx
	movq	32(%rsp), %rbp
	movq	40(%rsp), %r12
	movq	184(%rsp), %rbx
	addq	%r14, %rdx
	leaq	(%rdx,%rdx), %rax
	movq	%rdx, %r13
	imulq	184(%rsp), %rax
	salq	$4, %r13
	salq	$3, %rax
	addq	%rax, %rbp
	addq	%rax, %r12
	.p2align 4,,10
	.p2align 3
.L159:
	movq	%r12, %rdx
	movq	%rbp, %rsi
	movq	%r14, %rdi
	call	CopyColumn
	addq	$1, %rbx
	addq	%r13, %rbp
	addq	%r13, %r12
	cmpq	%rbx, 192(%rsp)
	jg	.L159
.L158:
	movq	Global(%rip), %rdi
	addq	$48, %rdi
	call	pthread_mutex_lock
	testl	%eax, %eax
	jne	.L171
	movq	Global(%rip), %rdx
	movq	136(%rdx), %rax
	movq	144(%rdx), %rbx
	addq	$1, %rax
	cmpq	P(%rip), %rax
	movq	%rax, 136(%rdx)
	je	.L161
	leaq	88(%rsp), %rsi
	movl	$1, %edi
	call	pthread_setcancelstate
	.p2align 4,,10
	.p2align 3
.L162:
	movq	Global(%rip), %rax
	cmpq	%rbx, 144(%rax)
	jne	.L163
	leaq	48(%rax), %rsi
	leaq	88(%rax), %rdi
	call	pthread_cond_wait
	testl	%eax, %eax
	je	.L162
.L163:
	movl	88(%rsp), %edi
	leaq	92(%rsp), %rsi
	call	pthread_setcancelstate
.L165:
	movq	Global(%rip), %rdi
	addq	$48, %rdi
	call	pthread_mutex_unlock
	addq	$104, %rsp
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
	.p2align 4,,10
	.p2align 3
.L175:
	cmpq	$0, doprint(%rip)
	je	.L158
	jmp	.L157
	.p2align 4,,10
	.p2align 3
.L172:
	leaq	64(%rsp), %r12
	xorl	%esi, %esi
	movq	%r12, %rdi
	call	gettimeofday
	movq	64(%rsp), %r8
	movq	200(%rsp), %rax
	movq	%r14, %rdi
	movq	32(%rsp), %rdx
	movq	192(%rsp), %r9
	movq	168(%rsp), %rcx
	movq	40(%rsp), %rsi
	imulq	$1000000, %r8, %rbx
	movq	184(%rsp), %r8
	movq	%rax, (%rsp)
	addq	72(%rsp), %rbx
	call	Transpose
	movq	%r12, %rdi
	xorl	%esi, %esi
	call	gettimeofday
	movq	64(%rsp), %rdi
	movq	176(%rsp), %rdx
	imulq	$1000000, %rdi, %rax
	addq	72(%rsp), %rax
	subq	%rbx, %rax
	addq	%rax, (%rdx)
	jmp	.L129
	.p2align 4,,10
	.p2align 3
.L123:
	xorl	%eax, %eax
	cmpq	$0, 144(%rdx)
	leaq	88(%rdx), %rdi
	movq	$0, 136(%rdx)
	sete	%al
	movq	%rax, 144(%rdx)
	call	pthread_cond_broadcast
	jmp	.L127
	.p2align 4,,10
	.p2align 3
.L173:
	leaq	64(%rsp), %r12
	xorl	%esi, %esi
	movq	%r12, %rdi
	call	gettimeofday
	movq	64(%rsp), %rsi
	movq	200(%rsp), %rcx
	movq	%r14, %rdi
	movq	40(%rsp), %rdx
	movq	192(%rsp), %r9
	movq	184(%rsp), %r8
	imulq	$1000000, %rsi, %rbx
	movq	%rcx, (%rsp)
	movq	32(%rsp), %rsi
	movq	168(%rsp), %rcx
	addq	72(%rsp), %rbx
	call	Transpose
	xorl	%esi, %esi
	movq	%r12, %rdi
	call	gettimeofday
	movq	64(%rsp), %rcx
	movq	176(%rsp), %rdx
	imulq	$1000000, %rcx, %rax
	addq	72(%rsp), %rax
	subq	%rbx, %rax
	addq	%rax, (%rdx)
	jmp	.L139
	.p2align 4,,10
	.p2align 3
.L133:
	xorl	%eax, %eax
	cmpq	$0, 144(%rdx)
	leaq	88(%rdx), %rdi
	movq	$0, 136(%rdx)
	sete	%al
	movq	%rax, 144(%rdx)
	call	pthread_cond_broadcast
	jmp	.L137
	.p2align 4,,10
	.p2align 3
.L174:
	leaq	64(%rsp), %r12
	xorl	%esi, %esi
	movq	%r12, %rdi
	call	gettimeofday
	movq	64(%rsp), %rdx
	movq	200(%rsp), %rcx
	movq	%r14, %rdi
	movq	192(%rsp), %r9
	movq	184(%rsp), %r8
	movq	40(%rsp), %rsi
	imulq	$1000000, %rdx, %rbx
	movq	%rcx, (%rsp)
	movq	32(%rsp), %rdx
	movq	168(%rsp), %rcx
	addq	72(%rsp), %rbx
	call	Transpose
	xorl	%esi, %esi
	movq	%r12, %rdi
	call	gettimeofday
	movq	64(%rsp), %rax
	movq	176(%rsp), %rdx
	imulq	$1000000, %rax, %rax
	addq	72(%rsp), %rax
	subq	%rbx, %rax
	addq	%rax, (%rdx)
	jmp	.L150
	.p2align 4,,10
	.p2align 3
.L144:
	xorl	%eax, %eax
	cmpq	$0, 144(%rdx)
	leaq	88(%rdx), %rdi
	movq	$0, 136(%rdx)
	sete	%al
	movq	%rax, 144(%rdx)
	call	pthread_cond_broadcast
	jmp	.L148
	.p2align 4,,10
	.p2align 3
.L152:
	xorl	%eax, %eax
	cmpq	$0, 144(%rdx)
	leaq	88(%rdx), %rdi
	movq	$0, 136(%rdx)
	sete	%al
	movq	%rax, 144(%rdx)
	call	pthread_cond_broadcast
	jmp	.L156
	.p2align 4,,10
	.p2align 3
.L161:
	xorl	%eax, %eax
	cmpq	$0, 144(%rdx)
	leaq	88(%rdx), %rdi
	movq	$0, 136(%rdx)
	sete	%al
	movq	%rax, 144(%rdx)
	call	pthread_cond_broadcast
	jmp	.L165
.L171:
	movl	$.LC3, %edi
	call	puts
	movl	$-1, %edi
	call	exit
.LFE21:
	.size	FFT1D, .-FFT1D
	.p2align 4,,15
.globl InitU2
	.type	InitU2, @function
InitU2:
.LFB19:
	pushq	%r15
.LCFI33:
	pushq	%r14
.LCFI34:
	pushq	%r13
.LCFI35:
	movq	%rdx, %r13
	pushq	%r12
.LCFI36:
	pushq	%rbp
.LCFI37:
	pushq	%rbx
.LCFI38:
	subq	$56, %rsp
.LCFI39:
	testq	%rdx, %rdx
	jle	.L180
	cvtsi2sdq	%rdi, %xmm0
	movq	pad_length(%rip), %rax
	movq	%rsi, %r15
	xorl	%r14d, %r14d
	movq	%rax, 40(%rsp)
	movq	rootN(%rip), %rax
	addq	%rax, 40(%rsp)
	salq	$4, 40(%rsp)
	movsd	%xmm0, 24(%rsp)
	.p2align 4,,10
	.p2align 3
.L179:
	cvtsi2sdq	%r14, %xmm0
	leaq	8(%r15), %r12
	movq	%r15, %rbp
	xorl	%ebx, %ebx
	movsd	%xmm0, 32(%rsp)
	.p2align 4,,10
	.p2align 3
.L178:
	cvtsi2sdq	%rbx, %xmm1
	addq	$1, %rbx
	movsd	.LC4(%rip), %xmm0
	mulsd	%xmm1, %xmm0
	movsd	%xmm1, (%rsp)
	mulsd	32(%rsp), %xmm0
	divsd	24(%rsp), %xmm0
	call	cos
	movsd	(%rsp), %xmm1
	movsd	%xmm0, (%rbp)
	addq	$16, %rbp
	mulsd	.LC5(%rip), %xmm1
	mulsd	32(%rsp), %xmm1
	movapd	%xmm1, %xmm0
	divsd	24(%rsp), %xmm0
	call	sin
	movsd	%xmm0, (%r12)
	addq	$16, %r12
	cmpq	%rbx, %r13
	jg	.L178
	addq	$1, %r14
	addq	40(%rsp), %r15
	cmpq	%r14, %r13
	jg	.L179
.L180:
	addq	$56, %rsp
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
.LFE19:
	.size	InitU2, .-InitU2
	.p2align 4,,15
.globl InitU
	.type	InitU, @function
InitU:
.LFB18:
	pushq	%r15
.LCFI40:
	pushq	%r14
.LCFI41:
	movq	%rsi, %r14
	pushq	%r13
.LCFI42:
	pushq	%r12
.LCFI43:
	pushq	%rbp
.LCFI44:
	pushq	%rbx
.LCFI45:
	subq	$56, %rsp
.LCFI46:
	movq	%rdi, 24(%rsp)
	subq	$1, %rdi
	jle	.L189
	movq	rootN(%rip), %r15
	movq	$0, 32(%rsp)
	movl	$1, %r13d
.L188:
	testq	%r13, %r13
	jle	.L185
	leaq	-1(%r13), %r12
	cmpq	%r15, %r12
	jge	.L189
	leaq	(%r13,%r13), %rax
	movq	%r13, %rbp
	xorl	%ebx, %ebx
	cvtsi2sdq	%rax, %xmm0
	movsd	%xmm0, 40(%rsp)
	jmp	.L186
	.p2align 4,,10
	.p2align 3
.L187:
	movq	%rbp, %r12
	addq	$1, %rbp
	leaq	-1(%rbp), %rax
	cmpq	%r15, %rax
	jge	.L189
.L186:
	cvtsi2sdq	%rbx, %xmm1
	addq	$1, %rbx
	movsd	.LC4(%rip), %xmm0
	mulsd	%xmm1, %xmm0
	movsd	%xmm1, (%rsp)
	divsd	40(%rsp), %xmm0
	call	cos
	movsd	(%rsp), %xmm1
	movq	%r12, %rax
	salq	$4, %rax
	mulsd	.LC5(%rip), %xmm1
	movsd	%xmm0, (%r14,%rax)
	movapd	%xmm1, %xmm0
	divsd	40(%rsp), %xmm0
	call	sin
	leaq	1(%r12,%r12), %rax
	cmpq	%rbx, %r13
	movsd	%xmm0, (%r14,%rax,8)
	jg	.L187
.L185:
	addq	$1, 32(%rsp)
	movl	$1, %eax
	movzbl	32(%rsp), %ecx
	sall	%cl, %eax
	movslq	%eax,%r13
	cmpq	24(%rsp), %r13
	jl	.L188
	.p2align 4,,10
	.p2align 3
.L189:
	addq	$56, %rsp
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
.LFE18:
	.size	InitU, .-InitU
	.p2align 4,,15
.globl InitX
	.type	InitX, @function
InitX:
.LFB17:
	pushq	%r14
.LCFI47:
	movq	%rdi, %r14
	xorl	%edi, %edi
	pushq	%r13
.LCFI48:
	pushq	%r12
.LCFI49:
	pushq	%rbp
.LCFI50:
	pushq	%rbx
.LCFI51:
	call	srand48
	movq	rootN(%rip), %rax
	testq	%rax, %rax
	jle	.L196
	xorl	%r13d, %r13d
	.p2align 4,,10
	.p2align 3
.L195:
	movq	%rax, %rdx
	addq	pad_length(%rip), %rdx
	imulq	%r13, %rdx
	cmpq	$0, rootN(%rip)
	jle	.L193
	movq	%rdx, %rax
	xorl	%ebx, %ebx
	salq	$4, %rax
	leaq	(%r14,%rax), %r12
	leaq	1(%rdx,%rdx), %rax
	leaq	(%r14,%rax,8), %rbp
	.p2align 4,,10
	.p2align 3
.L194:
	call	drand48
	movsd	%xmm0, (%r12)
	addq	$1, %rbx
	addq	$16, %r12
	call	drand48
	movsd	%xmm0, (%rbp)
	addq	$16, %rbp
	cmpq	%rbx, rootN(%rip)
	jg	.L194
	movq	rootN(%rip), %rax
.L193:
	addq	$1, %r13
	cmpq	%r13, %rax
	jg	.L195
.L196:
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	ret
.LFE17:
	.size	InitX, .-InitX
	.section	.rodata.str1.8
	.align 8
.LC6:
	.string	"Proc %ld could not malloc memory for upriv\n"
	.text
	.p2align 4,,15
.globl SlaveStart
	.type	SlaveStart, @function
SlaveStart:
.LFB14:
	pushq	%r15
.LCFI52:
	pushq	%r14
.LCFI53:
	pushq	%r13
.LCFI54:
	pushq	%r12
.LCFI55:
	pushq	%rbp
.LCFI56:
	pushq	%rbx
.LCFI57:
	subq	$104, %rsp
.LCFI58:
	movq	Global(%rip), %rdi
	movq	$0, 80(%rsp)
	addq	$8, %rdi
	call	pthread_mutex_lock
	movq	Global(%rip), %rdi
	movq	(%rdi), %r12
	leaq	1(%r12), %rbp
	movq	%rbp, (%rdi)
	addq	$8, %rdi
	call	pthread_mutex_unlock
	movq	Global(%rip), %rdi
	addq	$48, %rdi
	call	pthread_mutex_lock
	testl	%eax, %eax
	jne	.L223
	movq	Global(%rip), %rdx
	movq	136(%rdx), %rax
	movq	144(%rdx), %rbx
	addq	$1, %rax
	cmpq	P(%rip), %rax
	movq	%rax, 136(%rdx)
	je	.L201
	leaq	92(%rsp), %rsi
	movl	$1, %edi
	call	pthread_setcancelstate
	.p2align 4,,10
	.p2align 3
.L202:
	movq	Global(%rip), %rax
	cmpq	%rbx, 144(%rax)
	jne	.L203
	leaq	48(%rax), %rsi
	leaq	88(%rax), %rdi
	call	pthread_cond_wait
	testl	%eax, %eax
	je	.L202
.L203:
	movl	92(%rsp), %edi
	leaq	88(%rsp), %rsi
	call	pthread_setcancelstate
.L205:
	movq	Global(%rip), %rdi
	addq	$48, %rdi
	call	pthread_mutex_unlock
	movq	rootN(%rip), %rdi
	subq	$1, %rdi
	salq	$4, %rdi
	call	malloc
	testq	%rax, %rax
	movq	%rax, %rbx
	je	.L206
	movq	rootN(%rip), %rdi
	cmpq	$1, %rdi
	jle	.L208
	movq	umain(%rip), %rsi
	leaq	-2(%rdi,%rdi), %rcx
	xorl	%edx, %edx
	.p2align 4,,10
	.p2align 3
.L209:
	movq	(%rsi,%rdx,8), %rax
	movq	%rax, (%rbx,%rdx,8)
	addq	$1, %rdx
	cmpq	%rdx, %rcx
	jg	.L209
.L208:
	movq	%rdi, %rdx
	movq	P(%rip), %rcx
	movq	trans(%rip), %rsi
	imulq	%r12, %rdx
	imulq	%rbp, %rdi
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	%rcx
	movq	%rdi, %rdx
	sarq	$63, %rdx
	movq	%rax, %r14
	movq	%rdi, %rax
	movq	x(%rip), %rdi
	idivq	%rcx
	movq	umain2(%rip), %rdx
	movq	%r14, %r8
	movq	%rbx, %rcx
	movq	%rax, %r9
	movq	%rax, %r13
	call	TouchArray
	movq	Global(%rip), %rdi
	addq	$48, %rdi
	call	pthread_mutex_lock
	testl	%eax, %eax
	jne	.L223
	movq	Global(%rip), %rdx
	movq	136(%rdx), %rax
	movq	144(%rdx), %rbp
	addq	$1, %rax
	cmpq	P(%rip), %rax
	movq	%rax, 136(%rdx)
	je	.L211
	leaq	88(%rsp), %rsi
	movl	$1, %edi
	call	pthread_setcancelstate
	.p2align 4,,10
	.p2align 3
.L212:
	movq	Global(%rip), %rax
	cmpq	%rbp, 144(%rax)
	jne	.L213
	leaq	48(%rax), %rsi
	leaq	88(%rax), %rdi
	call	pthread_cond_wait
	testl	%eax, %eax
	je	.L212
.L213:
	movl	88(%rsp), %edi
	leaq	92(%rsp), %rsi
	call	pthread_setcancelstate
.L215:
	movq	Global(%rip), %rdi
	addq	$48, %rdi
	call	pthread_mutex_unlock
	call	CarbonEnableModels
	testq	%r12, %r12
	je	.L216
	cmpq	$0, dostats(%rip)
	jne	.L216
.L217:
	movq	dostats(%rip), %rax
	movq	N(%rip), %rdx
	leaq	80(%rsp), %rbp
	movq	trans(%rip), %r8
	movq	x(%rip), %rcx
	movq	%rbx, %r9
	movq	M(%rip), %rsi
	movl	$1, %edi
	movq	%r13, 32(%rsp)
	movq	%rax, 56(%rsp)
	movq	test_result(%rip), %rax
	movq	%r14, 24(%rsp)
	movq	%rbp, 16(%rsp)
	movq	%r12, 8(%rsp)
	movq	%rax, 48(%rsp)
	movq	pad_length(%rip), %rax
	movq	%rax, 40(%rsp)
	movq	umain2(%rip), %rax
	movq	%rax, (%rsp)
	call	FFT1D
	movq	test_result(%rip), %rdx
	testq	%rdx, %rdx
	jne	.L224
.L218:
	testq	%r12, %r12
	je	.L219
	cmpq	$0, dostats(%rip)
	je	.L221
.L219:
	leaq	64(%rsp), %rdi
	xorl	%esi, %esi
	call	gettimeofday
	movq	64(%rsp), %r9
	movq	Global(%rip), %rcx
	movq	80(%rsp), %rax
	imulq	$1000000, %r9, %rsi
	movq	152(%rcx), %rdx
	addq	72(%rsp), %rsi
	movq	%rax, (%rdx,%r12,8)
	movq	160(%rcx), %rdx
	movq	%rsi, %rax
	subq	%r15, %rax
	testq	%r12, %r12
	movq	%rax, (%rdx,%r12,8)
	jne	.L221
	movq	%rsi, 176(%rcx)
	movq	%r15, 184(%rcx)
.L221:
	addq	$104, %rsp
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
	.p2align 4,,10
	.p2align 3
.L216:
	leaq	64(%rsp), %rdi
	xorl	%esi, %esi
	call	gettimeofday
	movq	64(%rsp), %r10
	imulq	$1000000, %r10, %r15
	addq	72(%rsp), %r15
	jmp	.L217
	.p2align 4,,10
	.p2align 3
.L201:
	xorl	%eax, %eax
	cmpq	$0, 144(%rdx)
	leaq	88(%rdx), %rdi
	movq	$0, 136(%rdx)
	sete	%al
	movq	%rax, 144(%rdx)
	call	pthread_cond_broadcast
	jmp	.L205
	.p2align 4,,10
	.p2align 3
.L224:
	movq	dostats(%rip), %rax
	movq	%rdx, 48(%rsp)
	movq	%rbx, %r9
	movq	trans(%rip), %r8
	movq	x(%rip), %rcx
	movq	$-1, %rdi
	movq	N(%rip), %rdx
	movq	M(%rip), %rsi
	movq	%rax, 56(%rsp)
	movq	pad_length(%rip), %rax
	movq	%r13, 32(%rsp)
	movq	%r14, 24(%rsp)
	movq	%rbp, 16(%rsp)
	movq	%r12, 8(%rsp)
	movq	%rax, 40(%rsp)
	movq	umain2(%rip), %rax
	movq	%rax, (%rsp)
	call	FFT1D
	jmp	.L218
	.p2align 4,,10
	.p2align 3
.L211:
	xorl	%eax, %eax
	cmpq	$0, 144(%rdx)
	leaq	88(%rdx), %rdi
	movq	$0, 136(%rdx)
	sete	%al
	movq	%rax, 144(%rdx)
	call	pthread_cond_broadcast
	jmp	.L215
.L223:
	movl	$.LC3, %edi
	call	puts
	movl	$-1, %edi
	call	exit
.L206:
	movq	stderr(%rip), %rdi
	movq	%r12, %rdx
	movl	$.LC6, %esi
	xorl	%eax, %eax
	call	fprintf
	movl	$-1, %edi
	call	exit
.LFE14:
	.size	SlaveStart, .-SlaveStart
	.section	.rodata.str1.1
.LC7:
	.string	"P must be >= 1\n"
.LC8:
	.string	"M must be even\n"
	.section	.rodata.str1.8
	.align 8
.LC9:
	.string	"Number of cache lines must be >= 1\n"
	.align 8
.LC10:
	.string	"Log base 2 of cache line length in bytes must be >= 0\n"
	.section	.rodata.str1.1
.LC11:
	.string	"Usage: FFT <options>\n"
.LC12:
	.string	"options:"
	.section	.rodata.str1.8
	.align 8
.LC13:
	.string	"  -mM : M = even integer; 2**M total complex data points transformed."
	.align 8
.LC14:
	.string	"  -pP : P = number of processors; Must be a power of 2."
	.align 8
.LC15:
	.string	"  -nN : N = number of cache lines."
	.align 8
.LC16:
	.string	"  -lL : L = Log base 2 of cache line length in bytes."
	.align 8
.LC17:
	.string	"  -s  : Print individual processor timing statistics."
	.align 8
.LC18:
	.string	"  -t  : Perform FFT and inverse FFT.  Test output by comparing the"
	.align 8
.LC19:
	.string	"        integral of the original data to the integral of the data that"
	.align 8
.LC20:
	.string	"        results from performing the FFT and inverse FFT."
	.align 8
.LC21:
	.string	"  -o  : Print out complex data points."
	.align 8
.LC22:
	.string	"  -h  : Print out command line options.\n"
	.align 8
.LC23:
	.string	"Default: FFT -m%1d -p%1d -n%1d -l%1d\n"
	.section	.rodata.str1.1
.LC24:
	.string	"p:m:n:l:stoh"
	.section	.rodata.str1.8
	.align 8
.LC25:
	.string	"Matrix not large enough. 2**(M/2) must be >= P\n"
	.align 8
.LC26:
	.string	"WARNING: Each element is a complex double (%ld bytes)\n"
	.align 8
.LC27:
	.string	"  => Less than one element per cache line"
	.align 8
.LC28:
	.string	"     Computing transpose blocking factor"
	.align 8
.LC29:
	.string	"Padding algorithm unsuccessful\n"
	.align 8
.LC30:
	.string	"Could not malloc memory for Global\n"
	.align 8
.LC31:
	.string	"Could not malloc memory for x\n"
	.align 8
.LC32:
	.string	"Could not malloc memory for trans\n"
	.align 8
.LC33:
	.string	"Could not malloc memory for umain\n"
	.align 8
.LC34:
	.string	"Could not malloc memory for umain2\n"
	.section	.rodata.str1.1
.LC35:
	.string	"FFT with Blocking Transpose"
.LC36:
	.string	"   %ld Complex Doubles\n"
.LC37:
	.string	"   %ld Processors\n"
.LC38:
	.string	"   %ld Cache lines\n"
	.section	.rodata.str1.8
	.align 8
.LC39:
	.string	"   %ld Cache lines for blocking transpose\n"
	.section	.rodata.str1.1
.LC40:
	.string	"   %d Byte line size\n"
.LC41:
	.string	"   %d Bytes per page\n"
	.section	.rodata.str1.8
	.align 8
.LC42:
	.string	"Error while initializing barrier."
	.section	.rodata.str1.1
.LC43:
	.string	"Original data values:"
.LC44:
	.string	"Error in pthread_create()."
.LC45:
	.string	"Error in pthread_join()."
	.section	.rodata.str1.8
	.align 8
.LC46:
	.string	"Data values after inverse FFT:"
	.section	.rodata.str1.1
.LC47:
	.string	"Data values after FFT:"
	.section	.rodata.str1.8
	.align 8
.LC48:
	.string	"                 PROCESS STATISTICS"
	.align 8
.LC49:
	.string	"            Computation      Transpose     Transpose"
	.align 8
.LC50:
	.string	" Proc          Time            Time        Fraction"
	.align 8
.LC51:
	.string	"    0        %10ld     %10ld      %8.5f\n"
	.align 8
.LC52:
	.string	"  %3ld        %10ld     %10ld      %8.5f\n"
	.align 8
.LC53:
	.string	"  Avg        %10.0f     %10.0f      %8.5f\n"
	.align 8
.LC54:
	.string	"  Max        %10ld     %10ld      %8.5f\n"
	.align 8
.LC55:
	.string	"  Min        %10ld     %10ld      %8.5f\n"
	.align 8
.LC56:
	.string	"                 TIMING INFORMATION"
	.align 8
.LC57:
	.string	"Start time                        : %16lu\n"
	.align 8
.LC58:
	.string	"Initialization finish time        : %16lu\n"
	.align 8
.LC59:
	.string	"Overall finish time               : %16lu\n"
	.align 8
.LC60:
	.string	"Total time with initialization    : %16lu\n"
	.align 8
.LC61:
	.string	"Total time without initialization : %16lu\n"
	.align 8
.LC62:
	.string	"Overall transpose time            : %16ld\n"
	.align 8
.LC63:
	.string	"Overall transpose fraction        : %16.5f\n"
	.align 8
.LC64:
	.string	"              INVERSE FFT TEST RESULTS"
	.align 8
.LC65:
	.string	"Checksum difference is %.3f (%.3f, %.3f)\n"
	.section	.rodata.str1.1
.LC68:
	.string	"TEST PASSED"
.LC69:
	.string	"TEST FAILED"
.LC70:
	.string	"P must be a power of 2\n"
	.text
	.p2align 4,,15
.globl main
	.type	main, @function
main:
.LFB13:
	pushq	%r14
.LCFI59:
	pushq	%r13
.LCFI60:
	pushq	%r12
.LCFI61:
	movq	%rsi, %r12
	xorl	%esi, %esi
	pushq	%rbp
.LCFI62:
	movl	%edi, %ebp
	pushq	%rbx
.LCFI63:
	subq	$16, %rsp
.LCFI64:
	movq	%rsp, %rdi
	call	gettimeofday
	movq	8(%rsp), %r14
	movq	(%rsp), %r13
	.p2align 4,,10
	.p2align 3
.L296:
	xorl	%eax, %eax
	movl	$.LC24, %edx
	movq	%r12, %rsi
	movl	%ebp, %edi
	call	getopt
	cmpl	$-1, %eax
	movl	%eax, %ebx
	je	.L301
	leal	-104(%rbx), %eax
	cmpl	$12, %eax
	ja	.L296
	mov	%eax, %eax
	jmp	*.L235(,%rax,8)
	.section	.rodata
	.align 8
	.align 4
.L235:
	.quad	.L227
	.quad	.L296
	.quad	.L296
	.quad	.L296
	.quad	.L228
	.quad	.L229
	.quad	.L230
	.quad	.L231
	.quad	.L232
	.quad	.L296
	.quad	.L296
	.quad	.L233
	.quad	.L234
	.text
	.p2align 4,,10
	.p2align 3
.L234:
	xorl	%eax, %eax
	cmpq	$0, test_result(%rip)
	sete	%al
	movq	%rax, test_result(%rip)
	jmp	.L296
	.p2align 4,,10
	.p2align 3
.L233:
	xorl	%eax, %eax
	cmpq	$0, dostats(%rip)
	sete	%al
	movq	%rax, dostats(%rip)
	jmp	.L296
	.p2align 4,,10
	.p2align 3
.L232:
	movq	optarg(%rip), %rdi
	xorl	%esi, %esi
	movl	$10, %edx
	call	strtol
	cltq
	testq	%rax, %rax
	movq	%rax, P(%rip)
	jle	.L236
	xorl	%edx, %edx
	cmpq	$1, %rax
	movl	$1, %ecx
	je	.L296
	.p2align 4,,10
	.p2align 3
.L240:
	addq	%rcx, %rcx
	cmpq	%rcx, %rax
	jle	.L302
	addq	$1, %rdx
	cmpq	$50, %rdx
	jne	.L240
.L239:
	movl	$.LC70, %edx
.L300:
	movq	stderr(%rip), %rdi
	movl	$.LC1, %esi
	xorl	%eax, %eax
	call	fprintf
	movl	$-1, %edi
	call	exit
	.p2align 4,,10
	.p2align 3
.L231:
	xorl	%eax, %eax
	cmpq	$0, doprint(%rip)
	sete	%al
	movq	%rax, doprint(%rip)
	jmp	.L296
	.p2align 4,,10
	.p2align 3
.L230:
	movq	optarg(%rip), %rdi
	xorl	%esi, %esi
	movl	$10, %edx
	call	strtol
	cltq
	testq	%rax, %rax
	movq	%rax, num_cache_lines(%rip)
	movq	%rax, orig_num_lines(%rip)
	jg	.L296
	movl	$.LC9, %edx
	jmp	.L300
	.p2align 4,,10
	.p2align 3
.L229:
	movq	optarg(%rip), %rdi
	movl	$10, %edx
	xorl	%esi, %esi
	call	strtol
	cltq
	movq	%rax, %rdx
	movq	%rax, M(%rip)
	shrq	$63, %rdx
	addq	%rax, %rdx
	andq	$-2, %rdx
	cmpq	%rdx, %rax
	je	.L296
	movl	$.LC8, %edx
	jmp	.L300
	.p2align 4,,10
	.p2align 3
.L228:
	movq	optarg(%rip), %rdi
	xorl	%esi, %esi
	movl	$10, %edx
	call	strtol
	cltq
	testq	%rax, %rax
	movq	%rax, log2_line_size(%rip)
	jns	.L296
	movl	$.LC10, %edx
	jmp	.L300
	.p2align 4,,10
	.p2align 3
.L227:
	movl	$.LC11, %edi
	call	puts
	movl	$.LC12, %edi
	call	puts
	movl	$.LC13, %edi
	call	puts
	movl	$.LC14, %edi
	call	puts
	movl	$.LC15, %edi
	call	puts
	movl	$.LC16, %edi
	call	puts
	movl	$.LC17, %edi
	call	puts
	movl	$.LC18, %edi
	call	puts
	movl	$.LC19, %edi
	call	puts
	movl	$.LC20, %edi
	call	puts
	movl	$.LC21, %edi
	call	puts
	movl	$.LC22, %edi
	call	puts
	movl	$.LC23, %edi
	movl	$4, %r8d
	movl	$65536, %ecx
	movl	$1, %edx
	movl	$10, %esi
	xorl	%eax, %eax
	call	printf
	xorl	%edi, %edi
	call	exit
	.p2align 4,,10
	.p2align 3
.L301:
	movq	M(%rip), %rdx
	movl	$1, %esi
	movl	%esi, %eax
	movl	%edx, %ecx
	sall	%cl, %eax
	movq	%rdx, %rcx
	shrq	$63, %rcx
	cltq
	addq	%rdx, %rcx
	movq	%rax, N(%rip)
	movl	%esi, %eax
	shrq	%rcx
	sall	%cl, %eax
	movslq	%eax,%rdx
	movq	%rdx, %rax
	movq	%rdx, rootN(%rip)
	sarq	$63, %rdx
	idivq	P(%rip)
	testq	%rax, %rax
	movq	%rax, rowsperproc(%rip)
	je	.L303
	movzbl	log2_line_size(%rip), %ecx
	sall	%cl, %esi
	movslq	%esi,%rax
	cmpq	$15, %rax
	movq	%rax, line_size(%rip)
	jbe	.L304
.L243:
	movq	line_size(%rip), %rax
	cmpq	$16, %rax
	ja	.L244
	movq	$1, pad_length(%rip)
.L245:
	movq	rowsperproc(%rip), %rdi
	movq	%rdi, %rcx
	imulq	rootN(%rip), %rcx
	movq	%rcx, %rax
	salq	$4, %rax
	cmpq	$4095, %rax
	jbe	.L246
	movq	%rdi, %rax
	imulq	pad_length(%rip), %rax
	salq	$4, %rax
	movq	%rax, %rsi
	shrq	$12, %rsi
	movq	%rsi, %rcx
	salq	$12, %rcx
	cmpq	%rcx, %rax
	je	.L247
	leaq	1(%rsi), %rcx
	salq	$12, %rcx
.L247:
	salq	$4, %rdi
	movq	%rcx, %rax
	xorl	%edx, %edx
	divq	%rdi
	movq	%rax, pad_length(%rip)
.L248:
	movl	$192, %edi
	call	malloc
	movq	pad_length(%rip), %rdi
	movq	%rax, Global(%rip)
	imulq	rootN(%rip), %rdi
	addq	N(%rip), %rdi
	addq	$256, %rdi
	salq	$4, %rdi
	call	malloc
	movq	pad_length(%rip), %rdi
	movq	%rax, x(%rip)
	imulq	rootN(%rip), %rdi
	addq	N(%rip), %rdi
	addq	$256, %rdi
	salq	$4, %rdi
	call	malloc
	movq	rootN(%rip), %rdi
	movq	%rax, trans(%rip)
	salq	$4, %rdi
	call	malloc
	movq	pad_length(%rip), %rdi
	movq	%rax, umain(%rip)
	imulq	rootN(%rip), %rdi
	addq	N(%rip), %rdi
	addq	$256, %rdi
	salq	$4, %rdi
	call	malloc
	movq	P(%rip), %rdi
	movq	Global(%rip), %rbx
	movq	%rax, umain2(%rip)
	salq	$3, %rdi
	call	malloc
	movq	P(%rip), %rdi
	movq	%rax, 152(%rbx)
	movq	Global(%rip), %rbx
	salq	$3, %rdi
	call	malloc
	cmpq	$0, Global(%rip)
	movq	%rax, 160(%rbx)
	movl	$.LC30, %edx
	je	.L300
	movq	x(%rip), %rax
	testq	%rax, %rax
	je	.L305
	cmpq	$0, trans(%rip)
	movl	$.LC32, %edx
	je	.L300
	cmpq	$0, umain(%rip)
	movl	$.LC33, %edx
	je	.L300
	cmpq	$0, umain2(%rip)
	movl	$.LC34, %edx
	je	.L300
	andq	$-4096, %rax
	movl	$10, %edi
	addq	$4096, %rax
	movq	%rax, x(%rip)
	movq	trans(%rip), %rax
	andq	$-4096, %rax
	addq	$4096, %rax
	movq	%rax, trans(%rip)
	movq	umain2(%rip), %rax
	andq	$-4096, %rax
	addq	$4096, %rax
	movq	%rax, umain2(%rip)
	call	putchar
	movl	$.LC35, %edi
	call	puts
	movq	N(%rip), %rsi
	movl	$.LC36, %edi
	xorl	%eax, %eax
	call	printf
	movq	P(%rip), %rsi
	xorl	%eax, %eax
	movl	$.LC37, %edi
	call	printf
	movq	num_cache_lines(%rip), %rax
	movq	orig_num_lines(%rip), %rsi
	cmpq	%rsi, %rax
	je	.L254
	movl	$.LC38, %edi
	xorl	%eax, %eax
	call	printf
	movq	num_cache_lines(%rip), %rsi
	movl	$.LC39, %edi
	xorl	%eax, %eax
	call	printf
.L255:
	movzbl	log2_line_size(%rip), %ecx
	movl	$1, %esi
	movl	$.LC40, %edi
	xorl	%eax, %eax
	sall	%cl, %esi
	call	printf
	movl	$4096, %esi
	movl	$.LC41, %edi
	xorl	%eax, %eax
	call	printf
	movl	$10, %edi
	call	putchar
	movq	Global(%rip), %rdi
	xorl	%esi, %esi
	addq	$48, %rdi
	call	pthread_mutex_init
	testl	%eax, %eax
	jne	.L306
	movq	Global(%rip), %rdi
	xorl	%esi, %esi
	addq	$88, %rdi
	call	pthread_cond_init
	testl	%eax, %eax
	jne	.L307
	movq	Global(%rip), %rdi
	xorl	%esi, %esi
	movq	$0, 136(%rdi)
	movq	$0, 144(%rdi)
	addq	$8, %rdi
	call	pthread_mutex_init
	movq	Global(%rip), %rax
	movq	x(%rip), %rdi
	movq	$0, (%rax)
	call	InitX
	cmpq	$0, test_result(%rip)
	jne	.L308
.L258:
	cmpq	$0, doprint(%rip)
	jne	.L309
.L259:
	movq	umain(%rip), %rsi
	movq	N(%rip), %rdi
	call	InitU
	movq	rootN(%rip), %rdx
	movq	umain2(%rip), %rsi
	movq	N(%rip), %rdi
	call	InitU2
	cmpq	$1, P(%rip)
	jle	.L260
	movl	$PThreadTable, %ebp
	xorl	%ebx, %ebx
	.p2align 4,,10
	.p2align 3
.L262:
	xorl	%ecx, %ecx
	xorl	%esi, %esi
	movl	$SlaveStart, %edx
	movq	%rbp, %rdi
	call	pthread_create
	testl	%eax, %eax
	jne	.L310
	movq	P(%rip), %rax
	addq	$1, %rbx
	addq	$8, %rbp
	subq	$1, %rax
	cmpq	%rbx, %rax
	jg	.L262
.L260:
	call	SlaveStart
	cmpq	$1, P(%rip)
	je	.L263
	xorl	%ebx, %ebx
	jmp	.L265
	.p2align 4,,10
	.p2align 3
.L264:
	movq	P(%rip), %rax
	addq	$1, %rbx
	subq	$1, %rax
	cmpq	%rbx, %rax
	jbe	.L263
.L265:
	movq	PThreadTable(,%rbx,8), %rdi
	xorl	%esi, %esi
	call	pthread_join
	testl	%eax, %eax
	je	.L264
	movl	$.LC45, %edi
	call	puts
	movl	$-1, %edi
	call	exit
.L246:
	movl	$256, %r8d
	salq	$4, %rdi
	xorl	%edx, %edx
	subq	%rcx, %r8
	salq	$4, %r8
	movq	%r8, %rax
	divq	%rdi
	movl	$.LC29, %edx
	imulq	%rax, %rdi
	movq	%rax, pad_length(%rip)
	cmpq	%r8, %rdi
	je	.L248
	jmp	.L300
	.p2align 4,,10
	.p2align 3
.L244:
	shrq	$4, %rax
	movq	%rax, pad_length(%rip)
	jmp	.L245
.L302:
	.p2align 4,,4
	.p2align 3
	jne	.L239
	.p2align 4,,4
	.p2align 3
	jmp	.L296
.L263:
	cmpq	$0, doprint(%rip)
	.p2align 4,,6
	.p2align 3
	je	.L266
	cmpq	$0, test_result(%rip)
	je	.L267
	movl	$.LC46, %edi
	call	puts
.L268:
	movq	x(%rip), %rsi
	movq	N(%rip), %rdi
	call	PrintArray
.L266:
	movq	Global(%rip), %rax
	movl	$10, %edi
	movq	152(%rax), %rax
	movq	(%rax), %rax
	movq	%rax, transtime(%rip)
	call	putchar
	movl	$.LC48, %edi
	call	puts
	movl	$.LC49, %edi
	call	puts
	movl	$.LC50, %edi
	call	puts
	movq	Global(%rip), %rcx
	movl	$.LC51, %edi
	movq	152(%rcx), %rax
	movq	(%rax), %rdx
	movq	160(%rcx), %rax
	movq	(%rax), %rsi
	cvtsi2sdq	%rdx, %xmm0
	movl	$1, %eax
	cvtsi2sdq	%rsi, %xmm1
	divsd	%xmm1, %xmm0
	call	printf
	cmpq	$0, dostats(%rip)
	je	.L269
	movq	Global(%rip), %rsi
	movq	152(%rsi), %rcx
	movq	160(%rsi), %rdx
	movq	(%rcx), %rax
	movq	%rax, transtime2(%rip)
	movq	(%rcx), %rax
	movq	%rax, avgtranstime(%rip)
	movq	(%rdx), %rax
	movq	%rax, avgcomptime(%rip)
	movq	(%rdx), %rax
	movq	%rax, maxtotal(%rip)
	movq	(%rdx), %rax
	movq	%rax, mintotal(%rip)
	movq	P(%rip), %rax
	cvtsi2sdq	(%rcx), %xmm0
	cvtsi2sdq	(%rdx), %xmm1
	divsd	%xmm1, %xmm0
	cmpq	$1, %rax
	movsd	%xmm0, maxfrac(%rip)
	movapd	%xmm0, %xmm2
	movsd	%xmm0, minfrac(%rip)
	movsd	%xmm0, avgfractime(%rip)
	jle	.L270
	movl	$1, %ebp
	.p2align 4,,10
	.p2align 3
.L279:
	movq	152(%rsi), %rax
	leaq	0(,%rbp,8), %rbx
	movq	(%rax,%rbp,8), %rax
	cmpq	transtime(%rip), %rax
	jle	.L271
	movq	%rax, transtime(%rip)
.L271:
	movq	152(%rsi), %rax
	movq	(%rax,%rbx), %rax
	cmpq	transtime2(%rip), %rax
	jge	.L272
	movq	%rax, transtime2(%rip)
.L272:
	movq	160(%rsi), %rax
	movq	(%rax,%rbx), %rax
	cmpq	maxtotal(%rip), %rax
	jle	.L273
	movq	%rax, maxtotal(%rip)
.L273:
	movq	160(%rsi), %rax
	movq	(%rax,%rbx), %rax
	cmpq	mintotal(%rip), %rax
	jge	.L274
	movq	%rax, mintotal(%rip)
.L274:
	movq	152(%rsi), %rax
	cvtsi2sdq	(%rax,%rbx), %xmm1
	movq	160(%rsi), %rax
	cvtsi2sdq	(%rax,%rbx), %xmm0
	divsd	%xmm0, %xmm1
	ucomisd	maxfrac(%rip), %xmm1
	jbe	.L275
	movsd	%xmm1, maxfrac(%rip)
.L275:
	movq	152(%rsi), %rax
	cvtsi2sdq	(%rax,%rbx), %xmm1
	movq	160(%rsi), %rax
	cvtsi2sdq	(%rax,%rbx), %xmm0
	divsd	%xmm0, %xmm1
	movsd	minfrac(%rip), %xmm0
	ucomisd	%xmm1, %xmm0
	jbe	.L277
	movsd	%xmm1, minfrac(%rip)
.L277:
	movq	152(%rsi), %rax
	movl	$.LC52, %edi
	movq	(%rax,%rbx), %rcx
	movq	160(%rsi), %rax
	movq	%rbp, %rsi
	addq	$1, %rbp
	movq	(%rax,%rbx), %rdx
	cvtsi2sdq	%rcx, %xmm0
	movl	$1, %eax
	cvtsi2sdq	%rdx, %xmm1
	divsd	%xmm1, %xmm0
	call	printf
	movq	Global(%rip), %rsi
	movq	152(%rsi), %rcx
	movq	160(%rsi), %rdx
	movq	(%rcx,%rbx), %rax
	addq	%rax, avgtranstime(%rip)
	movq	(%rdx,%rbx), %rax
	addq	%rax, avgcomptime(%rip)
	movq	P(%rip), %rax
	cvtsi2sdq	(%rcx,%rbx), %xmm2
	cvtsi2sdq	(%rdx,%rbx), %xmm0
	divsd	%xmm0, %xmm2
	cmpq	%rbp, %rax
	addsd	avgfractime(%rip), %xmm2
	movsd	%xmm2, avgfractime(%rip)
	jg	.L279
.L270:
	cvtsi2sdq	%rax, %xmm3
	cvtsi2sdq	avgtranstime(%rip), %xmm1
	cvtsi2sdq	avgcomptime(%rip), %xmm0
	movl	$.LC53, %edi
	movl	$3, %eax
	divsd	%xmm3, %xmm0
	divsd	%xmm3, %xmm2
	divsd	%xmm3, %xmm1
	call	printf
	movq	transtime(%rip), %rdx
	movq	maxtotal(%rip), %rsi
	movl	$.LC54, %edi
	movsd	maxfrac(%rip), %xmm0
	movl	$1, %eax
	call	printf
	movq	transtime2(%rip), %rdx
	movq	mintotal(%rip), %rsi
	movl	$.LC55, %edi
	movsd	minfrac(%rip), %xmm0
	movl	$1, %eax
	call	printf
.L269:
	imulq	$1000000, %r13, %rax
	movq	Global(%rip), %rdx
	movl	$10, %edi
	addq	%r14, %rax
	movq	%rax, 168(%rdx)
	call	putchar
	movl	$.LC56, %edi
	call	puts
	movq	Global(%rip), %rax
	movl	$.LC57, %edi
	movq	168(%rax), %rsi
	xorl	%eax, %eax
	call	printf
	movq	Global(%rip), %rax
	movl	$.LC58, %edi
	movq	184(%rax), %rsi
	xorl	%eax, %eax
	call	printf
	movq	Global(%rip), %rax
	movl	$.LC59, %edi
	movq	176(%rax), %rsi
	xorl	%eax, %eax
	call	printf
	movq	Global(%rip), %rax
	movl	$.LC60, %edi
	movq	176(%rax), %rsi
	subq	168(%rax), %rsi
	xorl	%eax, %eax
	call	printf
	movq	Global(%rip), %rax
	movl	$.LC61, %edi
	movq	176(%rax), %rsi
	subq	184(%rax), %rsi
	xorl	%eax, %eax
	call	printf
	movq	transtime(%rip), %rsi
	xorl	%eax, %eax
	movl	$.LC62, %edi
	call	printf
	cvtsi2sdq	transtime(%rip), %xmm0
	movq	Global(%rip), %rdx
	movq	176(%rdx), %rax
	subq	184(%rdx), %rax
	movq	%rax, %rdx
	js	.L280
	cvtsi2sdq	%rax, %xmm1
.L281:
	divsd	%xmm1, %xmm0
	movl	$.LC63, %edi
	movl	$1, %eax
	call	printf
	movl	$10, %edi
	call	putchar
	cmpq	$0, test_result(%rip)
	jne	.L311
.L282:
	xorl	%edi, %edi
	call	exit
.L280:
	shrq	%rax
	andl	$1, %edx
	orq	%rdx, %rax
	cvtsi2sdq	%rax, %xmm1
	addsd	%xmm1, %xmm1
	jmp	.L281
.L304:
	movl	$16, %esi
	movl	$.LC26, %edi
	xorl	%eax, %eax
	call	printf
	movl	$.LC27, %edi
	call	puts
	movl	$.LC28, %edi
	call	puts
	movl	$16, %eax
	xorl	%edx, %edx
	divq	line_size(%rip)
	movq	orig_num_lines(%rip), %rdx
	movq	%rax, %rcx
	movq	%rdx, %rax
	sarq	$63, %rdx
	idivq	%rcx
	movq	%rax, num_cache_lines(%rip)
	jmp	.L243
.L254:
	movq	%rax, %rsi
	movl	$.LC38, %edi
	xorl	%eax, %eax
	call	printf
	jmp	.L255
.L309:
	movl	$.LC43, %edi
	call	puts
	movq	x(%rip), %rsi
	movq	N(%rip), %rdi
	call	PrintArray
	jmp	.L259
.L308:
	movq	x(%rip), %rdi
	call	CheckSum
	movsd	%xmm0, ck1(%rip)
	jmp	.L258
.L310:
	movl	$.LC44, %edi
	call	puts
	movl	$-1, %edi
	call	exit
.L311:
	movq	x(%rip), %rdi
	call	CheckSum
	movl	$.LC64, %edi
	movsd	%xmm0, ck3(%rip)
	call	puts
	movsd	ck1(%rip), %xmm0
	movl	$.LC65, %edi
	movsd	ck3(%rip), %xmm3
	movl	$3, %eax
	movapd	%xmm0, %xmm1
	subsd	%xmm3, %xmm0
	movapd	%xmm3, %xmm2
	call	printf
	movsd	ck1(%rip), %xmm1
	movsd	.LC66(%rip), %xmm0
	subsd	ck3(%rip), %xmm1
	andpd	%xmm0, %xmm1
	ucomisd	.LC67(%rip), %xmm1
	jb	.L312
.L299:
	movl	$.LC69, %edi
	call	puts
	jmp	.L282
.L267:
	movl	$.LC47, %edi
	call	puts
	.p2align 4,,3
	.p2align 3
	jmp	.L268
.L312:
	.p2align 4,,3
	.p2align 3
	jp	.L299
	movl	$.LC68, %edi
	.p2align 4,,6
	.p2align 3
	call	puts
	.p2align 4,,3
	.p2align 3
	jmp	.L282
.L236:
	movl	$.LC7, %edx
	jmp	.L300
.L303:
	movq	stderr(%rip), %rdi
	movl	$.LC25, %edx
	movl	$.LC1, %esi
	call	fprintf
	movl	%ebx, %edi
	call	exit
.L305:
	movq	stderr(%rip), %rdi
	movl	$.LC31, %edx
	movl	$.LC1, %esi
	call	fprintf
	movl	$-1, %edi
	call	exit
.L307:
	movl	$.LC42, %edi
	call	puts
	movq	Global(%rip), %rdi
	addq	$48, %rdi
	call	pthread_mutex_destroy
	movl	$-1, %edi
	call	exit
.L306:
	movl	$.LC42, %edi
	call	puts
	movl	$-1, %edi
	call	exit
.LFE13:
	.size	main, .-main
.globl P
	.data
	.align 8
	.type	P, @object
	.size	P, 8
P:
	.quad	1
.globl M
	.align 8
	.type	M, @object
	.size	M, 8
M:
	.quad	10
.globl test_result
	.bss
	.align 8
	.type	test_result, @object
	.size	test_result, 8
test_result:
	.zero	8
.globl doprint
	.align 8
	.type	doprint, @object
	.size	doprint, 8
doprint:
	.zero	8
.globl dostats
	.align 8
	.type	dostats, @object
	.size	dostats, 8
dostats:
	.zero	8
.globl transtime
	.align 8
	.type	transtime, @object
	.size	transtime, 8
transtime:
	.zero	8
.globl transtime2
	.align 8
	.type	transtime2, @object
	.size	transtime2, 8
transtime2:
	.zero	8
.globl avgtranstime
	.align 8
	.type	avgtranstime, @object
	.size	avgtranstime, 8
avgtranstime:
	.zero	8
.globl avgcomptime
	.align 8
	.type	avgcomptime, @object
	.size	avgcomptime, 8
avgcomptime:
	.zero	8
.globl transstart
	.align 8
	.type	transstart, @object
	.size	transstart, 8
transstart:
	.zero	8
.globl transend
	.align 8
	.type	transend, @object
	.size	transend, 8
transend:
	.zero	8
.globl maxtotal
	.align 8
	.type	maxtotal, @object
	.size	maxtotal, 8
maxtotal:
	.zero	8
.globl mintotal
	.align 8
	.type	mintotal, @object
	.size	mintotal, 8
mintotal:
	.zero	8
.globl maxfrac
	.align 8
	.type	maxfrac, @object
	.size	maxfrac, 8
maxfrac:
	.zero	8
.globl minfrac
	.align 8
	.type	minfrac, @object
	.size	minfrac, 8
minfrac:
	.zero	8
.globl avgfractime
	.align 8
	.type	avgfractime, @object
	.size	avgfractime, 8
avgfractime:
	.zero	8
.globl orig_num_lines
	.data
	.align 8
	.type	orig_num_lines, @object
	.size	orig_num_lines, 8
orig_num_lines:
	.quad	65536
.globl num_cache_lines
	.align 8
	.type	num_cache_lines, @object
	.size	num_cache_lines, 8
num_cache_lines:
	.quad	65536
.globl log2_line_size
	.align 8
	.type	log2_line_size, @object
	.size	log2_line_size, 8
log2_line_size:
	.quad	4
	.comm	PThreadTable,8192,32
	.comm	Global,8,8
	.comm	N,8,8
	.comm	rootN,8,8
	.comm	x,8,8
	.comm	trans,8,8
	.comm	umain,8,8
	.comm	umain2,8,8
	.comm	line_size,8,8
	.comm	rowsperproc,8,8
	.comm	ck1,8,8
	.comm	ck3,8,8
	.comm	pad_length,8,8
	.section	.rodata.cst8,"aM",@progbits,8
	.align 8
.LC4:
	.long	776530087
	.long	1075388927
	.align 8
.LC5:
	.long	776530087
	.long	-1072094721
	.section	.rodata.cst16,"aM",@progbits,16
	.align 16
.LC66:
	.long	4294967295
	.long	2147483647
	.long	0
	.long	0
	.section	.rodata.cst8
	.align 8
.LC67:
	.long	3539053052
	.long	1062232653
	.section	.eh_frame,"a",@progbits
.Lframe1:
	.long	.LECIE1-.LSCIE1
.LSCIE1:
	.long	0x0
	.byte	0x1
	.string	"zR"
	.uleb128 0x1
	.sleb128 -8
	.byte	0x10
	.uleb128 0x1
	.byte	0x3
	.byte	0xc
	.uleb128 0x7
	.uleb128 0x8
	.byte	0x90
	.uleb128 0x1
	.align 8
.LECIE1:
.LSFDE1:
	.long	.LEFDE1-.LASFDE1
.LASFDE1:
	.long	.LASFDE1-.Lframe1
	.long	.LFB15
	.long	.LFE15-.LFB15
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI0-.LFB15
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.long	.LCFI1-.LCFI0
	.byte	0xe
	.uleb128 0x18
	.byte	0x4
	.long	.LCFI2-.LCFI1
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.long	.LCFI3-.LCFI2
	.byte	0xe
	.uleb128 0x28
	.byte	0x4
	.long	.LCFI4-.LCFI3
	.byte	0xe
	.uleb128 0x30
	.byte	0x4
	.long	.LCFI5-.LCFI4
	.byte	0xe
	.uleb128 0x38
	.byte	0x83
	.uleb128 0x7
	.byte	0x86
	.uleb128 0x6
	.byte	0x8c
	.uleb128 0x5
	.byte	0x8d
	.uleb128 0x4
	.byte	0x8e
	.uleb128 0x3
	.byte	0x8f
	.uleb128 0x2
	.align 8
.LEFDE1:
.LSFDE3:
	.long	.LEFDE3-.LASFDE3
.LASFDE3:
	.long	.LASFDE3-.Lframe1
	.long	.LFB16
	.long	.LFE16-.LFB16
	.uleb128 0x0
	.align 8
.LEFDE3:
.LSFDE5:
	.long	.LEFDE5-.LASFDE5
.LASFDE5:
	.long	.LASFDE5-.Lframe1
	.long	.LFB20
	.long	.LFE20-.LFB20
	.uleb128 0x0
	.align 8
.LEFDE5:
.LSFDE7:
	.long	.LEFDE7-.LASFDE7
.LASFDE7:
	.long	.LASFDE7-.Lframe1
	.long	.LFB22
	.long	.LFE22-.LFB22
	.uleb128 0x0
	.align 8
.LEFDE7:
.LSFDE9:
	.long	.LEFDE9-.LASFDE9
.LASFDE9:
	.long	.LASFDE9-.Lframe1
	.long	.LFB23
	.long	.LFE23-.LFB23
	.uleb128 0x0
	.align 8
.LEFDE9:
.LSFDE11:
	.long	.LEFDE11-.LASFDE11
.LASFDE11:
	.long	.LASFDE11-.Lframe1
	.long	.LFB24
	.long	.LFE24-.LFB24
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI6-.LFB24
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.long	.LCFI7-.LCFI6
	.byte	0xe
	.uleb128 0x18
	.byte	0x4
	.long	.LCFI8-.LCFI7
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.long	.LCFI9-.LCFI8
	.byte	0xe
	.uleb128 0x28
	.byte	0x4
	.long	.LCFI10-.LCFI9
	.byte	0xe
	.uleb128 0x30
	.byte	0x4
	.long	.LCFI11-.LCFI10
	.byte	0xe
	.uleb128 0x38
	.byte	0x4
	.long	.LCFI12-.LCFI11
	.byte	0xe
	.uleb128 0x160
	.byte	0x83
	.uleb128 0x7
	.byte	0x86
	.uleb128 0x6
	.byte	0x8c
	.uleb128 0x5
	.byte	0x8d
	.uleb128 0x4
	.byte	0x8e
	.uleb128 0x3
	.byte	0x8f
	.uleb128 0x2
	.align 8
.LEFDE11:
.LSFDE13:
	.long	.LEFDE13-.LASFDE13
.LASFDE13:
	.long	.LASFDE13-.Lframe1
	.long	.LFB25
	.long	.LFE25-.LFB25
	.uleb128 0x0
	.align 8
.LEFDE13:
.LSFDE15:
	.long	.LEFDE15-.LASFDE15
.LASFDE15:
	.long	.LASFDE15-.Lframe1
	.long	.LFB26
	.long	.LFE26-.LFB26
	.uleb128 0x0
	.align 8
.LEFDE15:
.LSFDE17:
	.long	.LEFDE17-.LASFDE17
.LASFDE17:
	.long	.LASFDE17-.Lframe1
	.long	.LFB27
	.long	.LFE27-.LFB27
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI13-.LFB27
	.byte	0xe
	.uleb128 0x10
	.byte	0x8f
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI14-.LCFI13
	.byte	0xe
	.uleb128 0x18
	.byte	0x8e
	.uleb128 0x3
	.byte	0x4
	.long	.LCFI15-.LCFI14
	.byte	0xe
	.uleb128 0x20
	.byte	0x8d
	.uleb128 0x4
	.byte	0x4
	.long	.LCFI16-.LCFI15
	.byte	0xe
	.uleb128 0x28
	.byte	0x8c
	.uleb128 0x5
	.byte	0x4
	.long	.LCFI17-.LCFI16
	.byte	0xe
	.uleb128 0x30
	.byte	0x86
	.uleb128 0x6
	.byte	0x4
	.long	.LCFI18-.LCFI17
	.byte	0xe
	.uleb128 0x38
	.byte	0x83
	.uleb128 0x7
	.align 8
.LEFDE17:
.LSFDE19:
	.long	.LEFDE19-.LASFDE19
.LASFDE19:
	.long	.LASFDE19-.Lframe1
	.long	.LFB30
	.long	.LFE30-.LFB30
	.uleb128 0x0
	.align 8
.LEFDE19:
.LSFDE21:
	.long	.LEFDE21-.LASFDE21
.LASFDE21:
	.long	.LASFDE21-.Lframe1
	.long	.LFB29
	.long	.LFE29-.LFB29
	.uleb128 0x0
	.align 8
.LEFDE21:
.LSFDE23:
	.long	.LEFDE23-.LASFDE23
.LASFDE23:
	.long	.LASFDE23-.Lframe1
	.long	.LFB28
	.long	.LFE28-.LFB28
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI19-.LFB28
	.byte	0xe
	.uleb128 0x10
	.byte	0x8f
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI20-.LCFI19
	.byte	0xe
	.uleb128 0x18
	.byte	0x4
	.long	.LCFI21-.LCFI20
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.long	.LCFI22-.LCFI21
	.byte	0xe
	.uleb128 0x28
	.byte	0x4
	.long	.LCFI23-.LCFI22
	.byte	0xe
	.uleb128 0x30
	.byte	0x4
	.long	.LCFI24-.LCFI23
	.byte	0xe
	.uleb128 0x38
	.byte	0x4
	.long	.LCFI25-.LCFI24
	.byte	0xe
	.uleb128 0x40
	.byte	0x83
	.uleb128 0x7
	.byte	0x86
	.uleb128 0x6
	.byte	0x8c
	.uleb128 0x5
	.byte	0x8d
	.uleb128 0x4
	.byte	0x8e
	.uleb128 0x3
	.align 8
.LEFDE23:
.LSFDE25:
	.long	.LEFDE25-.LASFDE25
.LASFDE25:
	.long	.LASFDE25-.Lframe1
	.long	.LFB21
	.long	.LFE21-.LFB21
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI26-.LFB21
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.long	.LCFI27-.LCFI26
	.byte	0xe
	.uleb128 0x18
	.byte	0x4
	.long	.LCFI28-.LCFI27
	.byte	0xe
	.uleb128 0x20
	.byte	0x8d
	.uleb128 0x4
	.byte	0x8e
	.uleb128 0x3
	.byte	0x8f
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI29-.LCFI28
	.byte	0xe
	.uleb128 0x28
	.byte	0x4
	.long	.LCFI30-.LCFI29
	.byte	0xe
	.uleb128 0x30
	.byte	0x86
	.uleb128 0x6
	.byte	0x8c
	.uleb128 0x5
	.byte	0x4
	.long	.LCFI31-.LCFI30
	.byte	0xe
	.uleb128 0x38
	.byte	0x4
	.long	.LCFI32-.LCFI31
	.byte	0xe
	.uleb128 0xa0
	.byte	0x83
	.uleb128 0x7
	.align 8
.LEFDE25:
.LSFDE27:
	.long	.LEFDE27-.LASFDE27
.LASFDE27:
	.long	.LASFDE27-.Lframe1
	.long	.LFB19
	.long	.LFE19-.LFB19
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI33-.LFB19
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.long	.LCFI34-.LCFI33
	.byte	0xe
	.uleb128 0x18
	.byte	0x4
	.long	.LCFI35-.LCFI34
	.byte	0xe
	.uleb128 0x20
	.byte	0x8d
	.uleb128 0x4
	.byte	0x8e
	.uleb128 0x3
	.byte	0x8f
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI36-.LCFI35
	.byte	0xe
	.uleb128 0x28
	.byte	0x4
	.long	.LCFI37-.LCFI36
	.byte	0xe
	.uleb128 0x30
	.byte	0x4
	.long	.LCFI38-.LCFI37
	.byte	0xe
	.uleb128 0x38
	.byte	0x4
	.long	.LCFI39-.LCFI38
	.byte	0xe
	.uleb128 0x70
	.byte	0x83
	.uleb128 0x7
	.byte	0x86
	.uleb128 0x6
	.byte	0x8c
	.uleb128 0x5
	.align 8
.LEFDE27:
.LSFDE29:
	.long	.LEFDE29-.LASFDE29
.LASFDE29:
	.long	.LASFDE29-.Lframe1
	.long	.LFB18
	.long	.LFE18-.LFB18
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI40-.LFB18
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.long	.LCFI41-.LCFI40
	.byte	0xe
	.uleb128 0x18
	.byte	0x8e
	.uleb128 0x3
	.byte	0x8f
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI42-.LCFI41
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.long	.LCFI43-.LCFI42
	.byte	0xe
	.uleb128 0x28
	.byte	0x4
	.long	.LCFI44-.LCFI43
	.byte	0xe
	.uleb128 0x30
	.byte	0x4
	.long	.LCFI45-.LCFI44
	.byte	0xe
	.uleb128 0x38
	.byte	0x4
	.long	.LCFI46-.LCFI45
	.byte	0xe
	.uleb128 0x70
	.byte	0x83
	.uleb128 0x7
	.byte	0x86
	.uleb128 0x6
	.byte	0x8c
	.uleb128 0x5
	.byte	0x8d
	.uleb128 0x4
	.align 8
.LEFDE29:
.LSFDE31:
	.long	.LEFDE31-.LASFDE31
.LASFDE31:
	.long	.LASFDE31-.Lframe1
	.long	.LFB17
	.long	.LFE17-.LFB17
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI47-.LFB17
	.byte	0xe
	.uleb128 0x10
	.byte	0x8e
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI48-.LCFI47
	.byte	0xe
	.uleb128 0x18
	.byte	0x4
	.long	.LCFI49-.LCFI48
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.long	.LCFI50-.LCFI49
	.byte	0xe
	.uleb128 0x28
	.byte	0x4
	.long	.LCFI51-.LCFI50
	.byte	0xe
	.uleb128 0x30
	.byte	0x83
	.uleb128 0x6
	.byte	0x86
	.uleb128 0x5
	.byte	0x8c
	.uleb128 0x4
	.byte	0x8d
	.uleb128 0x3
	.align 8
.LEFDE31:
.LSFDE33:
	.long	.LEFDE33-.LASFDE33
.LASFDE33:
	.long	.LASFDE33-.Lframe1
	.long	.LFB14
	.long	.LFE14-.LFB14
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI52-.LFB14
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.long	.LCFI53-.LCFI52
	.byte	0xe
	.uleb128 0x18
	.byte	0x4
	.long	.LCFI54-.LCFI53
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.long	.LCFI55-.LCFI54
	.byte	0xe
	.uleb128 0x28
	.byte	0x4
	.long	.LCFI56-.LCFI55
	.byte	0xe
	.uleb128 0x30
	.byte	0x4
	.long	.LCFI57-.LCFI56
	.byte	0xe
	.uleb128 0x38
	.byte	0x4
	.long	.LCFI58-.LCFI57
	.byte	0xe
	.uleb128 0xa0
	.byte	0x83
	.uleb128 0x7
	.byte	0x86
	.uleb128 0x6
	.byte	0x8c
	.uleb128 0x5
	.byte	0x8d
	.uleb128 0x4
	.byte	0x8e
	.uleb128 0x3
	.byte	0x8f
	.uleb128 0x2
	.align 8
.LEFDE33:
.LSFDE35:
	.long	.LEFDE35-.LASFDE35
.LASFDE35:
	.long	.LASFDE35-.Lframe1
	.long	.LFB13
	.long	.LFE13-.LFB13
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI59-.LFB13
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.long	.LCFI60-.LCFI59
	.byte	0xe
	.uleb128 0x18
	.byte	0x4
	.long	.LCFI61-.LCFI60
	.byte	0xe
	.uleb128 0x20
	.byte	0x8c
	.uleb128 0x4
	.byte	0x8d
	.uleb128 0x3
	.byte	0x8e
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI62-.LCFI61
	.byte	0xe
	.uleb128 0x28
	.byte	0x86
	.uleb128 0x5
	.byte	0x4
	.long	.LCFI63-.LCFI62
	.byte	0xe
	.uleb128 0x30
	.byte	0x4
	.long	.LCFI64-.LCFI63
	.byte	0xe
	.uleb128 0x40
	.byte	0x83
	.uleb128 0x6
	.align 8
.LEFDE35:
	.ident	"GCC: (Debian 4.3.2-1.1) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
