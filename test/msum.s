	.global main

main:
	stmfd sp!, {r0, r1}
	sub	sp, sp, #8
	mov	r0, #0
	str	r0, [sp, #-4]
	mov	r1, #0
	str	r1, [sp, #-8]
loop:
	ldr	r0, [sp, #-4]
	ldr	r1, [sp, #-8]
	add	r1, r1, r0
	str	r1, [sp, #-8]
	ldr	r0, [sp, #-4]
	add	r0, r0, #1
	str	r1, [sp, #-4]
	ldr	r0, [sp, #-4]
	cmp	r0, #1024
	bhs	loop
end:
	ldmfd sp!, {r0, r1}
	bx	lr


