	.global main

main:
	stmfd sp!, {r0, r1}
	mov	r0, #0
	mov	r1, #0
loop:
	add	r1, r1, r0
	add	r0, r0, #1
	cmp	r0, #1024
	bhs	loop
end:
	ldmfd sp!, {r0, r1}
	bx	lr


