_alloc:
	cmp	eax, dword[alloc_data.available]	;check if more data needs allocated
	jbe	.no_alloc	;if no, then use empty allocated data
	;----------------------------------------
	;CALCULATE MINIMUM ALLOCATION SIZE
	push	rax	;save requested data size
	xor	rdx, rdx	;reset this, it screws divs
	mov	rbx, 4096	;divide rax by this page size
	idiv	rbx	;the actual divide on rax is here
	inc	rax	;increase so it doesnt allocate 0 bytes
	imul	rax, 4096	;multiply result by 4096 to get amount to allocate
	mov	dword[alloc_data.available], eax	;then save this amount here
	;----------------------------------------
	;ALLOCATE REQUESTED MEMORY
	mov	rsi, rax	;save page-multiple allocate size here
	mov	rax, 9	;sys_mmap
	xor	rdi, rdi	;let kernel choose start addr
	mov	rdx, 3	;PROT_READ or-ed with PROT_WRITE
	mov	r10, 0b00100010	;anonymous and private mapping
	mov	r8, -1	;ignored with anonymous
	xor	r9, r9	;this one too
	syscall
	;----------------------------------------
	;FIX ALLOC DATA STRUCTURE
	mov	ecx, dword[alloc_data.pointer]	;get pointer for new alloc here
	mov	qword[alloc_data.addr+rcx], rax	;save start addr to new slot
	add	dword[alloc_data.pointer], 12	;then increase the pointer
	lea	rbx, [rcx+8]	;save data length offset here
	mov	rcx, rax	;save start addr to rcx for later
	pop	rax	;get back requested data
	mov	dword[alloc_data.addr+rbx], eax	;move the length allocated here
	sub	dword[alloc_data.available], eax	;then subtract that from page-size allocated
	add	rax, rcx	;add the start addr to the requested
	mov	qword[alloc_data.current], rax	;then that is latest empty allocated data
	mov	rax, rcx	;move the start addr into rax for the return
	ret	;finished!
.no_alloc:
	;----------------------------------------
	;GIVE ADDR FROM PRE_ALLOCATED SPACE
	mov	ecx, dword[alloc_data.pointer]	;move pointer into rcx
	mov	dword[alloc_data.addr+rcx+8], eax	;move length into current iten
	sub	dword[alloc_data.available], eax	;and correct available
	mov	rax, qword[alloc_data.current]	;move current addr into rax
	mov	qword[alloc_data.addr+rcx], rax	;and save to addr items
	add	dword[alloc_data.pointer], 12	;then increase pointer
	ret	;and return

_barycentric:
	;----------------------------------------
	;GET VECTOR V2 = P-v0
	mov	dword[scratchpad+36], r8d	;save point y addr here
	mov	dword[scratchpad+32], r9d	;and x addr just before
	movaps	xmm0, [scratchpad+32]	;now load this addr into xmm0
	psubd	xmm0, xmm5	;subtract v0 to form V2
	;----------------------------------------
	;CALCULATE DOTPRODUCT D20
	movaps	xmm1, xmm0	;duplicate V2 into xmm1
	pmulld	xmm1, xmm3	;multiply it by V0
	phaddd	xmm1, xmm1	;and then add horizontal vals together for dp
	movss	dword[barycentric.w], xmm1	;first val to be multiplied by
	movss	dword[barycentric.w+12], xmm1	;and also fourth val
	;----------------------------------------
	;CALCULATE D21
	pmulld	xmm0, xmm4	;same thing but multiply by V1
	phaddd	xmm0, xmm0	;and horizontal add again
	movss	dword[barycentric.w+4], xmm0	;now this is second val to mul
	movss	dword[barycentric.w+8], xmm0	;and also third
	;----------------------------------------
	;CALCULATE BARYCENTRIC V & W
	movaps	xmm0, [barycentric.v]	;move barycentric v to xmm0
	movaps	xmm1, [barycentric.w]	;and w to xmm1
	;xmm0 = {d11 d01 d00 d01}
	;xmm1 = {d20 d21 d21 d20}
	pmulld	xmm0, xmm1	;multiply corresponding values
	;xmm0 = {d11*d20 d01*d21 d00*d21 d01*d20}
	phsubd	xmm0, xmm1	;and horizontal subtract
	;xmm0 = {d11*d20-d01*d21 d00*d21 - d01*d20}
	vbroadcastsd	ymm1, qword[barycentric.denom]	;load dp denom
	vcvtdq2pd	ymm0, xmm0	;convert int32 in xmm0 to float64
	vdivpd	ymm0, ymm1	;divide first two vals by denom
	vcvtpd2ps	xmm0, ymm0	;convert the doubles to singles
	;----------------------------------------
	;CALCULATE BARYCENTRIC U
	pshufd	xmm6, xmm0, 0b00010000	;shuffle xmm6 to {v v w 0}
	pshufd	xmm1, xmm0, 0b00000000	;shuffle xmm1 to {v v v v}
	pshufd	xmm2, xmm0, 0b01010101	;shuffle xmm2 to {w w w w}
	movss	xmm0, dword[simd_one]	;now load a 1 into xmm0
	subss	xmm0, xmm1	;and subtract both xmm1
	subss	xmm0, xmm2	;and xmm2 to get u
	;----------------------------------------
	;CHECK IF WITHIN TRIANGLE BOUNDS
	insertps	xmm6, xmm0, 0b00001000	;insert U to form {u v w 0}
	movaps	xmm7, [simd_one]	;load set of ones
	movaps	xmm8, [simd_zero]	;and set of zeroes
	cmpltps	xmm7, xmm6	;compare ones with barycentrics
	cmpnleps	xmm8, xmm6	;and zeroes with barycentrics
	movmskps	r12, xmm8	;move the mask to r12
	movmskps	rax, xmm7	;and rax
	or	rax, r12	;or the masks together, 0 if triangle in bounds
	movaps	xmm0, xmm6	;set xmm0
	movaps	xmm1, xmm6	;xmm1
	movaps	xmm2, xmm6	;and xmm2 ready for interpolation
	ret	;finished

_create_quat:
	;----------------------------------------
	;GET NORMALISED VECTOR
	movups	xmm0, [rsi]	;load vector into xmm0
	movups	[quaternion.norm+4], xmm0	;save this to normal addr+4
	mov	rsi, quaternion.norm+4	;save addr to rsi
	call	_normalise_vec	;and then normalise vector in place
	;----------------------------------------
	;CALCULATE COS(THETA/2) AND SIN(THETA/2)
	fld1	;load a 1
	fadd	st0	;now its 1+1 = 2
	fld	dword[rdi]	;load theta (addr in rdi)
	fdiv	st1	;divide by 2
	fsincos	;calculate sin(theta/2) (in st1) and cos(theta/2) (st0)
	fst	dword[quaternion.norm]	;store cos in first normal place
	fstp	dword[quaternion.conj]	;and conjugated, then pop stack to get sin
	fst	dword[quaternion.norm+16]	;store the sin at end here
	fchs	;then negate it
	fst	dword[quaternion.conj+16]	;and store that at end for conj
	emms	;clear fpu stack
	;----------------------------------------
	;MULTIPLY VECTOR BY ±SIN(THETA/2)
	movups	xmm0, [quaternion.norm+4]	;load normalised vector
	vbroadcastss	xmm1, dword[quaternion.norm+16]	;broadcast sin(theta/2)
	vbroadcastss	xmm2, dword[quaternion.conj+16]	;and -sin(theta/2)
	mulps	xmm1, xmm0	;multiply them both by the normalised vector
	mulps	xmm2, xmm0	;this one the the same but negative
	movups	[quaternion.norm+4], xmm1	;then save them to the normal
	movups	[quaternion.conj+4], xmm2	;and conjugated quaternions
	ret	;done

_get_winding:
	;----------------------------------------
	;PREPARE TRIANGLE EDGE VECTORS
	movzx	rax, word[r13+rsi]	;move face index A into rax
	movzx	rbx, word[r13+rsi+2]	;index B into rbx
	movzx	rcx, word[r13+rsi+4]	;and index C into rcx
	shl	rax, 4	;multiply them all by 16
	shl	rbx, 4	;this gets face offset from start of object matrix data
	shl	rcx, 4	;as each matrix row is 16 bytes
	mov	r8, qword[current.mesh]	;load current mesh addr
	movups	xmm2, [r8+rax]	;with offset calculated load point A into xmm2
	movups	xmm0, [r8+rbx]	;point B into xmm0
	movups	xmm1, [r8+rcx]	;and point C into xmm1
	subps	xmm1, xmm0	;vector B = pC→pB
	subps	xmm0, xmm2	;vector A = pB→pA
	;----------------------------------------
	;GET CROSS PRODUCT OF EDGE VECTORS (NORMAL)
	movaps	xmm3, xmm0	;save vA to xmm3 for second round of muls
	shufps	xmm0, xmm0, 0b11001001	;vA = [vY, vZ, vX]
	shufps	xmm1, xmm1, 0b11010010	;vB = [vZ, vX, vY]
	mulps	xmm0, xmm1	;multiply these together into xmm0
	shufps	xmm3, xmm3, 0b11010010	;vA now takes vB shuffle from last mul
	shufps	xmm1, xmm1, 0b11010010	;and vB takes vA shuffle
	mulps	xmm1, xmm3	;multiply them together into xmm1
	subps	xmm0, xmm1	;sub second round from first to get surface normal
	;----------------------------------------
	;DOT PRODUCT WITH CAMERA TO TRI VECTOR
	movaps	xmm1, [camera.pos]	;load the camera position into xmm1
	subps	xmm1, xmm2	;and then subtract vertex from earlier for vector
	dpps	xmm0, xmm1, 0b01110001	;then get 3d dp of normal and camera 
	;----------------------------------------
	;CHECK SIGN (WINDING ORDER)
	movss	dword[scratchpad], xmm0	;store result into scratchpad
	mov	rax, 1	;initialise return as clockwise winding
	test	dword[scratchpad], 0x80000000	;test msb (sign) of result
	jz	.positive	;if unsigned (bit not set) then clockwise winding
	sub	rax, 2	;for signed result rax now = -1, anticlockwise winding
.positive:
	ret

_matmul:
	;----------------------------------------
	;CREATE COLUMN BUFFERS FOR MULTIPLICATION
	%macro	vbuf	2
		insertps	%2, dword[rdx+%1], 0b00000000	;insert values into register
		insertps	%2, dword[rdx+%1+16], 0b00010000	;in correct positions
		insertps	%2, dword[rdx+%1+32], 0b00100000
		insertps	%2, dword[rdx+%1+48], 0b00110000
	%endmacro
	vbuf	0, xmm2	;use macro to create 4 buffers for each column
	vbuf	4, xmm3	;and then store them in other xmms
	vbuf	8, xmm4	;so they can be used in the mainloop
	vbuf	12, xmm5	;for multiplying rows
	xor	rbx, rbx	;reset rbx to go to start of matrix
.loop_mul:
	;----------------------------------------
	;MULTIPLY EACH ROW OF THE MATRIX
	movups	xmm1, [rsi+rbx]	;move the current row into xmm1
	%macro	mul_row	2
		vdpps	xmm0, xmm1, %1, 0b11110001	;get dot product of values
		movss	[rdi+rbx+%2], xmm0	;then store the scalar in destination
	%endmacro
	mul_row	xmm2, 0	;operate on column 1
	mul_row	xmm3, 4	;column 2
	mul_row	xmm4, 8	;column 3
	mul_row	xmm5, 12	;column 4
	;----------------------------------------
	;GO TO NEXT ROW TO MULTIPLY
	add	rbx, 16	;row length in bytes for 4 column matrix
	cmp	dword[rsi+rbx], MATRIX_DELIMITER	;check if at end of matrix
	jnz	.loop_mul	;if no, loop over
.finish_mul:
	mov	dword[rdi+rbx], MATRIX_DELIMITER	;terminate matrix with delimiter
	ret	;done!

_mat_persp:
	;----------------------------------------
	;CALCULATE 1/(ASPECT*TAN(FOV/2))
	fld1	;load one
	fadd	st0	;now add to itself to get 2
	fld	dword[camera.fov]	;now load the vertical fov
	fdiv	st1	;and divide it by 2
	fptan	;then get tan of fov/2, result in st1
	fild	word[term_size.y]	;load term y
	fild	word[term_size.x]	;and term x
	fdiv	st1	;divide x by y to get aspect
	fmul	st3	;then multiply this by tan(fov/2)
	fld1	;now load one
	fdiv	st1	;and divide this by aspect*tan(fov/2)
	fst	dword[matrix.persp]	;store final result in m00
	;----------------------------------------
	;CALCULATE 1/TAN(FOV/2)
	fxch	st4	;load back tan(fov/2)
	fld1	;get a 1
	fdiv	st1	;and then get 1/tan(fov/2)
	fst	dword[matrix.persp+20]	;store in m11
	emms	;clear stack now
	;----------------------------------------
	;CALCULATE -(NEAR*FAR)/(FAR-NEAR)
	fld	dword[camera.near]	;load near plane
	fld	dword[camera.far]	;and far plane
	fsub	st1	;calculate far-near
	fld	dword[camera.far]	;load far plane again
	fmul	st2	;multiply by near plane
	fdiv	st1	;divide near*far by far-near
	fchs	;negate answer
	fst	dword[matrix.persp+56]	;and store in m23
	;----------------------------------------
	;CALCULATE FAR/(FAR-NEAR)
	fld	dword[camera.far]	;load far plane
	fdiv	st2	;and divide by far-near from earlier
	fst	dword[matrix.persp+40]	;store in m22
	emms	;clear stack anddd
	ret	;done!

_mat_view:
	;----------------------------------------
	;MATRIX ITERATION MACRO
	%macro	iter	16	;iterate with this macro
		%assign	INDEX	0	;offset is 0
		%rep	%0	;16 times...
			%if	%1!=-1	;if current arg isnt -1,
				mov	eax, dword[SOURCE+%1]	;load item at offset from source
				mov	dword[DEST+INDEX], eax	;and put into matrix
			%endif
			%assign	INDEX	INDEX+4	;then go to next destination place
			%rotate	1	;and rotate params
		%endrep
	%endmacro
	;----------------------------------------
	;WRAPPER FOR ITER
	%macro	mat_create	18	;macro to create a matrix kinda
		%define	DEST	%1	;assign this to destination addr
		%define	SOURCE	%2	;and this to source addr
		iter	%3, %4, %5, %6,\
			%7, %8, %9, %10,\
			%11, %12, %13, %14,\
			%15, %16, %17, %18	;iterate over all offset args
	%endmacro
	;----------------------------------------
	;CREATE ROTATION MATRIX FROM CAMERA R, U, F
	mat_create	matrix.cam_rot, camera.r,\
		0, 16, 32, -1,\
		4, 20, 36, -1,\
		8, 24, 40, -1,\
		-1, -1, -1, -1	;use macro to create rotation matrix
	;----------------------------------------
	;CREATE TRANSLATION MATRIX FROM CAMERA POS
	movups	xmm1, [camera.pos]	;load position into xmm1
	movaps	xmm0, [simd_zero]	;load zero set into xmm0
	subps	xmm0, xmm1	;subtract position from 0 set (invert sign)
	movaps	[scratchpad], xmm0	;save to scratchpad
	mat_create	matrix.cam_trans, scratchpad,\
		-1, -1, -1, -1,\
		-1, -1, -1, -1,\
		-1, -1, -1, -1,\
		0, 4, 8, -1	;now use scratchpad data to create translation
	;----------------------------------------
	;CREATE VIEW MATRIX
	%macro	matmul	3
		mov	rdi, %1	;just a nice wrapper for _matmul
		mov	rsi, %2	;so u dont have to do mov spam
		mov	rdx, %3
		call	_matmul
	%endmacro
	matmul	matrix.view, matrix.cam_trans, matrix.cam_rot	;easy one line now
	ret

_normalise_vec:
	;----------------------------------------
	;CALCULATE SQRT(SUM(SOURCE^2))
	movups	xmm0, [rsi]	;load source into xmm0
	andps	xmm0, [vector.bitmask]	;clear last position of junk (4th is 0)
	mulps	xmm0, xmm0	;now square all items by each other
	times 2	haddps	xmm0, xmm0	;now get sum of all 3 elements
	sqrtss	xmm0, xmm0	;xmm0 now has sum, get sqrt of this
	movss	dword[scratchpad], xmm0	;save sqrt to scratchpad
	;----------------------------------------
	;DIVIDE EACH ELEMENT BY SQRT(SUM(SOURCE^2))
	vbroadcastss	xmm1, dword[scratchpad]	;broadcast this value into xmm1
	movups	xmm0, [rsi]	;and now load the original value back
	divps	xmm0, xmm1	;divide each element by the sqrt of sum of squares
	movups	[rsi], xmm0	;save that into rdi now
	ret	;and finished

_normalise_w:
	xor	rax, rax	;reset rax for counter
.loop_normalise:
	;----------------------------------------
	;DIVIDE POINTS XYZ BY W COMPONENT
	cmp	dword[objbuf+rax], MATRIX_DELIMITER	;check if current item is -1
	jz	.finish_normalise	;if yes finished
	movups	xmm0, [objbuf+rax]	;otherwise move all 4 points into xmm0
	push	qword[objbuf+rax+12]	;save w component
	vbroadcastss	xmm1, dword[objbuf+rax+12]	;then broadcast w to xmm1
	divps	xmm0, xmm1	;divide xyzw by wwww
	movups	[objbuf+rax], xmm0	;then store back into matrix
	;----------------------------------------
	;CLIP FOR FAR AND NEAR PLANES
	fld	dword[objbuf+rax+8]	;load ndc z coord
	fld1	;load a 1
	fcomi	st1	;compare against ndc z
	jb	.clip_obj	;if 1 is below then clip entire obj
	fchs	;change 1 to -1
	fcomi	st1	;compare against z again
	ja	.clip_obj	;if its above then clip entire obj again
	emms	;clear stack
	pop	qword[objbuf+rax+12]	;restore w from earlier
	add	rax, 16	;add 16 to go to next row
	jmp	.loop_normalise	;and loop over
.clip_obj:
	pop	qword[objbuf+rax+12]	;restore w from earlier
	emms	;clear stack
	mov	rax, -1	;move clip signal into rax
.finish_normalise:
	ret	;finished

_quatmul:
	;----------------------------------------
	;DO FIRST COL OF MULS
	vbroadcastss	xmm0, dword[rsi]	;load source quat real into xmm0
	movups	xmm2, [rdx]	;move the entire operand quat into xmm2
	mulps	xmm0, xmm2	;multiply w0 by second quat
	;----------------------------------------
	;MACRO FOR NEXT ROWS
	%macro	calc_unit	3
		vbroadcastss	xmm1, dword[rsi+%1]	;load scalar imaginary part into xmm1
		shufps	xmm2, xmm2, %2	;shuffle operand quat to match equation
		mulps	xmm1, xmm2	;multiply together
		xorps	xmm1, [quaternion.mulmask_%3]	;now negate certain values
		addps	xmm0, xmm1	;and add to xmm0
	%endmacro
	;----------------------------------------
	;FINISH	MULTIPLYING
	calc_unit	4, 0b10110001, npnp	;element i, [i, w, k, j]
	calc_unit	8, 0b00011011, nppn	;element j, [j, k, w, i]
	calc_unit	12, 0b10110001, nnpp	;element k, [k, j, i, w]
	movups	[rdi], xmm0	;save to destination
	ret	;done!

_sample_texture:
	;----------------------------------------
	;CLAMP UV COORDINATES
	insertps	xmm1, xmm2, 0b00010000	;xmm1 is now {u v 0 0}
	movaps	xmm15, [simd_one]	;load simd 1
	minps	xmm1, xmm15	;and get minimum to clamp to 1
	movaps	xmm15, [simd_zero]	;and then load 0
	maxps	xmm1, xmm15	;then get max to clamp to 0
	;----------------------------------------
	;CONVERT UV TO TEXTURE COORDINATES
	movaps	xmm15, [current.t_simd]	;load texture coord simd into xmm15
	mulps	xmm1, xmm15	;and then multiply by xmm1 to get texture coords
	cvtps2dq	xmm1, xmm1	;convert values to int32
	movss	dword[scratchpad+32], xmm1	;then save u to scratchpad
	pshufd	xmm1, xmm1, 0b00000001	;shuffle so first val is v
	movss	dword[scratchpad+36], xmm1	;then store v onto scratchpad
	;----------------------------------------
	;CONVERT TEXTURE COORDS TO ADDRESS
	mov	eax, dword[scratchpad+36]	;fetch v coords
	imul	rax, qword[current.t_width]	;multiply them by texture width
	mov	r12d, dword[scratchpad+32]	;fetch u coords
	imul	r12, 3	;multiply that by 3 (texture size)
	lea	rax, [rax+r12+4]	;rax is now equal to offset for pixel col
	add	rax, qword[current.texture]	;add texture addr
	ret	;finished!

_trans_viewport:
	xor	rax, rax
.loop_convert:
	cmp	dword[rdi+rax], MATRIX_DELIMITER
	jz	.finish_convert
	fld1
	fadd	st0
	fld	dword[rdi+rax]
	fld1
	fadd	st1
	fdiv	st2
	fild	word[term_size+6]
	fmul	st1
	fist	dword[rdi+rax]
	fld	dword[rdi+rax+4]
	fld1
	fsub	st1
	fdiv	st5
	fild	word[term_size+4]
	fmul	st1
	fist	dword[rdi+rax+4]
	emms
	add	rax, 16
	jmp	.loop_convert
.finish_convert:
	ret
