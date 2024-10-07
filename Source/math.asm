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
	mov	dword[alloc_data.addr+rcx+8], eax	;move length into current item
	sub	dword[alloc_data.available], eax	;and correct available
	mov	r8, rax	;save space requested to r8
	mov	rax, qword[alloc_data.current]	;move current addr into rax
	mov	qword[alloc_data.addr+rcx], rax	;and save to addr items
	add	qword[alloc_data.current], r8	;and correct current addr pointer
	add	dword[alloc_data.pointer], 12	;then increase pointer
	ret	;and return

_barycentric:
	;----------------------------------------
	;GET VECTOR V2 = P-v0
	mov	dword[scratchpad+36], r8d	;save point y addr here
	mov	dword[scratchpad+32], r9d	;and x addr just before
	movaps	xmm0, [scratchpad+32]	;now load this addr into xmm0
	cvtdq2ps	xmm0, xmm0	;convert point to a float
	mulps	xmm0, xmm12	;convert to range 0-2 also
	subps	xmm0, xmm5	;subtract v0 to form V2
	;----------------------------------------
	;CALCULATE DOTPRODUCT D20
	movaps	xmm1, xmm0	;duplicate V2 into xmm1
	dpps	xmm1, xmm3, 0b00110001	;now get 2 element dp, like before
	movss	dword[barycentric.w], xmm1	;first val to be multiplied by
	movss	dword[barycentric.w+12], xmm1	;and also fourth val
	;----------------------------------------
	;CALCULATE D21	
	dpps	xmm0, xmm4, 0b00110001	;and then final 2 element dp
	movss	dword[barycentric.w+4], xmm0	;now this is second val to mul
	movss	dword[barycentric.w+8], xmm0	;and also third
	;----------------------------------------
	;CALCULATE BARYCENTRIC V & W
	movaps	xmm0, [barycentric.v]	;move barycentric v to xmm0
	movaps	xmm1, [barycentric.w]	;and w to xmm1
	;xmm0 = {d11 d01 d00 d01}
	;xmm1 = {d20 d21 d21 d20}
	mulps	xmm0, xmm1	;then multiply these vecs together
	;xmm0 = {d11*d20 d01*d21 d00*d21 d01*d20}
	hsubps	xmm0, xmm0	;and get a horizontal sub
	;xmm0 = {d11*d20-d01*d21 d00*d21 - d01*d20}
	vbroadcastss	xmm1, dword[barycentric.denom]	;load dp denom
	mulps	xmm0, xmm1	;divide first two vals by denom
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
	movaps	xmm3, [quaternion.mulmask_npnp]	;load initial mulmask to xmm3
	;----------------------------------------
	;MACRO FOR NEXT ROWS
	%macro	calc_unit	2
		vbroadcastss	xmm1, dword[rsi+%1]	;load scalar imaginary part into xmm1
		shufps	xmm2, xmm2, %2	;shuffle operand quat to match equation
		mulps	xmm1, xmm2	;multiply together
		xorps	xmm1, xmm3	;now negate certain values
		addps	xmm0, xmm1	;and add to xmm0
	%endmacro
	;----------------------------------------
	;FINISH	MULTIPLYING
	calc_unit	4, 0b10110001	;element i, [i, w, k, j]
	pshufd	xmm3, xmm3, 0b10110100	;shuffle npnp mask to become nppn
	calc_unit	8, 0b00011011	;element j, [j, k, w, i]
	pshufd	xmm3, xmm3, 0b01101100	;shuffle nppn mask to become nnpp
	calc_unit	12, 0b10110001	;element k, [k, j, i, w]
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
	cmp	dword[rdi], MATRIX_DELIMITER	;check if at end of matrix
	jz	.finish_convert	;if yes finish
	;----------------------------------------
	;CONVERT NDC X COORD TO SCREEN SPACE
	fld1	;otherwise load a 1
	fadd	st0	;then add to itself so its 2
	fld	dword[rdi]	;load x position in range -1 to 1
	fld1	;load a 1
	fadd	st1	;add it to st1 to convert to range 0 to 2
	fdiv	st2	;divide by 2 to convert to range 0 to 1
	fild	word[term_size.dx]	;load terminal width
	fmul	st1	;now multiply by that by 0-1 range to get coords
	fist	dword[rdi]	;and store back
	;----------------------------------------
	;CONVERT NDC Y COORD TO SCREEN SPACE
	fld	dword[rdi+4]	;do the same with the other coord
	fld1	;load a 1...
	fsub	st1	;this time subtract the -1 to 1 range from 1
	fdiv	st5	;and then div by 2 (this flips the y coords)
	fild	word[term_size.dy]	;load y value
	fmul	st1	;multiply again
	fist	dword[rdi+4]	;then store
	emms	;clear stack
	add	rdi, 16	;go to next face
	jmp	_trans_viewport	;and loop over
.finish_convert:
	ret	;done

_rotate_obj:
	;----------------------------------------
	;GET AND STORE SIN/COS OF θ/2
	fld1	;load a 1
	fadd	st0	;and turn into a 2
	fld	dword[r8]	;now load θ
	fdiv	st1	;divide by 2
	fsincos	;now get sin then push cos
	fstp	dword[quaternion]	;store cos here
	fst	dword[quaternion+4]	;and store sin here
	fchs	;negate the sine
	fst	dword[quaternion+8]	;and store here also
	emms	;clear stack
	;----------------------------------------
	;CREATE NORMAL+CONJUGATE QUATERNION
	movups	xmm0, [rdx-4]	;load the axis to form [0, x, y, z]
	movaps	xmm1, xmm0	;dupe into xmm1 too
	vbroadcastss	xmm2, dword[quaternion+4]	;load sin θ/2
	vbroadcastss	xmm3, dword[quaternion+8]	;load -sin θ/2
	mulps	xmm0, xmm2	;and get ijk*sin θ/2
	mulps	xmm1, xmm3	;then get ijk*-sin θ/2
	insertps	xmm0, dword[quaternion], 0b00000000	;insert cos θ/2
	insertps	xmm1, xmm0, 0b00000000	;insert it here too
	movaps	xmm6, xmm1	;save to xmm6 bc its clobbered
	;----------------------------------------
	;CREATE XORMASKS FOR ADDITION
	movaps	xmm13, [quaternion.mulmask_npnp]	;load npnp xormask
	pshufd	xmm14, xmm13, 0b10110100	;rearrange to form nppn mask
	pshufd	xmm15, xmm14, 0b01101100	;and then form a nnpp mask
.loop_obj:
	;----------------------------------------
	;INITIALISE VALUES FOR VERTEX ROTATION
	cmp	dword[rsi], MATRIX_DELIMITER	;check if at end of matrix
	jz	.finish_obj	;if yes finish looping
	pshufd	xmm2, xmm0, 0b00000000	;otherwise xmm2 is normal quat real part
	movups	xmm3, [rsi-4]	;then xmm3 is [0 x y z] for vertex
	insertps	xmm3, xmm3, 0b00000001	;this just clears first slot
	mulps	xmm2, xmm3	;multiply this entirely by real part of quat
	;----------------------------------------
	;MACRO FOR PROCESSING MULTIPLICATION
	%macro	fast_unit	6
		pshufd	xmm4, xmm%5, 0b%1%1%1%1	;broadcast x value to xmm4
		pshufd	xmm%6, xmm%6, %2	;then shuffle operand to match pattern
		mulps	xmm4, xmm%6	;multiply these xmms together
		xorps	xmm4, xmm%3	;then apply xormask to flip signs
		addps	xmm%4, xmm4	;and then add to base values
	%endmacro
	;----------------------------------------
	;PROCESS VALUES FOR MULTIPLICATION
	fast_unit	01, 0b10110001, 13, 2, 0, 3	;operate second col
	fast_unit	10, 0b00011011, 14, 2, 0, 3	;third
	fast_unit	11, 0b10110001, 15, 2, 0, 3	;and fourth
	;----------------------------------------
	;MULTIPLY (Q*V) BY Q'
	pshufd	xmm5, xmm2, 0b00000000	;as above, broadcast real to xmm5
	mulps	xmm5, xmm1	;then multiply xmm5 by conjugate quat
	fast_unit	01, 0b10110001, 13, 5, 2, 1	;process second col
	fast_unit	10, 0b00011011, 14, 5, 2, 1	;third col
	fast_unit	11, 0b10110001, 15, 5, 2, 1	;and fourth col
	;----------------------------------------
	;STORE ROTATED VERTEX AND FINISH LOOP
	push	qword[rdi-8]	;push qword here to stop xmm clobbering vals
	movups	[rdi-4], xmm5	;save the quat back
	pop	qword[rdi-8]	;and pop this value to save previous w
	mov	dword[rdi+12], FLOAT_ONE	;insert a 1.0 at the end to end
	add	rsi, 16	;go to next row
	add	rdi, 16	;in both source and dest
	movaps	xmm1, xmm6	;and then restore conjugate quat
	jmp	.loop_obj	;loop over
.finish_obj:
	mov	dword[rdi], MATRIX_DELIMITER	;at end so delimit quat
	ret	;done

_rotate_obj_copy:
	;----------------------------------------
	;ROTATE OBJECT AND COPY REST OF STRUCT
	call	_rotate_obj	;rotate the object
	%macro	finish_copy	0
		add	rsi, 4	;add 4 onto source and dest addr
		add	rdi, 4	;to move the addr past the matrix delimiter
	%%loop_copy:
		cmp	word[rsi], -1	;check if current word is -1 (face data end)
		jz	%%finish_copy	;if yes then finish copying data
		mov	rax, qword[rsi]	;otherwise move the face data into rax
		mov	qword[rdi], rax	;and save it to the dest
		add	rsi, 6	;increase these two to go to next face
		add	rdi, 6
		jmp	%%loop_copy	;and loop over
	%%finish_copy:
		ret	;finished!
	%endmacro
	finish_copy	;copy the rest of the data after rotating

_scale_obj:
	;----------------------------------------
	;MACRO TO OPERATE ON ALL MATRIX VERTS
	%macro	matmod_simple	1
		movups	xmm1, [rdx]	;xmm1 is the vec to operate with
	%%loop:
		cmp	dword[rsi], MATRIX_DELIMITER	;check if at end
		jz	%%finish	;if yes go to finish
		movups	xmm0, [rsi]	;otherwise move current vert into xmm0
		%1	xmm0, xmm1	;then do an operation on it
		movups	[rdi], xmm0	;and store in destination
		add	rsi, 16	;then increase rsi
		add	rdi, 16	;and rdi to go to next row of matrix
		jmp	%%loop	;loop over
	%%finish:
		mov	dword[rdi], MATRIX_DELIMITER	;delimit the matrix
		ret	;and return
	%endmacro
	mov	dword[rdx+12], FLOAT_ONE	;make sure w multiplier is 1
	matmod_simple	mulps	;and then multiply each vert by the vec

_scale_obj_copy:
	;----------------------------------------
	;SCALE OBJECT AND COPY DATA
	call	_scale_obj	;scale the object
	finish_copy	;then finish copying over data

_translate_obj:
	;----------------------------------------
	;TRANSLATE OBJECT
	matmod_simple	addps	;simply add the vec to all elements

_translate_obj_copy:
	;----------------------------------------
	;TRANSLATE OBJECT AND COPY ALL DATA
	call	_translate_obj	;translate the object
	finish_copy	;copy the remaining data over
