_clear_fbuf:
	;----------------------------------------
	;PREPARE ADDRESSES AND VALUES
	mov	rdi, qword[FRAMEBUFFER]	;rdi is now start addr for fbuf
	mov	esi, dword[FRAMEBUFFER+8]	;and rsi is length of allocated space
	lea	rsi, [rsi+rdi]	;now rsi is addr+len so addr end
	add	rdi, HEADER_LEN	;add header length to rdi to skip header
	mov	r8w, word[var.sky_colour]	;move sky colour word into r8
	mov	r9b, byte[var.sky_colour+2]	;and last byte into r9
.loop_clear:
	;----------------------------------------
	;LOOP RESETING PIXEL COLOURS
	cmp	rdi, rsi	;check if at end of addr space
	jz	.finish_clear	;if yes, finished clearing
	mov	word[rdi+7], r8w	;otherwise move in sky col word
	mov	byte[rdi+9], r9b	;and then byte
	mov	word[rdi+HALF_UNIT+7], r8w	;move into the fg colour too
	mov	byte[rdi+HALF_UNIT+9], r9b
	add	rdi, UNIT_LEN	;go to next unit
	jmp	.loop_clear	;and continue clearing
.finish_clear:
	ret	;done!

_clear_zbuf:
	;----------------------------------------
	;INITIALISE ADDRESSES
	mov	rax, qword[zbuf_reset]	;move the reset qword into rax
	mov	rdi, qword[DEPTHBUFFER]	;and depth buffer start addr here
	mov	esi, dword[DEPTHBUFFER+8]	;then load buffer size
	lea	rsi, [rdi+rsi]	;then rsi is end addr of buffer
.loop_clear:
	;----------------------------------------
	;LOOP CLEAR ALL VALUES IN DEPTH BUFFER
	cmp	rdi, rsi	;check if at end of depth buffer
	jz	.finish_clear	;if yes go finished
	mov	qword[rdi], rax	;otherwise move in two high fps
	add	rdi, 8	;go to next qword
	jmp	.loop_clear	;continue clear
.finish_clear:
	ret

_draw_line:
	;----------------------------------------
	;GET ABS VALUES FOR X AND INCREMENT
	mov	r9, 1	;default increment for x is 1
	mov	di, word[line.x1]	;load x1 into rdi
	sub	di, word[line.x0]	;subtract x0 to get difference
	jns	.x0_smaller	;if no carry, dont modify increment or change sign
	dec	di	;decrease rdi
	not	di	;and then invert to flip sign
	sub	r9, 2	;r9 is now -1
.x0_smaller:
	;----------------------------------------
	;GET -ABS VALUES FOR Y AND INCREMENT
	mov	r10, -1	;default y increment is -1
	mov	si, word[line.y1]	;load y1 into rsi
	sub	si, word[line.y0]	;then get difference
	js	.y0_larger	;if carry, dont negate or change increment 
	dec	si	;otherwise decrease
	not	si	;and invert bits to negate
	add	r10, 2	;then make increment 1
.y0_larger:
	;----------------------------------------
	;GET INITIAL VALUES FOR MAINLOOP
	movsx	rsi, si	;sign extend rdi and rsi 
	movsx	rdi, di	;good for cases later that get fucked up otherwise
	lea	r11, [rsi+rdi]	;error value is dx+dy
	movzx	rax, word[line.x0]	;start line at x0
	movzx	rbx, word[line.y0]	;and y0
.loop_draw:
	;----------------------------------------
	;DRAW LINE AND CHECK IF DONE
	call	_draw_pixel	;draw a pixel at position
	cmp	ax, word[line.x1]	;check if x is at end
	jnz	.skip_ret	;if no, skip further checks
	cmp	bx, word[line.y1]	;otherwise, check if y at end
	jnz	.skip_ret	;if its not then skip again
	ret	;otherwise just return
.skip_ret:
	;----------------------------------------
	;DETECT LINE CHANGES
	lea	cx, [r11+r11]	;rcx, temporary register for error*2
	cmp	cx, si	;check it against dy
	jl	.skip_e1	;if its lower, dont change x val
	add	r11w, si	;otherwise, update error
	add	ax, r9w	;and add 1 / -1 to x val
.skip_e1:
	cmp	cx, di	;now check rcx against dx
	jg	.loop_draw	;if its greater dont change y val
	add	r11w, di	;update error again
	add	bx, r10w	;add 1 / -1 to y val
	jmp	.loop_draw	;loop back to draw

_draw_mesh:
	;----------------------------------------
	;PREPARE ADDRESSES AND OFFSETS
	mov	rsi, 4	;start addr for face offset
	mov	r15, qword[FRAMEBUFFER]	;use this addr to draw
	mov	r14, ansi_white	;and this colour for lines
.find_faces:
	;----------------------------------------
	;GET FACES DATA START
	cmp	dword[r13+rsi-4], MATRIX_DELIMITER	;check if at end of matrix segment
	jz	.found_faces	;if yes found faces offset
	add	rsi, 16	;otherwise go to next matrix row
	jmp	.find_faces	;and loop
.found_faces:
	;----------------------------------------
	;CALL RELEVANT MESH PROCESSOR
	test	byte[status.wireframe], 0b10000000	;check if wireframe mode
	jnz	_draw_wireframe	;if yes draw wireframe
	add	r13, rsi	;r13 is now face start addr
	xor	rsi, rsi	;now reset rsi for face counter
	movaps	xmm12, [simd_ndc]	;xmm12 is the simd struct to conv to ndc
	jmp	_draw_textures	;otherwise draw textures

_draw_textures:
	;----------------------------------------
	;CHECK IF FINISHED OBJECT OR FACE IS CULLED
	cmp	word[r13+rsi], 65535	;end of faces data
	jz	.finish	;if at end then finish this
	test	byte[status.backfaces], 0b10000000	;test if culling off
	jnz	.skip_cull	;if yes then skip culling
	call	_get_winding	;otherwise get the winding order
	cmp	rax, -1	;check against -1 (counterclockwise)
	jnz	.next_face	;if its not then skip this face
	;----------------------------------------
	;FETCH FACE UV COORDINATES
.skip_cull:
	mov	rax, rsi	;load the current face offset into rax
	shl	rax, 2	;multiply by 4 (8 bytes per vertex rather than 2)
	mov	rdi, qword[current.uv]	;and then load uv address into rdi
	vbroadcastsd	ymm0, qword[rdi+rax]	;xmm0 is now {u, v0 u0 v0}
	vbroadcastsd	ymm10, qword[rdi+rax+8]	;xmm10 is now {u1 v1 u1 v1}
	vbroadcastsd	ymm11, qword[rdi+rax+16]	;and xmm11 is... yeah
	pshufd	xmm10, xmm10, 0b10010000	;xmm10 is now {u1 u1 v1 u1}
	pshufd	xmm11, xmm11, 0b10010000	;xmm11 is now {u2 u2 v2 u2}
	insertps	xmm11, xmm10, 0b10010000	;xmm11 = {u2 v1 v2 u2}
	insertps	xmm11, xmm0, 0b01000000	;xmm11 = {v0 v1 v2 u2}
	insertps	xmm10, xmm11, 0b11100000	;xmm10 = {u1 u1 u2 u1}
	insertps	xmm10, xmm0, 0b00000000	;xmm10 = {u0 u1 u2 u1}
	;----------------------------------------
	;FETCH VERTEX W VALUES
	movzx	rax, word[r13+rsi]	;load face index A
	movzx	rbx, word[r13+rsi+2]	;load face index B
	movzx	rcx, word[r13+rsi+4]	;load face index C
	shl	rax, 4	;multiply each of them by 16 to get offset from start
	shl	rbx, 4	;shift left 4 times = multiply by 16
	shl	rcx, 4	;and ofc its faster
	movaps	xmm5, [objbuf+rax]	;load vertex data for point A
	movaps	xmm3, [objbuf+rbx]	;point B
	movaps	xmm4, [objbuf+rcx]	;and point C
	insertps	xmm0, xmm5, 0b11000000	;xmm0 = {w0 0 0 0}
	insertps	xmm0, xmm3, 0b11010000	;xmm0 = {w0 w1 0 0}
	insertps	xmm0, xmm4, 0b11100000	;xmm0 = {w0 w1 w2 0}
	;----------------------------------------
	;CONVERT COORDS TO RANGE 0-2
	cvtdq2ps	xmm5, xmm5	;convert all to floats
	cvtdq2ps	xmm3, xmm3	;this restores saves some errors
	cvtdq2ps	xmm4, xmm4	;from happening
	mulps	xmm5, xmm12	;multiply these by ndc simd to convert to range 0-2
	mulps	xmm3, xmm12	;do this with all of the coords
	mulps	xmm4, xmm12
	;----------------------------------------
	;GET U/W, V/W, 1/W
	divps	xmm10, xmm0	;divide u aspects by corresponding vertex w
	divps	xmm11, xmm0	;divide v aspects by corresponding vertex w
	movaps	xmm9, [simd_one]	;move {1, 1, 1, 1} into xmm9
	divps	xmm9, xmm0	;and then divide to get reciprocals of w
	;----------------------------------------
	;COMPUTE BARYCENTRIC VECTORS INDEPENDANT OF P
	subps	xmm3, xmm5	;compute V0 = p1 - p0
	subps	xmm4, xmm5	;compute V1 = p2 - p0
	;----------------------------------------
	;BARYCENTRIC DOT PRODUCTS INDEPENDANT OF P (D00)
	movaps	xmm0, xmm3	;save V0 to xmm0 to preserve values in xmm3
	dpps	xmm0, xmm0, 0b00110001	;get 2 element dp
	movss	dword[scratchpad+32], xmm0	;store d00 as first denom val to multiply
	movss	dword[barycentric.v+8], xmm0	;and third mul val for computing v and w
	;----------------------------------------
	;D01
	movaps	xmm0, xmm3	;save xmm3 to xmm0 again
	dpps	xmm0, xmm4, 0b00110001	;two element dp again
	movss	dword[scratchpad+36], xmm0	;and then save as second val for denom here
	movss	dword[scratchpad+52], xmm0	;and also fourth value
	movss	dword[barycentric.v+4], xmm0	;then second mul val here
	movss	dword[barycentric.v+12], xmm0	;and fourth mul val
	;----------------------------------------
	;D11
	movaps	xmm0, xmm4	;load xmm4 into xmm0 for d11	
	dpps	xmm0, xmm0, 0b00110001	;and again
	movss	dword[scratchpad+48], xmm0	;then store as third val to mul
	movss	dword[barycentric.v], xmm0	;and first val to mul here
	;----------------------------------------
	;COMPUTE BARYCENTRIC DENOMINATOR
	movaps	xmm0, [scratchpad+32]	;load {d00 d01 0 0}
	movaps	xmm1, [scratchpad+48]	;load {d11 d01 0 0}
	mulps	xmm0, xmm1	;multiply to get {d00*d11 d01*d01}
	hsubps	xmm0, xmm0	;subtract d01*d01 from d00*d11
	movss	dword[barycentric.denom], xmm0	;and store as denom
	fld	dword[barycentric.denom]	;now load the denom
	fld1	;load a 1 also
	fdiv	st1	;then get the reciprocal
	fst	dword[barycentric.denom]	;and store
	emms	;this turns a div later into a mul
	;----------------------------------------
	;GET TRIANGLE BOUNDING BOX
	mov	rdx, qword[objbuf+rax]	;move point A XY into rdx
	mov	qword[scratchpad], rdx	;and save to scratchpad
	mov	rdx, qword[objbuf+rbx]	;move point B XY into rdx
	mov	qword[scratchpad+8], rdx	;and save to scratchpad too
	movaps	xmm0, [scratchpad]	;now xmm0 = {X0 Y0 X1 Y1}
	movaps	xmm1, [objbuf+rcx]	;xmm1 = {X2 Y2 Z2 1}
	shufps	xmm1, xmm1, 0b01000100	;now xmm1 = {X2 Y2 X2 Y2}
	movaps	xmm2, xmm0	;moves xmm0 to xmm2 to get max coords
	pminsd	xmm0, xmm1	;get min values of all 4 coord pairs
	pmaxsd	xmm1, xmm2	;get max values of all 4 coord pairs
	pshufd	xmm2, xmm0, 0b00001110	;shuffle to compare second pair with first
	pminsd	xmm0, xmm2	;get minimum of these (final minimum coords)
	pshufd	xmm2, xmm1, 0b00001110	;do the same shuffle with xmm1
	pmaxsd	xmm1, xmm2	;and then get the max of that
	;----------------------------------------
	;CLAMP BOUNDING BOX LOWER BOUNDS TO ZERO
	movaps	xmm2, [simd_zero]	;now load zero set into xmm2
	pmaxsd	xmm0, xmm2	;and then get max of this to clamp lower to 0
	pcmpgtd	xmm2, xmm1	;check if 0 is greater than max vals
	movmskps	ecx, xmm2	;move the mask into rcx
	and	cl, ~0b00001100	;clear junk data results
	test	ecx, 0xffffffff	;and bit test against -1
	jnz	.next_face	;if any bits arent 0, box is offscreen
	;----------------------------------------
	;CLAMP BOUNDING BOX UPPER BOUNDS TO SCREEN SIZE
	movaps	xmm2, [simd_screen]	;move this set in {sX sY sX sY}
	movaps	xmm15, xmm0	;save xmm0 to xmm15
	pcmpgtd	xmm15, xmm2	;check if lower bounds offscreen
	movmskps	ecx, xmm15	;and move bit mask to ecx
	and	cl, ~0b00001100	;clear junk data results again
	test	ecx, 0xffffffff	;do the bit test again
	jnz	.next_face	;and if any arent 0 box is offscreen
	shufps	xmm0, xmm1, 0b01000100	;interleaved shuf for {Xmin Ymin Xmax Ymax}
	pminsd	xmm0, xmm2	;get min here to clamp higher bounds
	movaps	[scratchpad], xmm0	;save this to scratchpad
	;----------------------------------------
	;INITIALISE REGS FOR BOX
	mov	r8d, dword[scratchpad+4]	;r8 = start Y
	mov	r9d, dword[scratchpad]	;r9 = start X
	mov	r10d, dword[scratchpad+12]	;r10 = end Y
	mov	r11d, dword[scratchpad+8]	;r11 = end X
	;----------------------------------------
	;CONVERT START COORDS TO FRAMEBUFFER ADDR
	mov	edi, r8d	;move start Y into rdi
	xor	rax, rax	;xor rax for row offset
	shr	rdi, 1	;half rdi
	jnc	.even_row	;if no carry then row was even
	mov	rax, HALF_UNIT	;otherwise add on odd row
.even_row:
	imul	rdi, qword[term_size.xb]	;multiply Y by row size in bytes
	add	rdi, rax	;add half_unit / 0 to rdi
	mov	eax, r9d	;now move start X into rax
	imul	rax, UNIT_LEN	;and multiply by length of 1 unit
	add	rdi, rax	;add to start addr
	add	rdi, r15	;and also add addr for framebuffer
	push	rdi	;push this address to retrieve when increasing Y pos
	;----------------------------------------
	;GET START ADDR IN DEPTH BUFFER
	mov	r12, r8	;move start Y into r8
	imul	r12, qword[term_size.xzb]	;multiply that by zbuf width
	mov	rdx, r9	;then move start X into rdx
	shl	rdx, 3	;mul by 8
	add	rdx, r12	;then add to row offset
	add	rdx, qword[DEPTHBUFFER]	;now add zbuf addr to get start addr
	push	rdx	;push this value with rdi
	;----------------------------------------
	;SET UP ROW VALUE XOR MASK
	mov	rbx, HALF_UNIT	;move half unit into rbx
	mov	rcx, qword[term_size.xb]	;and row size into rcx
	sub	rcx, HALF_UNIT	;subtract half unit now
	xor	rbx, rcx	;xor HALF_UNIT with xb-HALF_UNIT
	test	dword[scratchpad+4], 0x00000001	;test if start row is odd
	jnz	.loop_draw	;if its odd then first row change = xb-HU
	mov	rcx, HALF_UNIT	;otherwise first row change = HU
.loop_draw:
	;----------------------------------------
	;CHECK IF POINT IS TO BE DRAWN
	cmp	r9, r11	;check if X addr is at end X addr
	ja	.next_row	;if its above go to next row
	call	_barycentric	;otherwise get barycentrics
	cmp	rax, 0	;check if rax is 0
	jnz	.skip_draw	;if its not then point is OOB
	;----------------------------------------
	;INTERPOLATE VALUES AND GET CORRECT UV
	dpps	xmm0, xmm9, 0b01110001	;else get dp of barycentrics and 1/W
	dpps	xmm1, xmm10, 0b01110001	;dp of barycentrics and U/W
	dpps	xmm2, xmm11, 0b01110001	;dp of barycentrics and V/W
	divss	xmm1, xmm0	;divide interpolated U by 1/W
	divss	xmm2, xmm0	;divide interpolated V by 1/W
	movss	xmm6, dword[simd_one]	;move a 1 into xmm6
	divss	xmm6, xmm0	;then get reciprocal of 1/W (interpolated W)
	;----------------------------------------
	;SAMPLE INTERPOLATED UV AND DRAW
	call	_sample_texture	;sample interpolated UV
	call	_draw_pixel_raw	;and draw the pixel
.skip_draw:
	;----------------------------------------
	;GO TO NEXT VALUE ON X AXIS
	add	rdi, UNIT_LEN	;add unit onto rdi to get next addr
	inc	r9	;and increase coordinate counter
	add	rdx, 8	;go to next position in depthbuffer
	jmp	.loop_draw	;draw next pixel
.next_row:
	;----------------------------------------
	;CHECK IF FINISHED OR NEW ROW
	pop	rdx	;get back row start zbuf addr
	pop	rdi	;get back start of row addr
	cmp	r8, r10	;check if Y position = end Y
	jz	.next_face	;if yes then draw next face
	add	rdi, rcx	;otherwise add on offset for next row
	add	rdx, qword[term_size.xzb]	;add on row to zbuf addr
	push	rdi	;then push start of row addr again
	push	rdx	;push xbuf addr too
	xor	rcx, rbx	;xor row offset with xormask (xb-HU <----> HU)
	inc	r8	;increase Y counter
	mov	r9d, dword[scratchpad]	;restore X counter
	jmp	.loop_draw	;and draw next row
.next_face:
	;----------------------------------------
	;CORRECT TO ADDR OF NEXT FACE
	add	rsi, 6	;all faces are 6 bytes (3 words)
	jmp	_draw_textures	;keep drawing textures
.finish:
	ret	;finished now

_draw_wireframe:
	;----------------------------------------
	;CHECK IF FINISHED FACES AND FACE WINDING
	cmp	word[r13+rsi], 65535	;check if at end of faces data
	jz	.finish	;if yes go finish
	test	byte[status.backfaces], 0b10000000	;test if culling disabled
	jnz	.skip_cull	;if yes skip culling
	call	_get_winding	;get winding of current face
	cmp	rax, -1	;check if counter-clockwise winding
	jnz	.next_face	;if no then cull backface
	;----------------------------------------
	;GET REFERENCE POINT ADDRESS
.skip_cull:
	movzx	rax, word[r13+rsi]	;otherwise load point index A into rax 
	movzx	rbx, word[r13+rsi+2]	;and point index B into rbx
	shl	rax, 4	;multiply them both by 16
	shl	rbx, 4	;to get offset in matrix
	push	rax	;then push them both for later use
	push	rbx
	;----------------------------------------
	;MOVE LINE DATA IN FROM MATRIX AND DRAW
	%macro	line_coords	0
		mov	r8d, dword[objbuf+rax]	;move x0 into r8
		mov	word[line.x0], r8w	;then r8 into line x0
		mov	r8d, dword[objbuf+rax+4]	;move y0 into r8
		mov	word[line.y0], r8w	;then r8 into line y0
		mov	r8d, dword[objbuf+rbx]	;move x1 into r8
		mov	word[line.x1], r8w	;then r8 into line x1
		mov	r8d, dword[objbuf+rbx+4]	;move y1 into r8
		mov	word[line.y1], r8w	;then r8 into line y1
		push	rsi	;push rsi bc its clobbered by draw line
		call	_draw_line	;draw the line
		pop	rsi	;pop back line
	%endmacro
	line_coords	;draw line AB
	;----------------------------------------
	;DRAW LINES BETWEEN ALL VERTICES
	pop	rbx	;get back rbx (point B)
	movzx	rax, word[r13+rsi+4]	;move point C into rax
	shl	rax, 4	;multiply by 16
	push	rax	;and then push for later
	line_coords	;draw line BC
	pop	rax	;pop back point C
	mov	rbx, rax	;move to rbx
	pop	rax	;pop back point A
	line_coords	;draw line CA
.next_face:
	add	rsi, 6	;go to next face
	jmp	_draw_wireframe	;and loop over
.finish:
	ret

_draw_pixel:
	push	rax	;push clobbered registers
	push	rbx
	push	r9
	push	r10
	;----------------------------------------
	;CHECK IF COORDS WITHIN BOUNDS
	movzx	r9, word[term_size+6]	;now get the width vals into here
	movzx	r10, word[term_size+4]	;and height values here
	cmp	rax, r9	;check rax against width
	jae	.oob	;stop if larger or less than (unsigned)
	cmp	rbx, r10	;check height also
	jae	.oob	;and same thing
	;----------------------------------------
	;CONVERT COORDS TO MEMORY OFFSET
	imul	rax, UNIT_LEN	;multiply x by unit length to get offset
	xor	rcx, rcx	;xor rcx (used for odd rows)
	shr	rbx, 1	;divide height by 2
	jnc	.even_row	;if no carry, row number is even
	mov	rcx, HALF_UNIT	;otherwise its odd so add half a height unit
.even_row:
	imul	rbx, qword[term_size.xb]	;multiply the halved number row width
	add	rbx, rcx	;then add the half unit, if row # is odd
	add	rax, rbx	;add together memory offsets
	;----------------------------------------
	;WRITE COLOUR TO PIXEL
	mov	bx, word[r14]	;load first 2 bytes of colour here
	mov	word[r15+rax+HEADER_LEN+7], bx	;then write then
	mov	bl, byte[r14+2]	;then load last byte
	mov	byte[r15+rax+HEADER_LEN+9], bl	;write it here
.oob:
	pop	r10
	pop	r9
	pop	rbx	;pop back clobbered registers
	pop	rax
	ret	;done!

_draw_pixel_raw:
	;----------------------------------------
	;PERFORM DEPTH CHECK
	movss	dword[scratchpad+64], xmm6	;move correct w into scratchpad
	fld	dword[scratchpad+64]	;load correct w
	fld	dword[rdx]	;and load depth value in depth buffer
	fcomi	st1	;compare against pixel w
	emms	;clear stack too
	jb	.no_draw	;if stored depth is below pixel depth dont draw
	movss	dword[rdx], xmm6	;otherwise save pixel depth to buffer
	;----------------------------------------
	;WRITE COLOUR TO PIXEL
	mov	r12w, word[rax]	;then load colour word into r12
	mov	word[rdi+HEADER_LEN+7], r12w	;then store here
	mov	r12b, byte[rax+2]	;and then the same with the remaining byte
	mov	byte[rdi+HEADER_LEN+9], r12b	;store in here
.no_draw:
	ret	;finished!

_init_screen:
	;----------------------------------------
	;GET FRAMEBUFFER START AND END ADDR
	mov	rdi, qword[FRAMEBUFFER]	;move in framebuffer addr
	mov	ecx, dword[FRAMEBUFFER+8]	;move in framebuffer length
	add	rcx, rdi	;now add together to get framebuffer end addr
	;----------------------------------------
	;WRITE FRAMEBUFFER HEADER AND LOAD ESCAPES
	mov	r8d, dword[esc_home]	;load the home escape into r8
	mov	dword[rdi], r8d	;write home escape to header area
	add	rdi, HEADER_LEN	;go to body section now
	mov	r8, qword[unit_template]	;load 8 bytes of unit template
	mov	r9, qword[unit_template+8]
	mov	r10, qword[unit_template+16]
	mov	r11w, word[unit_template+24]	;then remaining 2 bytes here
.loop_write:
	;----------------------------------------
	;LOOP WRITE UNITS INTO FRAMEBUFFER
	mov	qword[rdi], r8	;write these bytes to the framebuffer in order
	mov	qword[rdi+8], r9
	mov	qword[rdi+16], r10
	mov	word[rdi+24], r11w	;last 2 bytes, 24-26
	add	rdi, UNIT_LEN	;then increase rdi by one unit
	cmp	rdi, rcx	;check if at end addr
	jnz	.loop_write	;if no then write next unit
	ret	;otherwise, return

