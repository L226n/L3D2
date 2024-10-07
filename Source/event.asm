_init_sky_col:
	;----------------------------------------
	;HANDLE SKY COLOUR SET
	mov	r8d, dword[rsi+1]	;move sky col into r8
	mov	dword[var.sky_colour], r8d	;and then store in here
	add	rsi, qword[lsc_sizes.sky_col+8]	;then add on var length
	ret	;done

_init_trans:
	%macro	init_op	5
		;----------------------------------------
		;FETCH OBJECT INDEX
		movups	xmm0, [rsi+3]	;load vec/vert argument
		movups	[scratchpad], xmm0	;and store on scratchpad
		mov	byte[scratchpad+12], 0	;clear the last component
		movzx	rax, word[rsi+1]	;load object id
		imul	rax, 6	;multiply by 6
		movzx	rbx, word[lsc_objects+rax]	;rbx is now l3d object id
		;----------------------------------------
		;CHECK IF DUPLICATE OR NO
		push	rdi	;push arg section end addr 
		push	rsi	;push arg current addr
		test	bx, 0x8000	;test if msb is set
		jnz	%%op_dupe	;if yes then duplicate object
		;----------------------------------------
		;SET UP ADDRESSES AND CALL HANDLER
		mov	rdi, rbx	;otherwise is l3d object index
		shl	rdi, 3	;multiply by 8 (qword)
		mov	rdi, qword[lsc_assets.l3d+rdi]	;now rdi is l3d addr
		mov	rsi, rdi	;rsi is the same as rdi
		mov	rdx, %3	;rdx becomes addr specified here
		mov	r8, %4	;and so does r8
		call	%1	;call the relevant action handler
	%%finish_op:
		;----------------------------------------
		;CLEAN UP
		pop	rsi	;pop back values from earlier
		pop	rdi
		add	rsi, qword[lsc_sizes.%5+8]	;and then add on correct offset
		ret	;shrimple
	%%op_dupe:
		;----------------------------------------
		;GET NEW L3D INDEX
		and	bx, ~0x8000	;and bx with this to clear msb
		movzx	rcx, word[lsc_assets.l3d_index]	;rcx is now last l3d index
		mov	word[lsc_objects+rax], cx	;overwrite l3d index with this
		push	rcx	;push this value for later
		mov	rcx, rbx	;for now save original index
		shl	rcx, 1	;and then double it to get word offset
		;----------------------------------------
		;ALLOCATE SPACE FOR DUPED OBJECT
		movzx	rbx, word[lsc_assets.l3d_len+rcx]	;now get obj len index
		mov	eax, dword[alloc_data.addr+rbx-4]	;use to fetch actual length
		push	rcx	;push rcx again
		call	_alloc	;allocate data to hold a dupe
		pop	rcx	;get back rcx
		shl	rcx, 2	;and quadruple it to get qword from word offset
		mov	rsi, qword[lsc_assets.l3d+rcx]	;save old addr to source
		;----------------------------------------
		;UPDATE OLD ADDRESS AND PREPARE FOR HANDLER
		pop	rcx	;get back new index for l3d object
		shl	rcx, 3	;times by 8 to get qword
		mov	qword[lsc_assets.l3d+rcx], rax	;and save new addr
		shr	rcx, 3	;restore old val
		inc	rcx	;increase it
		mov	word[lsc_assets.l3d_index], cx	;and store here for future
		mov	rdi, rax	;store transformed in new addr
		mov	rdx, %3	;rdx is equal to some val
		mov	r8, %4	;and r8 another
		call	%2	;call relevant action handler
		jmp	%%finish_op	;and jump to finish operation
	%endmacro
	init_op	_translate_obj, _translate_obj_copy, scratchpad, 0, trans

_init_rot:
	;----------------------------------------
	;EXTRACT ROTATION-SPECIFIC VALUES
	mov	eax, dword[rsi+15]	;extract rotation θ
	mov	dword[scratchpad+16], eax	;and save here to use in macro
	init_op	_rotate_obj, _rotate_obj_copy, scratchpad, scratchpad+16, rot

_init_scale:
	init_op	_scale_obj, _scale_obj_copy, scratchpad, 0, scale	;very simple macro substitution

_mainloop:
	;----------------------------------------
	;LOAD VALS AND CHECK IF OPERATION	
	movzx	rax, byte[r8]	;rax is the byte at operation index
	movzx	rbx, word[r8+1]	;and rbx is the object to operate on
	cmp	rbx, rcx	;check if rbx is equal to the current object
	jz	.handle_ml	;if yes handle mainloop events
	ret	;otherwise return
.handle_ml:
	;----------------------------------------
	;CALL OPERATION AND CORRECT OFFSET
	xor	r15, r15	;clear r15
	shl	rax, 4	;rax (opcode) is multiplied by 16
	push	rax	;push this offset
	mov	rax, qword[lsc_sizes.ml+rax]	;and then load the handler addr
	call	rax	;call this handler
	pop	rax	;and pop back the addr
	add	r8, qword[lsc_sizes.ml+rax+8]	;add op length onto r8
	inc	r15	;not r15 to indicate operation
	jmp	_mainloop	;loop over to next operation

_main_rot:
	;----------------------------------------
	;CLAMP ROTATION TO 2π
	push	r8	;push operation offset
	fld	dword[r8+7]	;load the current rotation
	fldpi	;load a π
	fadd	st0	;add to itself to get 2π
	fld	dword[r8+3]	;then load the rotation increment
	fadd	st2	;add it to current rotation
	fprem	;get the remainder from this
	fst	dword[r8+7]	;and store here (repeat clamp to 2π)
	emms	;clear stack
	;----------------------------------------
	;PREPARE REGISTERS TO ROTATE OBJECT
	lea	rdx, [r8+11]	;rdx is the rotation axis
	mov	rsi, qword[current.mesh]	;convert current mesh
	mov	rdi, obj_transform	;dest is the object transform struct
	add	r8, 7	;add 7 to r8 for rotation angle offset
	;----------------------------------------
	;CHECK IF FIRST RUN AND ROTATE
	test	r15, -1	;test r15 against all 1s
	jz	.first_run_rot	;if its 0 then its first run of transformations
	call	_rotate_obj	;otherwise on first run just rotate obj
	jmp	.finish_rot	;and finish there
.first_run_rot:
	call	_rotate_obj_copy	;otherwise rotate and copy data
.finish_rot:
	;----------------------------------------
	;CORRECT MESH ADDRESS
	mov	qword[current.mesh], obj_transform	;new mesh is objtransform
	pop	r8	;get back offset
	ret	;and finish
