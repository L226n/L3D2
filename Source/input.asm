_input_main:
	;-----------------------------------------------
	;CHECK IF ARROWS AND TEST WASD
	cmp	byte[scratchpad], 27	;check if first byte is escape
	jz	.test_arrows	;if yes test for arrows
	cmp	MOVE_F	;otherwise test normal keys (wasd)
	jz	.move_f	;MOVE_F/B/L/R are macros that do the comparison to the key
	cmp	MOVE_B	;makes it easier for comparing the arrow keys because
	jz	.move_b	;who cares if a is up left right down arrow
	cmp	MOVE_L	;also looks a little cleaner
	jz	.move_l
	cmp	MOVE_R
	jz	.move_r
	cmp	MOVE_U
	jz	.move_u
	cmp	MOVE_D
	jz	.move_d
	ret	;if none of the above where pressed then do nothing
.test_arrows:
	;-----------------------------------------------
	;CHECK ARROW KEYS
	cmp	LOOK_UP	;macro for byte[scratchpad+2], {arrow key}
	jz	.look_up	;handler for if this button is pressed
	cmp	LOOK_DOWN	;do the same for all arrow keys
	jz	.look_down
	cmp	LOOK_LEFT
	jz	.look_left
	cmp	LOOK_RIGHT
	jz	.look_right
	;-----------------------------------------------
	;CHECK DEBUG KEYBINDS
	cmp	byte[status.debug], 1	;check if debug is on
	jnz	.ret	;if no then dont check debug binds
	cmp	F1	;otherwise check f1 key
	jz	.debug_f1	;and if pressed then...
	cmp	F2
	jz	.debug_f2
.ret:
	ret	;if it was none of the above again return doing nothing
.debug_f1:
	neg	byte[status.wireframe]	;toggle wireframe mode
	ret
.debug_f2:
	neg	byte[status.backfaces]	;toggle backface culling
	ret
.look_up:
	;-----------------------------------------------
	;MACROS FOR LOOKING IN CERTAIN DIRECTION
	%macro	rotate_unit	1
		mov	dword[scratchpad], 0	;clear real component of new quat
		movups	xmm0, [%1]	;move point to rotate into xmm0
		movups	[scratchpad+4], xmm0	;then insert that as imaginary part
		mov	rsi, quaternion.norm	;now multiply normal quat
		mov	rdx, scratchpad	;by this point
		mov	rdi, scratchpad+20	;and store result here
		call	_quatmul	;multiply quats
		mov	rsi, rdi	;now move new quat to be multiplied
		mov	rdx, quaternion.conj	;by conjugate quat
		mov	rdi, scratchpad+40	;and store here
		call	_quatmul	;multiply again
		movups	xmm0, [scratchpad+44]	;then save this to xmm0
		movups	[%1], xmm0	;and insert that into point to rotate
	%endmacro
	%macro	look_dir	4
		mov	rsi, camera.%1	;use this vector
		mov	rdi, camera.%2	;and this angle
		call	_create_quat	;to form a new quat
		rotate_unit	camera.%3	;then rotate these 2 args
		rotate_unit	camera.%4	;with this new quat
		call	_mat_view	;and update view matrix
	%endmacro
	look_dir	r, angle_neg, f, u	;move around right axis negative
	ret
.look_down:
	look_dir	r, angle, f, u	;move around right axis positive
	ret
.look_left:
	look_dir	u, angle, r, f	;move around up axis positive
	ret
.look_right:
	look_dir	u, angle_neg, r, f	;move around up axis negative
	ret
.move_f:
	;-----------------------------------------------
	;MACRO FOR MOVING ALONG UNIT VECTOR
	%macro	move_uvec	3
		movups	xmm1, [%2]	;move unit vector into xmm1
		vbroadcastss	xmm0, dword[camera.speed]	;broadcast speed multiplier
		mulps	xmm1, xmm0	;multiply unit vector by speed
		movups	xmm0, [%1]	;now load position into xmm0
		%3ps	xmm0, xmm1	;add/subtract new vector to/from position
		movups	[%1], xmm0	;now save as new position
	%endmacro
	;-----------------------------------------------
	;MOVE CAMERA ALONG FORWARD VECTOR
	move_uvec	camera.pos, camera.f, add	;add camera along forward vector
	call	_mat_view	;regenerate view matrix
	ret	;done!
.move_b:
	move_uvec	camera.pos, camera.f, sub	;now go backwards on forward vector
	call	_mat_view	;regenerate view matrix
	ret	;done
.move_l:
	move_uvec	camera.pos, camera.r, sub	;go backwards along right vector
	call	_mat_view	;regenerate view matrix
	ret	;done
.move_r:
	move_uvec	camera.pos, camera.r, add	;and move along right vector
	call	_mat_view	;regenerate view matrix
	ret	;done
.move_u:
	move_uvec	camera.pos, camera.u, add	;and move along right vector
	call	_mat_view	;regenerate view matrix
	ret	;done
.move_d:
	move_uvec	camera.pos, camera.u, sub	;and move along right vector
	call	_mat_view	;regenerate view matrix
	ret	;done

