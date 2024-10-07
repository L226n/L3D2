_load_l3d:
	;----------------------------------------
	;OPEN FILE AND GET UNPACKED + PACKED SIZE
	mov	rdi, rax	;move file buffer addr into rdi for sys_open
	mov	rax, 2	;sys_open
	xor	rsi, rsi	;reset flags because its not needed here
	syscall
	mov	r14, rax	;save fd to r14
	xor	rax, rax	;sys_read
	mov	rdi, r14	;use the opened fd
	mov	rsi, file.size	;save first qword to filesize
	mov	rdx, 8	;read first qword
	syscall
	;----------------------------------------
	;ALLOCATE MEMORY FOR FILE DATA
	mov	eax, dword[file.size+4]	;load the unpacked data into rax
	call	_alloc	;then allocate that amount
	push	rax	;push addr
	mov	r15, rax	;save the addr to r15
	xor	rax, rax	;another sys_read call
	mov	rdi, r14	;to the fd
	mov	rsi, r15	;and write the data to the new memory
	mov	edx, dword[file.size]	;read the packed size
	syscall
	;----------------------------------------
	;GO TO END OF FILE DATA AND INSERT TERMINATORS
	mov	edi, dword[file.size+4]	;load unpacked size here into destination
	mov	esi, dword[file.size]	;and packed size here for source
	sub	edi, 8	;subtract 8 here, go to last face position
	sub	esi, 6	;subtract 6 here (3x word) bc no terminating word
	mov	word[r15+rdi+6], 65535	;insert terminating word into end of data
	cmp	word[r15+rsi+4], 65535	;now check if no face data
	jz	.finish_faces	;if yes, finish reading face data
.loop_faces:
	;----------------------------------------
	;MOVE FACE DATA TO END
	mov	rax, qword[r15+rsi-2]	;now, load 1 face (+word) into rax
	mov	qword[r15+rdi-2], rax	;insert it word before it should to not overwrite data
	cmp	word[r15+rsi-2], 65535	;check if finished writing faces
	jz	.finish_faces	;if yes, finish!
	sub	rsi, 6	;otherwise, go to previous face
	sub	rdi, 6	;by decreasing source and destination index
	jmp	.loop_faces	;loop over
.finish_faces:
	sub	rdi, 20	;when finished, decrease by vertex length +dword here
	sub	rsi, 14	;and vertex length -dword +word here
	mov	dword[r15+rdi+16], MATRIX_DELIMITER	;move a -1 into end of vertex to show endd
.loop_vertices:
	;----------------------------------------
	;UNPACK AND MOVE VERTEX DATA TO END
	movups	xmm0, [r15+rsi]	;load xyz into xmm0
	movups	[r15+rdi], xmm0	;and insert them into the position at end
	mov	dword[r15+rdi+12], FLOAT_ONE	;then insert the 1.0 at end there
	cmp	rsi, 0	;check if source index counter is at 0 (finished everything)
	jz	.finish_load	;if yes, finish loading
	sub	rsi, 12	;otherwise, go to previous face
	sub	rdi, 16
	jmp	.loop_vertices	;loop over
.finish_load:
	mov	rax, 3	;sys_close
	mov	rdi, r14	;fd from the start
	syscall
	pop	rax	;and then get it back
	ret	;finished!

_load_lsc:
	;----------------------------------------
	;OPEN THE LSC FILE
	mov	rdi, rax	;move the file buffer into rdi
	push	rdi	;and push this addr
	mov	rax, 2	;rax is 2 for sys_open
	xor	rsi, rsi	;xor flags for opening read only
	syscall
	mov	r14, rax	;save fd to r14
	mov	rcx, rdi	;and then save the fd to rcx too
.loop_trunc_dir:
	;----------------------------------------
	;GET FILE PARENT DIRECTORY
	cmp	byte[rcx], 0	;check if current byte is 0
	jz	.trunc_dir	;if yes then truncate path
	cmp	byte[rcx], "/"	;otherwise check if current char is a /
	cmovz	rbx, rcx	;if it is then rbx is the current addr
	inc	rcx	;increase addr counter
	jmp	.loop_trunc_dir	;loop over to continue checking
.trunc_dir:
	;----------------------------------------
	;CHANGE WORKING DIRECTORY TO FILE
	mov	byte[rbx], 0	;0 replaces last / to get parent directory
	mov	rax, 80	;sys_chdir
	pop	rdi	;get back rdi (holds parent dir addr now)
	syscall	;change working dir to parent directory
	mov	byte[rbx], "/"	;move a / back into place to save file buf
	;----------------------------------------
	;READ OBJECT COUNT
	xor	rax, rax	;sys_read
	mov	rdi, r14	;read from the file
	mov	rsi, lsc_assets.l3d_index	;and store data here (object count)
	mov	rdx, 2	;object count is 2 bytes
	syscall
	;----------------------------------------
	;MACRO TO LOAD ALL ASSETS IN FILE
	%macro	load_asset	2
		;----------------------------------------
		;LOAD ASSET LENGTH AND DATA
		xor	rax, rax	;sys_read
		mov	rdi, r14	;read from lsc
		mov	rsi, file.size	;and store here
		mov	rdx, 4	;read dword
		syscall
		xor	rax, rax	;sys_read again
		mov	rsi, scratchpad+32	;but now store on scratchpad
		mov	edx, dword[file.size]	;read all segment data
		syscall
		mov	rax, scratchpad+32	;now save this addr to rax
		xor	rbx, rbx	;and clear rbx
	%%read_asset:
		;----------------------------------------
		;LOAD CURRENT FILE INTO MEMORY
		push	rax	;push the file path
		push	r14	;push the fd
		push	rbx	;and push the addr index
		call	_load_%1	;load the asset in correct format
		pop	rbx	;pop back the addr index
		pop	r14	;and fd
		;----------------------------------------
		;SAVE ALLOCATED ADDR (+ALLOC POINTER)
		%if	%2=1	;if loading an l3d:
		mov	ecx, dword[alloc_data.pointer]	;rcx = current allocation pointer
		shr	rbx, 2	;divide rbx by 4
		mov	word[lsc_assets.l3d_len+rbx], cx	;and save this value for copying
		shl	rbx, 2	;restore rbx
		%endif
		mov	qword[lsc_assets.%1+rbx], rax	;save the new addr to correct position
		add	rbx, 8	;go to next qword
		pop	rax	;and get back the addr pointer
	%%loop_next_file:
		;----------------------------------------
		;FIND NEXT FILE
		cmp	byte[rax], 0	;check if current byte is 0
		jz	%%found_next_file	;if yes then found next file to process
		inc	rax	;otherwise increase pointer
		jmp	%%loop_next_file	;and keep checking
	%%found_next_file:
		;----------------------------------------
		;CHECK IF FINISHED OR LOOP OVER
		cmp	byte[rax+1], 1	;check if next byte is 1
		jz	%%finish_files	;if yes finished checking segment
		inc	rax	;otherwise increase rax
		jmp	%%read_asset	;and keep looking for file end
	%%finish_files:
	%endmacro
	;----------------------------------------
	;READ L3D LTX AND LUV ASSETS
	load_asset	l3d, 1	;load l3d assets (save alloc pointer = on)
	load_asset	ltx, 0	;load ltx assets (save alloc pointer = off)
	load_asset	luv, 0	;load luv, again save pointer = off
	;----------------------------------------
	;READ LIST OF ALL OBJECTS
	xor	rax, rax	;sys_read again
	mov	rdi, r14	;read from file
	mov	rsi, status.obj_count	;store as object count
	mov	rdx, 2	;and read the obj count word
	syscall
	xor	rax, rax	;sys_read
	mov	rsi, lsc_objects	;now store data in the object buffer
	movzx	rdx, word[status.obj_count]	;rdx is the object count
	imul	rdx, 6	;but multiplied by 6 to give byte length
	syscall
	;----------------------------------------
	;READ INITIALISATION DATA	
	xor	rax, rax	;sys_readdddddd
	mov	rsi, file.size	;read to scratchpad now
	mov	rdx, 4	;read dword length of init data
	syscall
	xor	rax, rax	;sys read
	mov	rsi, scratchpad+32	;read this data to scratchpad
	mov	edx, dword[file.size]	;and use the dword length from before
	syscall
	lea	rdi, [rsi+rax]	;rdi is now the end address for the data
.loop_vars:
	;----------------------------------------
	;LOOP HANDLING INIT DATA	
	cmp	rsi, rdi	;check if rsi is rdi
	jz	.finish_var_read	;if yes then finished reading file data
	movzx	r8, byte[rsi]	;r8 is now equal to the operation type
	shl	r8, 4	;multiply by 16
	mov	rcx, qword[lsc_sizes+r8]	;get addr for handling operation
	call	rcx	;and then call this addr
	jmp	.loop_vars	;loop over
.finish_var_read:
	;----------------------------------------
	;READ ML DATA LENGTH	
	xor	rax, rax	;sys_read
	mov	rdi, r14	;read from open file
	mov	rsi, scratchpad	;save data to scratchpad
	mov	rdx, 4	;read 4 bytes of length info
	syscall
	;----------------------------------------
	;READ ALL ML DATA INTO RAM	
	xor	rax, rax	;read again
	mov	rsi, lsc_mainloop	;now read into mainloop data struct
	mov	edx, dword[scratchpad]	;read all data from prev read
	syscall
	;----------------------------------------
	;CLOSE THIS FD	
	mov	rax, 3	;sys_close
	mov	rdi, r14	;close this fd
	syscall
	ret	;done

_load_ltx:
	;----------------------------------------
	;OPEN FILE AND GET SIZE
	mov	rdi, rax	;move addr provided to rdi
	mov	rax, 2	;use sys_open
	xor	rsi, rsi	;and xor this for opening (read)
	syscall
	mov	r14, rax	;save fd to r14
	xor	rax, rax	;then use sys_read
	mov	rdi, r14	;read from new file
	mov	rsi, file.size	;store here
	mov	rdx, 4	;and read file size
	syscall
	;----------------------------------------
	;CORRECT FILESIZE AND ALLOC SPACE
	mov	eax, dword[file.size]	;then save into rax
	sub	rax, 4	;subtract dimension size from rax
	imul	rax, 3	;multiply length by 3 (bytes to unpacked size)
	add	rax, 4	;add back on dimension size
	lea	rdi, [rax-3]	;then load this (addr to start putting data at)
	push	rdi	;and push it for later
	add	rax, 24	;add on data for simd and width storage
	call	_alloc	;allocate size in rax
	add	rax, 24	;adjust addr to match
	pop	rdi	;get back rdi
	push	rax	;push addr
	push	rdi	;and then push this again
	mov	r13, rax	;and save addr here
	;----------------------------------------
	;READ REST OF FILE WITH CORRECT FILESIZE
	mov	r15, rax	;save addr to r15
	xor	rax, rax	;then sys_read again
	mov	rdi, r14	;read from open file
	mov	rsi, r15	;read data into the allocated data
	mov	edx, dword[file.size]	;use filesize as length
	sub	rdx, 4	;but expand it to be 2 bytes per pixel
	shl	rdx, 1	;because old file used to be 1 per pixel
	add	rdx, 4	;but then transparency bytes added
	syscall
	pop	rdi	;get back end addr of unpacked pixel data
	add	rdi, r15	;add on allocated data addr
	lea	rsi, [r15+rax-2]	;use data read in rax to find end of pixel data
	inc	r15	;increase r15 so can be used as stopping point for loop
.loop_convert:
	;----------------------------------------
	;LOOP CONVERT INT TO BCD STRING
	cmp	rdi, r15	;check if rdi is finished unpacking
	jz	.finish_convert	;if yes finish conversion
	xor	rdx, rdx	;otherwise clear rdx so it doesnt fuck idiv
	movzx	rax, byte[rsi]	;move colour byte into rax 
	mov	cl, byte[rsi+1]	;and save transparency byte to rcx
	mov	rbx, 10	;move 10 into rbx
	idiv	rbx	;and then div int col by 10
	add	rdx, "0"	;add 48 to remainder to conv to ansi
	mov	byte[rdi+2], dl	;then save as last digit in ansi col string
	xor	rdx, rdx	;reset rdx again
	idiv	rbx	;divide answer from previous div by 10 again
	add	rax, "0"	;then convert to ansi
	mov	byte[rdi], al	;and store answer as first digit
	add	rdx, "0"	;and then remainder
	mov	byte[rdi+1], dl	;stored as second digit (BCD string)
	;----------------------------------------
	;PROCESS TRANSPARENCY IF PRESENT
	cmp	cl, -1	;check if transparency byte is -1
	jz	.skip_transparency	;if yes its not transparent
	sub	byte[rdi], "0"	;otherwise convert first digit of col to int
	shl	cl, 4	;then shift transparency over 4 bytes
	or	cl, 0b10000000	;then set msb as a 1 to indicate transparency
	or	byte[rdi], cl	;then combine transparency with int colour
.skip_transparency:
	sub	rdi, 3	;go to previous unpacked pixel
	sub	rsi, 2	;and previous packed pixel
	jmp	.loop_convert	;loop over
.finish_convert:
	;----------------------------------------
	;CLOSE FILE
	mov	rax, 3	;sys_close
	mov	rdi, r14	;fd from the start
	syscall
	pop	rax	;get addr back
	;----------------------------------------
	;INITIALISE VALUES FOR SAMPLING
	movzx	ebx, word[rax]	;load image dimension x word here
	push	rbx	;and push it for later
	imul	rbx, 3	;multiply by 3 to get width in bytes
	mov	qword[rax-24], rbx	;and save here for sampling
	pop	rbx	;get back image dimension x
	dec	rbx	;decrease it
	mov	dword[scratchpad], ebx	;and save to scratchpad
	movzx	ebx, word[rax+2]	;then load y word
	dec	rbx	;decrease
	mov	dword[scratchpad+4], ebx	;and save to scratchpad again
	movaps	xmm0, [scratchpad]	;save these vals to xmm0
	cvtdq2ps	xmm0, xmm0	;convert them to a float
	movups	[rax-16], xmm0	;and store here (texture sampler uses)
	ret	;done

_load_luv:
	;----------------------------------------
	;OPEN FILE AND GET FILE LENGTH
	mov	rdi, rax	;move file string to rdi
	mov	rax, 2	;use sys_open
	xor	rsi, rsi	;reset rsi for reading file
	syscall
	mov	r14, rax	;save fd to r14
	xor	rax, rax	;sys_read now
	mov	rdi, r14	;read from open file
	mov	rsi, file.size	;and read data into here
	mov	rdx, 4	;use 4 bytes for file length
	syscall
	;----------------------------------------
	;ALLOCATE SPACE FOR FILE AND READ
	mov	eax, dword[file.size]	;load length into rax
	call	_alloc	;and then allocate that amount of data
	push	rax	;push the addr
	mov	rsi, rax	;read all data into this addr now
	xor	rax, rax	;then sys_read
	mov	rdi, r14	;read from open file again
	mov	edx, dword[file.size]	;and read the rest of the file
	syscall
	mov	rax, 3	;sys_close now
	mov	rdi, r14	;close the open file
	syscall
	pop	rax	;get back addr
	ret	;and done!

_write_lsc:
	;----------------------------------------
	;OPEN FILE WITH WRITE/CREATE ARG
	mov	rdi, rax	;file name is in rax
	mov	rax, 2	;use sys_open
	mov	rsi, 0b1001000010	;flags are O_CREAT O_RDWR O_TRUNC
	mov	rdx, 0q777	;open with all user permissions
	syscall	;create new file
	mov	r14, rax	;and save the fd to r14
	;----------------------------------------
	;WRITE FILE ASSETS LIST
	mov	rax, 1	;sys_write
	mov	rdi, r14	;write to new file
	mov	rsi, r15	;and also write start of data
	mov	rdx, 2	;write 2 bytes of asset length
	syscall
	%macro	asset_writes	0
		movzx	rcx, word[r15]	;move in asset count
		inc	rcx	;increase to account for end seq
		add	r15, 2	;add 2 to addr counter to skip asset counter
		mov	r13, r15	;save this addr to r13
	%%loop:
		jrcxz	%%end	;if rcx (asset counter) is 0 go to end
		cmp	byte[r15], 1	;check if current byte is 0 (EOS)
		jg	%%skip_dec	;if no skip decreasing rcx
		dec	rcx	;if yes decrease rcx
	%%skip_dec:
		inc	r15	;then increase addr counter
		jmp	%%loop	;and loop over
	%%end:
		;----------------------------------------
		;WRITE ALL DATA TO FILE
		mov	rax, 1	;sys_write
		mov	rdi, r14	;addr of file
		mov	rsi, scratchpad	;write scratchpad data
		mov	r12, r15	;move r12
		sub	r12, r13	;subtract old addr from new
		mov	dword[scratchpad], r12d	;save this (length)
		mov	rdx, 4	;write length qword (8)
		syscall
		mov	rax, 1	;write again
		mov	rsi, r13	;now use start addr of asset data
		mov	rdx, r12	;and write length of data specified
		syscall
	%endmacro
	;----------------------------------------
	;WRITE ALL ASSET LISTS
	asset_writes	;for l3d assets
	asset_writes	;ltx assets
	asset_writes	;and luv assets (order doesnt matter)
	;----------------------------------------
	;GET AND WRITE OBJECT ASSET INDICES
	movzx	rax, word[r15]	;rax is the amount of objects present
	imul	rax, 6	;multiply by 6 bytes (2 bytes per asset index)
	lea	rdx, [rax+2]	;rdx is now that amount +2 (asset data length)
	push	rdx	;push this for syscall later
	mov	rax, 1	;sys_write
	mov	rdi, r14	;write to the open file
	mov	rsi, r15	;using this start addr
	syscall
	pop	rdx	;now get back length used
	add	r15, rdx	;and add to r15 to get var segment offset
	;----------------------------------------
	;INITIALISE REGS TO WRITE VAR SEGMENT
	xor	rcx, rcx	;rcx is 0 to exclude segment length
	movzx	rbx, word[r15]	;rbx is now the size of var seg
	add	r15, 2	;add on 2 to get offset past the seg length
	mov	rsi, r15	;now rsi is the start addr for syscall later
.loop_write_var:
	cmp	rbx, 0	;check if var seg counter is 0
	jz	.finish_write_var	;if yes finished the write
	;----------------------------------------
	;GET CORRECT LENGTHS
	movzx	r8, byte[r15]	;get type byte
	shl	r8, 4	;multiply addr byte by 16
	mov	rax, qword[lsc_sizes+r8+8]	;rax is now len to add
	;----------------------------------------
	;CORRECT LENGTHS AND OFFSTS
	add	rcx, rax	;then add it to length counter
	add	r15, rax	;and addr counter
	dec	rbx	;decrease var counter
	jmp	.loop_write_var	;and loop over
.finish_write_var:
	;----------------------------------------
	;WRITE VAR DATA
	push	rsi	;push write addr for later
	mov	dword[scratchpad], ecx	;save write length to scratchpad
	mov	rax, 1	;sys_write
	mov	rdi, r14	;write to open file
	mov	rsi, scratchpad	;and write the length data
	mov	rdx, 4	;its dword so 4 bytes
	syscall	;go
	mov	rax, 1	;sys_write
	pop	rsi	;get back original addr
	mov	edx, dword[scratchpad]	;and length from loop
	syscall	;rsi was defined earlier as r15 before it was modified
	;----------------------------------------
	;PREPARE TO WRITE MAINLOOP SEG
	xor	rcx, rcx	;for mainloop seg rcx is 0 initially
	movzx	rbx, word[r15]	;rbx is still ml field counter
	mov	rsi, r15	;save the base addr to rsi again
	push	rsi	;and push it because its changed by first syscall
	add	r15, 2	;add 2 so it shows next field
.loop_write_ml:
	cmp	rbx, 0	;check if counter is 0
	jz	.finish_write_ml	;if yes go to finish ml loop
	;----------------------------------------
	;GET CORRECT LENGTHS
	movzx	r8, byte[r15]	;get type byte
	shl	r8, 4	;multiply this by 16
	mov	rax, qword[lsc_sizes.ml+r8+8]	;then load the length to add
	add	rcx, rax	;add this to length counter
	add	r15, rax	;and addr counter
	dec	rbx	;decrease ml counter
	jmp	.loop_write_ml	;and loop over
.next_ml:
	;----------------------------------------
	;CORRECT LENGTHS AND OFFSETS
	add	rcx, 2	;increase rcx by 2 to include -1
	movzx	rax, ax	;zero extend ax into itself
	add	rcx, rax	;add it onto length counter
	add	r15, rax	;and addr too
	dec	rbx	;then decrease ml counter
	jmp	.loop_write_ml	;and loop over (same as var segment really)
.finish_write_ml:
	add	rcx, 2
	mov	dword[file.size], ecx	;save this 32 bit value into filesize
	;----------------------------------------
	;WRITE SEGMENT BYTE SIZE AND DATA
	mov	rax, 1	;then sys_write
	mov	rsi, file.size	;write that length value
	mov	rdi, r14	;use this fd
	mov	rdx, 4	;and its 32 bits so 4 bytes
	syscall	;write
	mov	rax, 1	;sys_write again
	pop	rsi	;get back base addr from ml loop start
	add	rsi, 2	;add a 2 to skip the mainlist event counter
	mov	edx, dword[file.size]	;load length from file.size
	syscall	;and go write
	;----------------------------------------
	;CLOSE FILE
	mov	rax, 3	;sys_close
	mov	rdi, r14	;close the fd opened
	syscall	;and close
	ret	;done
