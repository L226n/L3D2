%include	"macro.asm"	;file for fun macros
%include	"data.asm"	;file for data initialisations
section	.text
	global	_start
	%include	"event.asm"
	%include	"file.asm"	;file for file io
	%include	"graphics.asm"	;file for graphics stuff
	%include	"input.asm"
	%include	"math.asm"	;file for maths related things
_start:
	;----------------------------------------
	;SET UP SIGNAL HANDLERS
	mov	rax, 13	;sys_rt_sigaction
	mov	rdi, 2	;signal type, 2 = SIGINT
	mov	rsi, handler_int	;struct to set handler to _int
	xor	rdx, rdx	;clear this register for something
	mov	r10, 8	;length of something not sure what
	syscall
	mov	rax, 13	;another sys_rt_sigaction
	mov	rdi, 11	;signal type, 11 = SIGSEGV
	mov	rsi, handler_seg	;_seg struct
	syscall
	;----------------------------------------
	;SET UP TERMINAL SETTINGS
	mov	rax, 16	;sys_ioctl
	mov	rdi, 1	;use stdout
	mov	rsi, 21505	;and command for TCGETS, get terminal info
	mov	rdx, term_io	;return it into this struct
	syscall
	and	dword[term_io.c_lflag], ~0b00000010	;clear canonical flag
	and	dword[term_io.c_lflag], ~0b00001000	;clear echo flag
	mov	rax, 16	;sys_ioctl
	mov	rdi, 1	;use stdout
	mov	rsi, 21506	;command for TCSETS, set terminal info
	mov	rdx, term_io	;modified struct to set it to
	syscall
	;----------------------------------------
	;GET AND CHECK TERMINAL SIZE
	mov	rax, 16	;sys_ioctl again
	mov	rdi, 1	;use stdout
	mov	rsi, 21523	;and this time its TIOCGWINSZ, get window size
	mov	rdx, term_size	;return to this truct
	syscall
	mov	ax, word[term_size.x]	;get terminal width into ax
	mov	word[term_size.dx], ax	;now save it to double res data
	shr	word[term_size.x], 1	;then half the normal res data
	jc	_odd	;if theres carry here, then the width is odd, quit
	mov	ax, word[term_size.y]	;now move in terminal height
	shl	ax, 1	;double it
	mov	word[term_size.dy], ax	;and save this in double res data
	movzx	rax, word[term_size.dx]	;load term x width
	imul	rax, UNIT_LEN	;multiply by unit size to get width of screen in bytes
	mov	qword[term_size.xb], rax	;now save as width in bytes
	;----------------------------------------
	;CREATE SCREEN CLAMP SIMD
	movzx	rax, word[term_size.dx]	;rax is now original term width
	dec	rax	;clamp 1 less than width
	mov	dword[simd_screen], eax	;store in screen clamp
	mov	dword[simd_screen+8], eax	;and again
	inc	rax	;correct value for later
	movzx	rbx, word[term_size.dy]	;now load rbx as double height
	dec	rbx	;clamp 1 less than width
	mov	dword[simd_screen+4], ebx	;store in screen clamp
	mov	dword[simd_screen+12], ebx	;x2
	movzx	rbx, word[term_size.y]	;and rbx is original term height	
	;----------------------------------------
	;CREATE NDC TRANSFORM SIMD
	fild	dword[term_size.x]	;load terminal x size
	fld1	;load a 1
	fdiv	st1	;and then get x size reciprocal to use in mul
	fst	dword[simd_ndc]	;store in ndc simd (its not ndc eye roll)
	fild	dword[term_size.y]	;to the same with the y coord
	fld1	;load a 1
	fdiv	st1	;and get reciprocal
	fst	dword[simd_ndc+4]	;and store
	emms	;then clear fpu stack
	;----------------------------------------
	;ALLOCATE AND INITIALISE FRAMEBUFFER
	imul	rax, rbx	;multiply them together to get total term cells
	imul	rax, UNIT_LEN	;now get data size of eventual framebuffer
	add	rax, HEADER_LEN	;add header length
	call	_alloc	;allocate that space
	call	_init_screen	;now space is allocated, initialise framebuffer
	;----------------------------------------
	;ALLOCATE AND INIT DEPTH BUFFER	
	movzx	rbx, word[term_size.dx]	;get terminal double width
	shl	rbx, 3	;multiply by 8 to get width in bytes
	mov	qword[term_size.xzb], rbx	;now save as depth buffer width
	movzx	rax, word[term_size.dy]	;now load y val
	imul	rax, rbx	;and multiply that by width to get total size
	call	_alloc	;allocate depth buffer
	call	_clear_zbuf	;fill depth buffer with high values
	;----------------------------------------
	;GENERATE FRAME TIME VALUES	
	xor	rdx, rdx	;xor rdx division thing
	mov	rax, NSEC	;move 1 sec in nsec into rax
	movzx	rbx, word[status.fps]	;now move fps val into rbx
	idiv	rbx	;divide 1 sec by fps to get frame time in ns
	dec	rax	;decrease so 1fps works
	mov	qword[status.nsec], rax
	fild	word[status.fps]	;load fps value now
	fld1	;and a 1
	fdiv	st1	;then get reciprocal of fps
	fst	dword[status.delta]	;and store this as delta for multiplying
	emms	;clear stack too ofc
	;----------------------------------------
	;INITIALISE MATRICES
	call	_mat_view	;generate view matrix
	call	_mat_persp	;generate perspective matrix

	mov	rax, file.buf
	call	_load_lsc

.new_frame:
	;----------------------------------------
	;GET FRAME START TIME
	mov	rax, 228	;sys_clock_gettime
	mov	rdi, 4	;get info from monotonic clock
	mov	rsi, monotonic	;and store it in this struct
	syscall
	;----------------------------------------
	;PREPARE PROGRAM FOR DRAWING A NEW FRAME
	call	_clear_fbuf	;clear screen from old chars
	call	_clear_zbuf	;and clear z buffer
	movzx	rcx, word[status.obj_count]	;load object count now
	dec	rcx	;decrease it
	imul	rcx, 6	;then multiply by 6 to get last object offset in obj data
	mov	r8, lsc_mainloop
.mainloop:
	;----------------------------------------
	;STORE ADDRESS FOR L3D OBJECT
	push	rcx	;push this value
	movzx	rbx, word[lsc_objects+rcx]	;then move the current obj l3d index here
	shl	rbx, 3	;multiply by 8 to get qword offset
	mov	rax, qword[lsc_assets.l3d+rbx]	;and then fetch the l3d address
	mov	qword[current.mesh], rax	;store in current mesh
	;----------------------------------------
	;STORE ADDRESS SIMD AND WIDTH FOR TEXTURE
	movzx	rbx, word[lsc_objects+rcx+2]	;then load the current ltx addr
	shl	rbx, 3	;get qword offset again
	mov	rax, qword[lsc_assets.ltx+rbx]	;fetch ltx addr
	mov	qword[current.texture], rax	;and store as current texture
	mov	rbx, qword[rax-24]	;now load the first qword here (textures width)
	mov	qword[current.t_width], rbx	;and store here
	movups	xmm0, [rax-16]	;then load the texture simd from here
	movaps	[current.t_simd], xmm0	;and store
	;----------------------------------------
	;STORE ADDRESS FOR LUV DATA
	movzx	rbx, word[lsc_objects+rcx+4]	;retrieve luv byte offset
	shl	rbx, 3	;and change to qword offset again
	mov	rax, qword[lsc_assets.luv+rbx]	;retrieve luv address
	mov	qword[current.uv], rax	;and store here
	;----------------------------------------
	;HANDLE MAINLOOP EVENTS
	call	_mainloop	;yeah
	push	r8	;push mainloop offset addr
	;----------------------------------------
	;CONVERT OBJECT TO SCREEN SPACE
	mov	rsi, qword[current.mesh]	;load object addr into source
	mov	rdi, objbuf	;and destination is objbuf
	mov	rdx, matrix.view	;multiply by view matrix
	call	_matmul	;multiply matrices together	
	mov	rsi, rdi	;source == destination (in place multiplication)
	mov	rdx, matrix.persp	;multiply by perspective matrix
	call	_matmul	;go
	call	_normalise_w	;now normalise by the w component
	cmp	rax, -1	;check if rax is -1 (clip obj)
	jz	.clip	;if yes then skip this obj
	call	_trans_viewport	;and then translate this directly to viewport
	;----------------------------------------
	;DRAW MESH
	mov	r13, qword[current.mesh]
	call	_draw_mesh	;now finally draw the mesh
.clip:
	;----------------------------------------
	;GET BACK PUSHED VALUES AND LOOP OVER
	pop	r8	;pop	back mainloop offset addr
	pop	rcx	;get back object counter thing
	sub	rcx, 6	;subtract 6 to go to previous object
	jns	.mainloop	;if result isnt signed then loop over
.print_fbuf:
	;----------------------------------------
	;PRINT FRAMEBUFFER
	mov	rax, 1	;sys_write
	mov	rdi, 1	;write to stdout
	mov	rsi, qword[FRAMEBUFFER]	;framebuffer addr start
	mov	edx, dword[FRAMEBUFFER+8]	;and length in following dword
	syscall
	;----------------------------------------
	;POLL STDIN FOR INPUT
	mov	rax, 7	;sys_poll ofc
	mov	rdi, poll	;struct to poll
	mov	rsi, 1	;only 1 fd to poll here (stdin)
	xor	rdx, rdx	;timeout is 0 (poll instantly)
	syscall
	cmp	rax, 0	;check if return is 0
	jz	.no_input	;if it is then nothing available
	;----------------------------------------
	;PROCESS INPUT
	xor	rax, rax	;otherwise input is available sys_read
	xor	rdi, rdi	;read from stdin
	mov	rsi, scratchpad	;put data onto scratchpad for now
	mov	rdx, 8	;good amount probably
	syscall
	call	_input_main	;handle the input
.no_input:
	;----------------------------------------
	;GET FRAME END TIME
	mov	rax, 228	;sys_clock_gettime again
	mov	rdi, 4	;and again clock_monotonic
	mov	rsi, nanosleep	;and now store data in nanosleep struct
	syscall
	;----------------------------------------
	;CALCULATE FRAME DELTA
	mov	rax, qword[nanosleep.nsec]	;fuck ton of memory accesses incomming
	sub	rax, qword[monotonic.nsec]	;calculate nsec difference between frame
	mov	rbx, qword[nanosleep.sec]	;and also get the time in seconds
	sub	rbx, qword[monotonic.sec]	;and calculate sec difference
	imul	rbx, NSEC	;multiply this by nanoseconds in 1 second
	add	rbx, rax	;and add together to get the frame delta
	mov	rax, qword[status.nsec]	;rax is now 1/<frame> in nanoseconds
	sub	rax, rbx	;and subtract the frame delta
	mov	qword[nanosleep.nsec], rax	;store here
	mov	qword[nanosleep.sec], 0	;and store a 0 in the seconds structure
	;----------------------------------------
	;SLEEP UNTIL NEXT FRAME IS READY
	mov	rax, 35	;sys_nanosleep
	mov	rdi, nanosleep	;sleep for <frame>-<frame delta>
	xor	rsi, rsi	;return struct for when sleep is interrupted is nil
	syscall
	jmp	.new_frame	;render another frame after sleeping
_int:
	;----------------------------------------
	;PREPARE EXIT MESSAGES
	mov	r15, stat_int	;exit message as interrupt msg
	mov	r14, stat_int_len	;and length is defined in this macro
	call	_kill	;call to kill
_man:
	mov	r15, stat_man	;exit message as manual stop msg
	mov	r14, stat_man_len	;length again
	call	_kill	;call to kill
_odd:
	mov	r15, stat_odd	;exit message as odd term msg
	mov	r14, stat_odd_len	;length
	call	_kill	;call to kill
_seg:
	mov	r15, stat_seg	;exit message as segfault msg
	mov	r14, stat_seg_len	;and length is defined again here

_kill:
	;----------------------------------------
	;DE-ALLOCATE MEMORY
	mov	r8d, dword[alloc_data.pointer]	;save allocation pointer
	xor	rbx, rbx	;will be used as counter for allocations
.loop_dealloc:
	cmp	rbx, r8	;check if counter is equal to last allocation
	jz	.finish_dealloc	;if yes, then finished deallocating
	mov	rax, 11	;sys_munmap
	mov	rdi, qword[alloc_data.addr+rbx]	;unmap this addr
	mov	esi, dword[alloc_data.addr+rbx+8]	;with this length
	syscall
	add	rbx, 12	;go to next address length pair
	jmp	.loop_dealloc	;and loop over
.finish_dealloc:
	;----------------------------------------
	;CLEAN UP TERMINAL SETTINGS
	or	dword[term_io.c_lflag], 0b00000010	;set canonical flag
	or	dword[term_io.c_lflag], 0b00001000	;set echo flag
	mov	rax, 16	;sys_ioctl
	mov	rdi, 1	;use stdout
	mov	rsi, 21506	;command TCSETS, clean terminal settings
	mov	rdx, term_io	;clean settings structure
	syscall
	;----------------------------------------
	;PRINT EXIT MESSAGES
	mov	rax, 1	;sys_write
	mov	rdi, 1	;write to stdout
	mov	rsi, esc_erase	;use erase escape
	mov	rdx, 11	;and also include esc_home + esc_reset
	syscall
	mov	rax, 1	;now at home position, write again
	mov	rdi, 1	;again to stdout
	mov	rsi, r15	;this time print exit message
	mov	rdx, r14	;length also here
	syscall
	mov	rax, 60	;then exit normaly
	mov	rdi, 0	;with exit code 0
	syscall
