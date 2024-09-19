section	.data
	;-----------------------------------------------
	;ANSI COLOURS
	ansi_white:	db	"231"
	;-----------------------------------------------
	;BUFFERS
	align	16
	scratchpad:	times 100	dd	0
	align	16
	objbuf:	times	500*4+4	dd	0
	;-----------------------------------------------
	;MATRICES
	camera:
		.r	dd	1.0, 0.0, 0.0, 0
		.u	dd	0.0, 1.0, 0.0, 0
		.f	dd	0.0, 0.0, 1.0, 0
		.far	dd	100.0
		.fov	dd	0.534
		.near	dd	1.0
		align	16
		.pos	dd	0.0, 0.0, -1.5, 0
		.speed	dd	0.2
		.angle	dd	0.1
		.angle_neg	dd	-0.1
	matrix:
		.cam_rot	dd	0.0, 0.0, 0.0, 0.0
				dd	0.0, 0.0, 0.0, 0.0
				dd	0.0, 0.0, 0.0, 0.0
				dd	0.0, 0.0, 0.0, 1.0, MATRIX_DELIMITER
		.cam_trans	dd	1.0, 0.0, 0.0, 0.0
				dd	0.0, 1.0, 0.0, 0.0
				dd	0.0, 0.0, 1.0, 0.0
				dd	0.0, 0.0, 0.0, 1.0, MATRIX_DELIMITER
		.persp	dd	0.0, 0.0, 0.0, 0.0
			dd	0.0, 0.0, 0.0, 0.0
			dd	0.0, 0.0, 0.0, 1.0
			dd	0.0, 0.0, 0.0, 0.0, MATRIX_DELIMITER
		.screen	dd	0.0, 0.0, 0.0, 0.0
			dd	0.0, 0.0, 0.0, 0.0
			dd	0.0, 0.0, 1.0, 0.0
			dd	0.0, 0.0, 0.0, 1.0, MATRIX_DELIMITER
		.view	dd	1.0, 0.0, 0.0, 0.0
			dd	0.0, 1.0, 0.0, 0.0
			dd	0.0, 0.0, 1.0, 0.0
			dd	0.0, 0.0, 0.0, 1.0, MATRIX_DELIMITER
		.cube	dd	0.5, 0.5, 0.5, 1.0
			dd	-0.5, 0.5, 0.5, 1.0
			dd	0.5, -0.5, 0.5, 1.0
			dd	-0.5, -0.5, 0.5, 1.0
			dd	0.5, 0.5, -0.5, 1.0
			dd	-0.5, 0.5, -0.5, 1.0
			dd	0.5, -0.5, -0.5, 1.0
			dd	-0.5, -0.5, -0.5, 1.0, MATRIX_DELIMITER
			dw	5, 7, 4
			dw	7, 6, 4
			dw	1, 5, 0
			dw	5, 4, 0
			dw	-1
			dw	65535
		.cubeuv:
			dd	0.0, 0.0, 0.0, 1.0, 1.0, 0.0
			dd	0.0, 1.0, 1.0, 1.0, 1.0, 0.0
			dd	0.0, 0.0, 0.0, 1.0, 1.0, 0.0
			dd	0.0, 1.0, 1.0, 1.0, 1.0, 0.0
			
	;-----------------------------------------------
	;MULTI-LINE STRUCTS
	alloc_data:
		.addr	times MAX_ALLOC	dd	0, 0, 0 
		.pointer	dd	0
		.available	dd	0
		.current	dq	0
	barycentric:
		align	16
		.v	dd	0, 0, 0, 0
		.w	dd	0, 0, 0, 0
		.denom	dq	0
	current:
		.mesh	dq	0
		.texture	dq	0
		.t_width	dq	0
		align	16
		.t_simd	dd	0, 0
		.uv	dq	0
	file:
		.buf	db	"../Resources/tetrapod.l3d", 0
		.buf2	db	"../Resources/cube.ltx", 0
		.buf3	db	"../Resources/tetrapod.luv", 0
		.size	dq	0
	handler_int:
		dq	_int
		dd	0x04000000
	handler_seg:
		dq	_seg
		dd	0x04000000
	line:
		.x0:	dw	0
		.y0:	dw	0
		.x1:	dw	0
		.y1:	dw	0
	quaternion:
		.norm	dd	0, 0, 0, 0, 0
		.conj	dd	0, 0, 0, 0, 0
		align	16
		.mulmask_npnp:
			dd	0x80000000
			dd	0x00000000
			dd	0x80000000
			dd	0x00000000
		.mulmask_nppn:
			dd	0x80000000
			dd	0x00000000
			dd	0x00000000
			dd	0x80000000
		.mulmask_nnpp:
			dd	0x80000000
			dd	0x80000000
			dd	0x00000000
			dd	0x00000000
	status:
		.debug	db	1
		.wireframe	db	1
		.backfaces	db	1
	term_io:
		.c_iflag	dd	0
		.c_oflag	dd	0
		.c_cflag	dd	0
		.c_lflag	dd	0
		.c_line	db	0
		.c_cc	db	0
	term_size:
		.y	dw	0
		.x	dw	0
		.dy	dw	0
		.dx	dw	0
		.xb	dq	0
		.xzb	dq	0
		.guard	dq	0, 0
	unit_template:
		db	27, "[48;5;000m"
		db	27, "[38;5;000m"
		dd	"▄"
	var:
		.sky_colour	db	"000"
	vector:
		align	16
		.bitmask	dd	0xffffffff
				dd	0xffffffff
				dd	0xffffffff
				dd	0x00000000
	;-----------------------------------------------
	;SIMD SETS
	align	16
	simd_zero	dd	0.0, 0.0, 0.0, 0.0
	simd_one	dd	1.0, 1.0, 1.0, 1.0
	simd_screen	dd	0.0, 0.0, 0.0, 0.0
	zbuf_reset	dd	0x7f7fffff, 0x7f7fffff
	;-----------------------------------------------
	;STRING CONSTANTS
	stat_int:	db	"Program recieved signal 2 (SIGINT)", 10
	stat_int_len	equ	$ - stat_int
	stat_man:	db	"Program stopped manually", 10
	stat_man_len	equ	$ - stat_man
	stat_odd:	db	"Program requires even terminal width", 10
	stat_odd_len	equ	$ - stat_odd
	stat_seg:	db	"Program recieved signal 11 (SIGSEGV)", 10
	stat_seg_len	equ	$ - stat_seg
	;-----------------------------------------------
	;USEFUL ESCAPES
	esc_erase:	db	27, "[2J"
	esc_home:	db	27, "[H"
	esc_reset:	db	27, "[0m"

