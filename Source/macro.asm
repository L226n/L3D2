;-----------------------------------------------
;MACROS
%define	LOOK_UP	byte[scratchpad+2], "A"
%define	F1	word[scratchpad+1], "OP"
%define	F2	word[scratchpad+1], "OQ"
%define	LOOK_DOWN	byte[scratchpad+2], "B"
%define	LOOK_LEFT	byte[scratchpad+2], "C"
%define	LOOK_RIGHT	byte[scratchpad+2], "D"
%define	MOVE_F	byte[scratchpad], "w"
%define	MOVE_B	byte[scratchpad], "s"
%define	MOVE_R	byte[scratchpad], "d"
%define	MOVE_L	byte[scratchpad], "a"
%define	MOVE_U	byte[scratchpad], "q"
%define	MOVE_D	byte[scratchpad], "e"
%define	ARROW_
%define	FRAMEBUFFER	alloc_data.addr
%define	DEPTHBUFFER	alloc_data.addr+12
%define	MATRIX_DELIMITER	0x7fffffff
%define	MAX_ALLOC	100
%define	HEADER_LEN	3
%define	UNIT_LEN	26
%define	HALF_UNIT	11
%define	FLOAT_ONE	0x3F800000
