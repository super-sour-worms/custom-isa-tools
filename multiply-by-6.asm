or zr, #10, g4 ; put any number in place of #10
add g4, g4, g7
add g7, g4, g7
add g7, g7, g7 ; and get the value 6 times larger in g7
mov g7, g6
;       mem   str    g7    zr        imm       #32
; .byte 0b00001_101, 0b00111_000, 0b00_0_00000, 0b00100000
.byte 0b00100000, 0b00_0_00000, 0b00111_000, 0b00001_101

; memory read example
; lod g5[42 lsl 2], g5
; lsl <<
; lsr >>
; asr +>
; ror <>
