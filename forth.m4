; The pointer to the threaded code is in r5,
; The data stack pointer will be r6
; The return stack pointer is r7

; Load the next threaded code word and execute it
nop
	jmp start
define(`NEXTi', `ld r0, [r5]
	add r5, 2
	ld r0, [r0]
	jmp r0')
define(`NEXT', `jmp next')
next:
	NEXTi
      	ret

define(`PUSHR', `push $1, r7')
define(`POPR', `pop $1, r7')
define(`PUSH', `push $1, r6')
define(`POP', `pop $1, r6')

DOCOL:
	PUSHR(r5)
	add r0, 2
	mov r5, r0
	NEXT

main:
	mov r7, 0xfe00
	mov r6, 0xfb00
	st [var_S0], r7
	;call set_up_data_segment
	mov r5, cold_start
	NEXT

cold_start:
	.dw QUIT

define(`F_IMMED', `0x80')
define(`F_HIDDEN', `0x20')
define(`F_LENMASK', `0x1f')
define(`link', `0')
define(`defword', `
.global name_$4:
	.dw link
	define(`link', name_$4)
	.db eval(`$2+$3')
	.ascii "$1"
	
.global $4:
	.dw DOCOL')

define(`defcode', `
.global name_$4:
	.dw link
	define(`link', `name_$4')
	.db eval(`$2+$3')
	.ascii "$1"
	
.global $4:
	.dw code_$4
code_$4:
')
	

defcode(DROP,4,0,DROP)
POP(r0)
NEXT

defcode(SWAP,4,0,SWAP)
POP(r0)
POP(r1)
PUSH(r0)
PUSH(r1)
NEXT

defcode(DUP,3,0,DUP)
ld r0, [r6]
PUSH(r0)
NEXT

defcode(OVER,4,0,OVER)
ld r0,[r6+2]
PUSH(r0)
NEXT

defcode(ROT,3,0,ROT)
POP(r0)
POP(r1)
POP(r2)
PUSH(r1)
PUSH(r0)
PUSH(r2)
NEXT

; need 2drop, 2dup, and 2swap
defcode(+,1,0,_ADD)
POP(r0)
POP(r1)
add r0, r1
PUSH(r0)
NEXT

defcode(-,1,0,_SUB)
POP(r0)
POP(r1)
sub r1, r0
PUSH(r1)
NEXT

defcode(=,1,0,EQU)
POP(r0)
POP(r1)
cmp r0, r1
set.eq r0
neg r0
PUSH(r0)
NEXT

defcode(<=,2,0,LE)
POP(r0)
POP(r1)
cmp r1, r0
set.le r0
neg r0
PUSH(r0)
NEXT

defcode(LT,2,0,LT)
POP(r0)
POP(r1)
cmp r1, r0
set.l r0
neg r0
PUSH(r0)
NEXT

defcode(AND,3,0,_AND)
POP(r0)
POP(r1)
and r0, r1
PUSH(r1)
NEXT
defcode(OR,2,0,_OR)
POP(r0)
POP(r1)
or r0, r1
PUSH(r1)
NEXT
defcode(XOR,3,0,_XOR)
POP(r0)
POP(r1)
xor r0, r1
PUSH(r1)
NEXT

defcode(INVERT,6,0,INVERT)
POP(r0)
not r0
PUSH(r0)
NEXT

defcode(EXIT,4,0,EXIT)
POPR(r5)
NEXT

defcode(LIT,3,0,LIT)
ld r0,[r5]
add r5,2
PUSH(r0)
NEXT

defcode(!,1,0,STORE)
POP(r1)
POP(r0)
st [r1], r0
NEXT

defcode(@,1,0,FETCH)
POP(r0)
ld r0,[r0]
PUSH(r0)
NEXT

defcode(C!,1,0,STORE)
POP(r1)
POP(r0)
st.b [r1], r0
NEXT

defcode(C@,1,0,FETCH)
POP(r0)
ld.b r0,[r0]
PUSH(r0)
NEXT

define(`defvar', `
defcode($1, $2, $3, $4)
PUSH(var_$4)
NEXT
var_$4:
.dw $5')


defcode(QUIT,4,0,QUIT)
POP(r0)
kill

defvar(STATE,5,0,STATE,0)
defvar(DP,5,0,DP,0)
defvar(LATEST,6,0,LATEST,name_QUIT) ; change this to the last thing defined
defvar(S0,2,0,S0,0)
defvar(BASE,4,0,BASE,10)

define(`defconst', `
defcode($1, $2, $3, $4)
PUSH($5)
NEXT')

defconst(VERSION,7,0,VERSION,1)
defconst(R0,2,0,RZ,0xfe00)
defconst(F_IMMED,7,0,__F_IMMED,F_IMMED)
defconst(F_HIDDEN,8,0,__F_HIDDEN,F_HIDDEN)
defconst(F_LENMASK,9,0,__F_LENMASK,F_LENMASK)

defcode(>R,2,0,TOR)
POP(r0)
PUSHR(r0)
NEXT

defcode(R>,2,0,FROMR)
POPR(r0)
PUSH(r0)
NEXT

defcode(RSP@,4,0,RSPFETCH)
PUSH(r7)
NEXT

defcode(RSP!,4,0,RSPSTORE)
POP(r7)
NEXT

defcode(DSP@,4,0,DSPFETCH)
mov r0,r6
PUSH(r0)
NEXT

defcode(DSP!,4,0,DSPSTORE)
POP(r6)
NEXT

defcode(EMIT,4,0,EMIT)
POP(r0)
1:
ld.b r1,[0xff03]
test r1,1
jmp.eq 1b
st.b [0xff02],r0
NEXT

defcode(KEY,3,0,KEY)
call _KEY
PUSH(r0)
NEXT
_KEY:
ld.b r0, [0xff03]
test r0, 4
jmp.eq _KEY
ld.b r0, [0xff02]
ret

defcode(WORD,4,0,WORD)
call _WORD
PUSH(r2)			;Base address
PUSH(r0)			;Length
NEXT

_WORD:
pushlr
1:
call _KEY
cmp r0, '\\' ;is it the start of a comment?
jmp.eq 3f			; if so, skip
cmp r0, ' '
jmp.le 1b			; keep looking for non-whitespace
mov r3, word_buffer
2:
; Write characters until a space is encountered
st.b [r3],r0
add r3, 1
call _KEY
cmp r0, ' '
jmp.g 2b
sub r3, word_buffer
mov r0, r3
mov r2, word_buffer
pop r1
jmp r1

3:
call _KEY
cmp r0, '\n'
jmp.ne 3b
jmp 1b

word_buffer:
	.ascii "                                "

defcode(NUMBER,6,0,NUMBER)
POP(r2)
POP(r3)
call _NUMBER
PUSH(r0)				;parsed number
PUSH(r2)				;number of unparsed characters
NEXT

_NUMBER:			;r2 = string length, r3 = start address
test r2, r2
jmp.ne 1f
ret
1:
	pushlr
	push r4
	mov r4, r3
	mov r1, 0
2:
	ld.b r0, [r3]
	cmp r0, '0'
	jmp.l 3f
	push r0
	push r3
	mov r0, 10
	call _mul 
	mov r1, r0
	pop r3
	pop r0
	add r3, 1
	sub r0, '0'
	add r1, r0
	sub r2, 1
	jmp.ne 2b

3:
	sub r3, r4
	mov r2, r3
	mov r0, r1

	pop r4
	pop r1
	jmp r1



prog:
	.dw WORD
	.dw NUMBER
	.dw DROP
	.dw QUIT


start:
	mov r7, 0xfe00
	mov r6, 0xfb00
	PUSH(3)
	PUSH(4)
	mov r5, prog
	jmp next
	
