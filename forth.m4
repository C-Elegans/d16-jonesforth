; The pointer to the threaded code is in r5,
; The data stack pointer will be r6
; The return stack pointer is r7

; Load the next threaded code word and execute it
nop
	jmp start
define(`NEXTi', `ld r0, [r5]
	add r5, 2
	ld r1, [r0]
	jmp r1')
define(`NEXT', `jmp next')
next:
	NEXTi
      	ret

define(`PUSHR', `push $1, r7')
define(`POPR', `pop $1, r7')
define(`PUSH', `push $1, r6')
define(`POP', `pop $1, r6')

.global DOCOL:
	PUSHR(r5)
	add r0, 2
	mov r5, r0
	NEXT
start:
main:
	mov r7, 0xfe00
	mov r6, 0xfb00
	st [var_S0], r7
	call set_up_data_segment
	mov r5, cold_start
	NEXT

set_up_data_segment:
	mov r0, data
	st [var_DP], r0
	ret

cold_start:
	.dw QUIT

define(`F_IMMED', `0x80')
define(`F_HIDDEN', `0x20')
define(`F_LENMASK', `0x1f')
define(`link', `0')
define(`defword', `
.global name_$4:
	.dw link
	.db eval(len(`$1')+$3)
	.ascii "$1"
	
	define(`link', 'name_$4`)
.global $4:
	.dw DOCOL')

define(`defcode', `
.global name_$4:
	.dw link
	define(`link', `name_$4')
	.db eval(len(`$1')+$3)
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

defcode(*,1,0,MUL)
POP(r0)
POP(r1)
call _mul
PUSH(r0)
NEXT

defcode(/MOD,4,0,DIVMOD)
POP(r1)
POP(r0)
call _div
PUSH(r1)
PUSH(r0)
NEXT

defcode(INCR4,5,0,INCR4)
POP(r0)
add r0, 4
PUSH(r0)
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
defcode(XOR,3,0,myXOR)
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

defcode(C!,2,0,CSTORE)
POP(r1)
POP(r0)
st.b [r1], r0
NEXT

defcode(C@,2,0,CFETCH)
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

defword(QUIT,4,0,QUIT)
.dw RZ
.dw RSPSTORE
.dw INTERPRET
.dw BRANCH
.dw -4

defvar(STATE,5,0,STATE,0)
defvar(DP,5,0,DP,0)
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
	add r4, r2
	mov r1, 0
4:
	ld.b r0, [r3]
	push 0
	cmp r0, '-'
	jmp.ne 2f
	pop r0
	push 1
	add r3, 1
	
2:
	ld.b r0, [r3]
	cmp r0, '0'
	jmp.l 3f
	cmp r0, '9'
	jmp.g 3f
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
	sub r4, r3
	mov r2, r4
	mov r0, r1

	pop r1
	test r1, r1
	jmp.eq 1f
	neg r0
1:	
	pop r4
	pop r1
	jmp r1
defcode(>CFA,4,0,TCFA)
POP(r0)
call _TCFA
PUSH(r0)
NEXT
_TCFA:
mov r1,0
add r0,2			; Skip link pointer
ld.b r1,[r0]			; Load flags byte
add r0, 1			; skip the flags byte
and r1,F_LENMASK		; Extract the length
add r0,r1			; Skip the length
add r0,1			; Align the address to 2
and r0,0xfffe
ret

defword(DFA,3,0,TDFA)
.dw TCFA
.dw INCR4
.dw EXIT

defcode(`(FIND)',6,0,PAREN_FIND)
POP(r1)
POP(r0)
call _FIND
PUSH(r0)
NEXT

_FIND:				; r1 = length, r0 = address
pushlr
push r4
mov r2, r0
ld r4,[var_LATEST]
1:
test r4, r4
jmp.eq 4f
ld.b r3,[r4+2]
and r3,eval(F_HIDDEN|F_LENMASK)
cmp r3, r1			; Lengths the same?
jmp.ne 2f

push r1
push r2
mov r0, r4
add r0, 3
mov r1, r3
call strcmp
pop r2
pop r1
test r0, r0
jmp.ne 2f
mov r0, r4
pop r4
pop r1
jmp r1

2:
ld r4,[r4]
jmp 1b

4:
pop r4
mov r0, 0
pop r1
jmp r1






.global strcmp: ; r0 = str0, r1 = length, r2 = str1
push r3
push r4
mov r3,r0
1:
ld.b r0, [r3]
ld.b r4, [r2]
sub r0, r4
jmp.ne 2f
add r3, 1
add r2, 1
sub r1, 1
jmp.eq 2f
jmp 1b
2:
pop r4
pop r3
ret



defcode(`HEADER,',7,0,HEADER_COMMA)
POP(r2)
POP(r3)
ld r1,[var_DP]
ld r0,[var_LATEST]
st [r1], r0
add r1,2
st.b [r1],r2
add r1, 1

1:
ld.b r0, [r3]
st.b [r1], r0
add r1, 1
add r3, 1
sub r2, 1
jmp.ne 1b

add r1,1
and r1,0xfffe
ld r0,[var_DP]
st [var_LATEST], r0
st [var_DP], r1
NEXT

defcode(`,',1,0,COMMA)
POP(r0)
call _COMMA
NEXT
_COMMA:
ld r1, [var_DP]
st [r1],r0
add r1,2
st [var_DP], r1
ret

defcode([,1,F_IMMED,LBRAC)
mov r0,0
st [var_STATE],r0
NEXT

defcode(],1,0,RBRAC)
mov r0,1
st [var_STATE], r0
NEXT

defword(:,1,0,COLON)
.dw WORD
.dw HEADER_COMMA
.dw LIT
.dw DOCOL
.dw COMMA
.dw LATEST
.dw FETCH
.dw HIDDEN
.dw RBRAC
.dw EXIT

defword(;,1,F_IMMED,SEMICOLON)
.dw LIT
.dw EXIT
.dw COMMA
.dw LATEST
.dw FETCH
.dw HIDDEN
.dw LBRAC
.dw EXIT

defcode(IMMEDIATE,9,F_IMMED,IMMEDIATE)
ld r0,[var_LATEST]
add r0,2
ld.b r1,[r0]
xor r1,F_IMMED
st.b [r0], r1
NEXT

defcode(HIDDEN,6,0,HIDDEN)
POP(r0)
add r0, 2
ld.b r1,[r0]
xor r1,F_HIDDEN
st.b [r0], r1
NEXT

defword(HIDE,4,0,HIDE)
.dw WORD
.dw PAREN_FIND
.dw HIDDEN
.dw EXIT

defcode(['],3,0,BRACKET_TICK)
ld r0,[r5]
add r5, 2
PUSH(r0)
NEXT

defcode(BRANCH,6,0,BRANCH)
ld r0, [r5]
add r5, r0
NEXT

defcode(0BRANCH,7,0,ZBRANCH)
POP(r0)
test r0, r0
jmp.eq code_BRANCH
add r5, 2
NEXT

defcode(LITSTRING,9,0,LITSTRING)
; This needs to be looked at...
ld r0, [r5]
add r5, 2
PUSH(r5)
PUSH(r0)
add r5,r0
add r5, 1
and r5, 0xfffe
NEXT

defcode(TELL,4,0,TELL)
POP(r2) 			; Length of string
POP(r1)				; Address of string
call _TELL
NEXT
_TELL:
ld.b r0, [0xff03]
test r0, 1
jmp.eq _TELL
ld.b r0, [r1]
st.b [0xff02], r0
add r1, 1
sub r2, 1
jmp.ne _TELL
ret

DODOES:
ld r1,[r0+2]
test r0, r0
jmp.eq 1f
sub r7,2
st [r7], r5
ld r5,[r0+2]
1:
add r0,4
PUSH(r0)
NEXT

defcode(KILL,4,0,_KILL)
POP(r0)
POP(r1)
ld r2, [var_STATE]
kill


defcode(INTERPRET,9,0,INTERPRET) 
call _WORD			; Returns r0 = length, r2 = base address
mov r1, 0
st [interpret_is_lit], r1
mov r1, r0
mov r0, r2
push r2
push r1
call _FIND
pop r1
pop r2

test r0, r0			; Did we find the word?
jmp.eq 1f

; We found the word
ld.b r1,[r0+2]
push r1
call _TCFA
pop r1
test r1, F_IMMED
jmp.ne 7f
jmp 2f

1:				; Not found in dictionary
mov r3, 1
st [interpret_is_lit], r3
mov r3, r2			;base address
mov r2, r1			;string length
call _NUMBER			; r0 = parsed number, r2 = unparsed characters
test r2, r2
jmp.ne 6f
mov r1, r0
mov r0, LIT

2:
ld r2,[var_STATE]
cmp r2, 1
jmp.ne 4f			; are we executing?
; compile the word
push r1
call _COMMA
pop r1
ld r2,[interpret_is_lit]
test r2, r2
jmp.eq 3f			; Is this a literal
; Yes, compile in the number
mov r0, r1
call _COMMA
3:
NEXT
7:
4:				; Interpret mode
ld r3,[interpret_is_lit]
test r3, r3
jmp.ne 5f			; Is this a literal
ld r1, [r0]
jmp r1

5:
PUSH(r1)
NEXT

6:				; parse error
mov r1, msg
mov r2, 12
call _TELL
NEXT

msg:
.ascii "Parse Error\n"


NEXT
interpret_is_lit:
.dw 0

defword(/,1,0,DIV)
.dw DIVMOD
.dw SWAP
.dw DROP
.dw EXIT


defvar(LATEST,6,0,LATEST,link) ; change this to the last thing defined

prog:
	.dw _KILL


	
