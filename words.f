: MOD /MOD DROP ;
: '\n' 10 ;
: BL 32 ;
: CR '\n' EMIT ;
: SPACE BL EMIT ;
: NEGATE 0 SWAP - ;

: 0= 0 = ;
: > <= INVERT ;
: >= < INVERT ;
: <> = INVERT ;
: 0< 0 < ;
: 0> 0 > ;
: 1+ 1 + ;
: 1- 1 - ;
: TRUE 1 ;
: FALSE 0 ;
: NOT 0= ;
: LITERAL IMMEDIATE
	  ' LIT ,
	  ,
;
: ':' [ CHAR : ] LITERAL ;
: ';' [ CHAR ; ] LITERAL ;
: '(' [ CHAR ( ] LITERAL ;
: ')' [ CHAR ) ] LITERAL ;
: '"' [ CHAR " ] LITERAL ;
: 'A' [ CHAR A ] LITERAL ;
: '0' [ CHAR 0 ] LITERAL ;
: '-' [ CHAR - ] LITERAL ;
: '.' [ CHAR . ] LITERAL ;

: [COMPILE] IMMEDIATE
 	    WORD
 	    FIND
	    >CFA
	    ,
;

: RECURSE IMMEDIATE
	  LATEST @
	  >CFA
	  ,
;



: IF IMMEDIATE
  ' 0BRANCH ,
  HERE @
  0 ,
  ;
: THEN IMMEDIATE
  DUP
  HERE @ SWAP - 
  SWAP !
;
: ELSE IMMEDIATE
  ' BRANCH ,
  HERE @
  0 ,
  SWAP
  DUP
  HERE @ SWAP -
  SWAP !
;

: BEGIN IMMEDIATE
  HERE @
;

: UNTIL IMMEDIATE
	' 0BRANCH ,
	HERE @ -
	,
;

: AGAIN IMMEDIATE
	' BRANCH ,
	HERE @ -
	,
;

: WHILE IMMEDIATE
	' 0BRANCH ,
	HERE @
	0 ,
;

: REPEAT IMMEDIATE
	 ' BRANCH ,
	 SWAP
	 HERE @ - ,
	 DUP
	 HERE @ SWAP -
	 SWAP !
       ;

: UNLESS IMMEDIATE
	 ' NOT ,
	  [COMPILE] IF
	    ;

: ( IMMEDIATE
    1
    BEGIN
      KEY
      DUP '(' = IF
	DROP
	1 +
      ELSE
	')' = IF
	  1 -
	THEN
      THEN
      DUP 0= UNTIL
    DROP
;

: NIP  SWAP DROP ;
: TUCK  SWAP OVER ; 

: SPACES 
  DUP 0>
  IF
    BEGIN
      SPACE
      1-
      DUP 0=
    UNTIL
  ELSE
    DROP
  THEN
;

: TEST3
  20
  BEGIN
    97 EMIT
    1-
    DUP 0=
  UNTIL
  DROP
  CR
  ;
  

: U.
  BASE @ /MOD
  ?DUP IF
    RECURSE
  THEN


  DUP 10 < IF
    '0'
  ELSE

    10 -
    'A'
  THEN
  +
  EMIT
;

: .S
  DSP@
  
  BEGIN
    DUP S0 @ <
  WHILE
    DUP @ U.
    SPACE
    2 +
  REPEAT
  DROP
  ;

: UWIDTH
  BASE @ /
  ?DUP IF
    RECURSE 1+
  ELSE
    1
  THEN
;

: U.R
  SWAP
  DUP
  UWIDTH
  ROT
  SWAP -
  SPACES
  U.
;



: .R
  SWAP
  DUP 0< IF
    NEGATE
    1
    SWAP
    ROT
    1-
  ELSE
    0
    SWAP
    ROT
  THEN
  SWAP
  DUP
  UWIDTH
  ROT
  SWAP -
  SPACES
  SWAP
  IF '-' EMIT THEN
  U.
;

: . 0 .R SPACE ;
: U. U. SPACE ;
: ? @ . ;
: DEPTH
  S0 @ DSP@ -
  4 - ;
: ALIGNED
  1 + 1 INVERT AND ;
: ALIGN HERE @ ALIGNED HERE ! ;
: C,
  HERE @ C!
  1 HERE + !
  ;

: S" IMMEDIATE
     STATE @ IF
       ' LITSTRING
       HERE @
       0 ,
       BEGIN
	 KEY
	 DUP '"' <>
       WHILE
	 C,
       REPEAT
       DROP
       DUP
       HERE @ SWAP -
       2 -
       SWAP !
       ALIGN
     ELSE
       HERE @
       BEGIN
	 KEY
	 DUP '"' <>
       WHILE
	 OVER C!
	 1+
       REPEAT
       DROP
       HERE @ -
       HERE @
       SWAP
     THEN
;

: ." IMMEDIATE
     STATE @ IF
       [COMPILE] S"
       ' TELL ,
     ELSE
       BEGIN
	 KEY
	 DUP '"' = IF
	   DROP
	   EXIT
	 THEN
	 EMIT
       AGAIN
     THEN
;

: CONSTANT
  WORD
  CREATE
  DOCOL ,
  ' LIT ,
  ,
  ' EXIT ,
;
: ALLOT
  HERE @ SWAP
  HERE +!
;
: CELLS 2 * ;

:  VARIABLE
  2 ALLOT
  WORD CREATE
  DOCOL ,
  ' LIT ,
  ,
  ' EXIT ,
;
: ID
  2 +
  DUP C@
  F_LENMASK AND
  BEGIN
    DUP 0>
  WHILE
    SWAP 1+
    DUP C@
    EMIT
    SWAP 1-
  REPEAT
  DROP
  DROP
;

  

: ?HIDDEN
  2 + C@
  F_HIDDEN AND
;


: WORDS
  LATEST @
  BEGIN
    ?DUP 
  WHILE
    DUP ?HIDDEN NOT IF
      DUP ID
      SPACE
    THEN
    @
  REPEAT
  CR
  ;

: FORGET
  WORD FIND
  DUP @ LATEST !
  HERE !
;


: CASE IMMEDIATE
       0
       ;
: OF IMMEDIATE
     ' OVER ,
     ' = ,
     [COMPILE] IF
     ' DROP ,
     ;

: ENDOF IMMEDIATE
     [COMPILE] ELSE
     ;
: ENDCASE IMMEDIATE
	  ' DROP ,
	  BEGIN
	    ?DUP
	  WHILE
	    [COMPILE] THEN
	    REPEAT
	    ;
: :NONAME
    0 0 CREATE
    HERE @
    DOCOL ,
    ]
  ;
: ['] IMMEDIATE
    ' LIT ,
    ;
\ I'm done for today




       
       
	 


KILL
