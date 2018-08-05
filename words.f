: MOD /MOD DROP ;
: '\n' 10 ;
: BL 32 ;
: CR '\n' EMIT ;
: SPACE BL EMIT ;
: NEGATE 0 SWAP 0 ;

: 0= 0 = ;
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
	' 0BRANCH
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

 97 EMIT     
	







: TEST IF 97 EMIT ELSE 98 EMIT THEN CR ;
\ 1
1 TEST

KILL
