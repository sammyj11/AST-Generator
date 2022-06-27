functor WhileLexFun(structure Tokens:While_TOKENS)=
   struct
    structure UserDeclarations =
      struct
(*datatype lexresult= DIV | EOF | EOS | MOD |PLUS |  MINUS | TIMES | PROG| INT| BOOL| TT| FF| NOT| AND| OR| LT|LEQ|GT|GEQ|EQ|NEQ| SET | SEQ| ITE | WH | BLK | DEC |CMD | IEXP | BEXP *)
structure Tokens= Tokens
type pos = int
 type svalue = Tokens.svalue
 type ('a,'b) token = ('a,'b) Tokens.token  
 type lexresult = (svalue, pos) token
type lexarg = string
type arg = lexarg

  val pos = ref 0
  val lnnum = ref 1             

 val error = fn (token, col:int, lnnum:int) => TextIO.output (TextIO.stdOut,"Error at:" ^ token ^ ":"^ (Int.toString lnnum) ^ ":" ^  (Int.toString col)  ^ ", ")

 val eof = fn (file) => Tokens.EOF(!lnnum, !pos)
 
 val reserveWords = [("while", Tokens.WH), ("do", Tokens.DO), ("endwh", Tokens.ENDWH), ("if", Tokens.IF),("then", Tokens.THEN), ("else",Tokens.ELSE), ("endif",Tokens.ENDIF),("program", Tokens.PROG),("read", Tokens.READ), ("write", Tokens.WRITE),("var", Tokens.VAR), ("tt", Tokens.TT),("ff", Tokens.FF)]
 
 fun findReserveWords( token: string , lnnums : int , poss : int) =
 case List.find(fn (given, _)=> token = given) reserveWords of
   SOME (_, TOKEN) => TOKEN(lnnums, poss)
  | NONE => Tokens.VARI(token, lnnums, poss)

 
end (* end of user routines *)
exception LexError (* raised if illegal leaf action tried *)
structure Internal =
	struct

datatype yyfinstate = N of int
type statedata = {fin : yyfinstate list, trans: string}
(* transition & final state table *)
val tab = let
val s = [ 
 (0, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (1, 
"\000\000\000\000\000\000\000\000\000\040\043\000\000\041\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\040\039\000\000\000\038\036\000\035\034\033\032\031\030\029\028\
\\027\027\027\027\027\027\027\027\027\027\024\023\020\018\016\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000\008\012\008\008\008\008\008\008\009\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\007\005\004\003\000\
\\000"
),
 (5, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\006\000\000\000\
\\000"
),
 (8, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\008\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000"
),
 (9, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\008\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\010\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000"
),
 (10, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\008\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\011\008\008\008\008\008\008\000\000\000\000\000\
\\000"
),
 (12, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\008\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\013\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000"
),
 (13, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\008\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\014\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000"
),
 (14, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\008\
\\000\008\008\008\008\008\008\008\008\008\008\008\015\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000"
),
 (16, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\017\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (18, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\019\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (20, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\022\021\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (24, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\026\000\000\025\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (27, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\027\027\027\027\027\027\027\027\027\027\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (36, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\037\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (40, 
"\000\000\000\000\000\000\000\000\000\040\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\040\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (41, 
"\000\000\000\000\000\000\000\000\000\000\042\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (43, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\044\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
(0, "")]
fun f x = x 
val s = List.map f (List.rev (tl (List.rev s))) 
exception LexHackingError 
fun look ((j,x)::r, i: int) = if i = j then x else look(r, i) 
  | look ([], i) = raise LexHackingError
fun g {fin=x, trans=i} = {fin=x, trans=look(s,i)} 
in Vector.fromList(List.map g 
[{fin = [], trans = 0},
{fin = [], trans = 1},
{fin = [], trans = 1},
{fin = [(N 62)], trans = 0},
{fin = [(N 73)], trans = 0},
{fin = [], trans = 5},
{fin = [(N 24)], trans = 0},
{fin = [(N 71)], trans = 0},
{fin = [(N 90)], trans = 8},
{fin = [(N 90)], trans = 9},
{fin = [(N 90)], trans = 10},
{fin = [(N 82),(N 90)], trans = 8},
{fin = [(N 90)], trans = 12},
{fin = [(N 90)], trans = 13},
{fin = [(N 90)], trans = 14},
{fin = [(N 78),(N 90)], trans = 8},
{fin = [(N 34)], trans = 16},
{fin = [(N 32)], trans = 0},
{fin = [], trans = 18},
{fin = [(N 48)], trans = 0},
{fin = [(N 39)], trans = 20},
{fin = [(N 45)], trans = 0},
{fin = [(N 37)], trans = 0},
{fin = [(N 21)], trans = 0},
{fin = [(N 67)], trans = 24},
{fin = [(N 42)], trans = 0},
{fin = [(N 65)], trans = 0},
{fin = [(N 85)], trans = 27},
{fin = [(N 19)], trans = 0},
{fin = [(N 92)], trans = 0},
{fin = [(N 52)], trans = 0},
{fin = [(N 69)], trans = 0},
{fin = [(N 54)], trans = 0},
{fin = [(N 50)], trans = 0},
{fin = [(N 60)], trans = 0},
{fin = [(N 58)], trans = 0},
{fin = [], trans = 36},
{fin = [(N 27)], trans = 0},
{fin = [(N 56)], trans = 0},
{fin = [(N 29)], trans = 0},
{fin = [(N 17)], trans = 40},
{fin = [(N 4),(N 14)], trans = 41},
{fin = [(N 4),(N 7)], trans = 0},
{fin = [(N 4),(N 9)], trans = 43},
{fin = [(N 12)], trans = 0}])
end
structure StartStates =
	struct
	datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val INITIAL = STARTSTATE 1;

end
type result = UserDeclarations.lexresult
	exception LexerError (* raised if illegal leaf action tried *)
end

fun makeLexer yyinput =
let	val yygone0=1
	val yyb = ref "\n" 		(* buffer *)
	val yybl = ref 1		(*buffer length *)
	val yybufpos = ref 1		(* location of next character to use *)
	val yygone = ref yygone0	(* position in file of beginning of buffer *)
	val yydone = ref false		(* eof found yet? *)
	val yybegin = ref 1		(*Current 'start state' for lexer *)

	val YYBEGIN = fn (Internal.StartStates.STARTSTATE x) =>
		 yybegin := x

fun lex (yyarg as (file:string)) =
let fun continue() : Internal.result = 
  let fun scan (s,AcceptingLeaves : Internal.yyfinstate list list,l,i0) =
	let fun action (i,nil) = raise LexError
	| action (i,nil::l) = action (i-1,l)
	| action (i,(node::acts)::l) =
		case node of
		    Internal.N yyk => 
			(let fun yymktext() = String.substring(!yyb,i0,i-i0)
			     val yypos = i0+ !yygone
			open UserDeclarations Internal.StartStates
 in (yybufpos := i; case yyk of 

			(* Application actions *)

  12 => ( lnnum := !lnnum +1; pos :=  0 ;continue())
| 14 => ( lnnum := !lnnum +1; pos :=  0 ;continue())
| 17 => (continue())
| 19 => let val yytext=yymktext() in 
              pos := !pos + size yytext;
              Tokens.DIV(!lnnum, !pos)
 
              end
| 21 => let val yytext=yymktext() in 
              pos := !pos + size yytext;
              Tokens.EOS(!lnnum, !pos)
             end
| 24 => let val yytext=yymktext() in 
         
         
         pos := !pos + size yytext;
         Tokens.OR(!lnnum, !pos)
          end
| 27 => let val yytext=yymktext() in 
         pos := !pos + size yytext;
         Tokens.AND(!lnnum, !pos)
          end
| 29 => let val yytext=yymktext() in 
         pos := !pos + size yytext;
         Tokens.NOT(!lnnum, !pos)
          end
| 32 => let val yytext=yymktext() in 
              pos := !pos + size yytext;
              Tokens.GE(!lnnum, !pos)
         end
| 34 => let val yytext=yymktext() in 
              pos := !pos + size yytext;
              Tokens.GT(!lnnum, !pos)
         end
| 37 => let val yytext=yymktext() in 
              pos := !pos + size yytext;
              Tokens.LE(!lnnum, !pos)
         end
| 39 => let val yytext=yymktext() in 
              pos := !pos + size yytext;
              Tokens.LT(!lnnum, !pos)
        end
| 4 => ( lnnum := !lnnum +1; pos :=  0 ;continue())
| 42 => let val yytext=yymktext() in 
              pos := !pos + size yytext;
              Tokens.ASSIGN(!lnnum, !pos)
        end
| 45 => let val yytext=yymktext() in 
              pos := !pos + size yytext;
              Tokens.NEQ(!lnnum, !pos)
         end
| 48 => let val yytext=yymktext() in 
              pos := !pos + size yytext;
              Tokens.EQ(!lnnum, !pos)
         end
| 50 => let val yytext=yymktext() in 
              pos := !pos + size yytext;
              Tokens.TIMES(!lnnum, !pos)
 
         end
| 52 => let val yytext=yymktext() in 
              pos := !pos + size yytext;
              Tokens.MINUS(!lnnum, !pos)
         end
| 54 => let val yytext=yymktext() in 
              pos := !pos + size yytext;
              Tokens.PLUS(!lnnum, !pos)
         end
| 56 => let val yytext=yymktext() in 
              pos := !pos + size yytext;
              Tokens.MOD(!lnnum, !pos)
             
         end
| 58 => let val yytext=yymktext() in 
              pos := !pos + size yytext;
              Tokens.LPAREN(!lnnum, !pos)
         end
| 60 => let val yytext=yymktext() in 
              pos := !pos + size yytext;
              Tokens.RPAREN(!lnnum, !pos)
         end
| 62 => let val yytext=yymktext() in 
              pos := !pos + size yytext;
              Tokens.UMINUS(!lnnum, !pos)
         end
| 65 => let val yytext=yymktext() in 
              pos := !pos + size yytext;
              Tokens.DCOLON(!lnnum, !pos)
         end
| 67 => let val yytext=yymktext() in 
              pos := !pos + size yytext;
              Tokens.COLON(!lnnum, !pos)
         end
| 69 => let val yytext=yymktext() in 
              pos := !pos + size yytext;
              Tokens.COMMA(!lnnum, !pos)
         end
| 7 => ( lnnum := !lnnum +1; pos :=  0 ;continue())
| 71 => let val yytext=yymktext() in 
              pos := !pos + size yytext;
              Tokens.LBRACE(!lnnum, !pos)
         end
| 73 => let val yytext=yymktext() in 
              pos := !pos + size yytext;
              Tokens.RBRACE(!lnnum, !pos)
         end
| 78 => let val yytext=yymktext() in 
              pos := !pos + size yytext;
              Tokens.BOOL(yytext,!lnnum, !pos)
         end
| 82 => let val yytext=yymktext() in 
              pos := !pos + size yytext;
              Tokens.INT(yytext,!lnnum, !pos)
         end
| 85 => let val yytext=yymktext() in 
              pos := !pos + size yytext;
              Tokens.NUM
	     (List.foldl(fn (n,m) => ord(n) - ord(#"0") + 10*m) 0 (explode yytext),
	      !lnnum, !pos)
         end
| 9 => ( lnnum := !lnnum +1; pos :=  0 ;continue())
| 90 => let val yytext=yymktext() in 
   pos := !pos + size yytext;
   findReserveWords(yytext, !lnnum, !pos)
         end
| 92 => let val yytext=yymktext() in error ( yytext, !lnnum, !pos);Tokens.BADCHAR( !lnnum, !pos) end
| _ => raise Internal.LexerError

		) end )

	val {fin,trans} = Unsafe.Vector.sub(Internal.tab, s)
	val NewAcceptingLeaves = fin::AcceptingLeaves
	in if l = !yybl then
	     if trans = #trans(Vector.sub(Internal.tab,0))
	       then action(l,NewAcceptingLeaves
) else	    let val newchars= if !yydone then "" else yyinput 1024
	    in if (String.size newchars)=0
		  then (yydone := true;
		        if (l=i0) then UserDeclarations.eof yyarg
		                  else action(l,NewAcceptingLeaves))
		  else (if i0=l then yyb := newchars
		     else yyb := String.substring(!yyb,i0,l-i0)^newchars;
		     yygone := !yygone+i0;
		     yybl := String.size (!yyb);
		     scan (s,AcceptingLeaves,l-i0,0))
	    end
	  else let val NewChar = Char.ord(Unsafe.CharVector.sub(!yyb,l))
		val NewChar = if NewChar<128 then NewChar else 128
		val NewState = Char.ord(Unsafe.CharVector.sub(trans,NewChar))
		in if NewState=0 then action(l,NewAcceptingLeaves)
		else scan(NewState,NewAcceptingLeaves,l+1,i0)
	end
	end
(*
	val start= if String.substring(!yyb,!yybufpos-1,1)="\n"
then !yybegin+1 else !yybegin
*)
	in scan(!yybegin (* start *),nil,!yybufpos,!yybufpos)
    end
in continue end
  in lex
  end
end
