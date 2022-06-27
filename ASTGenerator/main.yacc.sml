functor WhileLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : While_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* nil for User declarations *)
open SymbolTable
exception typeCheckerror

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\028\000\000\000\
\\001\000\001\000\041\000\000\000\
\\001\000\002\000\055\000\004\000\054\000\006\000\053\000\007\000\052\000\
\\008\000\051\000\009\000\050\000\010\000\049\000\011\000\048\000\
\\015\000\047\000\016\000\046\000\018\000\045\000\019\000\044\000\
\\020\000\043\000\022\000\060\000\000\000\
\\001\000\002\000\055\000\004\000\054\000\006\000\053\000\007\000\052\000\
\\008\000\051\000\009\000\050\000\010\000\049\000\011\000\048\000\
\\015\000\047\000\016\000\046\000\018\000\045\000\019\000\044\000\
\\020\000\043\000\025\000\059\000\000\000\
\\001\000\002\000\055\000\004\000\054\000\006\000\053\000\007\000\052\000\
\\008\000\051\000\009\000\050\000\010\000\049\000\011\000\048\000\
\\015\000\047\000\016\000\046\000\018\000\045\000\019\000\044\000\
\\020\000\043\000\034\000\074\000\000\000\
\\001\000\003\000\037\000\013\000\036\000\014\000\035\000\017\000\034\000\
\\033\000\033\000\039\000\032\000\040\000\031\000\000\000\
\\001\000\005\000\000\000\000\000\
\\001\000\012\000\029\000\000\000\
\\001\000\021\000\023\000\024\000\022\000\032\000\021\000\037\000\020\000\
\\038\000\019\000\040\000\018\000\000\000\
\\001\000\023\000\078\000\000\000\
\\001\000\026\000\077\000\000\000\
\\001\000\027\000\080\000\000\000\
\\001\000\028\000\005\000\000\000\
\\001\000\029\000\015\000\000\000\
\\001\000\031\000\011\000\000\000\
\\001\000\031\000\011\000\042\000\010\000\000\000\
\\001\000\035\000\026\000\036\000\025\000\000\000\
\\001\000\040\000\004\000\000\000\
\\001\000\040\000\013\000\000\000\
\\001\000\040\000\038\000\000\000\
\\001\000\041\000\003\000\000\000\
\\082\000\000\000\
\\083\000\000\000\
\\084\000\000\000\
\\085\000\000\000\
\\086\000\000\000\
\\087\000\000\000\
\\088\000\000\000\
\\089\000\000\000\
\\090\000\030\000\016\000\000\000\
\\091\000\000\000\
\\092\000\000\000\
\\093\000\000\000\
\\094\000\002\000\055\000\004\000\054\000\006\000\053\000\007\000\052\000\
\\008\000\051\000\009\000\050\000\010\000\049\000\011\000\048\000\
\\015\000\047\000\016\000\046\000\018\000\045\000\019\000\044\000\
\\020\000\043\000\000\000\
\\095\000\000\000\
\\096\000\002\000\055\000\004\000\054\000\006\000\053\000\007\000\052\000\
\\008\000\051\000\009\000\050\000\010\000\049\000\011\000\048\000\
\\015\000\047\000\016\000\046\000\018\000\045\000\019\000\044\000\
\\020\000\043\000\000\000\
\\097\000\000\000\
\\098\000\000\000\
\\099\000\004\000\054\000\006\000\053\000\007\000\052\000\008\000\051\000\
\\009\000\050\000\010\000\049\000\011\000\048\000\015\000\047\000\
\\016\000\046\000\018\000\045\000\019\000\044\000\020\000\043\000\000\000\
\\100\000\015\000\047\000\016\000\046\000\020\000\043\000\000\000\
\\101\000\015\000\047\000\016\000\046\000\020\000\043\000\000\000\
\\102\000\000\000\
\\103\000\000\000\
\\104\000\000\000\
\\105\000\000\000\
\\106\000\000\000\
\\107\000\000\000\
\\108\000\000\000\
\\109\000\006\000\053\000\007\000\052\000\008\000\051\000\009\000\050\000\
\\010\000\049\000\011\000\048\000\015\000\047\000\016\000\046\000\
\\018\000\045\000\019\000\044\000\020\000\043\000\000\000\
\\110\000\000\000\
\\111\000\000\000\
\\112\000\000\000\
\\113\000\015\000\047\000\016\000\046\000\018\000\045\000\019\000\044\000\
\\020\000\043\000\000\000\
\\114\000\015\000\047\000\016\000\046\000\018\000\045\000\019\000\044\000\
\\020\000\043\000\000\000\
\\115\000\015\000\047\000\016\000\046\000\018\000\045\000\019\000\044\000\
\\020\000\043\000\000\000\
\\116\000\015\000\047\000\016\000\046\000\018\000\045\000\019\000\044\000\
\\020\000\043\000\000\000\
\\117\000\006\000\053\000\007\000\052\000\009\000\050\000\010\000\049\000\
\\015\000\047\000\016\000\046\000\018\000\045\000\019\000\044\000\
\\020\000\043\000\000\000\
\\118\000\006\000\053\000\007\000\052\000\008\000\051\000\009\000\050\000\
\\010\000\049\000\015\000\047\000\016\000\046\000\018\000\045\000\
\\019\000\044\000\020\000\043\000\000\000\
\"
val actionRowNumbers =
"\020\000\017\000\012\000\024\000\
\\015\000\021\000\022\000\023\000\
\\018\000\032\000\013\000\029\000\
\\008\000\016\000\018\000\000\000\
\\007\000\005\000\019\000\030\000\
\\005\000\005\000\001\000\026\000\
\\027\000\028\000\031\000\005\000\
\\035\000\045\000\044\000\005\000\
\\005\000\050\000\049\000\005\000\
\\034\000\003\000\002\000\025\000\
\\033\000\005\000\005\000\005\000\
\\005\000\005\000\005\000\005\000\
\\005\000\005\000\005\000\005\000\
\\005\000\005\000\004\000\047\000\
\\051\000\014\000\014\000\043\000\
\\039\000\040\000\041\000\042\000\
\\057\000\055\000\054\000\056\000\
\\052\000\053\000\048\000\038\000\
\\046\000\010\000\009\000\014\000\
\\037\000\011\000\036\000\006\000"
val gotoT =
"\
\\001\000\079\000\000\000\
\\000\000\
\\000\000\
\\002\000\005\000\003\000\004\000\000\000\
\\004\000\007\000\007\000\006\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\010\000\000\000\
\\009\000\012\000\000\000\
\\000\000\
\\000\000\
\\008\000\015\000\000\000\
\\006\000\022\000\000\000\
\\005\000\025\000\000\000\
\\000\000\
\\000\000\
\\010\000\028\000\000\000\
\\000\000\
\\000\000\
\\010\000\037\000\000\000\
\\010\000\038\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\040\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\054\000\000\000\
\\010\000\055\000\000\000\
\\000\000\
\\000\000\
\\010\000\056\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\059\000\000\000\
\\010\000\060\000\000\000\
\\010\000\061\000\000\000\
\\010\000\062\000\000\000\
\\010\000\063\000\000\000\
\\010\000\064\000\000\000\
\\010\000\065\000\000\000\
\\010\000\066\000\000\000\
\\010\000\067\000\000\000\
\\010\000\068\000\000\000\
\\010\000\069\000\000\000\
\\010\000\070\000\000\000\
\\010\000\071\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\073\000\000\000\
\\007\000\074\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\077\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 80
val numrules = 37
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = string
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | VARI of unit ->  (string) | NUM of unit ->  (int)
 | INT of unit ->  (string) | BOOL of unit ->  (string)
 | EXPRE of unit ->  (AST.exp*string) | CMDS of unit ->  (AST.exp)
 | CMD of unit ->  (AST.exp) | COMSEQ of unit ->  (AST.exp)
 | TYP of unit ->  (AST.exp*string) | VLIST of unit ->  (string list)
 | DEC of unit ->  (AST.exp) | DECSEQ of unit ->  (AST.exp)
 | BLK of unit ->  (AST.exp) | START of unit ->  (AST.exp)
end
type svalue = MlyValue.svalue
type result = AST.exp
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 4) => true | _ => false
val showTerminal =
fn (T 0) => "EOS"
  | (T 1) => "OR"
  | (T 2) => "NOT"
  | (T 3) => "AND"
  | (T 4) => "EOF"
  | (T 5) => "GE"
  | (T 6) => "GT"
  | (T 7) => "EQ"
  | (T 8) => "LE"
  | (T 9) => "LT"
  | (T 10) => "NEQ"
  | (T 11) => "ASSIGN"
  | (T 12) => "TT"
  | (T 13) => "FF"
  | (T 14) => "DIV"
  | (T 15) => "TIMES"
  | (T 16) => "UMINUS"
  | (T 17) => "MINUS"
  | (T 18) => "PLUS"
  | (T 19) => "MOD"
  | (T 20) => "WH"
  | (T 21) => "DO"
  | (T 22) => "ENDWH"
  | (T 23) => "IF"
  | (T 24) => "THEN"
  | (T 25) => "ELSE"
  | (T 26) => "ENDIF"
  | (T 27) => "DCOLON"
  | (T 28) => "COLON"
  | (T 29) => "COMMA"
  | (T 30) => "LBRACE"
  | (T 31) => "RBRACE"
  | (T 32) => "LPAREN"
  | (T 33) => "RPAREN"
  | (T 34) => "BOOL"
  | (T 35) => "INT"
  | (T 36) => "READ"
  | (T 37) => "WRITE"
  | (T 38) => "NUM"
  | (T 39) => "VARI"
  | (T 40) => "PROG"
  | (T 41) => "VAR"
  | (T 42) => "BADCHAR"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 37) $$ (T 36) $$ (T 33) $$ (T 32)
 $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25)
 $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18)
 $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11)
 $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ 
(T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (file ):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.BLK BLK1, _, BLK1right)) :: _ :: ( _, ( 
MlyValue.VARI VARI1, _, _)) :: ( _, ( _, PROG1left, _)) :: rest671))
 => let val  result = MlyValue.START (fn _ => let val  (VARI as VARI1)
 = VARI1 ()
 val  (BLK as BLK1) = BLK1 ()
 in (AST.PROG(VARI, BLK))
end)
 in ( LrTable.NT 0, ( result, PROG1left, BLK1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.COMSEQ COMSEQ1, _, COMSEQ1right)) :: ( _, ( 
MlyValue.DECSEQ DECSEQ1, DECSEQ1left, _)) :: rest671)) => let val  
result = MlyValue.BLK (fn _ => let val  (DECSEQ as DECSEQ1) = DECSEQ1
 ()
 val  (COMSEQ as COMSEQ1) = COMSEQ1 ()
 in (AST.SEQ(DECSEQ, COMSEQ))
end)
 in ( LrTable.NT 1, ( result, DECSEQ1left, COMSEQ1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.DEC DEC1, _, DEC1right)) :: ( _, ( 
MlyValue.DECSEQ DECSEQ1, DECSEQ1left, _)) :: rest671)) => let val  
result = MlyValue.DECSEQ (fn _ => let val  (DECSEQ as DECSEQ1) = 
DECSEQ1 ()
 val  (DEC as DEC1) = DEC1 ()
 in (AST.SEQ(DECSEQ,DEC))
end)
 in ( LrTable.NT 2, ( result, DECSEQ1left, DEC1right), rest671)
end
|  ( 3, ( rest671)) => let val  result = MlyValue.DECSEQ (fn _ => (
 AST.nuexp()))
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 4, ( ( _, ( _, _, EOS1right)) :: ( _, ( MlyValue.TYP TYP1, _, _))
 :: _ :: ( _, ( MlyValue.VLIST VLIST1, _, _)) :: ( _, ( _, VAR1left, _
)) :: rest671)) => let val  result = MlyValue.DEC (fn _ => let val  (
VLIST as VLIST1) = VLIST1 ()
 val  (TYP as TYP1) = TYP1 ()
 in (
 if (#2(TYP) = "int") then SymbolTable.insert((VLIST,1)) else SymbolTable.insert((VLIST,0)); AST.DEC(VLIST, TYP) 
)
end)
 in ( LrTable.NT 3, ( result, VAR1left, EOS1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671))
 => let val  result = MlyValue.TYP (fn _ => let val  (INT as INT1) = 
INT1 ()
 in ((AST.TYPES(INT),"int"))
end)
 in ( LrTable.NT 5, ( result, INT1left, INT1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.BOOL BOOL1, BOOL1left, BOOL1right)) :: 
rest671)) => let val  result = MlyValue.TYP (fn _ => let val  (BOOL
 as BOOL1) = BOOL1 ()
 in ((AST.TYPES(BOOL),"bool"))
end)
 in ( LrTable.NT 5, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.VLIST VLIST1, _, VLIST1right)) :: _ :: ( _, 
( MlyValue.VARI VARI1, VARI1left, _)) :: rest671)) => let val  result
 = MlyValue.VLIST (fn _ => let val  (VARI as VARI1) = VARI1 ()
 val  (VLIST as VLIST1) = VLIST1 ()
 in ((VARI :: VLIST))
end)
 in ( LrTable.NT 4, ( result, VARI1left, VLIST1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.VARI VARI1, VARI1left, VARI1right)) :: 
rest671)) => let val  result = MlyValue.VLIST (fn _ => let val  (VARI
 as VARI1) = VARI1 ()
 in ([VARI])
end)
 in ( LrTable.NT 4, ( result, VARI1left, VARI1right), rest671)
end
|  ( 9, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.CMDS CMDS1, _
, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let val  result = 
MlyValue.COMSEQ (fn _ => let val  (CMDS as CMDS1) = CMDS1 ()
 in (CMDS)
end)
 in ( LrTable.NT 6, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 10, ( ( _, ( _, _, EOS1right)) :: ( _, ( MlyValue.CMD CMD1, _, _)
) :: ( _, ( MlyValue.CMDS CMDS1, CMDS1left, _)) :: rest671)) => let
 val  result = MlyValue.CMDS (fn _ => let val  (CMDS as CMDS1) = CMDS1
 ()
 val  (CMD as CMD1) = CMD1 ()
 in (AST.SEQ(CMDS, CMD))
end)
 in ( LrTable.NT 8, ( result, CMDS1left, EOS1right), rest671)
end
|  ( 11, ( rest671)) => let val  result = MlyValue.CMDS (fn _ => (
 AST.nuexp()))
 in ( LrTable.NT 8, ( result, defaultPos, defaultPos), rest671)
end
|  ( 12, ( ( _, ( MlyValue.EXPRE EXPRE1, _, EXPRE1right)) :: _ :: ( _,
 ( MlyValue.VARI VARI1, VARI1left, _)) :: rest671)) => let val  result
 = MlyValue.CMD (fn _ => let val  (VARI as VARI1) = VARI1 ()
 val  (EXPRE as EXPRE1) = EXPRE1 ()
 in (
if (((SymbolTable.search(VARI) = SOME 0) andalso (#2(EXPRE) = "bool")) orelse ((SymbolTable.search(VARI) = SOME 1) andalso (#2(EXPRE) = "int"))) then AST.SET(VARI,((EXPRE))) else raise typeCheckerror
)
end)
 in ( LrTable.NT 7, ( result, VARI1left, EXPRE1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.VARI VARI1, _, VARI1right)) :: ( _, ( _, 
READ1left, _)) :: rest671)) => let val  result = MlyValue.CMD (fn _ =>
 let val  (VARI as VARI1) = VARI1 ()
 in (AST.READ(VARI))
end)
 in ( LrTable.NT 7, ( result, READ1left, VARI1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.EXPRE EXPRE1, _, EXPRE1right)) :: ( _, ( _,
 WRITE1left, _)) :: rest671)) => let val  result = MlyValue.CMD (fn _
 => let val  (EXPRE as EXPRE1) = EXPRE1 ()
 in (AST.WRITE(EXPRE))
end)
 in ( LrTable.NT 7, ( result, WRITE1left, EXPRE1right), rest671)
end
|  ( 15, ( ( _, ( _, _, ENDIF1right)) :: ( _, ( MlyValue.COMSEQ 
COMSEQ2, _, _)) :: _ :: ( _, ( MlyValue.COMSEQ COMSEQ1, _, _)) :: _ ::
 ( _, ( MlyValue.EXPRE EXPRE1, _, _)) :: ( _, ( _, IF1left, _)) :: 
rest671)) => let val  result = MlyValue.CMD (fn _ => let val  (EXPRE
 as EXPRE1) = EXPRE1 ()
 val  COMSEQ1 = COMSEQ1 ()
 val  COMSEQ2 = COMSEQ2 ()
 in (
if(#2(EXPRE) = "bool" ) then (AST.ITE(((EXPRE)),COMSEQ1,COMSEQ2)) else raise typeCheckerror
)
end)
 in ( LrTable.NT 7, ( result, IF1left, ENDIF1right), rest671)
end
|  ( 16, ( ( _, ( _, _, ENDWH1right)) :: ( _, ( MlyValue.COMSEQ 
COMSEQ1, _, _)) :: _ :: ( _, ( MlyValue.EXPRE EXPRE1, _, _)) :: ( _, (
 _, WH1left, _)) :: rest671)) => let val  result = MlyValue.CMD (fn _
 => let val  (EXPRE as EXPRE1) = EXPRE1 ()
 val  (COMSEQ as COMSEQ1) = COMSEQ1 ()
 in (
if(#2(EXPRE) = "bool" ) then (AST.WH(((EXPRE)),COMSEQ)) else raise typeCheckerror
)
end)
 in ( LrTable.NT 7, ( result, WH1left, ENDWH1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.EXPRE EXPRE2, _, EXPRE2right)) :: _ :: ( _,
 ( MlyValue.EXPRE EXPRE1, EXPRE1left, _)) :: rest671)) => let val  
result = MlyValue.EXPRE (fn _ => let val  EXPRE1 = EXPRE1 ()
 val  EXPRE2 = EXPRE2 ()
 in (
if (#2(EXPRE1)= "bool" andalso #2(EXPRE2)="bool") then (AST.OR( EXPRE1, EXPRE2),"bool") else raise typeCheckerror 
)
end)
 in ( LrTable.NT 9, ( result, EXPRE1left, EXPRE2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.EXPRE EXPRE2, _, EXPRE2right)) :: _ :: ( _,
 ( MlyValue.EXPRE EXPRE1, EXPRE1left, _)) :: rest671)) => let val  
result = MlyValue.EXPRE (fn _ => let val  EXPRE1 = EXPRE1 ()
 val  EXPRE2 = EXPRE2 ()
 in (
 if (#2(EXPRE1)= "int" andalso #2(EXPRE2) = "int") then (AST.PLUS( EXPRE1, EXPRE2),"int") else raise typeCheckerror
)
end)
 in ( LrTable.NT 9, ( result, EXPRE1left, EXPRE2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.EXPRE EXPRE2, _, EXPRE2right)) :: _ :: ( _,
 ( MlyValue.EXPRE EXPRE1, EXPRE1left, _)) :: rest671)) => let val  
result = MlyValue.EXPRE (fn _ => let val  EXPRE1 = EXPRE1 ()
 val  EXPRE2 = EXPRE2 ()
 in (
 if (#2(EXPRE1)= "int" andalso #2(EXPRE2) = "int") then (AST.MINUS( EXPRE1, EXPRE2),"int") else raise typeCheckerror
)
end)
 in ( LrTable.NT 9, ( result, EXPRE1left, EXPRE2right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.EXPRE EXPRE2, _, EXPRE2right)) :: _ :: ( _,
 ( MlyValue.EXPRE EXPRE1, EXPRE1left, _)) :: rest671)) => let val  
result = MlyValue.EXPRE (fn _ => let val  EXPRE1 = EXPRE1 ()
 val  EXPRE2 = EXPRE2 ()
 in (
 if (#2(EXPRE1)= "int" andalso #2(EXPRE2) = "int") then (AST.TIMES( EXPRE1, EXPRE2),"int") else raise typeCheckerror
)
end)
 in ( LrTable.NT 9, ( result, EXPRE1left, EXPRE2right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.EXPRE EXPRE2, _, EXPRE2right)) :: _ :: ( _,
 ( MlyValue.EXPRE EXPRE1, EXPRE1left, _)) :: rest671)) => let val  
result = MlyValue.EXPRE (fn _ => let val  EXPRE1 = EXPRE1 ()
 val  EXPRE2 = EXPRE2 ()
 in (
 if (#2(EXPRE1)= "int" andalso #2(EXPRE2) = "int") then (AST.DIV( EXPRE1, EXPRE2),"int") else raise typeCheckerror
)
end)
 in ( LrTable.NT 9, ( result, EXPRE1left, EXPRE2right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.EXPRE EXPRE2, _, EXPRE2right)) :: _ :: ( _,
 ( MlyValue.EXPRE EXPRE1, EXPRE1left, _)) :: rest671)) => let val  
result = MlyValue.EXPRE (fn _ => let val  EXPRE1 = EXPRE1 ()
 val  EXPRE2 = EXPRE2 ()
 in (
 if (#2(EXPRE1)= "int" andalso #2(EXPRE2) = "int") then (AST.MOD( EXPRE1, EXPRE2),"int") else raise typeCheckerror
)
end)
 in ( LrTable.NT 9, ( result, EXPRE1left, EXPRE2right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671)
) => let val  result = MlyValue.EXPRE (fn _ => let val  (NUM as NUM1)
 = NUM1 ()
 in ((AST.numexp(NUM), "int"))
end)
 in ( LrTable.NT 9, ( result, NUM1left, NUM1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.VARI VARI1, VARI1left, VARI1right)) :: 
rest671)) => let val  result = MlyValue.EXPRE (fn _ => let val  (VARI
 as VARI1) = VARI1 ()
 in (
if SymbolTable.search(VARI) = SOME 0 then (AST.varexp(VARI) ,"bool" ) else (AST.varexp(VARI) ,"int")
)
end)
 in ( LrTable.NT 9, ( result, VARI1left, VARI1right), rest671)
end
|  ( 25, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXPRE EXPRE1
, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result
 = MlyValue.EXPRE (fn _ => let val  (EXPRE as EXPRE1) = EXPRE1 ()
 in ((#1(EXPRE), #2(EXPRE)))
end)
 in ( LrTable.NT 9, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.EXPRE EXPRE1, _, EXPRE1right)) :: ( _, ( _,
 UMINUS1left, _)) :: rest671)) => let val  result = MlyValue.EXPRE (fn
 _ => let val  (EXPRE as EXPRE1) = EXPRE1 ()
 in (
if #2(EXPRE) = "int" then (AST.UMINUS( EXPRE),"int") else raise typeCheckerror
)
end)
 in ( LrTable.NT 9, ( result, UMINUS1left, EXPRE1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.EXPRE EXPRE2, _, EXPRE2right)) :: _ :: ( _,
 ( MlyValue.EXPRE EXPRE1, EXPRE1left, _)) :: rest671)) => let val  
result = MlyValue.EXPRE (fn _ => let val  EXPRE1 = EXPRE1 ()
 val  EXPRE2 = EXPRE2 ()
 in (
if (#2(EXPRE1)= "bool" andalso #2(EXPRE2)="bool") then (AST.AND( EXPRE1, EXPRE2),"bool") else raise typeCheckerror
)
end)
 in ( LrTable.NT 9, ( result, EXPRE1left, EXPRE2right), rest671)
end
|  ( 28, ( ( _, ( _, TT1left, TT1right)) :: rest671)) => let val  
result = MlyValue.EXPRE (fn _ => ((AST.bexp(true), "bool")))
 in ( LrTable.NT 9, ( result, TT1left, TT1right), rest671)
end
|  ( 29, ( ( _, ( _, FF1left, FF1right)) :: rest671)) => let val  
result = MlyValue.EXPRE (fn _ => ((AST.bexp(false), "bool")))
 in ( LrTable.NT 9, ( result, FF1left, FF1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.EXPRE EXPRE1, _, EXPRE1right)) :: ( _, ( _,
 NOT1left, _)) :: rest671)) => let val  result = MlyValue.EXPRE (fn _
 => let val  (EXPRE as EXPRE1) = EXPRE1 ()
 in (
if #2(EXPRE) = "bool" then (AST.NOT( EXPRE),"bool") else raise typeCheckerror
)
end)
 in ( LrTable.NT 9, ( result, NOT1left, EXPRE1right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.EXPRE EXPRE2, _, EXPRE2right)) :: _ :: ( _,
 ( MlyValue.EXPRE EXPRE1, EXPRE1left, _)) :: rest671)) => let val  
result = MlyValue.EXPRE (fn _ => let val  EXPRE1 = EXPRE1 ()
 val  EXPRE2 = EXPRE2 ()
 in (
if ((#2(EXPRE1)= "bool" andalso #2(EXPRE2)="bool") orelse (#2(EXPRE1)= "int" andalso #2(EXPRE2)="int") ) then (AST.GT( EXPRE1, EXPRE2),"bool") else raise typeCheckerror
)
end)
 in ( LrTable.NT 9, ( result, EXPRE1left, EXPRE2right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.EXPRE EXPRE2, _, EXPRE2right)) :: _ :: ( _,
 ( MlyValue.EXPRE EXPRE1, EXPRE1left, _)) :: rest671)) => let val  
result = MlyValue.EXPRE (fn _ => let val  EXPRE1 = EXPRE1 ()
 val  EXPRE2 = EXPRE2 ()
 in (
if ((#2(EXPRE1)= "bool" andalso #2(EXPRE2)="bool") orelse (#2(EXPRE1)= "int" andalso #2(EXPRE2)="int") ) then (AST.GE( EXPRE1, EXPRE2),"bool") else raise typeCheckerror
)
end)
 in ( LrTable.NT 9, ( result, EXPRE1left, EXPRE2right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.EXPRE EXPRE2, _, EXPRE2right)) :: _ :: ( _,
 ( MlyValue.EXPRE EXPRE1, EXPRE1left, _)) :: rest671)) => let val  
result = MlyValue.EXPRE (fn _ => let val  EXPRE1 = EXPRE1 ()
 val  EXPRE2 = EXPRE2 ()
 in (
if ((#2(EXPRE1)= "bool" andalso #2(EXPRE2)="bool") orelse (#2(EXPRE1)= "int" andalso #2(EXPRE2)="int") ) then (AST.LE( EXPRE1, EXPRE2),"bool") else raise typeCheckerror
)
end)
 in ( LrTable.NT 9, ( result, EXPRE1left, EXPRE2right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.EXPRE EXPRE2, _, EXPRE2right)) :: _ :: ( _,
 ( MlyValue.EXPRE EXPRE1, EXPRE1left, _)) :: rest671)) => let val  
result = MlyValue.EXPRE (fn _ => let val  EXPRE1 = EXPRE1 ()
 val  EXPRE2 = EXPRE2 ()
 in (
if ((#2(EXPRE1)= "bool" andalso #2(EXPRE2)="bool") orelse (#2(EXPRE1)= "int" andalso #2(EXPRE2)="int") ) then (AST.LT( EXPRE1, EXPRE2),"bool") else raise typeCheckerror
)
end)
 in ( LrTable.NT 9, ( result, EXPRE1left, EXPRE2right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.EXPRE EXPRE2, _, EXPRE2right)) :: _ :: ( _,
 ( MlyValue.EXPRE EXPRE1, EXPRE1left, _)) :: rest671)) => let val  
result = MlyValue.EXPRE (fn _ => let val  EXPRE1 = EXPRE1 ()
 val  EXPRE2 = EXPRE2 ()
 in (
if ((#2(EXPRE1)= "bool" andalso #2(EXPRE2)="bool") orelse (#2(EXPRE1)= "int" andalso #2(EXPRE2)="int") )then (AST.EQ( EXPRE1, EXPRE2),"bool") else raise typeCheckerror
)
end)
 in ( LrTable.NT 9, ( result, EXPRE1left, EXPRE2right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.EXPRE EXPRE2, _, EXPRE2right)) :: _ :: ( _,
 ( MlyValue.EXPRE EXPRE1, EXPRE1left, _)) :: rest671)) => let val  
result = MlyValue.EXPRE (fn _ => let val  EXPRE1 = EXPRE1 ()
 val  EXPRE2 = EXPRE2 ()
 in (
if ((#2(EXPRE1)= "bool" andalso #2(EXPRE2)="bool") orelse (#2(EXPRE1)= "int" andalso #2(EXPRE2)="int") )then (AST.NEQ( EXPRE1, EXPRE2),"bool") else raise typeCheckerror
)
end)
 in ( LrTable.NT 9, ( result, EXPRE1left, EXPRE2right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.START x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : While_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun TT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun FF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun UMINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun WH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDWH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDIF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun DCOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.BOOL (fn () => i),p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun READ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun WRITE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.NUM (fn () => i),p1,p2))
fun VARI (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VARI (fn () => i),p1,p2))
fun PROG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun BADCHAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
end
end
