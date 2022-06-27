structure AST = 
struct

datatype binop = PLUS | MINUS |  TIMES | DIV| MOD | AND | OR | ASSIGN | GT | GE | LE | LT | EQ | NEQ
datatype unop = NOT | UMINUS 

datatype multiexpr = ValDecls of string*exp 
and exp = numexp of int
| binexp of binop * exp * exp
| unexp of unop * exp 
| boolexp of binop * exp * exp
| stringexp of string
| varexp of string
| ITE of (exp*string)* exp * exp
| whexp of exp * exp
| consexp of string
| blkexp of string*string*exp
| seqexp of exp*exp
| strexp of exp*string
| singexp of exp
| nuexp of unit
| bexp of bool
| PROG of string*exp
| DEC of string list * (exp*string)
|SEQ of exp*exp
| VLIST of string list
| PLUS of (exp*string)*(exp*string)
|MINUS of (exp*string)*(exp*string)
|TIMES of (exp*string)*(exp*string)
|MOD of (exp*string)*(exp*string)
|DIV of (exp*string)*(exp*string)
|UMINUS of (exp*string)
| GT of (exp*string)*(exp*string)
| GE of (exp*string)*(exp*string)
| LT of (exp*string)*(exp*string)
| LE of (exp*string)*(exp*string)
| AND of (exp*string)*(exp*string)
| OR of (exp*string)*(exp*string)
| EQ of (exp*string)*(exp*string)
| NEQ of (exp*string)*(exp*string)
|NOT of (exp*string)
| SET of string*(exp*string)
|TYPES of string
|READ of string
|WRITE of (exp*string)
|WH of (exp*string)*exp




datatype oneexp = Intval of int
| boolval of bool 
| stringval of string

							    
end


















