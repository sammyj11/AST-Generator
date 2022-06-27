SAMYAK JAIN

2020CS50667

COL226 ASSIGNMENT 3

README

--- 

## RULES TO LOAD PROGRAM
Following files are present in the folder
main.lex
main.yacc
glue.sml
compiler.sml
symboltable.sml
input.txt
readme.md
while.cm
while_ast.sml

please note the main file is in while_ast.sml

Firstly run the lex file using ml-lex main.lex
Then run yacc file with ml-yacc main.yacc
Then in the sml environment run the make file with CM.make “while.cm”
Then use the command While.compiler(“input.txt) to parse the program present in the input file

Comments at relevant places to make code clear are there
Apart from that readme provides important insights.

---

### DESCRIPTION

The goal was to make an abstract syntax tree and do type checking for a given language, namely the while language
We utilized the following steps
We made a lexer to analyze the incoming token using Ml lex and the passed these into the yacc file which could generate the parseTree using the semantics and also check the types
We describe the grammar of the language in the yacc file as specified in the EBNF provided by professor

### CONTEXT FREE GRAMMAR
The CFG is defined as G = <N,T,P,S> where the following are:
N
The non terminals 
{START of AST.exp | BLK of AST.exp | DECSEQ of AST.exp | DEC of AST.exp  | VLIST of string list | TYP of string | COMSEQ of AST.exp | CMD of AST.exp| CMDS of AST.exp | EXPRE of AST.exp * string }

T
Terminals
{EOS| OR | NOT  | AND| EOF | GE | GT | EQ | LE | LT | NEQ | ASSIGN | TT  | FF  | DIV| TIMES | UMINUS | MINUS | PLUS | MOD | WH | DO | ENDWH | IF | THEN | ELSE | ENDIF | DCOLON  | COLON | COMMA | LBRACE | RBRACE | LPAREN | RPAREN | BOOL of string | INT of string | READ | WRITE | NUM of int | VARI of string | PROG | VAR | BADCHAR}

P

Production Rules

```
START : PROG VARI DCOLON BLK  
BLK : DECSEQ COMSEQ
DECSEQ : DECSEQ DEC  
                |
DEC : VAR VLIST COLON TYP EOS
TYP : INT 
        |BOOL
VLIST : VARI COMMA VLIST  
           | VARI
COMSEQ :LBRACE CMDS RBRACE 
CMDS: CMDS CMD EOS 
                 |
CMD    : VARI ASSIGN EXPRE 
         | READ VARI  
            | WRITE EXPRE  
       | IF EXPRE THEN COMSEQ ELSE COMSEQ ENDIF 
         | WH EXPRE DO COMSEQ ENDWH
EXPRE : EXPRE OR EXPRE

|EXPRE PLUS EXPRE    
| EXPRE MINUS EXPRE  

|EXPRE TIMES EXPRE   
| EXPRE DIV EXPRE    
| EXPRE MOD EXPRE    

|NUM                 
| VARI               
| LPAREN EXPRE RPAREN
| UMINUS EXPRE       

| EXPRE AND EXPRE    

|TT                  
|      FF            

| NOT EXPRE          

|EXPRE GT EXPRE      
|EXPRE GE EXPRE      
|EXPRE LE EXPRE      
|EXPRE LT EXPRE      
|EXPRE EQ EXPRE      
|EXPRE NEQ EXPRE 

 ```  


Start symbol

START

AST DATATYPES
These have been specified in the the file AST.sml . I have created different datatypes to fit constructor of AST, to define the binary expressions and unary expressions and some values which are just singular expressions

```
datatype binop = PLUS | MINUS |  TIMES | DIV| MOD | AND | OR | ASSIGN | GT | GE | LE | LT | EQ | NEQ
datatype unop = NOT | UMINUS 
```

``` 
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
| DEC of string list * string
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
```

This has both some auxiliary constructors and some predefined constructors
Predefined constructors are given as such, following is the justification of only the ones that i created myself and also utilized in the code. (i created some more for testing purpose and hence not deleted)
``` 
READ of string
|WRITE of (exp*string)
 ```

We need to read and write as specified in EBNF and hence these will be separate constructors of their own.  READ reads  a variable which is string and write prints expression hence its type is exp*string

``` 
| nuexp of unit 
```
Due to kleene closure present in command sequence we need to make an epsilon transition hence null constructor with return type unit

``` 
numexp of int
varexp of string 
```

For identifiers, we need to check whether they are strings or ints (when finally expression becomes a variable or numeral) hence we need a numeral expression of type int and for variables varexp of type string

``` 
UMINUS of (exp*string) 
```

Unary minus was not given as constructor and hence needed for the construction of AST

### SYNTAX DIRECTED TRANSLATIONS


``` 
START : PROG VARI DCOLON BLK               (AST.PROG(VARI, BLK))
BLK : DECSEQ COMSEQ                 (AST.SEQ(DECSEQ, COMSEQ))
DECSEQ : DECSEQ DEC                  (AST.SEQ(DECSEQ,DEC))
         |                          ( AST.nuexp())
DEC : VAR VLIST COLON TYP EOS       ( if (TYP = "int") then SymbolTable.insert((VLIST,1)) else SymbolTable.insert((VLIST,0)); AST.DEC(VLIST, TYP) )
TYP : INT                            (AST.TYPES(INT))
| BOOL                              (AST.TYPES(BOOL))
VLIST : VARI COMMA VLIST            ((VARI :: VLIST))
       | VARI                       ([VARI])    
COMSEQ :LBRACE CMDS RBRACE          (CMDS)
 CMDS: CMDS CMD EOS        (AST.SEQ(CMDS, CMD))
           |                        ( AST.nuexp())
           
CMD    : VARI ASSIGN EXPRE          (AST.SET(VARI,EXPRE))
         | READ VARI                (AST.READ(VARI))
         | WRITE EXPRE             (AST.WRITE(EXPRE))
         | IF EXPRE THEN COMSEQ ELSE COMSEQ ENDIF    (AST.ITE(EXPRE, COMSEQ1,COMSEQ2))
         | WH EXPRE DO COMSEQ ENDWH (AST.WH(EXPRE, COMSEQ))
EXPRE : EXPRE OR EXPRE    (if (#2(EXPRE1)= "bool" andalso #2(EXPRE2)="bool") then (AST.OR( EXPRE1, EXPRE2),"bool") else WhileError)

|EXPRE PLUS EXPRE           ( if (#2(EXPRE1)= "int" andalso #2(EXPRE2) = "int") then (AST.PLUS( EXPRE1, EXPRE2),"int") else WhileError)
| EXPRE MINUS EXPRE          ( if (#2(EXPRE1)= "int" andalso #2(EXPRE2) = "int") then (AST.MINUS( EXPRE1, EXPRE2),"int") else WhileError)

|EXPRE TIMES EXPRE            ( if (#2(EXPRE1)= "int" andalso #2(EXPRE2) = "int") then (AST.TIMES( EXPRE1, EXPRE2),"int") else WhileError)
| EXPRE DIV EXPRE           ( if (#2(EXPRE1)= "int" andalso #2(EXPRE2) = "int") then (AST.DIV( EXPRE1, EXPRE2),"int") else WhileError)
| EXPRE MOD EXPRE           ( if (#2(EXPRE1)= "int" andalso #2(EXPRE2) = "int") then (AST.MOD( EXPRE1, EXPRE2),"int") else WhileError)

|NUM                 ((AST.numexp(NUM), "int"))
| VARI                     (if SymbolTable.search(VARI)= 0 then (AST.varexp(VARI) ,"bool" ) else (AST.varexp(VARI) ,"int"))
| LPAREN EXPRE RPAREN     ((#1(EXPRE), #2(EXPRE)))
| UMINUS EXPRE            (if #2(EXPRE) = "int" then (AST.UMINUS( EXPRE),"int") else WhileError)

| EXPRE AND EXPRE     (if (#2(EXPRE1)= "bool" andalso #2(EXPRE2)="bool") then (AST.AND( EXPRE1, EXPRE2),"bool") else WhileError)

|TT                      ((AST.bexp(true), "bool"))
|      FF                  ((AST.bexp(false), "bool"))

| NOT EXPRE             (if #2(EXPRE) = "bool" then (AST.NOT( EXPRE),"bool") else WhileError)

|EXPRE GT EXPRE        (if (#2(EXPRE1)= "bool" andalso #2(EXPRE2)="bool") then (AST.GT( EXPRE1, EXPRE2),"bool") else WhileError)
|EXPRE GE EXPRE        (if (#2(EXPRE1)= "bool" andalso #2(EXPRE2)="bool") then (AST.GE( EXPRE1, EXPRE2),"bool") else WhileError)
|EXPRE LE EXPRE       (if (#2(EXPRE1)= "bool" andalso #2(EXPRE2)="bool") then (AST.LE( EXPRE1, EXPRE2),"bool") else WhileError)
|EXPRE LT EXPRE        (if (#2(EXPRE1)= "bool" andalso #2(EXPRE2)="bool") then (AST.LT( EXPRE1, EXPRE2),"bool") else WhileError)
|EXPRE EQ EXPRE       (if (#2(EXPRE1)= "bool" andalso #2(EXPRE2)="bool") then (AST.EQ( EXPRE1, EXPRE2),"bool") else WhileError)
|EXPRE NEQ EXPRE        (if (#2(EXPRE1)= "bool" andalso #2(EXPRE2)="bool") then (AST.NEQ( EXPRE1, EXPRE2),"bool") else WhileError)  
```

In the semantics analysis, we give you the production rules along with the corresponding construction of the nodes for a better understanding of the program. We have also passed attributes here for the purpose of typeChecking . This ensures that non parseable programs dont get parsed
LHS shows the production rule and inside the parenthesis is the semantic action fir node creation and typechecking

---

## OTHER DESIGN & IMPLEMENTATION DECISIONS

The EBNF as told by professor just highlights the outline of the language and we needed to define the grammar afresh. Hence for the purpose of avoiding reduce reduce conflicts we merged the intExp and boolExp into a single EXPRE and then to ensure that non parseable programs don't parse we introduced type checking in the semantics using the attributes

we allow usage of terms like ~2 and instead of -2 as specified in the EBNF .

In semantic analysis, we made a separate symbolTable using a hashTable and in that we keep integer as 1 and boolean as 0 and initialize it with 7 for error detection(like variable not found) 

 Lastly for the purpose of giving errors, we give line number followed by character at which the problematic lexer ends

## sample output for input.txt
we have given you a sample input.txt file
following is the output
val it =
  PROG
    ("good",
     SEQ
       (SEQ
          (SEQ (nuexp (),DEC (["A","B"],(TYPES "int","int"))),
           DEC (["C","D"],(TYPES "bool","bool"))),
        SEQ
          (SEQ
             (SEQ (nuexp (),READ "A"),
              WRITE
                (PLUS ((numexp 4,"int"),(UMINUS (numexp 34,"int"),"int")),
                 "int")),
           SET
             ("C",
              (AND
                 ((GT ((varexp "A","int"),(varexp "B","int")),"bool"),
                  (LE ((varexp "C","bool"),(varexp "D","bool")),"bool")),
               "bool"))))) : ?.WhileParser.result


## ACKNOWLEDGEMENT

I would thank the authors of Guide to ML lex and yacc David R. Tarditi Andrew W. Appel for their documentation and sample project. Most files have been built on their works

Other references

Modern Compiler Design in Standard ML - Andrew W. Appel 

COL226 Hyper Notes, Section 4.6 (WHILE Language EBNF) 

Sample code of Professor Subodh V sharma

https://smlfamily.github.io/Basis/array.html

http://cs.wellesley.edu/~cs235/

fall08/lectures/35_YACC_revised.pdf

http://rogerprice.org/ug/ug.pdf

Lecture Slides on moodle







