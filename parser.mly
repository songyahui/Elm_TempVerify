%{ open Ast %}
%{ open List %}


%token <string> VAR
%token <int> INTE
%token <bool> TRUEE FALSEE 
%token NOTHING PAUSE PAR  LOOP SIGNAL LPAR RPAR EMIT PRESENT TRAP EXIT SIMI
%token AWAIT ASYNC ASSERT COLON COUNT QUESTION SHARP MINUS PLUS POWER TRUEToken FALSEToken NEGATION
%token EOF GT LT EQ CONJ GTEQ LTEQ ENTIL EMPTY DISJ COMMA CONCAT UNDERLINE KLEENE OMEGA END IN RUN
(* LBrackets  RBrackets POWER
%token THEN ELSE ABORT WHEN LBRACK RBRACK      

*)
%left CONCAT DISJ 
(* %right SIMI PAR *)
%token FUTURE GLOBAL IMPLY LTLNOT NEXT UNTIL LILAND LILOR 
%token LSPEC RSPEC ENSURE REQUIRE MODULE OUT INOUT
%token LBrackets RBrackets HIPHOP 

%start ee ltl_p
%type <(Ast.inclusion) list > ee
%type <(Ast.ltl) list > ltl_p


%%


ee: 
| EOF {[]}
| a = entailment SIMI r = ee { append [a] r }



entailment:
| lhs = effect   ENTIL   rhs = effect COLON re = expectation { (lhs, rhs, re)} 

expectation:
| TRUEE {true}
| FALSEE {false}



effect:
| LPAR r = effect RPAR { r }
| a = pure  CONJ  b= es  {[(a, b)]}
| a = effect  DISJ  b=effect  { List.append a b}

pure:
| TRUEToken {TRUE}
| FALSEToken {FALSE}
| NEGATION LPAR a = pure RPAR {Neg a}
| LPAR r = pure RPAR { r }
| a = term GT b = term {Gt (a, b)}
| a = term LT b = term {Lt (a, b)}
| a = term GTEQ b = term {GtEq (a, b)}
| a = term LTEQ b = term {LtEq (a, b)}
| a = term EQ b = term {Eq (a, b)}
| a = pure CONJ b = pure {PureAnd (a, b)}
| a = pure DISJ b = pure {PureOr (a, b)}


term:
| str = VAR { Var str }
| n = INTE {Number n}
| LPAR r = term RPAR { r }
| a = term b = INTE {Minus (a, Number (0 -  b))}

| LPAR a = term MINUS b = term RPAR {Minus (a, b )}

| LPAR a = term PLUS b = term RPAR {Plus (a, b)}


es:
| EMPTY { Emp }
| var = VAR {(Event var)}
| LTLNOT var = VAR {(Not var)}
  
| LPAR r = es RPAR { r }
| a = es DISJ b = es { ESOr(a, b) }
| LPAR r = es POWER OMEGA RPAR{ Omega r }
| UNDERLINE {Underline}
| a = es CONCAT b = es { Cons(a, b) } 
| LPAR a = es POWER KLEENE RPAR{Kleene a}



ltl_p: 
| EOF {[]}
| a = ltl SIMI r = ltl_p { append [a] r }

ltl : 
| s = VAR {Lable s} 
| LPAR r = ltl RPAR { r }
| NEXT p = ltl  {Next p}
| LPAR p1= ltl UNTIL p2= ltl RPAR {Until (p1, p2)}
| GLOBAL p = ltl {Global p}
| FUTURE p = ltl {Future p}
| LTLNOT p = ltl {NotLTL p}
| LPAR p1= ltl IMPLY p2= ltl RPAR {Imply (p1, p2)}
| LPAR p1= ltl LILAND p2= ltl RPAR {AndLTL (p1, p2)}  
| LPAR p1= ltl LILOR p2= ltl RPAR {OrLTL (p1, p2)}  

