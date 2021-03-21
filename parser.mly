%{ open Ast %}
%{ open List %}

%token NEWLINE
%token <string> UVAR LVAR COP
%token <int> INTE
%token <bool> TRUEE FALSEE 
%token  LPAR RPAR SIMI LBrackets  RBrackets  COMMA LBRACK RBRACK      
%token  MINUS PLUS POWER TRUEToken COLON FALSEToken NEGATION
%token EOF GT LT EQ CONJ GTEQ LTEQ ENTIL EMPTY DISJ  CONCAT UNDERLINE KLEENE OMEGA 
%token IMPORT EXPOSING AS ALLEX MODULE
(*  POWER
%token THEN ELSE ABORT WHEN 
AWAIT ASYNC ASSERT  COUNT QUESTION SHARP
END IN RUN
*)
%left CONCAT  DISJ 

(* %right SIMI PAR NOTHING PAUSE PAR  LOOP SIGNAL EMIT PRESENT TRAP EXIT 
%token LSPEC RSPEC ENSURE REQUIRE  OUT INOUT
%token LBrackets RBrackets HIPHOP 
 *)
%token FUTURE GLOBAL IMPLY LTLNOT NEXT UNTIL LILAND LILOR 
(* 
%token TYPE ALIAS 
*)


%start ee ltl_p program
%type <(Ast.inclusion) list > ee
%type <(Ast.ltl) list > ltl_p
%type <(Ast.statement) list> program


%%

newlines:
| {()}
| NEWLINE newlines {()}

newline_none:
| {()}
| NEWLINE {()}

program:
| newlines EOF {[]}
| newlines a = statement r = program { append [a] r }


maybeNM:
| {None}
| AS str = UVAR {Some str}

exportSet:
| ALLEX {AllExport}
| se =  subsetExport {se}

typeExport: 
| mn= UVAR m = constructorExports { TypeExport (mn, m)}


constructorExports :
| {None}
|  LPAR  r = constructorExportsIn RPAR  {Some r}

constructorExportsIn:
| ALLEX {AllExport}

functionExport : 
| mn= LVAR { FunctionExport mn}
| LPAR op = COP RPAR { FunctionExport op}

subsetExport:
| obj = separated_list (COMMA, subsetExportIn) {SubsetExport obj}


subsetExportIn:
| r = functionExport {r}
| r = typeExport {r}



maybeExport:
| {None}
| EXPOSING LPAR expSet = exportSet RPAR{Some expSet}


statement:
| p = pattern EQ newlines expr = expression {FunctionDeclaration (p, expr)}
| IMPORT str = moduleName alise = maybeNM export = maybeExport {ImportStatement (str, alise, export)}

(*
| MODULE mn = moduleName EXPOSING LPAR expSet = exportSet RPAR {ModuleDeclaration (mn, expSet)}
| TYPE ALIAS t1= _type EQ t2 = _type {TypeAliasDeclaration (t1, t2)}

_type:
| mn = LVAR {TypeVariable mn}
| mn_li = separated_list (CONCAT, UVAR) obj = typeParameterAUX {TypeConstructor (mn_li, obj ) }


typeParameterAUX:
| {[]}
| x= typeParameter xs = typeParameterAUX {x :: xs }

typeParameter:
| mn = LVAR {TypeVariable mn}

*)

moduleName:
| obj = separated_list (CONCAT, UVAR) {
  List.fold_left (fun acc a -> acc ^"."^a) "" obj
}


pattern:
| UNDERLINE {PWildcard}
| str = LVAR { PVariable str }
| l = literal {PLiteral l}


expression: 
| t = expr_term newline_none m = maybeExpr {
  match m with
  | None -> t
  | Some t2 -> Application (t, t2)
}

maybeExpr:
| {None}
| t = expression {Some t}

expr_term:
| l = literal {Literal l }
| str = loName CONCAT f = LVAR {Access  (Variable str, [f])}
| str = loName {Variable str}
| LBRACK newlines obj = separated_list (COMMA, record_aux)  RBRACK  {Record obj}


record_aux: 
| newlines str = LVAR EQ ex =expression newlines {(str, ex)}


(*
expression: 


| ex = ex_aux {ex}
| ex1 = ex_aux newline_none ex2 = ex_aux {Application (ex1, ex2)}



ex_aux:







*)

literal: 
| n = INTE {Integer n}

loName: 
| UNDERLINE {"_"}
| str = LVAR {str}
| str = UVAR {str}

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
| a = term GT b = term {Gt (a, b)}
| a = term LT b = term {Lt (a, b)}
| a = term GTEQ b = term {GtEq (a, b)}
| a = term LTEQ b = term {LtEq (a, b)}
| a = term EQ b = term {Eq (a, b)}

| NEGATION  a = pure {Neg a}
| LBrackets r = pure RBrackets { r }
| LBrackets a = pure CONJ b = pure RBrackets {PureAnd (a, b)}
| LBrackets a = pure DISJ b = pure RBrackets {PureOr (a, b)}



(*


*)

term:
| str = UVAR { Var str }
| n = INTE {Number n}
| LPAR r = term RPAR { r }
| a = term b = INTE {Minus (a, Number (0 -  b))}

| LPAR a = term MINUS b = term RPAR {Minus (a, b )}

| LPAR a = term PLUS b = term RPAR {Plus (a, b)}


es:
| EMPTY { Emp }
| var = UVAR {(Event var)}
| LTLNOT var = UVAR {(Not var)}
  
| LPAR r = es RPAR { r }
| LPAR a = es DISJ b = es RPAR { ESOr(a, b) }
| LPAR r = es RPAR POWER OMEGA { Omega r }
| UNDERLINE {Underline}
| a = es CONCAT b = es { Cons(a, b) } 
| LPAR a = es RPAR POWER KLEENE  {Kleene a}



ltl_p: 
| EOF {[]}
| a = ltl SIMI r = ltl_p { append [a] r }

ltl : 
| s = UVAR {Lable s} 
| LPAR r = ltl RPAR { r }
| NEXT p = ltl  {Next p}
| LPAR p1= ltl UNTIL p2= ltl RPAR {Until (p1, p2)}
| GLOBAL p = ltl {Global p}
| FUTURE p = ltl {Future p}
| LTLNOT p = ltl {NotLTL p}
| LPAR p1= ltl IMPLY p2= ltl RPAR {Imply (p1, p2)}
| LPAR p1= ltl LILAND p2= ltl RPAR {AndLTL (p1, p2)}  
| LPAR p1= ltl LILOR p2= ltl RPAR {OrLTL (p1, p2)}  

