%{ open Ast %}
%{ open List %}

%token NEWLINE
%token <string> UVAR LVAR COP STRING
%token <int> INTE
%token <float> FLOAT
%token <bool> TRUEE FALSEE 
%token  LPAR RPAR SIMI LBrackets  RBrackets  COMMA LBRACK RBRACK      
%token  MINUS PLUS POWER TRUEToken COLON FALSEToken NEGATION
%token EOF GT LT EQ CONJ GTEQ LTEQ ENTIL EMPTY DISJ  CONCAT UNDERLINE KLEENE OMEGA 
%token IMPORT EXPOSING AS ALLEX MODULE CHOICE EQEQ
%token CASE OF LAMDA THEN_  DIV DIVEQ LET IN  PREPAND PLUSPLUS LTCHOICE
%token IF ELSE THEN  PORT
(*  POWER
%token THEN ELSE ABORT WHEN 
AWAIT ASYNC ASSERT  COUNT QUESTION SHARP
END IN RUN
*)
%left  DISJ 
%left  CONCAT

(* %right SIMI PAR NOTHING PAUSE PAR  LOOP SIGNAL EMIT PRESENT TRAP EXIT 
%token LSPEC RSPEC ENSURE REQUIRE  OUT INOUT
%token  HIPHOP 
 *)
%token FUTURE GLOBAL IMPLY LTLNOT NEXT UNTIL LILAND LILOR 

%token TYPE ALIAS 



%start ee ltl_p program
%type <(Ast.inclusion) list > ee
%type <(Ast.ltl) list > ltl_p
%type <(Ast.statement) list> program


%%

newline_none:
| {()}
| NEWLINE {()}

newlines:
| {()}
| NEWLINE newlines {()}

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
| MODULE mn = moduleName EXPOSING LPAR expSet = exportSet RPAR {ModuleDeclaration (mn, expSet)}
| TYPE ALIAS t1= _type newlines EQ newlines t2 = _type {TypeAliasDeclaration (t1, t2)}
| mn = LVAR COLON t = _type {FunctionTypeDeclaration (mn, t)}
| TYPE t = _type newlines EQ newlines t1 = typeConstructor newlines obj = typeDeclaration  {TypeDeclaration(t, t1::obj)}
| PORT p = port_aux {p}

port_aux:
| mn = LVAR COLON t = typeAnnotation { PortTypeDeclaration (mn, t)}
| MODULE mn = moduleName EXPOSING  LPAR expSet = exportSet RPAR {PortModuleDeclaration (mn, expSet)}


typeDeclaration:
|  {[]}
|  CHOICE t = typeConstructor newlines obj = typeDeclaration {t:: obj}

_type:
| mn = LVAR {TypeVariable mn}
| t = typeConstructor {t}
| t = typeAnnotation {t}
| obj = typeTuple  {TypeTuple obj}
| t = t_record {t}

typeConstructor:
| mn = UVAR mn_li = typeConstructor_help obj = typeParameterAUX {TypeConstructor (mn::mn_li, obj ) }

typeConstructor_help:
| {[]}
|  CONCAT mn = UVAR obj =  typeConstructor_help { mn:: obj}


(*
| mn_li = UVAR obj = typeParameterAUX {TypeConstructor ([mn_li], obj ) }

*)



typeTuple:
| LPAR RPAR {[]}
| LPAR obj = separated_list (COMMA, _type) RPAR {obj}

typeAnnotation:
| t = _type IMPLY ta = _type {TypeApplication (t, ta)}


typeParameterAUX:
| {[]}
| x= typeParameter xs = typeParameterAUX {x :: xs }

t_record_aux: 
| newlines str = LVAR COLON ex = _type newlines {(str, ex)}

t_record:
| LBRACK obj = separated_list (COMMA, t_record_aux)  RBRACK  {TypeRecord obj}


typeParameter:
| mn = loName {TypeVariable mn}
| t = t_record {t}
| obj = typeTuple  {TypeTuple obj}
| t = typeConstructor {t}


moduleName:
| obj = separated_list (CONCAT, UVAR) {
  List.fold_left (fun acc a -> acc ^"."^a) "" obj
}


pattern:
| UNDERLINE {PWildcard}
| str = loName { PVariable str }
| l = literal {PLiteral l}
| p = patternTuple {PTuple p}
| p1= pattern p2 = pattern {PApplication (p1, p2)}
| LBRACK obj = separated_list(COMMA, LVAR) RBRACK {PRecord obj}

patternTuple:
| LPAR RPAR {[]}
| LPAR obj = separated_list (COMMA, pattern) RPAR {obj}


up_pattern:
| UNDERLINE {PWildcard}
| str = UVAR { PVariable str }
| l = literal {PLiteral l}
| p1= up_pattern p2 = pattern {PApplication (p1, p2)}


(*
   [              binary ops
                , letExpression ops
                , caseExpression ops
                , ifExpression ops
                , lambda ops
                ]
*)
expression: 
| b = binary {b}
| CASE ex1 = expression OF newlines 
  p = up_pattern IMPLY newlines ex = expression newline_none newlines 
  obj = bindings {Case (ex1, (p, ex) ::obj) }
| t = lambda {t}
| LET newlines obj = let_bindings newlines IN  newlines ex = expression {Let (obj, ex)}
| IF ex1 = expression newlines THEN  newlines ex2 = expression  newlines ELSE newlines ex3 = expression {If (ex1, ex2, ex3)}


let_bindings:
| {[]}
|  p = pattern EQ  newlines ex = expression newlines  b = let_bindings {(p, ex):: b}




maybeExpr:
| {None}
| t = expression {Some t}

binary:
| t = expr_term newline_none m = maybeExpr {
  match m with
  | None -> t
  | Some t2 -> Application (t, t2)
}
|  b = binOp  {b}



bindings:
| {[]}
|  p = up_pattern IMPLY newlines ex = expression newline_none newlines  b = bindings {(p, ex):: b}


binOp:
| e1 = expression THEN_ e2 = expression   {BinOp (Variable "|>", e1, e2)}
| e1 = expression PLUS e2 = expression   {BinOp (Variable "+", e1, e2)}
| e1 = expression MINUS e2 = expression   {BinOp (Variable "-", e1, e2)}
| e1 = expression DIV e2 = expression   {BinOp (Variable "/", e1, e2)}
| e1 = expression EQ e2 = expression   {BinOp (Variable "=", e1, e2)}
| e1 = expression KLEENE e2 = expression   {BinOp (Variable "*", e1, e2)}
| e1 = expression PREPAND e2 = expression   {BinOp (Variable "::", e1, e2)}
| e1 = expression LT e2 = expression   {BinOp (Variable "<", e1, e2)}
| e1 = expression GT e2 = expression   {BinOp (Variable ">", e1, e2)}
| e1 = expression PLUSPLUS e2 = expression   {BinOp (Variable "++", e1, e2)}
| e1 = expression LTCHOICE newlines e2 = expression   {BinOp (Variable ">|", e1, e2)}
| e1 = expression EQEQ e2 = expression   {BinOp (Variable "==", e1, e2)}
| e1 = expression DIVEQ e2 = expression   {BinOp (Variable "/=", e1, e2)}



lambda:
| LAMDA obj = pattern IMPLY newlines ex = expression {
  let rec applicationToList o = 
    match o with 
    | PApplication (p1, p2) -> List.append (applicationToList p1) (applicationToList p2)
    | _ -> [o]
  in Lambda (applicationToList obj, ex)}


expr_term:
| l = literal {Literal l }
| str = loName CONCAT f = LVAR  obj =  access_aux {Access  (Variable str, f::obj)}
| str = loName {Variable str}
| LPAR obj = separated_list (COMMA, expression) RPAR {Tuple obj}
| LBrackets obj = separated_list (COMMA, list_aux) RBrackets {List obj}
| LBRACK str = LVAR newlines r = record_or_record_update RBRACK {
  match r with 
  | (None, obj) -> RecordUpdate( str, obj)
  | (Some ex, obj) -> Record ((str, ex)::obj)
  }

list_aux:
| ex = expression newlines {ex}

access_aux:
| {[]}
| CONCAT f = LVAR obj = access_aux  {f::obj}

record_or_record_update:
| CHOICE newlines obj = separated_list (COMMA, record_aux) {(None, obj)} (*RecordUpdate *)
| EQ newlines ex =expression newlines  mo = record_help  {
  match mo with 
  | None -> (Some ex, [])
  | Some obj ->  (Some ex, obj)}


record_help:
| {None}
| COMMA obj = separated_list (COMMA, record_aux) {Some obj}

record_aux: 
| newlines str = LVAR EQ ex =expression newlines {(str, ex)}


literal: 
| n = INTE {Integer n}
| str = STRING {String str}
| f = FLOAT {Float f }

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

