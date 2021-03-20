type terms = Var of string
           | Number of int
           | Plus of terms * terms
           | Minus of terms * terms 

type event =  string 
type mn = string
type var = string 
type includ = string 

type es = Bot 
        | Emp 
        | Underline
        | Event of event
        | Not of event
        | Cons of es * es
        | ESOr of es * es
        | Kleene of es
        | Omega of es


(*Arithimetic pure formulae*)
type pure = TRUE
          | FALSE
          | Gt of terms * terms
          | Lt of terms * terms
          | GtEq of terms * terms
          | LtEq of terms * terms
          | Eq of terms * terms
          | PureOr of pure * pure
          | PureAnd of pure * pure
          | Neg of pure

type effect = (pure * es) list 

type inclusion = effect * effect * bool;;

type spec = PrePost of effect * effect


(* Elm Syntax *)
(*-| Representations for modules' exports.*)

type exportSet = AllExport
        | SubsetExport of ( exportSet list)
        | FunctionExport of mn
        | TypeExport of mn *( exportSet option)


(*-| Representations for Elm's type syntax.*)
type _type = TypeConstructor of ( _type list)
    | TypeVariable of mn
    | TypeRecordConstructor of _type * ( ( mn* _type ) list)
    | TypeRecord of ( ( mn*  _type ) list)
    | TypeTuple of ( _type list)
    | TypeApplication of _type * _type

type literal = Character of char
    | String of string
    | Integer of int
    | Float of float


type pattern = PWildcard
    | PVariable of mn
    | PLiteral of literal
    (*
    | PConstructor of mn
    | PTuple of ( pattern list)
    | PCons of pattern * pattern
    | PList of ( pattern list)
    | PRecord of (mn list)
    | PA of pattern * mn
    | PApplication of pattern * pattern
    *)



type expression = Literal of literal 
    | Variable of mn
    | List of ( expression list)
    | Tuple of ( expression list)
    | Access of expression * ( mn list)
    | AccessFunction of mn
    | Record of ( ( mn * expression ) list)
    | RecordUpdate of mn * ( ( mn * expression )list)
    | If of expression * expression * expression
    | Let of (  ( pattern* expression )list ) * expression
    | Case of expression * ( ( pattern * expression ) list)
    | Lambda of (pattern list) * expression
    | Application of expression * expression
    | BinOp of expression * expression * expression


(* -| Representations for Elm's statements.*)
type statement = 
    | ImportStatement of mn * (mn option) * ( exportSet option)
    | TypeAliasDeclaration of _type * _type
    | TypeDeclaration of _type * (_type list)
    | PortTypeDeclaration of mn * _type
    | PortDeclaration of mn * ( mn list)  * expression
    | FunctionTypeDeclaration of mn * _type
    | FunctionDeclaration of pattern * expression
    (*
    ModuleDeclaration of mn * exportSet
    | PortModuleDeclaration of mn * exportSet
    | EffectModuleDeclaration of mn * ( ( mn * mn )list)  *exportSet
    *)

type program = statement list 

type prog_states = (pure * es ) list

type ltl = Lable of string 
        | Next of ltl
        | Until of ltl * ltl
        | Global of ltl
        | Future of ltl
        | NotLTL of ltl
        | Imply of ltl * ltl
        | AndLTL of ltl * ltl
        | OrLTL of ltl * ltl
