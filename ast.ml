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
        | Or of es * es
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

type inclusion = effect * effect;;

type inclusion_sleek = effect * effect * bool;;    (*the bool is the expected result*) 

type prog = Halt 
          | Yield 
          | Seq of prog * prog 
          | Fork of prog * prog
          | Loop of prog
          | Declear of name * prog
          | Emit of name
          | Present of name * prog * prog
          | Trap of lable * prog
          | Break of lable
          | Run of name
          | Abort of int * prog
(*Esterel SYNC*)
          | Async of name * prog * int (*the delay*)  (*set a timeout*)
          | Await of name 
          | Assert of effect
(*JS ASYNC*)

type prog_states = (pure * es * instance * name option) list

type fst = (instance * string * pure)

type parfst = SL of instance | W of name

type ltl = Lable of string 
        | Next of ltl
        | Until of ltl * ltl
        | Global of ltl
        | Future of ltl
        | NotLTL of ltl
        | Imply of ltl * ltl
        | AndLTL of ltl * ltl
        | OrLTL of ltl * ltl


type spec_prog = name * string list * string list * effect * effect * prog
              (* name , input,     output, precon, postcon, body*)