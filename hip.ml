
open Pretty
open Ast


let string_of_literal (l:literal) : string = 
  match l with 
  | Character c -> String.make 1 c 
  | String str -> str
  | Integer n -> string_of_int n 
  | Float f -> string_of_float f
  ;;

let rec string_of_type (t:_type): string = 
  match t with 
  | TypeConstructor (mn_li, t_li) -> 
    List.fold_left (fun acc a -> acc ^"." ^ a) ""  mn_li ^ 
    List.fold_left (fun acc a -> acc ^" " ^ string_of_type a) "" t_li
  | TypeVariable v -> v
  | TypeRecord tuple_li -> "{" ^ List.fold_left (fun acc (a, b) -> acc ^"," ^ a^"="^ string_of_type b ) "" tuple_li ^ "}"
  | _ -> "later"
  ;;

let rec string_of_expression (expr:expression) : string = 
  match expr with
  | Literal l -> string_of_literal l 
  | Variable str -> str
  | Record tuple_li -> "{" ^ List.fold_left (fun acc (a, b) -> acc ^"," ^ a^"="^ string_of_expression b ) "" tuple_li ^ "}"
  | Access (ex, mn_li) ->  string_of_expression ex ^ List.fold_left (fun acc a -> acc ^"."^a) "." mn_li 
  | Application (ex1, ex2) -> string_of_expression ex1 ^" \n " ^ string_of_expression ex2
  | _ -> "later"
  ;;

let string_of_pattern (pat:pattern) : string = 
  match pat with
  | PWildcard -> "_"
  | PVariable mn -> mn
  | PLiteral l ->   string_of_literal l
  ;;

let rec string_of_exportSet (ex: exportSet): string = 
  match ex with 
  | AllExport -> ".."
  | SubsetExport (ex_li) -> List.fold_left (fun acc a -> acc ^","^ string_of_exportSet a ) "" ex_li
  | FunctionExport str -> str
  | TypeExport (mn, exportSet_option) -> mn ^ (
    match exportSet_option with 
    | None -> ""
    | Some s -> "(" ^ string_of_exportSet s ^ ")"
  )
  ;;


let string_of_statement (state:statement) : string = 
  match state with
  | FunctionDeclaration (pat, expr) -> string_of_pattern pat ^ " = " ^ string_of_expression expr 
  | ImportStatement (mn, mn_option, exportSet_option) ->  "import " ^ mn ^  (
      match mn_option with 
      | None -> ""
      | Some str -> "as "^ str ^" ")  ^  (
      match exportSet_option with
      | None -> ""
      | Some con -> "(" ^ string_of_exportSet con ^ ")"
      )
  | TypeAliasDeclaration (t1, t2) -> string_of_type t1 ^" = " ^ string_of_type t2
  | _ -> "later"

  ;;


let rec string_of_program (states : statement list) : string =
  match states with
    [] -> ""
  | x::xs -> string_of_statement x ^ "\n" ^ string_of_program xs 
  ;;


let () =
  let inputfile = (Sys.getcwd () ^ "/" ^ Sys.argv.(1)) in
(*    let outputfile = (Sys.getcwd ()^ "/" ^ Sys.argv.(2)) in
print_string (inputfile ^ "\n" ^ outputfile^"\n");*)
  let ic = open_in inputfile in
  try
      let lines =  (input_lines ic ) in
      let line = List.fold_right (fun x acc -> acc ^ "\n" ^ x) (List.rev lines) "" in
      let progs = Parser.program Lexer.token (Lexing.from_string line) in
      

      print_string (string_of_program progs^"\n");
      (*print_string ( (forward_verification progs) ^"\n");*)
      
      flush stdout;                (* 现在写入默认设备 *)
      close_in ic                  (* 关闭输入通道 *)

    with e ->                      (* 一些不可预见的异常发生 *)
      close_in_noerr ic;           (* 紧急关闭 *)
      raise e                      (* 以出错的形式退出: 文件已关闭,但通道没有写入东西 *)

   ;;
