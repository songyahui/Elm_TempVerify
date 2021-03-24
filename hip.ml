
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
  | TypeApplication (t1, t2) -> string_of_type t1 ^" -> " ^ string_of_type t2
  | TypeTuple t_li -> "(" ^List.fold_left (fun acc a -> acc ^", " ^ string_of_type a) "" t_li ^")"
  | _ -> "later"
  ;;

let rec string_of_pattern (pat:pattern) : string = 
  match pat with
  | PWildcard -> "_"
  | PVariable mn -> mn
  | PLiteral l ->   string_of_literal l
  | PApplication (p1, p2) -> string_of_pattern p1 ^ " " ^ string_of_pattern p2 
  | PTuple p_li -> "(" ^ List.fold_left (fun acc a -> acc ^","^ string_of_pattern a ) "" p_li ^ ")"
  | PRecord obj -> "{" ^ List.fold_left (fun acc a -> acc ^","^ a ) "" obj ^ "}"
  ;;

let rec string_of_expression (expr:expression) : string = 
  match expr with
  | Literal l -> string_of_literal l 
  | Variable str -> str
  | Record tuple_li -> "{" ^ List.fold_left (fun acc (a, b) -> acc ^"," ^ a^"="^ string_of_expression b ) "" tuple_li ^ "}"
  | Access (ex, mn_li) ->  " "^ string_of_expression ex ^ List.fold_left (fun acc a -> acc ^"."^a) "." mn_li 
  | Application (ex1, ex2) -> " "^ string_of_expression ex1 ^" " ^ string_of_expression ex2
  | Tuple (ex_li) -> "(" ^List.fold_left (fun acc a -> acc ^", " ^ string_of_expression a) "" ex_li ^")"
  | Case (ex, p_ex_li) -> 
    "case " ^ string_of_expression ex ^ " of " ^ 
    "(" ^List.fold_left (fun acc (a, b) -> acc ^"\n " ^ string_of_pattern a ^" -> " ^ string_of_expression b) "" p_ex_li ^")"
  | Lambda (p_li, ex) -> "(" ^List.fold_left (fun acc a -> acc ^" " ^ string_of_pattern a) "\\" p_li ^" -> e"^ string_of_expression ex ^")"
  | BinOp (e1, e2, e3) -> string_of_expression e2 ^ " "^ string_of_expression e1 ^ " " ^ string_of_expression e3
  | List ex_li -> "[" ^List.fold_left (fun acc a -> acc ^", " ^ string_of_expression a) "" ex_li ^"]"
  | RecordUpdate (str, tuple_li) -> "{" ^ str ^ " | " ^ List.fold_left (fun acc (a, b) -> acc ^"," ^ a^"="^ string_of_expression b ) "" tuple_li ^ "}"
  | Let ( p_ex_li, ex) -> "let" ^ List.fold_left (fun acc (a, b) -> acc ^"\n" ^string_of_pattern a^"="^ string_of_expression b ) "" p_ex_li ^ "in\n"^string_of_expression ex
  | If (ex1, ex2, ex3) -> "if " ^ string_of_expression ex1 ^ " then " ^ string_of_expression ex2 ^ " else " ^ string_of_expression ex3 
  | _ -> "later"
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
  | TypeDeclaration (t1, t2_li) -> string_of_type t1 ^" = " ^ List.fold_left (fun acc a -> acc ^" " ^ string_of_type a) "" t2_li
  | FunctionTypeDeclaration (mn, t) -> mn ^ " : " ^ string_of_type t
  | _ -> "later"

  ;;


let rec string_of_program (states : statement list) : string =
  match states with
    [] -> ""
  | x::xs -> string_of_statement x ^ "\n\n" ^ string_of_program xs 
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
