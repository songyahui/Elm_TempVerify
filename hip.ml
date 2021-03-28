
open Pretty
open Ast

exception Foo of string


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
  | Glsl -> "Glsl expression"
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
  | ModuleDeclaration (mn, exportSet) -> "module " ^ mn ^ " exposing " ^  string_of_exportSet exportSet
  | PortModuleDeclaration (mn, exportSet) -> "module " ^ mn ^ " exposing " ^  string_of_exportSet exportSet
  | PortTypeDeclaration (mn, t) -> "port " ^ mn ^ ":"^string_of_type t 
  | _ -> "later"

  ;;


let rec string_of_program (states : statement list) : string =
  match states with
    [] -> ""
  | x::xs -> string_of_statement x ^ "\n\n" ^ string_of_program xs 
  ;;

let rec string_of_transition_rules (tr: transition_rules): string = 
  match tr with 
  | [] -> ""
  | (str, s_li) :: xs -> str ^ " -> " ^ (List.fold_left (fun acc a -> acc ^ " " ^ a) "" s_li) ^"\n" ^ string_of_transition_rules xs 

  ;;

let rec get_fun_type_from_prog (states : statement list)  (nm:string): _type = 
  match states with 
  | [] -> raise (Foo " this program has no main \n")
  | (FunctionTypeDeclaration ( str, _type)):: xs -> if (String.compare str nm == 0) then _type else get_fun_type_from_prog xs  nm
  | _ :: xs -> get_fun_type_from_prog xs nm
  ;;

let rec get_fun_from_prog (states : statement list)  (nm:string): expression = 
  match states with 
  | [] -> raise (Foo " this program has no main \n")
  | (FunctionDeclaration ((PVariable str), expr)):: xs -> if (String.compare str nm == 0) then expr else get_fun_from_prog xs nm
  | _ :: xs -> get_fun_from_prog xs nm
  ;;



let rec getfeildFromRecord map_li str: string = 
  match map_li with 
  | [] -> raise (Foo ("looking for a filed " ^ str ^ " which is not exist"))
  | (x, y) :: xs -> if String.compare x str == 0 then string_of_expression y else getfeildFromRecord xs str
  ;;

let getMsg_type (states : statement list)  str :string =
  let view_type = get_fun_type_from_prog states str in 
  match view_type with 
  | (TypeApplication (_, TypeConstructor (_, _type_list))) -> string_of_type (List.hd (_type_list))
  | _ -> ""
;;


let string_of_elm_frame frame : string =
  match frame with 
  | Frameless -> "no frame \n"
  | FourEle (s1, s2, s3, s4, s5)  ->
  "init = " ^s1 ^
  "\nupdate = " ^s2 ^
  "\nsubscriptions = " ^s3 ^
  "\nview = " ^ s4 ^
  "\nMsg type = " ^ s5 ^"\n"
  ;;

let get_elm_frame (states : statement list) : elm_framework = 
  let main_fun = get_fun_from_prog states "main" in 
  match main_fun with 
  | Application (Access ((Variable bro), ele) , Record expr) -> 
    if (String.compare bro "Browser" == 0  && String.compare (List.hd ele) "element" == 0) then  
      FourEle (getfeildFromRecord expr "init"
              ,getfeildFromRecord expr "update"
              ,getfeildFromRecord expr "subscriptions"
              ,getfeildFromRecord expr "view" 
              ,getMsg_type states (getfeildFromRecord expr "view"))
    else Frameless

  | _ -> Frameless
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
      print_string (string_of_elm_frame (get_elm_frame progs) ^"\n");
      
      flush stdout;                (* 现在写入默认设备 *)
      close_in ic                  (* 关闭输入通道 *)

    with e ->                      (* 一些不可预见的异常发生 *)
      close_in_noerr ic;           (* 紧急关闭 *)
      raise e                      (* 以出错的形式退出: 文件已关闭,但通道没有写入东西 *)

   ;;
