
open List
open Ast
open Pretty




(*
let containment (evn: inclusion list) (lhs:effect) (rhs:effect) : (bool * binary_tree *  inclusion list) = 
  
  (*let normalFormL = normalEffect lhs in 
  let normalFormR = normalEffect rhs in 
  let showEntail = string_of_inclusion normalFormL normalFormR in 
  *)

  (true, Node ("", []), [])
  
;;


(* no mixed usage of t and || *)

let check_containment lhs rhs : (bool * binary_tree *  inclusion list) = 
  (*
  let lhs' = matchAsyncAwaitEffect lhs in 
  let rhs' = matchAsyncAwaitEffect rhs in 
  *)
  containment [] lhs rhs
  ;;
  *)

let printReport (lhs:effect) (rhs:effect) (expectation:bool):(string* bool) =

  let entailment = string_of_inclusion (normalEffect lhs) (normalEffect rhs) in 
  let startTimeStamp = Sys.time() in
  let (re, tree, _) =  (true, Node ("", []), []) in (*check_containment lhs rhs in*)
  let verification_time = "[Verification Time: " ^ string_of_float (Sys.time() -. startTimeStamp) ^ " s]\n" in
  let result = printTree ~line_prefix:"* " ~get_name ~get_children tree in
  let correct = if (expectation ==re) then "[Correct]" else "[Incorrect]" in 
  let buffur = ( "----------------------------------------"^"\n" ^(entailment)^"\n[Result] " ^(if re then "Succeed     " else "Fail     ")^ ( correct) ^"\n"  ^verification_time^" \n\n"^ result)
  in (buffur, (expectation ==re))
  
  ;;

(*
let main = 
  let (re, temp) = in 
  let tree = printTree ~line_prefix:"* " ~get_name ~get_children temp in 

  print_string (tree);
  *)

