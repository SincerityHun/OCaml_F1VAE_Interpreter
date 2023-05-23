module F = Format

(* Interpret an expression *)
let rec interp_expr (e: Ast.expr) (g: FStore.t) (s: Store.t) : Value.t = 
  (* Function for errors *)
  let error id = failwith ("Free identifier: " ^ id) in
  let function_error fn = failwith ("Undefined function: " ^ fn) in
  let arg_mismatch_error fn req act = 
    failwith ("The number of arguments of " ^ fn ^ " mismatched: Required: " ^ string_of_int req ^ ", Actual: " ^ string_of_int act) in
  match e with
  | Ast.Add (e1, e2) -> (* Addition operation *)
    (match (interp_expr e1 g s, interp_expr e2 g s) with
    | (Value.NumV n1, Value.NumV n2) -> Value.NumV (n1 + n2))
  | Ast.Sub (e1, e2) -> (* Subtraction operation *)
    (match (interp_expr e1 g s, interp_expr e2 g s) with
    | (Value.NumV n1, Value.NumV n2) -> Value.NumV (n1 - n2))
  | Ast.Id x -> (* Identifier interpretations *)
    if Store.mem x s then Store.find x s
    else error x
  | Ast.Num n -> Value.NumV n (* Number Value *)
  | Ast.LetIn (x, e1, e2) -> (* LetIn expression interpretations *)
    let n1 = interp_expr e1 g s in
    let s' = Store.add x n1 s in
    interp_expr e2 g s'
  | Ast.Call (f, args) -> (* Function call interpretation *) 
    if FStore.mem f g then
      let (params, body) = FStore.find f g in
      if List.length params = List.length args then
        let s' = List.fold_left2 (fun s param arg -> Store.add param (interp_expr arg g s) s) s params args in
        interp_expr body g s'
      else arg_mismatch_error f (List.length params) (List.length args)
    else function_error f

(* Interpret a function definition *)
let interp_fundef (d: Ast.fundef) (g: FStore.t) : FStore.t = 
  match d with
  | Ast.FunDef (f, params, body) -> FStore.add f (params, body) g

(* Interpret a program *)
let interp (p: Ast.prog) : Value.t = 
  match p with
  | Ast.Prog (fdefs, main_expr) -> 
    let g = List.fold_left (fun g fdef -> interp_fundef fdef g) FStore.empty fdefs in
    interp_expr main_expr g Store.empty
