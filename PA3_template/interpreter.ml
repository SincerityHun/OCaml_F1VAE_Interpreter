module F = Format

let rec interp_expr (e: Ast.expr) (g: FStore.t) (s: Store.t) : Value.t = 
 (* Implement this function *)

let interp_fundef (d: Ast.fundef) (g: FStore.t) : FStore.t = 
 (* Implement this function *)

let interp (p: Ast.prog) : Value.t = 
 (* Implement this function *)

