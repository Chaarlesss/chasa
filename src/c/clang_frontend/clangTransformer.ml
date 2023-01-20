open ClangSyntax
open Lang.Ast

let rec translate_function_decl body decl_loc =
  { decl_kind= D_Function (translate_stmt body); decl_loc }

and translate_var_decl name expr decl_loc =
  { decl_kind= D_Var (name, Option.map translate_expr expr); decl_loc }

and translate_decl decl =
  ((fun _ -> failwith "NYI") |>
   match_function_decl translate_function_decl |>
   match_var_decl translate_var_decl) decl

and translate_expr_stmt expr stmt_loc =
  assert false

and translate_if_stmt cond_expr then_stmt else_stmt stmt_loc =
  assert false

and translate_stmt stmt =
  ((fun _ -> failwith "NYI") |>
   match_expr_stmt translate_expr_stmt |>
   match_if_stmt translate_if_stmt) stmt

and translate_expr expr =
  assert false
