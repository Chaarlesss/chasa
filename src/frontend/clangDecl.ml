open Decl
open Context

let parse_function_decl ctx cursor =
  let body = Clang.ext_function_decl_get_body cursor in
  let body = ctx.stmt_parser ctx body in
  mk_function_decl body

let parse_var_decl ctx cursor =
  let var_name = Clang.get_cursor_spelling cursor in
  let var_init_cursor = Clang.ext_var_decl_get_init cursor in
  let var_init_opt = Clang.option_cursor_map (ctx.expr_parser ctx) var_init_cursor in
  Var.mk var_name var_init_opt |>
  mk_var_decl

let parse_decl ctx cursor =
  let kind =
    match Clang.get_cursor_kind cursor with
    | FunctionDecl -> parse_function_decl ctx cursor
    | VarDecl -> parse_var_decl ctx cursor
    | kind -> Printf.printf "Unknown declaration: %s\n" (Clang.get_cursor_kind_spelling kind); assert false
  in
  with_loc kind ()
