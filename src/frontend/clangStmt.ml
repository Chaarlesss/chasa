open Stmt
open Context

let parse_compound_stmt ctx cursor =
  let children = Clang.list_of_children_map (ctx.stmt_parser ctx) cursor in
  mk_compound_stmt children

let parse_decl_stmt ctx cursor =
  let children = Clang.list_of_children_map (ctx.decl_parser ctx) cursor in
  mk_decl_stmt children

let parse_while_stmt ctx cursor =

  let parse_expr ctx cursor =
    let expr = ctx.expr_parser ctx cursor in
    mk_expr expr

let parse_stmt ctx cursor =
  let kind =
    match Clang.ext_stmt_get_kind cursor with
    | CompoundStmt -> parse_compound_stmt ctx cursor
    | DeclStmt -> parse_decl_stmt ctx cursor
    | WhileStmt -> parse_while_stmt ctx cursor
    | _ -> parse_expr ctx cursor (* test if it an instance of Expr *)
  in
  with_loc kind ()
