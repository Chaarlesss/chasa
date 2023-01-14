open ClangContext
open ClangSyntax

let parse_compound_stmt ctx cursor =
  let children = Clang.list_of_children_map (ctx.stmt_parser ctx) cursor in
  mk_compound_stmt children

let parse_decl_stmt ctx cursor =
  let children = Clang.list_of_children_map (ctx.decl_parser ctx) cursor in
  mk_decl_stmt children

let parse_if_stmt ctx cursor =
  let cond_cursor = Clang.ext_if_stmt_get_cond cursor in
  let then_cursor = Clang.ext_if_stmt_get_then cursor in
  let else_cursor = Clang.ext_if_stmt_get_else cursor in
  let cond = ctx.expr_parser ctx cond_cursor in
  let then_stmt = ctx.stmt_parser ctx then_cursor in
  let else_stmt = Clang.option_cursor_map (ctx.stmt_parser ctx) else_cursor in
  mk_if_stmt cond then_stmt else_stmt

let parse_while_stmt ctx cursor =
  let cond_cursor = Clang.ext_while_stmt_get_cond cursor in
  let body_cursor = Clang.ext_while_stmt_get_body cursor in
  let cond = ctx.expr_parser ctx cond_cursor in
  let body = ctx.stmt_parser ctx body_cursor in
  mk_while_stmt cond body

let parse_expr ctx cursor =
  let expr = ctx.expr_parser ctx cursor in
  mk_expr expr

let parse_stmt ctx cursor =
  let kind =
    match Clang.ext_stmt_get_kind cursor with
    | CompoundStmt -> parse_compound_stmt ctx cursor
    | DeclStmt -> parse_decl_stmt ctx cursor
    | WhileStmt -> parse_while_stmt ctx cursor
    | IfStmt -> parse_if_stmt ctx cursor
    | _ -> parse_expr ctx cursor (* TODO: test if it an instance of Expr *)
  in
  stmt_with_loc kind cursor
