open Context

let default_ctx trsl_unit =
  {
    trsl_unit;
    decl_parser= ClangDecl.parse_decl;
    stmt_parser= ClangStmt.parse_stmt;
    expr_parser= ClangExpr.parse_expr;
  }

let parse src =
  let trsl_unit = Clang.parse_string src in
  let ctx = default_ctx trsl_unit in
  let cursor = Clang.get_translation_unit_cursor trsl_unit in
  match Clang.get_cursor_kind cursor with
  | TranslationUnit ->
    Clang.list_of_children_map
      (ctx.decl_parser ctx)
      cursor
  | _ -> failwith ("Frontend: unknown type" ^ Clang.get_cursor_display_name cursor)
