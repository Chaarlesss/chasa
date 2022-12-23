
type t =
  {
    trsl_unit: Clang.cxtranslationunit;
    decl_parser: t -> Clang.cxcursor -> Decl.t;
    stmt_parser: t -> Clang.cxcursor -> Stmt.t;
    expr_parser: t -> Clang.cxcursor -> Expr.t
  }
