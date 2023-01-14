open ClangSyntax

type t =
  {
    trsl_unit: Clang.cxtranslationunit;
    decl_parser: t -> Clang.cxcursor -> decl;
    stmt_parser: t -> Clang.cxcursor -> stmt;
    expr_parser: t -> Clang.cxcursor -> expr
  }
