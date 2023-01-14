
module type S = sig
  type expr
  type stmt
  type decl

  type stmt_kind

  val mk_compound_stmt: stmt list -> stmt_kind

  val mk_decl_stmt: decl list -> stmt_kind

  val mk_expr: expr -> stmt_kind

  val mk_if_stmt: expr -> stmt -> stmt option -> stmt_kind

  val mk_while_stmt: expr -> stmt -> stmt_kind

  val stmt_with_loc: stmt_kind -> Clang.cxcursor -> stmt

end

module Make(E: sig type expr end)(D: sig type decl end): (S with type expr = E.expr and type decl = D.decl)
