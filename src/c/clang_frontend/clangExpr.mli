
module type S = sig
  type unop =
    | OPlus: unop
    | OMinus: unop
    | ONot: unop
    | OLNot: unop

  type binop =
    | OAdd: binop
    | OSub: binop
    | OMul: binop
    | ODiv: binop
    | ORem: binop
    | OLAnd: binop
    | OLOr: binop

  type identifier = string

  type expr_kind

  type stmt
  type expr
  type decl

  val mk_integer_literal: int -> expr_kind

  val mk_decl_ref: identifier -> expr_kind

  val mk_implicit_cast: expr -> expr_kind

  val mk_unary_operator: unop -> expr -> expr_kind

  val mk_binary_operator: binop -> expr -> expr -> expr_kind

  val mk_assign: expr -> expr -> expr_kind

  val expr_with_loc: expr_kind -> Clang.cxcursor -> expr

end

module Make(D: sig type decl end)(S: sig type stmt end): (S with type stmt = S.stmt and type decl = D.decl)
