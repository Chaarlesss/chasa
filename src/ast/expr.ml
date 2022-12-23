open Syntax

type t = Syntax.expr

type kind = Syntax.expr_kind

let mk_integer_literal i =
  IntegerLiteral i

let mk_decl_ref id =
  DeclRef id

let mk_implicit_cast expr =
  ImplicitCast expr

let mk_binary_operator kind lhs rhs =
  BinaryOperator(kind, lhs, rhs)

let with_loc expr_kind expr_loc =
  { expr_kind; expr_loc }
