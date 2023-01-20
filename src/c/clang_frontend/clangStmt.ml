
module type S = sig
  type expr
  type stmt
  type decl

  type stmt_kind

  val mk_compound_stmt: stmt list -> stmt_kind

  val mk_decl_stmt: decl list -> stmt_kind

  val mk_expr_stmt: expr -> stmt_kind

  val mk_if_stmt: expr -> stmt -> stmt option -> stmt_kind

  val mk_while_stmt: expr -> stmt -> stmt_kind

  val stmt_with_loc: stmt_kind -> Clang.cxcursor -> stmt

end

module Make(E: sig type expr end)(D: sig type decl end): (S with type expr = E.expr and type decl = D.decl) = struct
  type expr = E.expr
  type decl = D.decl

  type stmt =
    {
      stmt_kind: stmt_kind;
      stmt_loc: Clang.cxcursor;
    }

  and stmt_kind =
    | SCompound: stmt list -> stmt_kind
    | SDecl: decl list -> stmt_kind
    | SIf: {
        cond_expr: expr;
        then_stmt: stmt;
        else_stmt: stmt option
      } -> stmt_kind
    | SWhile: {
        cond_expr: expr;
        body_stmt: stmt
      } -> stmt_kind
    | SContinue: stmt_kind
    | SBreak: stmt_kind
    | SExpr: expr -> stmt_kind

  let mk_compound_stmt children =
    SCompound children

  let mk_decl_stmt decl =
    SDecl decl

  let mk_expr_stmt expr =
    SExpr expr

  let mk_if_stmt cond_expr then_stmt else_stmt =
    SIf { cond_expr; then_stmt; else_stmt }

  let mk_while_stmt cond_expr body_stmt =
    SWhile { cond_expr; body_stmt }

  let stmt_with_loc stmt_kind stmt_loc =
    { stmt_kind; stmt_loc }

end
