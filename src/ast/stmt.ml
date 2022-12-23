open Syntax

type t = Syntax.stmt

type kind = Syntax.stmt_kind

let mk_compound_stmt children =
  Compound children

let mk_decl_stmt children =
  Decl children

let mk_expr expr =
  Expr expr

let with_loc stmt_kind _ =
  { stmt_kind; stmt_loc= () }
