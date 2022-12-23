open Syntax

type t = Syntax.decl

type kind = Syntax.decl_kind

let mk_function_decl body =
  Function body

let mk_var_decl var_decl =
  Var var_decl

let with_loc decl_kind _ =
  { decl_kind; decl_loc= () }
