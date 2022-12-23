open Syntax

type t = Syntax.var_decl

let mk var_name var_init =
  { var_name; var_init }
