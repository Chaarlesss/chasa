
module type S = sig
  type expr
  type stmt
  type decl

  type decl_kind

  type var_decl

  val mk_function_decl: stmt -> decl_kind

  val mk_var_decl: string -> expr option -> decl_kind

  val decl_with_loc: decl_kind -> Clang.cxcursor -> decl
end

module Make(E: sig type expr end)(S: sig type stmt end): (S with type expr = E.expr and type stmt = S.stmt) = struct
  type expr = E.expr
  type stmt = S.stmt

  type decl =
    {
      decl_kind: decl_kind;
      decl_loc: Clang.cxcursor
    }

  and decl_kind =
    | Function: stmt -> decl_kind
    | Var: var_decl -> decl_kind

  and var_decl =
    {
      var_name: string;
      var_init: expr option
    }

  let mk_function_decl body =
    Function body

  let mk_var_decl var_name var_init =
    Var { var_name; var_init }

  let decl_with_loc decl_kind decl_loc =
    { decl_kind; decl_loc }

end
