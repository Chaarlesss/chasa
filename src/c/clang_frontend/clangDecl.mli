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

module Make(E: sig type expr end)(S: sig type stmt end): (S with type expr = E.expr and type stmt = S.stmt)
