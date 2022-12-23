open Expr
open Context

let parse_binary_operator kind: Syntax.binop =
  match kind with
  | Clang.Mul -> Mul
  | Add -> Add
  | _ -> Printf.printf "Unknown binop: %s\n" (Clang.ext_binary_operator_get_opcode_spelling kind);
    assert false

let parse_integer_literal cursor =
  let cxint = Clang.ext_integer_literal_get_value cursor in
  let i = Clang.ext_int_get_sext_value cxint in
  mk_integer_literal i

let parse_binary_operator ctx cursor =
  let lhs, rhs =
    match Clang.list_of_children cursor with
    | [lhs; rhs] -> lhs, rhs
    | _ -> failwith "Invalid binary operator"
  in
  let binopkind = Clang.ext_binary_operator_get_opcode cursor in
  let lhs = ctx.expr_parser ctx lhs in
  let rhs = ctx.expr_parser ctx rhs in
  let binop = parse_binary_operator binopkind in
  mk_binary_operator binop lhs rhs

let parse_decl_ref_expr cursor =
  let cxname = Clang.ext_decl_get_name cursor in
  let id = Clang.ext_declaration_name_get_as_identifier cxname in
  mk_decl_ref id

let parse_implicit_cast_expr ctx cursor =
  Clang.list_of_children cursor |>
  List.hd |>
  ctx.expr_parser ctx |>
  mk_implicit_cast

exception Error of Clang.clang_ext_stmtkind

let parse_expr ctx cursor =
  let kind =
    match Clang.ext_stmt_get_kind cursor with
    | IntegerLiteral -> parse_integer_literal cursor
    | BinaryOperator -> parse_binary_operator ctx cursor
    | DeclRefExpr -> parse_decl_ref_expr cursor
    | ImplicitCastExpr -> parse_implicit_cast_expr ctx cursor
    | kind -> raise (Error kind)
  in
  with_loc kind ()
