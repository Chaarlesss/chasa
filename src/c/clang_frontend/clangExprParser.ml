open ClangContext
open ClangSyntax

let parse_unary_operator (kind: Clang.clang_ext_unaryoperatorkind) =
  match kind with
  | Clang.Plus -> OPlus
  | Minus -> OMinus
  | Not -> ONot
  | LNot -> OLNot
  | _ -> Printf.printf "Unknown binop: %s\n" (Clang.ext_unary_operator_get_opcode_spelling kind);
    assert false

let parse_binary_operator kind =
  match kind with
  | Clang.Mul -> OMul
  | Div -> ODiv
  | Add -> OAdd
  | Sub -> OSub
  | LAnd -> OLAnd
  | LOr -> OLOr
  | Assign -> failwith "Impossible"
  | _ -> Printf.printf "Unknown binop: %s\n" (Clang.ext_binary_operator_get_opcode_spelling kind);
    assert false

let parse_integer_literal cursor =
  Clang.ext_integer_literal_get_value cursor |>
  Clang.ext_int_get_sext_value |>
  mk_integer_literal_expr

let parse_unary_operator ctx cursor =
  let child =
    match Clang.list_of_children cursor with
    | [child] -> child
    | _ -> failwith "Invalid unary operator"
  in
  let unopkind = Clang.ext_unary_operator_get_opcode cursor in
  let child = ctx.expr_parser ctx child in
  let unop = parse_unary_operator unopkind in
  mk_unary_operator_expr unop child

let parse_binary_operator ctx cursor =
  let lhs, rhs =
    match Clang.list_of_children cursor with
    | [lhs; rhs] -> lhs, rhs
    | _ -> failwith "Invalid binary operator"
  in
  let binopkind = Clang.ext_binary_operator_get_opcode cursor in
  let lhs = ctx.expr_parser ctx lhs in
  let rhs = ctx.expr_parser ctx rhs in
  if binopkind = Clang.Assign then
    mk_assign_expr lhs rhs
  else
    let binop = parse_binary_operator binopkind in
    mk_binary_operator_expr binop lhs rhs

let parse_decl_ref_expr cursor =
  Clang.ext_decl_get_name cursor |>
  Clang.ext_declaration_name_get_as_identifier |>
  mk_decl_ref_expr

let parse_implicit_cast_expr ctx cursor =
  Clang.list_of_children cursor |>
  List.hd |>
  ctx.expr_parser ctx |>
  mk_implicit_cast_expr

exception Error of Clang.clang_ext_stmtkind

let parse_expr ctx cursor =
  let kind =
    match Clang.ext_stmt_get_kind cursor with
    | IntegerLiteral -> parse_integer_literal cursor
    | UnaryOperator -> parse_unary_operator ctx cursor
    | BinaryOperator -> parse_binary_operator ctx cursor
    | DeclRefExpr -> parse_decl_ref_expr cursor
    | ImplicitCastExpr -> parse_implicit_cast_expr ctx cursor
    | kind -> raise (Error kind)
  in
  expr_with_loc kind cursor
