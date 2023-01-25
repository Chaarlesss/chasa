open ClangAST
open Ast
open Frontend

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

type expr_kind +=
  | E_const_int of int
  | E_var of string
  | E_unop of unop * expr
  | E_binop of binop * expr * expr
  | E_cast of expr
  | E_assign of expr * expr

type stmt_kind +=
  | S_compound of stmt list
  | S_decl of decl list
  | S_expr of expr
  | S_if of expr * stmt * stmt option
  | S_while of expr * stmt
  | S_continue
  | S_break

let transform_intraproc_stmt ({ stmt_kind; stmt_loc }: ClangAST.stmt): stmt Option.t =
  let stmt_kind_opt =
    match stmt_kind with
    | ClangAST.SCompound stmt_list -> Some (S_compound (List.map transform_stmt stmt_list))
    | ClangAST.SDecl decl_list -> Some (S_decl (List.map transform_decl decl_list))
    | ClangAST.SIf { cond_expr; then_stmt; else_stmt } -> Some (S_if (transform_expr cond_expr, transform_stmt then_stmt, Option.map transform_stmt else_stmt))
    | ClangAST.SWhile { cond_expr; body_stmt } -> Some (S_while (transform_expr cond_expr, transform_stmt body_stmt))
    | ClangAST.SExpr expr -> Some (S_expr (transform_expr expr))
    | ClangAST.SContinue -> Some S_continue
    | ClangAST.SBreak -> Some S_break
    | _ -> None
  in
  Option.map
    (fun stmt_kind ->
       let stmt_loc = transform_loc stmt_loc in
       { stmt_kind; stmt_loc }
    )
    stmt_kind_opt

let _ =
  register_stmt_transformer transform_intraproc_stmt
