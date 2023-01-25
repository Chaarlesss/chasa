open ClangAST
open Ast

let transform_loc (_cursor: Clang.cxcursor): Location.t =
  assert false

let rec transform_stmt ({ stmt_kind; stmt_loc }: ClangAST.stmt): stmt =
  let stmt_loc = transform_loc stmt_loc in
  let stmt_kind =
    match stmt_kind with
    | ClangAST.SIf { cond_expr; then_stmt; else_stmt } -> Ast.S_if (transform_expr cond_expr, transform_stmt then_stmt, Option.map transform_stmt else_stmt)
    | _ -> assert false
  in
  { stmt_kind; stmt_loc }

and transform_expr ({ expr_kind; expr_loc }: ClangAST.expr): expr =
  assert false
