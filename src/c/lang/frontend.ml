open Ast

type stmt_transformer = ClangAST.stmt -> stmt Option.t
type expr_transformer = ClangAST.expr -> expr Option.t
type decl_transformer = ClangAST.decl -> decl Option.t

let stmt_transformer_list = ref []
let expr_transformer_list = ref []
let decl_transformer_list = ref []

let register_stmt_transformer (transformer: stmt_transformer) =
  stmt_transformer_list := transformer :: !stmt_transformer_list

let register_expr_transformer (transformer: expr_transformer) =
  expr_transformer_list := transformer :: !expr_transformer_list

let register_decl_transformer (transformer: decl_transformer) =
  decl_transformer_list := transformer :: !decl_transformer_list

let transform_loc (_cursor: Clang.cxcursor): Location.t =
  ()

let transform_term term transformer_list term_name =
  List.fold_until
    ~f:(fun term trsf ->
        match trsf term with
        | None -> Continue term
        | Some t_term -> Stop t_term
      )
    ~init:term
    ~finish:(fun _ -> failwith ("Don't know how to transform " ^ term_name))
    transformer_list

let transform_stmt (stmt: ClangAST.stmt): stmt =
  transform_term stmt !stmt_transformer_list "statement"

let transform_expr (expr: ClangAST.expr): expr =
  transform_term expr !expr_transformer_list "expression"

let transform_decl (decl: ClangAST.decl): decl =
  transform_term decl !decl_transformer_list "declaration"
