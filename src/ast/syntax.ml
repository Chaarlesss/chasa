
type unop =
  | Not

type binop =
  | Mul
  | Div
  | Rem
  | Add
  | Sub
  | Assign

type identifier = string

type stmt =
  {
    stmt_kind: stmt_kind;
    stmt_loc: Location.t
  }

and stmt_kind =
  | Compound: stmt list -> stmt_kind
  | Decl: decl list -> stmt_kind
  | If: {
      cond_expr: expr;
      then_stmt: stmt;
      else_stmt: stmt option
    } -> stmt_kind
  | While: {
      cond_expr: expr;
      body_stmt: stmt
    } -> stmt_kind
  | Continue: stmt_kind
  | Break: stmt_kind
  | Expr: expr -> stmt_kind

and expr =
  {
    expr_kind: expr_kind;
    expr_loc: Location.t
  }

and expr_kind =
  | IntegerLiteral: int -> expr_kind
  | DeclRef: identifier -> expr_kind
  | ImplicitCast: expr -> expr_kind
  | BinaryOperator: binop * expr * expr -> expr_kind

and decl =
  {
    decl_kind: decl_kind;
    decl_loc: Location.t
  }

and decl_kind =
  | Function: stmt -> decl_kind
  | Var: var_decl -> decl_kind

and var_decl =
  {
    var_name: string;
    var_init: expr option;
  }
