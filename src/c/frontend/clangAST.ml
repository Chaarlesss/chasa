
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

type identifier = string

type stmt =
  {
    stmt_kind: stmt_kind;
    stmt_loc: Clang.cxcursor;
  }

and stmt_kind =
  | SCompound: stmt list -> stmt_kind
  | SDecl: decl list -> stmt_kind
  | SIf: {
      cond_expr: expr;
      then_stmt: stmt;
      else_stmt: stmt option
    } -> stmt_kind
  | SWhile: {
      cond_expr: expr;
      body_stmt: stmt
    } -> stmt_kind
  | SContinue: stmt_kind
  | SBreak: stmt_kind
  | SExpr: expr -> stmt_kind

and expr =
  {
    expr_kind: expr_kind;
    expr_loc: Clang.cxcursor
  }

and expr_kind =
  | EIntegerLiteral: int -> expr_kind
  | EDeclRef: identifier -> expr_kind
  | EUnaryOperator: unop * expr -> expr_kind
  | EBinaryOperator: binop * expr * expr -> expr_kind
  | EImplicitCast: expr -> expr_kind
  | EAssign: expr * expr -> expr_kind

and decl =
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

let mk_compound_stmt children =
  SCompound children

let mk_decl_stmt decl =
  SDecl decl

let mk_expr_stmt expr =
  SExpr expr

let mk_if_stmt cond_expr then_stmt else_stmt =
  SIf { cond_expr; then_stmt; else_stmt }

let mk_while_stmt cond_expr body_stmt =
  SWhile { cond_expr; body_stmt }

let stmt_with_loc stmt_kind stmt_loc =
  { stmt_kind; stmt_loc }

let mk_integer_literal_expr i =
  EIntegerLiteral i

let mk_decl_ref_expr id =
  EDeclRef id

let mk_implicit_cast_expr expr =
  EImplicitCast expr

let mk_unary_operator_expr kind child =
  EUnaryOperator (kind, child)

let mk_binary_operator_expr kind lhs rhs =
  EBinaryOperator (kind, lhs, rhs)

let mk_assign_expr lhs rhs =
  EAssign (lhs, rhs)

let expr_with_loc expr_kind expr_loc =
  { expr_kind; expr_loc }

let mk_function_decl body =
  Function body

let mk_var_decl var_name var_init =
  Var { var_name; var_init }

let decl_with_loc decl_kind decl_loc =
  { decl_kind; decl_loc }
