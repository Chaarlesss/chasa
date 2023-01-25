open Framework.Term

type expr_kind = ..
type stmt_kind = ..
type decl_kind = ..

type expr =
  {
    expr_kind: expr_kind;
    expr_loc: Location.t;
  }

type stmt =
  {
    stmt_kind: stmt_kind;
    stmt_loc: Location.t;
  }

type decl =
  {
    decl_kind: decl_kind;
    decl_loc: Location.t;
  }

let expr_compare_chain =
  Type.mk_compare_chain compare

let compare_expr expr1 expr2 =
  Type.compare expr_compare_chain expr1 expr2

let expr_pp_chain =
  Type.mk_pp_chain (fun _ _ -> failwith "Unknown expression")

let pp_expr fmt expr =
  Type.pp expr_pp_chain fmt expr

let register_expr (info: expr Type.info) =
  Type.register info expr_compare_chain expr_pp_chain

let register_expr_compare cmp =
  Type.register_compare cmp expr_compare_chain

let register_expr_pp pp =
  Type.register_pp pp expr_pp_chain

let stmt_compare_chain =
  Type.mk_compare_chain compare

let compare_stmt stmt1 stmt2 =
  Type.compare stmt_compare_chain stmt1 stmt2

let stmt_pp_chain =
  Type.mk_pp_chain (fun _ _ -> failwith "Unknown statement")

let pp_stmt fmt stmt =
  Type.pp stmt_pp_chain fmt stmt

let register_stmt (info: stmt Type.info) =
  Type.register info stmt_compare_chain stmt_pp_chain

let register_stmt_compare cmp =
  Type.register_compare cmp stmt_compare_chain

let register_stmt_pp pp =
  Type.register_pp pp stmt_pp_chain

let decl_compare_chain =
  Type.mk_compare_chain compare

let compare_decl decl1 decl2 =
  Type.compare decl_compare_chain decl1 decl2

let decl_pp_chain =
  Type.mk_pp_chain (fun _ _ -> failwith "Unknown declaration")

let pp_decl fmt stmt =
  Type.pp decl_pp_chain fmt stmt

let register_decl (info: decl Type.info) =
  Type.register info decl_compare_chain decl_pp_chain

let register_decl_compare cmp =
  Type.register_compare cmp decl_compare_chain

let register_decl_pp pp =
  Type.register_pp pp decl_pp_chain

type pure_expr =
  | Var of string

type _ term_kind +=
  | C_expr: expr -> pure_expr term_kind
  | C_stmt: stmt -> unit term_kind
  | C_decl: decl -> unit term_kind

let () =
  register_term
    {
      compare = (fun (type a) (type b) (comparator: TermType.comparator) (t1: a term) (t2: b term) ->
          match term_kind t1, term_kind t2 with
          | C_expr e1, C_expr e2 -> compare_expr e1 e2
          | C_stmt s1, C_stmt s2 -> compare_stmt s1 s2
          | C_decl d1, C_decl d2 -> compare_decl d1 d2
          | _ -> comparator.f t1 t2
        );
      pp = (fun (type a) (printer: TermType.printer) fmt (t: a term) ->
          match term_kind t with
          | C_expr e -> pp_expr fmt e
          | C_stmt s -> pp_stmt fmt s
          | C_decl d -> pp_decl fmt d
          | _ -> printer.f fmt t
        );
    }

type decl_kind +=
  | D_Function of stmt
  | D_Var of string * expr option
