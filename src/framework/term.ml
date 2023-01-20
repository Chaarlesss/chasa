
type _ term_kind = ..

(* the polymorphic argument is the result of the computation of the term *)
type 'a term =
  {
    term_kind: 'a term_kind;
    term_loc: Location.t;
  }

module TermType = Type.MakePoly(struct type 'a t = 'a term end)

let term_kind (term: 'a term) = term.term_kind

let term_loc (term: 'a term) = term.term_loc

let term_compare_chain =
  TermType.mk_compare_chain { f= fun _ _ -> failwith "Unable to compare terms" }

let compare_term t1 t2 =
  TermType.compare term_compare_chain t1 t2

let term_pp_chain =
  TermType.mk_pp_chain { f= fun _ _ -> failwith "Unknown term" }

let pp_term fmt t =
  TermType.pp term_pp_chain fmt t

let register_term info =
  TermType.register info term_compare_chain term_pp_chain

let register_term_compare cmp = TermType.register_compare cmp term_compare_chain

let register_term_pp pp = TermType.register_pp pp term_pp_chain
