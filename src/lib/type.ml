
type 'a compare = ('a -> 'a -> int) -> 'a -> 'a -> int

type 'a compare_chain = ('a -> 'a -> int) ref

type 'a pp = (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit

type 'a pp_chain = (Format.formatter -> 'a -> unit) ref

let mk_compare_chain (default: 'a -> 'a -> int): 'a compare_chain =
  ref default

let mk_pp_chain (default: Format.formatter -> 'a -> unit): 'a pp_chain =
  ref default

let register_compare (compare: 'a compare) (chain: 'a compare_chain): unit =
  chain := compare !chain

let register_pp (pp: 'a pp) (chain: 'a pp_chain): unit =
  chain := pp !chain

type 'a info =
  {
    compare: 'a compare;
    pp: 'a pp;
  }

let register info compare_chain pp_chain =
  register_compare info.compare compare_chain;
  register_pp info.pp pp_chain

let compare (chain: 'a compare_chain) x y =
  if x == y then
    0
  else
    !chain x y

let pp (chain: 'a pp_chain) fmt x =
  !chain fmt x

module MakePoly(S: sig type _ t end) = struct
  type comparator = { f: 'a 'b. 'a S.t -> 'b S.t -> int }

  type compare = comparator -> comparator

  type printer = { f: 'a. Format.formatter -> 'a S.t -> unit }

  type compare_chain = comparator ref

  type pp = printer -> printer

  type pp_chain = printer ref

  let mk_compare_chain (default: comparator): compare_chain =
    ref default

  let mk_pp_chain (default: printer): pp_chain =
    ref default

  let register_compare (compare: compare) (chain: compare_chain): unit =
    chain := compare !chain

  let register_pp (pp: pp) (chain: pp_chain): unit =
    chain := pp !chain

  type info =
    {
      compare: 'a 'b. comparator -> 'a S.t -> 'b S.t -> int;
      pp: 'a. printer -> Format.formatter -> 'a S.t -> unit;
    }

  let register info compare_chain pp_chain =
    register_compare (fun comparator -> { f=(fun s1 s2 -> info.compare comparator s1 s2) }) compare_chain;
    register_pp (fun printer -> { f=(fun fmt s -> info.pp printer fmt s) }) pp_chain

  let compare (chain: compare_chain) x y =
    !chain.f x y

  let pp (chain: pp_chain) fmt x =
    !chain.f fmt x
end
