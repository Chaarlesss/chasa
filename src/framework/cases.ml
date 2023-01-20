open Flow

type 'a case =
  | Empty
  | NotHandled

type ('a, 'r) cases =
  {
    cases: ('r case * 'a flow) list
  }
