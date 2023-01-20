open Flow
open Cases
open Term

type ('a, 't) man =
  {
    compute: 'r. 'r term -> 'a flow -> ('a, 'r) cases;
  }
