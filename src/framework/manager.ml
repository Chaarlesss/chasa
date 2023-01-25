open Flow
open Cases
open Term

type 'a man =
  {
    compute: 'r. 'r term -> 'a flow -> ('a, 'r) cases;
  }
