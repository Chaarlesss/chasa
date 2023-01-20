
type token = ..

type token += T_cur

let token_compare_chain =
  Type.mk_compare_chain
    (fun tk1 tk2 ->
       match tk1, tk2 with
       | T_cur, T_cur -> 0
       | _ -> compare tk1 tk2
    )

let compare_token tk1 tk2 = Type.compare token_compare_chain tk1 tk2

let token_pp_chain =
  Type.mk_pp_chain
    (fun fmt tk ->
       match tk with
       | T_cur -> Format.fprintf fmt "cur"
       | _ -> failwith "unknown token"
    )

let pp_token fmt tk = Type.pp token_pp_chain fmt tk

let register_token (info:token Type.info) =
  Type.register info token_compare_chain token_pp_chain

module TokenMap = Map.Make(
  struct
    type t = token
    let compare = compare_token
    let pp = pp_token
  end
  )
