
module type S = sig
  type t

  val bottom: t

  val top: t

  val is_bottom: t -> bool

  val leq: t -> t -> bool

  val join: t -> t -> t

  val meet: t -> t -> t

  val widen: t -> t -> t

  val pp: Format.formatter -> t -> unit

end
