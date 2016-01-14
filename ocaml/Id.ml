open Core.Std

module Id : sig
  type t
  val of_int : int -> t
  val to_int : t -> int
  val of_string : string -> t
  val to_string : t -> string
  val increment : t -> t
  val decrement : t -> t
end
= struct
  type t = int
  let of_int x = 
    assert (x>0); x
  let to_int x = x
  let of_string x = 
    Int.of_string x
    |> of_int
  let to_string x =
    Int.to_string x
  let increment x = x + 1
  let decrement x = x - 1
end

module Task = struct
  include Id
end

module Client = struct
  include Id
end

