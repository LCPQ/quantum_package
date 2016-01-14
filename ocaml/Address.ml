open Core.Std

module Tcp : sig
  type t 
  val of_string : string -> t
  val to_string : t -> string 
end = struct
  type t = string
  let of_string x =
    assert (String.is_prefix ~prefix:"tcp://" x);
    x
  let to_string x = x
end

module Ipc : sig
  type t 
  val of_string : string -> t
  val to_string : t -> string 
end = struct
  type t = string
  let of_string x =
    assert (String.is_prefix ~prefix:"ipc://" x);
    x
  let to_string x = x
end

module Inproc : sig
  type t 
  val of_string : string -> t
  val to_string : t -> string 
end = struct
  type t = string
  let of_string x =
    assert (String.is_prefix ~prefix:"inproc://" x);
    x
  let to_string x = x
end

type t =
| Tcp    of Tcp.t
| Ipc    of Ipc.t
| Inproc of Inproc.t

let to_string = function
| Tcp x -> Tcp.to_string x
| Ipc x -> Ipc.to_string x
| Inproc x -> Inproc.to_string x

