module type Js = sig
  type js
  type 'a codec

  val int : int codec
  val js : js codec
  val bool : bool codec
  val string : string codec
  val array : 'a codec -> 'a array codec
  val pair : 'a codec -> 'b codec -> ('a * 'b) codec
  val triple : 'a codec -> 'b codec -> 'c codec -> ('a * 'b * 'c) codec
  val nullable : 'a codec -> 'a option codec
  val optional : 'a codec -> 'a option codec
  val encode : 'a codec -> 'a -> js
  val decode : 'a codec -> js -> 'a

  module Obj : sig
    type t

    val empty : unit -> t
    val make : (string * js) list -> t
    val get : t -> string -> 'a codec -> 'a
    val set : t -> string -> 'a codec -> 'a -> unit
    val del : t -> string -> unit
  end
end
