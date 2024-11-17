type 'a typ

val int : int typ
val float : float typ
val string : string typ
val bool : bool typ
val null : 'a typ -> 'a option typ

type field

val field : 'a typ -> field

type schema
val schema : field list -> schema


module Schema : sig
  type t = schema

  val fields : t -> field list
end
