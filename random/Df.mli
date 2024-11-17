(*

  - Datatype
  - Field
  - Series
  - Dataframe

*)

module Datatype : sig
  type 'a t

  val int : int t
  val bool : bool t
  val string : string t
end

module Field : sig
  type 'a t

  type ('fields, 'last) list

  val name : 'a t -> string

  val rename : string -> 'a t -> 'a t

  val datatype : 'a t -> 'a Datatype.t
end

(* module Schema : sig end *)

module Series : sig
  type 'a t

  val of_array : 'a array -> 'a t
end

module Dataframe : sig
  type ('schema, 'last) t

  val schema : ('schema, 'last) t -> ('schema, 'last) Field.list

  val empty : ('last, 'last) t

  val add_series :
    'a Field.t -> 'a Series.t -> ('schema, 'last) t -> ('a -> 'schema, 'last) t

  val join : ('start, 'mid) t -> ('mid, 'last) t -> ('start, 'last) t

  val get : 'a Field.t -> ('schema, 'last) t -> 'a Series.t option

  val select :
    ('schema, 'last) Field.list -> ('schema, 'last) t -> ('schema, 'last) t
end
