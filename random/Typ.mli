type void
(** The impossible type.

    No values can exist for this type. *)

type 'a t
(** The type for runtime representation of values of type ['a]. *)

type 'a typ = 'a t
(** Alias for ['a t]. *)

(** {1 Primitive types} *)

type empty = |

val int : int t
(** Type representation for ints. *)

val bool : bool t
(** Type representation for booleans. *)

val string : string t
(** Type representation for strings. *)

(** {1 Records} *)

type ('record, 'fields) record

module Record : sig
  type ('record, 'fields) t = ('record, 'fields) record
  type ('r, 'a) field

  type ('r, 'fs) fields =
    | [] : ('r, 'r) fields
    | ( :: ) : ('r, 'f) field * ('r, 'fs) fields -> ('r, 'f -> 'fs) fields

  module Field : sig
    type ('r, 'a) t = ('r, 'a) field
    type 'r any = Any : ('r, 'a) field -> 'r any

    val name : ('r, 'a) field -> string
    val typ : ('r, 'a) field -> 'a typ
    val get : 'r -> ('r, 'a) field -> 'a
    val view : 'r -> ('r, 'a) field -> (string -> 'a typ -> 'a -> 'b) -> 'b
    val make : string -> 'a typ -> ('r -> 'a) -> ('r, 'a) field
  end

  val make : string -> ('r, 'types) fields -> 'r typ

  val field : string -> 'a typ -> ('r -> 'a) -> ('r, 'a) field
  (** [field name typ get] is the representation of a field called [name] of
      type [typ] and getter function [get]. *)

  val name : ('r, 'fs) t -> string
  val fields : ('r, 'fs) t -> ('r, 'fs) fields
  val any_fields : ('r, 'fs) t -> 'r Field.any list
end

val record : string -> ('r, 'types) Record.fields -> 'r typ
(** [record name fields] is the type representation of the record with [name]
    and [fields].

    {[
      type person = { name : string; age : int }

      let person_typ =
        record "person"
          Record.
            [
              Record.field "name" string (fun p -> p.name);
              Record.field "age" int (fun p -> p.age);
            ]
    ]} *)

val view :
  'a typ ->
  int:(unit -> 'b) ->
  bool:(unit -> 'b) ->
  string:(unit -> 'b) ->
  record:(('r, 'fs) record -> 'b) ->
  'b

type 'a mapper = {
  int : 'a mapper -> int -> 'a;
  bool : 'a mapper -> bool -> 'a;
  string : 'a mapper -> string -> 'a;
  record : 'r. 'a mapper -> string -> 'r Record.Field.any list -> 'r -> 'a;
}

val map : 'b mapper -> 'a t -> 'a -> 'b
