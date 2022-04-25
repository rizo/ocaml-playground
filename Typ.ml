(* Type identifiers.
   See http://alan.petitepomme.net/cwn/2015.03.24.html#1 *)

module Witness = struct
  type _ t = ..
end

module type Witness = sig
  type t
  type _ Witness.t += Witness : t Witness.t
end

type 'a witness = (module Witness with type t = 'a)

let witness () (type s) =
  let module M = struct
    type t = s
    type _ Witness.t += Witness : t Witness.t
  end in
  (module M : Witness with type t = s)

type ('a, 'b) type_eq = Type_eq : ('a, 'a) type_eq

let type_eq : type r s. r witness -> s witness -> (r, s) type_eq option =
 fun r s ->
  let module R = (val r : Witness with type t = r) in
  let module S = (val s : Witness with type t = s) in
  match R.Witness with
  | S.Witness -> Some Type_eq
  | _ -> None

type void = { void : 'a. 'a }
type empty = |

module rec T : sig
  type _ t =
    | Int : int t
    | Bool : bool t
    | String : string t
    | Record : ('record, 'fields) Record.t -> 'record t
end =
  T

and Record : sig
  type ('record, 'a) field

  type ('record, 'fields) fields =
    | [] : ('record, 'record) fields
    | ( :: ) :
        ('record, 'a) field * ('record, 'rest) fields
        -> ('record, 'a -> 'rest) fields

  type ('record, 'fields) t

  module Field : sig
    type ('r, 'a) t = ('r, 'a) field
    type 'r any = Any : ('r, 'a) field -> 'r any

    val name : ('record, 'a) field -> string
    val typ : ('record, 'a) field -> 'a T.t
    val get : 'record -> ('record, 'a) field -> 'a
    val view : 'r -> ('r, 'a) field -> (string -> 'a T.t -> 'a -> 'b) -> 'b
    val make : string -> 'a T.t -> ('r -> 'a) -> ('r, 'a) field
  end

  val field : string -> 'a T.t -> ('record -> 'a) -> ('record, 'a) field
  val make : string -> ('record, 'types) fields -> 'record T.t
  val name : ('r, 'fs) t -> string
  val fields : ('record, 'fields) t -> ('record, 'fields) fields
  val any_fields : ('r, 'fs) t -> 'r Field.any list
end = struct
  type ('record, 'a) field = {
    name : string;
    get : 'record -> 'a;
    typ : 'a T.t;
  }

  type ('record, 'fields) fields =
    | [] : ('record, 'record) fields
    | ( :: ) :
        ('record, 'a) field * ('record, 'rest) fields
        -> ('record, 'a -> 'rest) fields

  type ('record, 'fields) t = {
    name : string;
    fields : ('record, 'fields) Record.fields;
    witness : 'record witness;
  }

  module Field = struct
    type ('r, 'a) t = ('r, 'a) field
    type 'r any = Any : ('r, 'a) field -> 'r any

    let name (field : ('record, 'a) field) = field.name
    let typ (field : ('record, 'a) field) = field.typ
    let get r (field : ('record, 'a) field) = field.get r

    let view r field f =
      let name = Record.Field.name field in
      let t = Record.Field.typ field in
      let v = Record.Field.get r field in
      f name t v

    let make name typ get = { name; typ; get }
  end

  let fields t = t.fields

  let any_fields t =
    let rec loop : type r fs. (r, fs) Record.fields -> r Record.Field.any list =
     fun fields ->
      match fields with
      | [] -> []
      | field :: fields -> Record.Field.Any field :: loop fields
    in
    loop t.fields

  let field = Field.make
  let make name fields = T.Record { name; fields; witness = witness () }
  let name t = t.name
end

include T

type 'a typ = 'a T.t

let int = Int
let bool = Bool
let string = String
let record = Record.make

type ('record, 'fields) record = ('record, 'fields) Record.t

type 'out mapper = {
  int : 'out mapper -> int -> 'out;
  bool : 'out mapper -> bool -> 'out;
  string : 'out mapper -> string -> 'out;
  record : 'r. 'out mapper -> string -> 'r Record.Field.any list -> 'r -> 'out;
}

let map : type a out. out mapper -> a t -> a -> out =
 fun mapper typ x ->
  match typ with
  | Int -> mapper.int mapper x
  | Bool -> mapper.bool mapper x
  | String -> mapper.string mapper x
  | Record r ->
    let name = Record.name r in
    let any_fields = Record.any_fields r in
    mapper.record mapper name any_fields x

module Examples = struct
  type person = { name : string; age : int }

  let typ =
    Record.(
      make "person"
        [
          Record.field "name" string (fun p -> p.name);
          Record.field "age" int (fun p -> p.age);
        ])

  module Show = struct
    let int _self = string_of_int
    let bool _self = string_of_bool
    let string _self x = x

    let field self r (Record.Field.Any field) =
      Record.Field.view r field (fun name typ x ->
          String.concat " = " [ name; map self typ x ])

    let record self name fields r =
      let fields = List.map (field self r) fields in
      String.concat "" [ name; " : "; "{"; String.concat ";" fields; "}" ]

    let to_string_i = { int; bool; string; record }
  end
end
