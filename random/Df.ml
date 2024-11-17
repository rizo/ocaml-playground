(* Type identifiers.
   See http://alan.petitepomme.net/cwn/2015.03.24.html#1 *)

module Type_id = struct
  type _ t = ..
end
module type Type_id = sig
  type t
  type _ Type_id.t += Type_id : t Type_id.t
end

type 'a type_id = (module Type_id with type t = 'a)

let type_id () (type s) =
  let module M = struct
    type t = s
    type _ Type_id.t += Type_id : t Type_id.t
  end in
  (module M : Type_id with type t = s)


type ('a, 'b) type_eq = Type_eq : ('a, 'a) type_eq

let type_eq : type r s. r type_id -> s type_id -> (r, s) type_eq option =
 fun r s ->
  let module R = (val r : Type_id with type t = r) in
  let module S = (val s : Type_id with type t = s) in
  match R.Type_id with
  | S.Type_id -> Some Type_eq
  | _ -> None


module Datatype = struct
  type 'a info = {
    pp : Format.formatter -> 'a -> unit;
    cmp : 'a -> 'a -> int;
    hash : 'a -> int;
  }
  type 'a t =
    | Int : int info -> int t
    | Bool : bool info -> bool t
    | String : string info -> string t
    | Char : char info -> char t

  let int =
    Int { pp = Format.pp_print_int; cmp = Stdlib.compare; hash = Hashtbl.hash }


  let bool =
    Bool
      { pp = Format.pp_print_bool; cmp = Stdlib.compare; hash = Hashtbl.hash }


  let string =
    String
      { pp = Format.pp_print_string; cmp = Stdlib.compare; hash = Hashtbl.hash }


  let char =
    Char
      { pp = Format.pp_print_char; cmp = Stdlib.compare; hash = Hashtbl.hash }
end

module Field = struct
  type 'a t = { name : string; datatype : 'a Datatype.t; type_id : 'a type_id }

  type ('fields, 'last) list =
    | [] : ('last, 'last) list
    | ( :: ) : 'a t * ('fields, 'last) list -> ('a -> 'fields, 'last) list

  let make name datatype = { name; datatype; type_id = type_id () }

  let name self = self.name
  let type_id self = self.type_id
  let rename name self = { self with name }
  let datatype self = self.datatype
end
(*
   module Schema = struct
     let rec concat :
         type start mid last. (start, mid) t -> (mid, last) t -> (start, last) t =
      fun x y ->
       match x with
       | a :: q -> a :: concat q y
       | [] -> y
   end *)

module Series = struct
  type 'a t = { data : 'a array }

  let of_array data = { data }
end

module Dataframe = struct
  type ('fields, 'last) t =
    | [] : ('last, 'last) t
    | ( :: ) :
        ('a Field.t * 'a Series.t) * ('fields, 'last) t
        -> ('a -> 'fields, 'last) t

  (* type packed = Packed : ('a, 'b) t -> packed [@@unboxed] *)

  let empty = []

  let add_series field series self = (field, series) :: self

  let rec schema :
      type fields last. (fields, last) t -> (fields, last) Field.list =
   fun self ->
    match self with
    | [] -> Field.[]
    | (field, _series) :: rest -> Field.(field :: schema rest)


  let rec get :
      type a fields last. a Field.t -> (fields, last) t -> a Series.t option =
   fun target self ->
    match self with
    | [] -> None
    | (field, series) :: rest -> (
      match type_eq (Field.type_id field) (Field.type_id target) with
      | Some Type_eq -> Some series
      | None -> get target rest)


  let select :
      type target_fields target_last fields last.
      (target_fields, target_last) Field.list ->
      (fields, last) t ->
      (target_fields, target_last) t =
   fun _target_fields _self -> assert false


  (* match (target_fields, self) with
     | [], [] -> None
     | (), (field, series) :: rest -> (
       match type_eq (Field.type_id field) (Field.type_id target) with
       | Some Type_eq -> Some series
       | None -> get target rest) *)

  let rec join :
      type start mid last. (start, mid) t -> (mid, last) t -> (start, last) t =
   fun x y ->
    match x with
    | a :: q -> a :: join q y
    | [] -> y
end

let series1 = Series.of_array [| 1; 2; 3 |]
let series2 = Series.of_array [| true; false |]
let series3 = Series.of_array [| "a"; "foo" |]
let series4 = Series.of_array [| 'x'; 'y' |]

let _ =
  let enabled_field = Field.make "enabled" Datatype.bool in
  let df1 =
    Dataframe.
      [ (Field.make "ts" Datatype.int, series1); (enabled_field, series2) ]
  in

  let df2 =
    Dataframe.
      [
        (Field.make "name" Datatype.string, series3);
        (Field.make "char" Datatype.char, series4);
      ]
  in

  let df3 = Dataframe.join df1 df2 in
  let enabled = Dataframe.get enabled_field df2 in
  ignore (df3, enabled)
