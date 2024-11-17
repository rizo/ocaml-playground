
module type Lenses = sig
  type ('s, 'a) t = {
    get : 's -> 'a;
    map : ('a -> 'a) -> 's -> 's;
  }

  val get : ('s, 'a) t -> 's -> 'a
  (** [get lens state] is a value obtained from [state] using [lens]. *)

  val set : ('s, 'a) t -> 'a -> 's -> 's
  (** [set lens x state] is [state] updated with [x] using [lens]. *)

  val map : ('s, 'a) t -> ('a -> 'a) -> 's -> 's

  val ( >> ) : ('s, 'a) t -> ('a, 'b) t -> ('s, 'b) t
  (** [lens1 >> lens2] is a composed lens to access and update values obtained
      with [lens2] from a nested state obtained with [lens1]. *)

  (* val at : int -> ('s, 'a option) t *)

  (* val zip : ('s1, 'a1) t -> ('s2, 'a2) t -> ('s1 * 's2, 'a1 * 'a2) t *)
end

module Lenses : Lenses = struct
  type ('s, 'a) t = {
    get : 's -> 'a;
    map : ('a -> 'a) -> 's -> 's;
  }

  let get lens s =
    lens.get s

  let set lens a s =
    lens.map (fun _ -> a) s

  let map lens f s =
    lens.map f s

  let (>>) l1 l2 =
    let get big =
      l2.get (l1.get big) in
    let map f big =
      l1.map (l2.map f) big in
    { get; map }
end

include Lenses




type address = { street : string ; number : int; postcode : string }

type person = { name : string; age : int; address : address }

let postcode =
  let get p = p.postcode in
  let map f s = { s with postcode = f s.postcode } in
  { get; map }

let name =
  let get p = p.name in
  let map f s = { s with name = f s.name } in
  { get; map }

let age =
  let get p = p.age in
  let map f s = { s with age = f s.age } in
  { get; map }

let address =
  let get p = p.address in
  let map f s = { s with address = f s.address } in
  { get; map }


let alice = {
  name = "Alice";
  age  = 21;
  address = {
    street = "Rua da Alice";
    number = 2;
    postcode = "AAAA"
  }
}


let bob = {
  name = "Bob";
  age  = 28;
  address = {
    street = "Rua do Bob";
    number = 2;
    postcode = "BBBB"
  }
}


module Ix = struct
  let array i =
    let get array = Array.get array i in
    let map f array =
      Array.set array i (f (get array));
      array
    in
    { get; map }
end


