module Belt = struct
  type ('a, 'id) default = 'a
  type ('a, 'id) eq = 'a -> 'a -> bool

  (* Comparable *)

  type ('a, 'id) cmp = 'a -> 'a -> int

  module type Comparable = sig
    type id
    type t
    val cmp : (t, id) cmp
  end

  type ('a, 'id) comparable =
    (module Comparable with type t = 'a and type id = 'id)

  module Comparable (Base : sig
    type t
    val cmp : t -> t -> int
  end) =
  struct
    type id
    type t = Base.t

    let cmp = Base.cmp
  end

  let comparable (type t) cmp =
    let module Cmp = Comparable (struct
      type nonrec t = t
      let cmp = cmp
    end) in
    (module Cmp : Comparable with type t = t)


  (* Hashable *)
  type ('a, 'id) hash = 'a -> int

  module type Hashable = sig
    type id
    type t
    val hash : (t, id) hash
    val eq : (t, id) eq
  end

  type ('a, 'id) hashable = (module Hashable with type t = 'a and type id = 'id)

  module Hashable (Base : sig
    type t
    val hash : t -> int
    val eq : t -> t -> bool
  end) =
  struct
    type id
    type t = Base.t
    let hash = Base.hash

    let eq = Base.eq
  end

  let hashable (type a) hash eq =
    let module H = Hashable (struct
      type t = a
      let hash = hash
      let eq = eq
    end) in
    (module H : Hashable with type t = a)
end

module type Tid = sig
  type t
  type id
end

type 'a tid = (module Tid with type t = 'a)

let tid (type a) () =
  let module M = struct
    type id
    type t = a
  end in
  (module M : Tid with type t = a)


module Records = struct
  type 'a hash =
    | Hash : {
        tid : 'a tid;
        hash : 'a -> int;
        eq : 'a -> 'a -> bool;
      }
        -> 'a hash

  let int_hash = Hash { tid = tid (); hash = Hashtbl.hash; eq = Int.equal }
end
