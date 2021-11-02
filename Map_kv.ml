type ('a, 'id) cmp = ('a -> 'a -> int [@bs])


module type Comparable = sig
  type identity
  type t
  val cmp: (t, identity) cmp
end

type ('key, 'id) comparable =
  (module Comparable with type t = 'key and type identity = 'id)




type ('key, 'id ) id = ('key, 'id) comparable


type ('a, 'b, 'c) dict = unit

type ('k,'v,'id) t = {
    cmp: ('k,'id) cmp;
    data: ('k,'v, 'id) dict
}




