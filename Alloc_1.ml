type size = int
type 'a block = 'a array

module Block = struct
  type 'a t = 'a block
  
  let null self = Array.length self = 0
end


type 'a t =
  Alloc of {
    alloc : size -> 'a block;
    free : 'a block -> unit;
    owns : 'a block -> bool;
  }


module type S1 = sig
  type 'a t
  
  val make :
    alloc:(size -> 'a block) ->
    free: ('a block -> unit) ->
    owns:('a block -> bool) ->
    'a t
  
  val fallback : 'a t -> 'a t -> 'a t
end

module M1 = struct
  let fallback (Alloc a1) (Alloc a2) =
    let alloc size =
      let block = a1.alloc size in
      if Block.null block then
        a2.alloc size
      else block
    in
    Alloc { a1 with alloc }
end
