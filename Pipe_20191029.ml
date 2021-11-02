module type Pipe = sig
  type 'a t

  val put_async : 'a -> 'a t -> unit
  val put_block : 'a -> 'a t -> unit

  val get_async : 'a t -> 'a

  val get_block : 'a t -> 'a

  val go : (unit -> 'a) -> 'a
end

module Pipe_1 = struct end
