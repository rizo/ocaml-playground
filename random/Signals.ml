let print ?formatter:(f = Format.err_formatter) fmt =
  Stdlib.Format.(kfprintf (fun f -> pp_print_newline f ()) f fmt)


let () = print ""

module type Sig1 = sig
  type 'a t

  val mk : 'a -> 'a t
  val set : 'a -> 'a t -> unit
  val use : ('a -> unit) -> 'a t -> unit
end

module Sig1 = struct
  type 'a t = { mutable value : 'a; mutable subs : ('a -> unit) list }

  let mk x = { value = x; subs = [] }

  let use f t =
    t.subs <- f :: t.subs;
    f t.value


  let set x t =
    t.value <- x;
    List.iter (fun sub -> sub x) t.subs
end

let () =
  let x_sig = Sig1.mk 42 in
  Sig1.use (fun x -> print ">> eff_1: %d" x) x_sig;
  Sig1.set 0 x_sig;
  Sig1.use (fun x -> print ">> eff_2: %d" x) x_sig;
  Sig1.set 1 x_sig


module type Sig2 = sig
  type 'a t

  val init : 'a -> 'a t

  (* val bind : ('a -> 'b t) -> 'a t -> 'b t *)
  (* val set : 'a -> 'a t -> unit *)
  val map : ('a -> 'b) -> 'a t -> 'b t
end

module Test2 (Signal : sig
  type 'a t

  val init : 'a -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'a t
  val bind : ('a -> 'b t) -> 'a t -> 'b t

  module Op : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end
end) =
struct
  open Signal.Op

  let x = Signal.init 42 in
  let y = Signal.map string_of_int x in
  let
  ()
end
