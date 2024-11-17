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

let test1 () =
  let x_sig = Sig1.mk 42 in
  Sig1.use (fun x -> print ">> eff_1: %d" x) x_sig;
  Sig1.set 0 x_sig;
  Sig1.use (fun x -> print ">> eff_2: %d" x) x_sig;
  Sig1.set 1 x_sig


module type Sig2 = sig
  type 'a t

  val init : 'a -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val use : ('a -> unit) -> 'a t -> unit
  val set : 'a -> 'a t -> unit

  val bind : ('a -> 'b t) -> 'a t -> 'b t
end

module Sig2_1 : Sig2 = struct
  type 'a t = { mutable value : 'a; mutable subs : ('a -> unit) list }

  let init value = { value; subs = [] }
  let map f t = { value = f t.value; subs = [] }
  let use f t =
    t.subs <- t.subs @ [ f ];
    f t.value


  let set x t =
    t.value <- x;
    List.iter (fun sub -> sub x) t.subs


  let bind : ('a -> 'b t) -> 'a t -> 'b t = fun f t -> f t.value
end

module Test2 (Signal : Sig2) = struct
  let test1 () =
    let x = Signal.init 42 in

    let y = Signal.map (fun value -> value + 10000) x in

    Signal.use (fun value -> print "signal value x: %d" value) x;
    Signal.use (fun value -> print "signal value y: %d" value) y;

    Signal.set 100 x;
    Signal.set 5 y


  let () = test1 ()
end

include Test2 (Sig2_1)

module type Sig3 = sig
  type 'a t

  val init : 'a -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val filter : ('a -> bool) -> 'a t -> 'a t
  val use : ('a -> unit) -> 'a t -> unit
  val set : 'a -> 'a t -> unit
end

module Sig3_1 : Sig3 = struct
  type 'a t = { mutable value : 'a option; mutable subs : ('a -> unit) list }

  let init value = { value = Some value; subs = [] }

  let map f t =
    match t.value with
    | Some value -> { value = Some (f value); subs = [] }
    | None -> { value = None; subs = [] }


  let filter f t =
    match t.value with
    | None -> t
    | Some value when f value -> t
    | Some _ -> { value = None; subs = [] }


  let use f t =
    t.subs <- t.subs @ [ f ];
    match t.value with
    | Some value -> f value
    | None -> ()


  let set x t =
    t.value <- Some x;
    List.iter (fun sub -> sub x) t.subs
end

module Sig3_2 : Sig3 = struct
  type 'a t = {
    mutable value : 'a;
    mutable subs : ('a -> unit) list;
    filter : 'a -> bool;
  }

  let init value = { value; subs = []; filter = (fun _ -> true) }

  let map f t = { value = f t.value; subs = []; filter = (fun _ -> true) }

  (* let filter f t = { t with filter = f } *)
  let filter f t = { value = t.value; subs = []; filter = f }

  let use f t =
    t.subs <- t.subs @ [ f ];
    f t.value


  let set x t =
    t.value <- x;
    if t.filter x then List.iter (fun sub -> sub x) t.subs
end

let test_ () =
  let module Signal = Sig3_2 in
  let x' = Signal.init 42 in
  let y' = Signal.map (fun x -> string_of_int (x + 1)) x' in

  x' |> Signal.use (fun x -> print "use x 1: %d" x);

  y'
  (* |> Signal.map (fun x -> x ^ " ***") *)
  (* |> Signal.map (fun x -> x ^ " ***") *)
  (* |> Signal.filter (fun _ -> false) *)
  |> Signal.use (print "use y 1: %S");

  Signal.set 9 x'


let test () =
  let module Signal = Sig3_2 in
  let x' = Signal.init 42 in
  x'
  |> Signal.map (fun x -> string_of_int (x + 1))
  |> Signal.use (fun x -> print "use x 1: %s" x);

  (* x' |> Signal.use (fun x -> print "use x 2: %d" x); *)
  Signal.set 9 x'

(* y' *)
(* |> Signal.filter (fun y -> String.length y >= 0) *)
(* |> Signal.map (fun y -> "prefix-" ^ y) *)
(* |> Signal.use (print "use y 1: %S"); *)
(***)
(* Signal.use (print "use x 1: %d") x'; *)
(* Signal.set 1042 x'; *)
(* Signal.set "hey" y'; *)
(* Signal.set "hey 2" y'; *)
(* Signal.set "" y' *)
