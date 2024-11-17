
(* Partially inspired by http://okmij.org/ftp/Haskell/Iteratee/LogParser.hs *)

type ('a, 'b) sink =
  Sink : {
    init : unit -> 'acc;
    push : 'acc -> 'a -> 'acc;
    full : 'acc -> bool;
    stop : 'acc -> 'b;
  } -> ('a, 'b) sink


let first =
  Sink {
    init = (fun () -> None);
    push = (fun _ x -> Some x);
    full = (function None -> false | _ -> true);
    stop = (fun acc -> acc);
  }


type ('top, 'a, 'b) flat_map =
  | Flat_map_top : 'top -> ('top, 'a, 'b) flat_map
  | Flat_map_sub : {
    init: 'sub;
    push: 'sub -> 'a -> 'sub;
    full: 'sub -> bool;
    stop: 'sub -> 'b;
  } -> ('top, 'a, 'b) flat_map


let flat_map f (Sink top) =
  let init () = Flat_map_top (top.init ()) in
  let push s x =
    match s with
    | Flat_map_top acc ->
      let acc' = top.push acc x in
      if top.full acc' then
        let r = top.stop acc' in
        let Sink sub = f r in
        Flat_map_sub {
          init = sub.init ();
          push = sub.push;
          full = sub.full;
          stop = sub.stop;
        }
      else
        Flat_map_top acc'
    | Flat_map_sub sub -> Flat_map_sub { sub with init = sub.push sub.init x } in
  let full = function
    | Flat_map_top acc -> top.full acc
    | Flat_map_sub sub -> sub.full sub.init in
  let stop = function
    | Flat_map_top acc ->
      let Sink sub = f (top.stop acc) in
      sub.stop (sub.init ())
    | Flat_map_sub sub -> sub.stop sub.init in
  Sink { init; push; full; stop }

(* let (>>=) m f = flat_map f m *)

(* -- Make sure the the stream contains at least one space. Skip it. *)
(* space :: Monad m => Iteratee Char m () *)
(* space = headM >>= check *)
(*  where *)
(*  check (Just ' ') = dropWhile (== ' ') >> return ()  *)
(*  check _          = parser_error "space expected but not found" *)



type error =
  | Unexpected     of { expected : char option; actual : char option }
  | Invalid_infix  of char
  | Invalid_prefix of char
  | Zero

type 'a parser = char array -> ('a * char array, error) result
(* type 'a parser = s -> ('a * 's) option *)

let return x =
  fun input -> Ok (x, input)

let (>>=) p f =
  fun input ->
    match p input with
    | Ok (x, input') ->
      let p' = f x in p' input'
    | Error e -> Error e



module Stream_p = struct
(* module State = struct *)
(*   type 'a t = *)
(*     | Partial of 'a partial *)
(*     | Lazy    of 'a t Lazy.t *)
(*     | Done    of int * 'a *)
(*     | Fail    of int * string list * string *)

(*   and 'a partial = *)
(*     { committed : int *)
(*     ; continue  : Bigstringaf.t -> off:int -> len:int -> More.t -> 'a t } *)

(* end *)
(* type 'a with_state = Input.t ->  int -> More.t -> 'a *)

(* type 'a failure = (string list -> string -> 'a State.t) with_state *)
(* type ('a, 'r) success = ('a -> 'r State.t) with_state *)

(* type 'a t = *)
(*   { run : 'r. ('r failure -> ('a, 'r) success -> 'r State.t) with_state } *)

  type 'a t =
    { stream : 'r . ('a, 'r) sink -> 'r }
    [@@unboxed]

(* let return v = *)
(*     { run = fun input pos more _fail succ -> *)
(*       succ input pos more v *)
(*     } *)

  let return x =
    { stream = fun (Sink k) ->
      k.stop (k.push (k.init ()) x)
    }

(* let fail msg = *)
(*     { run = fun input pos more fail _succ -> *)
(*       fail input pos more [] msg *)
(*     } *)

(* - Should the sink be able to accept "error" input state? *)
(* - Should the stream take a fail sink? *)

(* let (>>=) p f = *)
(*   { run = fun input pos more fail succ -> *)
(*     let succ' input' pos' more' v = (f v).run input' pos' more' fail succ in *)
(*     p.run input pos more fail succ' *)
(*   } *)

  let flat_map f self =
    let stream (Sink k) =
      let push r x =
        (f x).stream (Sink { k with
            init = (fun () -> r);
            stop = (fun r -> r)
          }) in
      self.stream (Sink { k with push }) in
    { stream }

  let (>>=) m f = flat_map f m


  let map f self =
    let stream (Sink k) =
      let push acc x =
        k.push acc (f x) in
      self.stream (Sink { k with push })
    in
    { stream }

  let (<$>) f self = map f self

(* let (<*>) f m = *)
(*     (* f >>= fun f -> m >>| f *) *)
(*     { run = fun input pos more fail succ -> *)
(*       let succ0 input0 pos0 more0 f = *)
(*         let succ1 input1 pos1 more1 m = succ input1 pos1 more1 (f m) in *)
(*         m.run input0 pos0 more0 fail succ1 *)
(*       in *)
(*       f.run input pos more fail succ0 } *)


  let (<*>) f m = f >>= fun f -> f <$> m

  type 'a parser = char list -> 'a t

  let error _ = failwith "no"

  let expect expected = fun input ->
    match input with
    | [] -> error `End_of_input
    | actual :: _leftover when actual = expected -> return actual
    | actual :: _leftover -> error (`Unexpected_token actual)


  (* let consume expected =
    expect expected >>= fun _ -> advance *)
end

