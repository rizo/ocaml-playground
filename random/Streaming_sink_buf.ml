
module Sink_state = struct
  (*

    This version of sinks is buffered. The state of buffer indicators
    (consumed/examined/required) is managed with sink's accumulator.


    [init : unit -> 'acc state]

    The initializer:
    - must be lazy
    - may _suggest_ how much data the sink requires
      - should it be an error if available < required
      - should the producer feed the sink more data if available > required?
    - must provid an initial sink state
    - may be immediately ready/full
      - this be represented as a dedicated constructor; or
      - the sink.full function can be used (no way to enforce calling this function)
        - may be possible to do some type trickery


    [push : 'acc -> 'a buffer -> 'acc state]

    The state update function:
    - must process multiple elements
      - empty input might mean two things (TODO: decide):
        - no more data will be available, push must attempt to produce a ready
          state (to be closed)
        - no data is currently available, keep waiting (this is an identity for acc)
    - must roduce an updated state (may be unchanged)
    - must inform how much data from buffer was actually consumed
    - may inform how much data from buffer was only examined but not yet consumed
      - examined will be assumed to be equal to consumed if not provided
    - may suggest how much data the sink requires
      - supplying less than require should result in the entire buffering being
        "examined" but not "consumed"
    - should (TODO: confirm) produce an error on push when a ready state was
      previously produced

    [stop : 'acc -> 'b]

    The termination function:
    - must close the resources acquired by the acc
      - should only attempt to close if the resources were actually acquired.
    - must produce a final accumulated value
      - if the sink cannot produce a value, ['b] should be an [Error].

   *)

  type 's state =
    | Continue of { state: 's; consumed: int; examined: int; required: int }
    | Stop of { state: 's; consumed: int }
    | Error of { error: exn; consumed: int }

  module State : sig
    type 's t = 's state

    val continue
      :  consumed:int
     -> ?examined:int
     -> ?required:int
     -> 's
     -> 's state

    val init
      : ?required:int
     -> 's
     -> 's state

    val stop
      :  consumed:int
     -> 's
     -> 's state

    val error
      : ?consumed:int
     ->  string
     -> 's state

    val flat_map : ('s1 -> 's2 t) -> 's1 t -> 's2 t

    val to_result : 's t -> ('s, exn) result
  end = struct
    type 's t = 's state

    let continue ~consumed ?(examined=consumed) ?(required=0) state =
      Continue { state; consumed; examined; required }

    let init ?required state = continue ?required ~consumed:0 state

    let stop ~consumed state =
      Stop { state; consumed }

    let error ?(consumed=0) error =
      Error { error = Failure error; consumed }

    let flat_map f self =
      match self with
      | Continue x -> f x.state
      | Stop x -> f x.state
      | Error x -> Error x

    let to_result self =
      match self with
      | Continue x -> Ok x.state
      | Stop x -> Ok x.state
      | Error x -> Result.Error x.error
  end

  type 'a input = 'a array

  module Input = struct
    type 'a t = 'a input

    let len = Array.length

    let get i self =
      try Some (Array.get self i)
      with Not_found -> None

    let fold f init = Array.fold_left f init
  end

  type ('a, 'b) sink =
    Sink : {
      init : unit -> 'acc state;
      push : 'acc -> 'a input -> 'acc state;
      stop : 'acc state -> ('b, exn) result;
    } -> ('a, 'b) sink

  let empty =
    let init () = State.stop ~consumed:0 () in
    let push () _input = init () in
    let stop _ = Ok () in
    Sink { init; push; stop }

  let length =
    let init () = State.init 0 in
    let push n input =
      let consumed = Input.len input in
      State.continue ~consumed (n + consumed) in
    let stop state = State.to_result state in
    Sink { init; push; stop }

  let fold f init =
    let init () = State.init init in
    let push state input =
      let consumed = Input.len input in
      let state = Input.fold f state input in
      State.continue ~consumed state in
    let stop state = State.to_result state in
    Sink { init; push; stop }

  let fill x =
    Sink {
      init = (fun () -> State.stop ~consumed:0 ());
      push = (fun () _ -> assert false);
      stop = (fun _ -> Ok x)
    }

  (* Parsing *)

  let consume expected =
    let init () = State.init ~required:1 () in
    let stop state = State.to_result state in
    let push () input =
      match Input.get 0 input with
      | Some actual when expected = actual -> State.stop ~consumed:1 ()
      | Some _actual -> State.error "Unexpected token TODO"
      | None -> State.error "EOF when expectind TODO"
    in
    Sink { init; stop; push }


  (* WIP *)
  (* let feed_sink l (Sink k) = *)
  (*   let rec loop l acc = *)
  (*   match l with *)
  (*   | [] -> acc *)
  (*   | input :: rest -> *)
  (*     match acc with *)
  (*     | Continue { state; consumed; examined; required } -> *)
  (*       k.push state input *)
  (*     | Stop { state; consumed } -> *)
  (*       state *)
  (*     | Error { error; consumed } -> 0 *)
  (*   in *)
  (*   k.stop (loop l (k.init ())) *)
end

type 'a slice


module Sink_input = struct
  (* Sink with buffered input.  No changes to acc. The input is a "volatile"
     buffer. The sink must explicitly discard the contents. *)

  module type Input = sig
    (*

     Reading data from the buffer with !{val:get} and !{val:fold} does not
     consume it. To mark the data as consumed use {!val:next}, {!val:drop},
     {!val:drop_while} or {!discard}.

     Data access:
     - The sink has access to all data passed in the buffer.
     - The buffer (or higher-level abstraction must control the flow of data, i.e. flush it).
     - The sink does not need to worry about the source terminating early, the fusion code must decide when to stop the sink early.

     {2 Consuming data}

     - The memory for the consumed data will be released and no longer available.
     - The data can be consumed with [next, slice, take, drop, discard].


     Termination:
     - In pipelines: "Signals to the producer that the consumer is done reading."
       - We don't need this in streaming because sources and sinks are completely decoupled.
       - The [Stream] (i.e. the fuser) will check when the sink is done and signal terminate to the source.

      Requesting more data:
      - Buffer.request : int -> 'a t -> unit

     Requirements:

       - consume all
       - consume one
       - consume range
       - mark as examine
       - is_empty?
       - len?
       - view?
       - fold?
       - automatic consume management
       - ?examine
       - non-sequential consumption?
         - non-sequential peeking is of course ok
         - consuming intermediate chunks might be useful for some advanced use cases
           - this would require a more sophisticated storage data structure

     *)

    type 'a t

    val of_array : 'a array -> 'a t

    val to_array : 'a t -> 'a array

    val fold : ('r -> 'a -> 'r) -> 'r -> 'a t -> 'r

    val len : 'a t -> int
    (* The length of the buffer. Does not consume the buffer. *)

    val find : ('a -> bool) -> 'a t -> 'a option
    (** [find predicate buf] returns the first leftmost element from [buf]
        matching [predicate], or [None] if there is no such element. *)

    val get : int -> 'a t -> 'a option
    (** [get i buf] obtains the element at index [i] from [buf].
        Does not remove the element from the buffer. *)

    val next : 'a t -> 'a option
    (** [next buf] obtains and removes the first element from [buf]. *)

    (* XXX: Should elements before [start] be discarded?
            Probably better to remove slice to avoid this inconsistency. *)
    val slice : start:int -> stop:int -> 'a t -> 'a slice
    (** [slice ~start ~stop buf] obtains and removes all elements with indices
        from [start] to [stop] from [buf], returning them as a slice. *)

    val take : int -> 'a t -> 'a slice
    (** [take n] obtains and removes the first [n] elements from [buf],
        returning them as a slice. *)

    val drop : int -> 'a t -> unit
    (** [drop n buf] drops the first [n] elements dropped in [buf]. The dropped
        elements will no longer be part of [buf]. *)

    val drop_while : ('a -> bool) -> 'a t -> unit
    (** [drop f buf] applies [f] to the leftmost elements of [buf], one by one,
        dropping them from [buf] while [f] produces [true]. *)

    val discard : 'a t -> unit
    (** [discard buf] marks the buffer as discarded. Must be called when
        [buf]'s content is no longer needed by the sink.

        {b Warning:} If the sink does not consume the elements of the buffer
        or discard the entire buffer, the buffer will be pushed again into
        the sink with the updated state. This can be desired if the sink
        requires more input.

        {b Warning:} Reading data from a discarded buffer is undefined
        behaviour. In practice the buffer operations may raise [Invalid_argument]
        or act as an empty buffer. *)
  end

  module Input  = struct
    type 'a t = {
      data : 'a array;
      mutable offset : int;
    }

    let of_array data = { data; offset = 0 }

    let fold f x self =
      let r = ref x in
      for i = self.offset to Array.length self.data - 1 do
        r := f !r (Array.unsafe_get self.data i)
      done;
      !r

    let len self =
      Array.length self.data - self.offset

    let get i self =
      if i >= len self then
        None
      else
        Some (Array.get self.data (i + self.offset))

    let drop n self =
      self.offset <- self.offset + n

    let next self =
      match get 0 self with
      | None -> None
      | some ->
        drop 1 self;
        some

    let drop_while f self =
      let rec loop i =
        let x = Array.get self.data i in
        if f x then begin
          self.offset <- i + 1;
          loop (i + 1)
        end
      in
      loop self.offset


    let discard self =
      self.offset <- Array.length self.data
  end

  type ('a, 'b) sink =
    Sink : {
      init : unit -> 'acc;
      push : 'acc -> 'a Input.t -> 'acc;
      full : 'acc -> bool;
      stop : 'acc -> 'b;
    } -> ('a, 'b) sink

  let full =
    let init () = () in
    let push () _input = init () in
    let full () = true in
    let stop _ = Ok () in
    Sink { init; push; stop; full }

  let length =
    let init () = 0 in
    let full _ = false in
    let push n buf =
      let len = Input.len buf in
      Input.discard buf;
      len + n
    in
    let stop state = state in
    Sink { init; push; stop; full }

  let fold f init =
    let init () = init in
    let push s buf =
      let r = Input.fold f s buf in
      Input.discard buf;
      r
    in
    let stop s = s in
    let full _ = false in
    Sink { init; push; stop; full }

  let fill x =
    Sink {
      init = (fun () -> ());
      push = (fun () _ -> ());
      stop = (fun _ -> Ok x);
      full = (fun _ -> true);
    }

  (* Parsing *)

  let satisfy condition =
    let init () = `Init in
    let full = function `Satisfied _ -> true | _ -> false in
    let stop state =
      match state with
      | `Init -> Error `Premature_termination
      | `Satisfied x -> Ok x
      | `Unsatisfied x -> Error (`Unsatisfied x)
      | `End_of_input -> Error `End_of_input in
    let push _did_consume buf =
      match Input.next buf with
      | Some x when condition x -> `Satisfied x
      | Some x -> `Unsatisfied x
      | None -> `End_of_input
    in
    Sink { init; stop; push; full }

  let any =
    let init () = `Init in
    let full = function `Satisfied _ -> true | _ -> false in
    let stop state =
      match state with
      | `Init -> Error `Premature_termination
      | `Satisfied x -> Ok x
      | `Unsatisfied x -> Error (`Unsatisfied x)
      | `End_of_input -> Error `End_of_input in
    let push _did_consume buf =
      match Input.next buf with
      | Some x -> `Satisfied x
      | None -> `End_of_input
    in
    Sink { init; stop; push; full }

  let from list =
    satisfy (fun x -> List.mem x list)

  let none list =
    satisfy (fun x -> not (List.mem x list))

  let consume expected =
    let init () = `Incomplete in
    let full s =
      match s with
      | `Incomplete -> false
      | `Consumed
      | `Unexpected _ -> true in
    let stop s =
      match s with
      | `Incomplete -> Error `Incomplete
      | `Unexpected x -> Error (`Unexpected x)
      | `Consumed -> Ok () in
    let push s buf =
      if full s then
        invalid_arg "Push called on a full sink.";
      match Input.next buf with
      | Some actual when expected = actual -> `Consumed
      | Some actual -> `Unexpected actual
      | None -> `Incomplete
    in
    Sink { init; stop; push; full }

  let a_or_b input a b =
    (* type 'a parser = char array -> ('a, char array) *)
    (* type 'a parser = char array -> (char, 'a) sink *)
    (* feed input into a; chech if done; feed input into b *)
    ignore (input, a, b)

end


let sink_input_demo () =
  let module S = Sink_input in
  let S.Sink k = S.consume 'x' in

  let rec loop buf r =
    if S.Input.len buf = 0 then r else
    if k.full r then r else
    loop buf (k.push r buf) in
  let buf = S.Input.of_array [|'x'; 'y'; 'z'|] in
  let r0 = k.init () in
  let r = loop buf r0 in
  k.stop r

module Buffer = Sink_input.Input

module Slice = struct
  type 'a t
end

(* let try_read_line (buf : char Buffer.t) : char Slice.t =
   match Buffer.find (fun x -> x = '\n') buf with
   | Some newline_index ->
     let line = Buffer.slice ~start:0 ~stop:newline_index buf in
     Buffer.drop 1 buf; (* drop the '\n' *)
     Some line
   | None -> None *)


(* let lines_sink () =
  let init () = [] in
  let push lines buf =
    match Buffer.find (fun x -> x = '\n') buf with
    | Some newline_index ->
      (* XXX: Read data. This might trigger a request to the source *)
      let line = Buffer.take newline_index buf in
      Buffer.drop 1 buf; (* drop the '\n' *)
      line :: lines
    | None -> acc in
  let stop lines = List.rev lines in
  let full _ = false in
  Sink { init; push; stop; full } *)



(* let stdin_bytes_source output =
  let init () = Unix.stdin in
  let pull fd =
    (* No local allocations are made. Pipe manages buffers. *)
    (* Request a buffer, instead of allocating a new one. *)
    (* XXX: How do we control read throughput?
            The pipe should have a mechanism to introduce delays. *)
    let buf = Pipe.request ~size_hint:512 output in
    let n = Unix.read fd buf 0 512 in
    if n = 0 then None
    else
      (* Add buffer back to Pipe. *)
      (* Alternatively Pipe.advance n pipe. *)
      (* Do we want to flush async to notify reader and apply backpressure? *)
      Some (Pipe.add_bytes buf ~len:n output)
  in
  let stop _buf = () in
  Source.make () ~init ~pull ~stop *)
