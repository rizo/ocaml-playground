let round_time_int ~(resolution : int) ts =
  let ts' = int_of_float ts in
  ts' / resolution * resolution |> float_of_int


let time_belongs_to_span ~span ~resolution t =
  let rounded = round_time_int ~resolution t in
  rounded = span


module type S = sig
  type 'a stream

  val group_by_time : int -> 'a stream -> (int, 'a) Hashtbl.t stream
end

module Producer = struct
  let count ~every:interval k =
    prerr_endline "count";
    let rec loop i =
      let%lwt () = k ("msg-" ^ string_of_int i) in
      let%lwt () = Lwt_unix.sleep interval in
      loop (i + 1)
    in
    loop 0
end

let map f seq k = seq (fun x -> k (f x))

type ('a, 'r) sink =
  | Sink : {
      init : unit -> 's Lwt.t;
      push : 's -> 'a -> 's Lwt.t;
      full : 's -> bool Lwt.t;
      stop : 's -> 'r Lwt.t;
    }
      -> ('a, 'r) sink

let group_by_time ~resolution ~yield input =
  let acc = ref [] in

  let start_span_timer duration =
    let%lwt () = Lwt_unix.sleep duration in
    yield !acc;
    Lwt.return true
  in

  let rec read_span ~resolution current_span =
    match%lwt input () with
    | Some str ->
      let now = Unix.gettimeofday () in
      if time_belongs_to_span ~span:current_span ~resolution now then begin
        acc := (now, str) :: !acc;
        read_span ~resolution current_span
      end
      else begin
        yield !acc;
        acc := (now, str) :: !acc;
        Lwt.return true
      end
    | None ->
      yield !acc;
      Lwt.return false
  in

  let rec run ~resolution () =
    let now = Unix.gettimeofday () in
    let current_span = round_time_int ~resolution now in
    let delay_until_next = current_span +. float_of_int resolution -. now in
    let%lwt continue =
      Lwt.pick
        [
          start_span_timer delay_until_next; read_span ~resolution current_span;
        ]
    in
    if continue then run ~resolution () else Lwt.return_unit
  in
  run ~resolution ()


let thread_2 () =
  let pipe = Lwt_pipe.create () in
  prerr_endline "thread_2: created pipe";
  let producer = Producer.count ~every:3.0 (Lwt_pipe.write_exn pipe) in
  let rec consumer () =
    match%lwt Lwt_pipe.read pipe with
    | Some x ->
      let%lwt () = Lwt_io.printl x in
      consumer ()
    | None -> Lwt_io.printl "Done"
  in
  Lwt.all [ producer; consumer () ]


let thread_1 () =
  let producer = Producer.count ~every:3.0 in
  producer Lwt_io.printl
