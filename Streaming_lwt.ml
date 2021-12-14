open Lwt.Infix

let bracket ~(init : unit -> 's Lwt.t) ~(stop : 's -> 'b Lwt.t)
    (f : 's -> 's Lwt.t) : 'b Lwt.t =
  let%lwt acc = init () in
  try
    let%lwt acc' = f acc in
    stop acc'
  with exn ->
    let%lwt _acc' = stop acc in
    raise exn


let round_time_int ~(resolution : int) ts =
  let ts' = int_of_float ts in
  ts' / resolution * resolution |> float_of_int


let time_belongs_to_span ~span ~resolution t =
  let rounded = round_time_int ~resolution t in
  rounded = span


let pp_time ppf x =
  Fmt.pf ppf "%a"
    (Ptime.pp_human ~frac_s:5 ())
    (Ptime.of_float_s x |> Option.get)


let log ?time str =
  let time =
    match time with
    | Some time -> time
    | None -> Unix.gettimeofday ()
  in
  Fmt.epr "@.[%a] %s@.%!" pp_time time str


let lwt_never () =
  let t, _ = Lwt.task () in
  t


let lwt_is_resolved lwt = not (Lwt.is_sleeping lwt)

type +'a source =
  | Source : {
      init : unit -> 's Lwt.t;
      pull : 's -> ('a * 's) option Lwt.t;
      stop : 's -> unit Lwt.t;
    }
      -> 'a source

type ('a, 'r) sink =
  | Sink : {
      init : unit -> 's Lwt.t;
      push : 's -> 'a -> 's Lwt.t;
      full : 's -> unit Lwt.t;
      stop : 's -> 'r Lwt.t;
    }
      -> ('a, 'r) sink

type 'a stream = { stream : 'r. ('a, 'r) sink -> 'r Lwt.t } [@@unboxed]

type ('a, 'b) flow = { flow : 'r. ('b, 'r) sink -> ('a, 'r) sink } [@@unboxed]

module Sink = struct
  let list =
    Sink
      {
        init = (fun () -> Lwt.return []);
        push = (fun acc x -> Lwt.return (x :: acc));
        full = (fun _ -> lwt_never ());
        stop = (fun acc -> Lwt.return (List.rev acc));
      }


  let list' =
    Sink
      {
        init = (fun () -> Lwt.return []);
        push = (fun acc x -> Lwt.return (x :: acc));
        full = (fun _ -> lwt_never ());
        stop = (fun acc -> Lwt.return (List.rev acc));
      }


  let each f =
    Sink
      {
        init = (fun () -> Lwt.return_unit);
        push = (fun _ x -> f x);
        full = (fun _ -> lwt_never ());
        stop = (fun _ -> Lwt.return_unit);
      }


  let full =
    Sink
      {
        init = (fun () -> Lwt.return_unit);
        push = (fun _ _ -> assert false);
        full = (fun _ -> Lwt.return_unit);
        stop = (fun _ -> Lwt.return_unit);
      }


  let first () =
    let full, u = Lwt.task () in
    Sink
      {
        init = (fun () -> Lwt.return_none);
        push =
          (fun _ x ->
            Lwt.wakeup u ();
            Lwt.return (Some x));
        full = (fun _ -> full);
        stop = (fun acc -> Lwt.return acc);
      }


  let print_for_duration duration =
    let timer = ref Lwt.return_unit in
    let init () =
      timer := Lwt_unix.sleep duration;
      Lwt.return_unit
    in
    let push () x =
      print_endline x;
      Lwt.return_unit
    in
    let full () = !timer in
    let stop () =
      Lwt.cancel !timer;
      Lwt.return ()
    in
    Sink { init; push; full; stop }


  type 'a list_only = { mutable count : int; mutable list : 'a list }

  let list_only n =
    let full_thread, full_resolver = Lwt.task () in
    let init () = Lwt.return { count = 0; list = [] } in
    let push acc x =
      if acc.count = n - 1 then (
        acc.list <- x :: acc.list;
        Lwt.wakeup full_resolver ();
        Lwt.return acc)
      else (
        acc.list <- x :: acc.list;
        acc.count <- acc.count + 1;
        Lwt.return acc)
    in
    let full _ = full_thread in
    let stop { list; _ } = Lwt.return (List.rev list) in
    Sink { init; push; full; stop }
end

module Flow = struct
  type 'inner_acc take = {
    mutable inner_acc : 'inner_acc;
    counter : int;
    full : unit Lwt.t;
    full_resolver : unit Lwt.u;
  }

  let take n =
    let flow (Sink snk) =
      let init () =
        let full, full_resolver = Lwt.wait () in
        let%lwt inner_acc = snk.init () in
        Lwt.return { inner_acc; counter = 0; full; full_resolver }
      in
      let push acc x =
        let%lwt inner_acc = snk.push acc.inner_acc x in
        acc.inner_acc <- inner_acc;
        if acc.counter = n - 1 then (
          Lwt.wakeup acc.full_resolver ();
          Lwt.return acc)
        else Lwt.return { acc with counter = acc.counter + 1 }
      in

      let full acc = Lwt.pick [ snk.full acc.inner_acc; acc.full ] in
      let stop acc = snk.stop acc.inner_acc in
      Sink { init; push; full; stop }
    in
    { flow }


  let during n =
    let flow (Sink k) =
      let timer = ref Lwt.return_unit in
      let init () =
        let%lwt inner_acc = k.init () in
        timer := Lwt_unix.sleep n;
        Lwt.return inner_acc
      in
      let push acc x = k.push acc x in
      let stop acc = k.stop acc in
      let full inner_acc = Lwt.pick [ k.full inner_acc; !timer ] in
      Sink { init; push; full; stop }
    in
    { flow }


  type ('group_acc, 'inner_acc) group_by_time = {
    mutable inner_acc : 'inner_acc;
    mutable group_acc : 'group_acc;
    mutable group_interval : float;
    mutable group_timer : unit Lwt.t;
  }

  let group_by_time ~into:(Sink group_snk) resolution =
    let flow (Sink inner_snk) =
      let push_group s =
        let%lwt group = group_snk.stop s.group_acc in
        let%lwt inner_acc' = inner_snk.push s.inner_acc group in
        s.inner_acc <- inner_acc';
        let%lwt group_acc = group_snk.init () in
        s.group_acc <- group_acc;
        Lwt.return_unit
      in

      let rec reset_group_timer s =
        Lwt.cancel s.group_timer;
        let now = Unix.gettimeofday () in
        s.group_interval <- round_time_int ~resolution now;
        let delay_until_next =
          s.group_interval +. float_of_int resolution -. now
        in
        s.group_timer <- start_group_timer delay_until_next s
      and start_group_timer duration s =
        let%lwt () = Lwt_unix.sleep duration in
        let%lwt () = push_group s in
        reset_group_timer s;
        Lwt.return_unit
      in

      let init () =
        let%lwt inner_acc = inner_snk.init () in
        let%lwt group_acc = group_snk.init () in

        let acc =
          {
            inner_acc;
            group_acc;
            group_interval = 0.0;
            group_timer = Lwt.return_unit;
          }
        in
        reset_group_timer acc;
        Lwt.return acc
      in

      let push s x =
        let now = Unix.gettimeofday () in
        if time_belongs_to_span ~span:s.group_interval ~resolution now then begin
          let%lwt group_acc' = group_snk.push s.group_acc (now, x) in
          s.group_acc <- group_acc';
          Lwt.return s
        end
        else begin
          reset_group_timer s;
          let%lwt () = push_group s in
          let%lwt group_acc' = group_snk.push s.group_acc (now, x) in
          s.group_acc <- group_acc';
          Lwt.return s
        end
      in

      let full acc = inner_snk.full acc.inner_acc in

      let stop acc =
        Lwt.cancel acc.group_timer;
        inner_snk.stop acc.inner_acc
      in

      Sink { init; push; full; stop }
    in
    { flow }
end

module Source = struct
  let read_lines file =
    let init () = Lwt_io.open_file ~mode:Input file in
    let pull chan =
      match%lwt Lwt_io.read_line_opt chan with
      | Some line -> Lwt.return (Some (line, chan))
      | None -> Lwt.return_none
    in
    let stop = Lwt_io.close in
    Source { init; pull; stop }
end

module Stream = struct
  let empty =
    let stream (Sink k) = k.init () >>= k.stop in
    { stream }


  let single x =
    let stream (Sink snk) =
      let%lwt snk_acc = snk.init () in
      let full = snk.full snk_acc in
      if lwt_is_resolved full then snk.stop snk_acc
      else
        let%lwt snk_acc = snk.push snk_acc x in
        snk.stop snk_acc
    in
    { stream }


  let unavailable =
    let stream (Sink k) =
      let rec loop r =
        let full =
          k.full r >>= fun _ ->
          log "unavailable: full";
          Lwt.return r
        in
        Lwt.pick [ full; (Lwt_unix.sleep 1.0 >>= fun () -> loop r) ]
      in
      bracket loop ~init:k.init ~stop:k.stop
    in
    { stream }


  let from (Source src) =
    let stream (Sink snk) =
      let rec loop s r =
        match%lwt src.pull s with
        | None ->
          let%lwt () = src.stop s in
          snk.stop r
        | Some (x, s') ->
          let%lwt r' = snk.push r x in
          loop s' r'
      in
      let%lwt r0 = snk.init () in
      Lwt.pick
        [
          (let%lwt () = snk.full r0 in
           snk.stop r0);
          (let%lwt s0 =
             try%lwt src.init ()
             with exn ->
               let%lwt _ = snk.stop r0 in
               raise exn
           in
           try%lwt loop s0 r0
           with exn ->
             let%lwt () = src.stop s0 in
             let _ = snk.stop r0 in
             raise exn);
        ]
    in

    { stream }


  exception Stop_iter

  let into sink stream = stream.stream sink

  let via { flow } this =
    let stream sink = into (flow sink) this in
    { stream }


  let take n this = via (Flow.take n) this
end

module Ex_push_producer = struct
  let count ~every:interval k =
    let rec loop i =
      let%lwt () = k i in
      let%lwt () = Lwt_unix.sleep interval in
      loop (i + 1)
    in
    loop 0
end
