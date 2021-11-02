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
end

module Flow = struct
  let take n =
    let flow (Sink snk) =
      let i = ref 0 in
      let full_waiter, full_resolver = Lwt.task () in
      let push acc x =
        if !i = n then Lwt.wakeup full_resolver ();
        let%lwt acc' = snk.push acc x in
        incr i;
        Lwt.return acc'
      in
      let full acc = Lwt.pick [ snk.full acc; full_waiter ] in
      Sink { init = snk.init; push; full; stop = snk.stop }
    in
    { flow }


  let during n =
    let flow (Sink k) =
      let timer = ref Lwt.return_unit in
      let init () =
        let%lwt k_s = k.init () in
        timer := Lwt_unix.sleep n;
        Lwt.return k_s
      in
      let push acc x = k.push acc x in
      let stop acc = k.stop acc in
      let full k_s = Lwt.pick [ k.full k_s; !timer ] in
      Sink { init; push; full; stop }
    in
    { flow }


  type ('a, 'k_s) group_by_time = {
    mutable k_s : 'k_s;
    mutable group : 'a list;
    mutable group_interval : float;
    mutable group_timer : unit Lwt.t;
  }

  let group_by_time resolution =
    let flow (Sink k) =
      let s0 =
        {
          k_s = Obj.magic ();
          group = [];
          group_interval = 0.0;
          group_timer = Lwt.return_unit;
        }
      in

      let add_to_group s x =
        log "group_by_time: add_to_group";
        s.group <- x :: s.group;
        Lwt.return s
      in

      let push_group s =
        log "group_by_time: push_group";
        let%lwt k_s' = k.push s.k_s (List.rev s.group) in
        s.k_s <- k_s';
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
        log "group_by_time: start_group_timer";
        (* if%lwt k.full s.k_s then Lwt.return_unit
           else *)
        let%lwt () = Lwt_unix.sleep duration in
        log "group_by_time: start_group_timer - expired";
        let%lwt () = push_group s in
        reset_group_timer s;
        Lwt.return_unit
      and stop_group_timer s =
        Lwt.cancel s.group_timer;
        s.group_timer <- Lwt.return_unit
      in

      let init () =
        log "group_by_time: init";
        let%lwt k_s0 = k.init () in
        s0.k_s <- k_s0;
        (* if%lwt k.full k_s0 then Lwt.return s0
           else *)
        reset_group_timer s0;
        Lwt.return s0
      in

      let push s x =
        let now = Unix.gettimeofday () in
        if time_belongs_to_span ~span:s.group_interval ~resolution now then
          add_to_group s (now, x)
        else begin
          log "group_by_time: push - new group";
          reset_group_timer s;
          let%lwt () = push_group s in
          add_to_group s (now, x)
        end
      in

      let full s = k.full s.k_s in

      let stop s =
        log "group_by_time: stop";
        stop_group_timer s;
        log "group_by_time: canceled group_timer";
        let%lwt () =
          let full = k.full s.k_s in
          if lwt_is_resolved full then Lwt.return_unit else push_group s
        in
        k.stop s.k_s
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
      let%lwt r = Lwt.map ref (snk.init ()) in
      let rec loop s =
        match%lwt src.pull s with
        | None ->
          let%lwt () = src.stop s in
          snk.stop !r
        | Some (x, s') ->
          let%lwt r' = snk.push !r x in
          r := r';
          loop s'
      in
      Lwt.pick
        [
          (let%lwt () = snk.full !r in
           snk.stop !r);
          (let%lwt s0 =
             try%lwt src.init ()
             with exn ->
               let%lwt _ = snk.stop !r in
               raise exn
           in
           try%lwt loop s0
           with exn ->
             let%lwt () = src.stop s0 in
             let _ = snk.stop !r in
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
