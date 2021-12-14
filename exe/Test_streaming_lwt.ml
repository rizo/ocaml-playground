open Ocaml_playground
open Streaming_lwt

let empty_in_full_out () =
  let g5 = Flow.group_by_time ~into:Sink.list 5 in
  Stream.empty |> Stream.via g5 |> Stream.into Sink.full


let single_in_full_out () =
  let g5 = Flow.group_by_time ~into:Sink.list 5 in
  Stream.single "hello" |> Stream.via g5 |> Stream.into Sink.full


let empty_in_first_out () =
  let g5 = Flow.group_by_time ~into:Sink.list 5 in
  match%lwt Stream.empty |> Stream.via g5 |> Stream.into (Sink.first ()) with
  | Some [] -> Lwt.return_unit
  | Some _ -> assert false
  | None -> Lwt.return_unit


let single_in_first_out () =
  let g5 = Flow.group_by_time ~into:Sink.list 5 in
  match%lwt
    Stream.single 'a' |> Stream.via g5 |> Stream.into (Sink.first ())
  with
  | Some [ (_ts, 'a') ] -> Lwt.return_unit
  | Some _ -> assert false
  | None -> Lwt.return_unit


let unavailable_in_first_out () =
  let g5 = Flow.group_by_time ~into:Sink.list 3 in
  match%lwt
    Stream.unavailable |> Stream.via g5 |> Stream.into (Sink.first ())
  with
  | Some [ (_, 'a') ] -> Lwt.return_unit
  | Some _ -> assert false
  | None -> Lwt.return_unit


let () =
  let threads = [ unavailable_in_first_out () ] in
  Lwt_main.run (Lwt.join threads)
