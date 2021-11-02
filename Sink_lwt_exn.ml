type ('a, 'r) sink =
  | Sink : {
      init : unit -> 's Lwt.t;
      push : 's -> 'a -> 's Lwt.t;
      stop : 's -> 'r Lwt.t;
    }
      -> ('a, 'r) sink
let list =
  Sink
    {
      init = (fun () -> Lwt.return []);
      push = (fun acc x -> Lwt.return (x :: acc));
      stop = (fun acc -> Lwt.return (List.rev acc));
    }
