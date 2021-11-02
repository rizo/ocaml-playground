exception Resumable of (unit -> unit)


let rec work () =
  print_endline "hello";
  Unix.sleepf 1.0;
  raise (Resumable work)


let () =
  try work () with
  Resumable k -> k ()
