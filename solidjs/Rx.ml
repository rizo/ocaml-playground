let print ?formatter:(f = Format.err_formatter) fmt =
  Stdlib.Format.(kfprintf (fun f -> pp_print_newline f ()) f fmt)


let context = ref []

let context_push x = context := x :: !context

let context_pop () =
  match !context with
  | [] -> ()
  | _ :: c -> context := c


type 'a subscription = {
  mutable dependencies : 'a list;
  execute : unit -> unit;
}

let subscribe running subscriptions = subscriptions := running :: !subscriptions
(* running.dependencies <- !subscriptions :: running.dependencies *)

let signal default =
  let subscriptions = ref [] in
  let current = ref default in
  let get () =
    let () =
      match !context with
      | running :: _ -> subscribe running subscriptions
      | [] -> ()
    in
    !current
  in
  let set x =
    current := x;
    List.iter (fun sub -> sub ()) !subscriptions
  in
  (get, set)


let effect f =
  let rec execute () =
    cleanup running;
    context_push running;
    Fun.protect f ~finally:context_pop
  in
  let running = { execute; dependencies = [] } in
  ()


let () =
  print "";

  let get, set = signal 3 in
  print "initial read: %d" (get ());
  set 5;
  print "updated read: %d" (get ());
  set (get () * 2);
  print "updated read: %d" (get ())
