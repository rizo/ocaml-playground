print_newline ();
print_endline "yoii"

module K = struct
  let id : int Hmap.key = Hmap.Key.create ()
  let email : string Hmap.key = Hmap.Key.create ()
  let name : string Hmap.key = Hmap.Key.create ()
  let description : string Hmap.key = Hmap.Key.create ()
  let created_time : int Hmap.key = Hmap.Key.create ()
end

module Account_full : sig
  type t

  val make : id:int -> email:string -> name:string -> ?description:string -> created_time:int -> unit -> t
end = struct
  type t = Hmap.t

  let make ~id ~email ~name ?description ~created_time () =
    let out = Hmap.empty
      |> Hmap.add K.id id
      |> Hmap.add K.email email
      |> Hmap.add K.name name
      |> Hmap.add K.created_time created_time in
    match description with
    | None -> out
    | Some description -> Hmap.add K.description description out
end


let () =
  let msg_full =
    Account_full.make ~id:42 ~email:"a@b.com" ~name:"Foo" ~created_time:2023
  in
  ()
