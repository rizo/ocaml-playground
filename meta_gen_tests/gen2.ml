let str ?(sep = "") = String.concat sep

(* Potential library interface *)
module Code : sig
  type 'a t
  val int : int -> int t
  val list : 'a t list -> 'a list t
  val add : int -> int -> int t
  val let_in : string -> 'a t -> ('a t -> 'body t) -> 'body t
  val gen : 'a t -> unit
end = struct
  type 'a t = string
  let int = string_of_int

  let list xs = str [ "["; str (List.map (fun xs -> xs ^ ";") xs); "]" ]
  let add x1 x2 = str [ "("; int x1; "+"; int x2; ")" ]
  let let_in name v body = str [ "let "; name; " = "; v; " in "; body name ]
  let gen x = print_endline x
end

(*

  let x = <<1>> in
  let y = <<2>> in
  let z = 200 in
  [$(x); $(y) + 100; z]

*)
let () =
  let x = 1 in
  let y = 2 in
  let open Code in
  gen (let_in "z" (int 200) (fun z -> list [ int x; add 100 y; z ]))
