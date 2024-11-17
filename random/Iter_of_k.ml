type 'a iter = ('a -> unit) -> unit

let count n : int iter =
 fun k ->
  for i = 0 to n - 1 do
    k i
  done


(* Impossible as we cannot create a producer out of a consumer. *)
let of_consumer : ('a -> unit) -> 'a iter = fun _k -> assert false

let to_consumer : 'a iter -> 'a -> unit = fun _iter -> assert false
let to_consumer : (('a -> unit) -> unit) -> 'a -> unit =
 fun _iter -> assert false


let create : unit -> ('a -> unit) * 'a iter =
 fun () ->
  let k x = 0 in
  9


let create : 'a iter -> _ =
 fun iter ->
  let k x = () in
  iter k;
  k
