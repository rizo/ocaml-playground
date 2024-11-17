


module type S = sig
  include Map.S

  val find : key -> 'a t -> 'a option
end

module Make (Ord : Map.OrderedType) = struct
  include Map.Make (Ord)

  let find k m = try Some (find k m) with Not_found -> None
end

module type Default = sig
  type t
  val default : t
end

module Make_default (K : Map.OrderedType)  (V : Default) = struct
  include Map.Make (K)

  let find k m = try Some (find k m) with Not_found -> None
end


module String_map = Make(String)

let m1 =
  String_map.empty
  |> String_map.add "alice" 34
  |> String_map.add "aria" 18
