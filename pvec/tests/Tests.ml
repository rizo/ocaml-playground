module Pvec = Pvec.Rb_tree

let%test "singleton" = Pvec.idx 0 (Pvec.singleton 0) = 0

let%test "three" =
  let vec = Pvec.of_list [ 'a'; 'b'; 'c' ] in
  Pvec.idx 0 vec = 'a' && Pvec.idx 1 vec = 'b' && Pvec.idx 2 vec = 'c'

let%test "n1000" =
  let vec = Pvec.of_list (List.init 1000 (fun x -> x)) in
  let out = ref false in
  for i = 0 to 1000 - 1 do
    out := Pvec.idx i vec = i
  done;
  !out
