let%test "singleton" = Pvec.idx (Pvec.singleton 0) 0 = 0

let%test "three" =
  let vec = Pvec.of_list [ 'a'; 'b'; 'c' ] in
  Pvec.idx vec 0 = 'a' && Pvec.idx vec 1 = 'b' && Pvec.idx vec 2 = 'c'

let%test "n1000" =
  let vec = Pvec.of_list (List.init 1000 (fun x -> x)) in
  let out = ref false in
  for i = 0 to 1000 - 1 do
    out := Pvec.idx vec i = i
  done;
  !out
