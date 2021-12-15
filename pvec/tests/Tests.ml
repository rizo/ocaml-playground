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

let%test "set_tail" =
  let vec = Pvec.of_list (List.init 64 (fun x -> x)) in
  let vec' = Pvec.set 63 163 vec in
  let copy_ok = ref false in
  for i = 0 to 64 - 1 do
    copy_ok := if i = 63 then Pvec.idx i vec' = 163 else Pvec.idx i vec' = i
  done;
  let orig_ok = ref false in
  for i = 0 to 64 - 1 do
    orig_ok := Pvec.idx i vec = i
  done;
  !copy_ok && !orig_ok

let%test "set_tail" =
  let vec = Pvec.of_list (List.init 64 (fun x -> x)) in
  let vec' = Pvec.set 31 131 vec in
  let copy_ok = ref false in
  for i = 0 to 64 - 1 do
    copy_ok := if i = 31 then Pvec.idx i vec' = 131 else Pvec.idx i vec' = i
  done;
  let orig_ok = ref false in
  for i = 0 to 64 - 1 do
    orig_ok := Pvec.idx i vec = i
  done;
  !copy_ok && !orig_ok

let%test "push_tail" =
  let vec = Pvec.of_list (List.init 3 (fun x -> x)) in
  let vec' = Pvec.push 3 vec in
  let copy_ok = ref false in
  for i = 0 to 4 - 1 do
    copy_ok := Pvec.idx i vec' = i
  done;
  let orig_ok = ref false in
  for i = 0 to 3 - 1 do
    orig_ok := Pvec.idx i vec' = i
  done;
  !copy_ok && !orig_ok && Pvec.len vec' = 4 && Pvec.len vec = 3

let%test "push_tail_full" =
  let vec = Pvec.of_list (List.init 4 (fun x -> x)) in
  let vec' = Pvec.push 4 vec in
  let copy_ok = ref false in
  for i = 0 to 5 - 1 do
    copy_ok := Pvec.idx i vec' = i
  done;
  let orig_ok = ref false in
  for i = 0 to 4 - 1 do
    orig_ok := Pvec.idx i vec' = i
  done;
  !copy_ok && !orig_ok && Pvec.len vec' = 5 && Pvec.len vec = 4
