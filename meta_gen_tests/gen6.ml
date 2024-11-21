let () =
  let x = 1 in
  let y = 2 in
  [%code
    let x = 200 in
    2]

(* [%code
   let z = 200 in
   [ [%int x]; 100 + [%int y]; z ]] *)
