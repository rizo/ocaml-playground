let () =
  let str0 = [%eval String.concat "," [ "a"; "b"; "c" ]] in
  print_endline str0
