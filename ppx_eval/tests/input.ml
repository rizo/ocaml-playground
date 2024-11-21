
open struct
  type 'a code
end


let () =
  let str0 = [%eval String.concat "," ["a"; "b"; "c"]] in
  print_endline str0

let () =
  let items : string list code = [%code ["a"; "b"; "c"]] in
  let str0 = [%eval String.concat "," ["a"; "b"; "c"]] in
  print_endline str0
