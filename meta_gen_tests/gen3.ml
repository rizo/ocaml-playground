let x = [%code 1] in
let y = [%code 2] in
[%code
  let z = 200 in
  [ [%e x]; 100 + [%e y]; z ]]
|> Format.printf "%a@." Ppx_stage.print
