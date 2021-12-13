let input_sizes = [ 1; 10; 100; 200; 300; 400 ]

let array_input = Array.init 10_000 (fun x -> x)
let pvec_input = Pvec.of_list (Array.to_list array_input)

let array_get () = ignore (Array.get array_input 100)
let pvec_get () = ignore (Pvec.idx pvec_input 100)

open Core_bench

let () =
  Bench.Test.
    [ create ~name:"array_get" array_get; create ~name:"pvec_get" pvec_get ]
  |> Bench.make_command
  |> Core.Command.run
