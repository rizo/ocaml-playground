let input_sizes = [ 1; 10; 100; 1_000; 10_000; 100_000 ]

open Core_bench

let () =
  Bench.Test.
    [
      create_indexed ~name:"boxed" ~args:input_sizes (fun len ->
          Core.Staged.stage (fun () -> Pvec.Pvec_boxed_node.iota len));
      create_indexed ~name:"magic" ~args:input_sizes (fun len ->
          Core.Staged.stage (fun () -> Pvec.Pvec_magic_node.iota len));
    ]
  |> Bench.make_command
  |> Core.Command.run
