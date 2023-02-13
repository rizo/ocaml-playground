let opaque = Sys.opaque_identity

let bench_html1 () =
  Html.a
    Html.Attr.
      [ href "#";
        class_list [ "button"; "mt-2" ];
        style [ "red" ];
        Html.attr "data-target" "content"
      ]
    []
  |> Html.to_string

let bench_html2 () =
  Core.a ~class_list:[ "button"; "mt-2" ] ~style:"red"
    ~data:[ ("target", "content") ]
    ~href:"#" []
  |> Core.to_string

let cases =
  [ ("html1", opaque bench_html1, ()); ("html2", opaque bench_html2, ()) ]

let () =
  let res = (Sys.opaque_identity Benchmark.throughputN) ~repeat:2 3 cases in
  Benchmark.tabulate res