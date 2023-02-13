open Printf

let rec fold_lines f accu ic =
  match Stdlib.input_line ic with
  | line -> fold_lines f (f accu line) ic
  | exception End_of_file -> accu

module M = Map.Make (String)

type attr_elems = { attr : string; elems : string list; type' : string }
type attr = { name : string; type' : string }

let escape_name name =
  match name with
  | "object" | "method" | "type" | "open" | "class" | "for" -> name ^ "'"
  | _ ->
    String.map
      (function
        | '-' -> '_'
        | x -> x)
      name

let parse_line line =
  match String.split_on_char ',' line with
  | [ attr; "GLOBAL"; type' ] -> `global { name = attr; type' }
  | [ attr; elems_str; type' ] ->
    let elems = String.split_on_char ';' elems_str in
    `nonglobal { attr; elems; type' }
  | _ -> invalid_arg ("invalid line: " ^ line)

let attr_to_arg attr =
  String.concat "" [ "?"; escape_name attr.name; ":"; attr.type' ]

let print_elem elem attrs =
  let elem' = escape_name elem in
  printf
    {|
val %s : (%s -> t) global
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/%s} %s}. *)
|}
    elem'
    (String.concat " -> " (List.map attr_to_arg attrs))
    elem elem

let print_global g =
  printf
    {|
type flag = bool
(** The type for boolean attributes. *)

type 'attrs global = %s -> 'attrs
(** The generic constructor function for elements with global attributes. *)

type t
(** The type for HTML elements or character data. *)
|}
    (String.concat " -> " (List.map attr_to_arg g))

let () =
  let g = ref [] in
  let m =
    fold_lines
      (fun acc line ->
        match parse_line line with
        | `global g_attr ->
          g := g_attr :: !g;
          acc
        | `nonglobal attr_elems ->
          let attr = { name = attr_elems.attr; type' = attr_elems.type' } in
          List.fold_left
            (fun acc elem ->
              M.update elem
                (function
                  | Some attrs -> Some (attr :: attrs)
                  | None -> Some [ attr ])
                acc)
            acc attr_elems.elems)
      M.empty stdin
  in
  print_global !g;
  M.iter print_elem m
