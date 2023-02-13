(* Buffer writers. *)
let add_char char buf = Buffer.add_char buf char
let add_string string buf = Buffer.add_string buf string

let add_string_escaped string buf =
  let len = String.length string in
  let max_idx = len - 1 in
  let flush b start i =
    if start < len then Buffer.add_substring b string start (i - start)
  in
  let rec loop start i =
    if i > max_idx then flush buf start i
    else
      let next = i + 1 in
      match String.get string i with
      | '&' ->
        flush buf start i;
        add_string "&amp;" buf;
        loop next next
      | '<' ->
        flush buf start i;
        add_string "&lt;" buf;
        loop next next
      | '>' ->
        flush buf start i;
        add_string "&gt;" buf;
        loop next next
      | '\'' ->
        flush buf start i;
        add_string "&apos;" buf;
        loop next next
      | '\"' ->
        flush buf start i;
        add_string "&quot;" buf;
        loop next next
      | '@' ->
        flush buf start i;
        add_string "&commat;" buf;
        loop next next
      | _c -> loop start next
  in
  loop 0 0

(* Public definitions. *)

type attr = Buffer.t -> unit
type html = Buffer.t -> unit

module Attr = struct
  type t = attr

  let make ~name value : attr =
   fun buf ->
    add_string name buf;
    add_string "=\"" buf;
    add_string value buf;
    add_char '\"' buf

  let of_list name ~sep values : attr =
   fun buf ->
    add_string name buf;
    add_string "=\"" buf;
    begin
      match values with
      | [] -> ()
      | [ v1 ] -> add_string v1 buf
      | v1 :: vs ->
        add_string v1 buf;
        List.iter
          (fun value ->
            add_string sep buf;
            add_string value buf)
          vs
    end;
    add_char '\"' buf

  let empty _ = ()
  let on bool attr = if bool then attr else empty

  let on_some = function
    | None -> empty
    | Some at -> at

  let on_ok = function
    | Error _ -> empty
    | Ok attr -> attr

  let bool name bool = if bool then make ~name "" else empty
  let int name i = make ~name (string_of_int i)
end

let attr name value = Attr.make ~name value

(* Standard constructors. *)
let accept value = attr "accept" value
let accesskey value = attr "accesskey" value
let action value = attr "action" value
let autocomplete value = attr "autocomplete" value
let autofocus = Attr.bool "autofocus" true
let charset value = attr "charset" value
let checked = Attr.bool "checked" true
let class' value = attr "class_name" value
let cols value = Attr.int "cols" value
let content value = attr "content" value
let contenteditable value = Attr.bool "contenteditable" value
let defer = Attr.bool "defer" true
let dir value = attr "dir" value
let disabled = Attr.bool "disabled" true
let draggable value = Attr.bool "draggable" value
let for' value = attr "for" value
let height value = Attr.int "height" value
let hidden = Attr.bool "hidden" true
let href value = attr "href" value
let id value = attr "id" value
let lang value = attr "lang" value
let list value = attr "list" value
let media value = attr "media" value
let method' value = attr "method" value
let name value = attr "name" value
let placeholder value = attr "placeholder" value
let rel value = attr "rel" value
let required = Attr.bool "required" true
let rows value = Attr.int "rows" value
let selected = Attr.bool "selected" true
let spellcheck value = attr "spellcheck" value
let src value = attr "src" value
let style value = Attr.of_list "style" ~sep:";" value
let tabindex value = Attr.int "tabindex" value
let title value = attr "title" value
let type' value = attr "type" value
let value value = attr "value" value
let wrap value = attr "wrap" value
let width value = Attr.int "width" value
let class_list xs = Attr.of_list "class" ~sep:" " xs

let class_flags options =
  let list = List.fold_left (fun acc (c, b) -> if b then c :: acc else acc) [] options in
  class_list list

let empty _ = ()

let elem name (attrs : attr list) (children : html list) : html =
 fun buf ->
  let attrs =
    match attrs with
    | [] -> empty
    | attrs ->
      fun buf ->
        add_char ' ' buf;
        List.iter (fun attr -> attr buf) attrs
  in
  add_char '<' buf;
  add_string name buf;
  attrs buf;
  add_char '>' buf;
  List.iter (fun child -> child buf) children;
  add_string "</" buf;
  add_string name buf;
  add_char '>' buf

let text string = add_string_escaped string
let int n = add_string (string_of_int n)

(* Standard element constructors. *)
let doctype buf = add_string "<!DOCTYPE html>\n" buf
let sp = add_string " "
let nbsp = add_string "\u{00A0}"
let a attrs children = elem "a" attrs children
let abbr attrs children = elem "abbr" attrs children
let address attrs children = elem "address" attrs children
let area attrs = elem "area" attrs []
let article attrs children = elem "article" attrs children
let aside attrs children = elem "aside" attrs children
let audio attrs children = elem "audio" attrs children
let b attrs children = elem "b" attrs children
let base attrs = elem "base" attrs []
let bdi attrs children = elem "bdi" attrs children
let bdo attrs children = elem "bdo" attrs children
let blockquote attrs children = elem "blockquote" attrs children
let br attrs = elem "br" attrs []
let button attrs children = elem "button" attrs children
let canvas attrs children = elem "canvas" attrs children
let caption attrs children = elem "caption" attrs children
let cite attrs children = elem "cite" attrs children
let code attrs children = elem "code" attrs children
let col attrs = elem "col" attrs []
let colgroup attrs children = elem "colgroup" attrs children
let command attrs children = elem "command" attrs children
let datalist attrs children = elem "datalist" attrs children
let dd attrs children = elem "dd" attrs children
let del attrs children = elem "del" attrs children
let details attrs children = elem "details" attrs children
let dfn attrs children = elem "dfn" attrs children
let div attrs children = elem "div" attrs children
let dl attrs children = elem "dl" attrs children
let dt attrs children = elem "dt" attrs children
let em attrs children = elem "em" attrs children
let embed attrs = elem "embed" attrs []
let fieldset attrs children = elem "fieldset" attrs children
let figcaption attrs children = elem "figcaption" attrs children
let figure attrs children = elem "figure" attrs children
let footer attrs children = elem "footer" attrs children
let form attrs children = elem "form" attrs children
let h1 attrs children = elem "h1" attrs children
let h2 attrs children = elem "h2" attrs children
let h3 attrs children = elem "h3" attrs children
let h4 attrs children = elem "h4" attrs children
let h5 attrs children = elem "h5" attrs children
let h6 attrs children = elem "h6" attrs children
let head attrs children = elem "head" attrs children
let header attrs children = elem "header" attrs children
let hgroup attrs children = elem "hgroup" attrs children
let hr attrs = elem "hr" attrs []
let html attrs children = elem "html" attrs children
let i attrs children = elem "i" attrs children
let iframe attrs children = elem "iframe" attrs children
let img attrs = elem "img" attrs []
let input attrs = elem "input" attrs []
let ins attrs children = elem "ins" attrs children
let kbd attrs children = elem "kbd" attrs children
let keygen attrs children = elem "keygen" attrs children
let label attrs children = elem "label" attrs children
let legend attrs children = elem "legend" attrs children
let li attrs children = elem "li" attrs children
let main attrs children = elem "main" attrs children
let map attrs children = elem "map" attrs children
let mark attrs children = elem "mark" attrs children
let menu attrs children = elem "menu" attrs children
let meta attrs = elem "meta" attrs []
let meter attrs children = elem "meter" attrs children
let nav attrs children = elem "nav" attrs children
let object' attrs children = elem "object" attrs children
let ol attrs children = elem "ol" attrs children
let optgroup attrs children = elem "optgroup" attrs children
let option attrs children = elem "option" attrs children
let output attrs children = elem "output" attrs children
let p attrs children = elem "p" attrs children
let param attrs = elem "param" attrs []
let pre attrs string = elem "pre" attrs [ text string ]
let progress attrs children = elem "progress" attrs children
let q attrs children = elem "q" attrs children
let rp attrs children = elem "rp" attrs children
let rt attrs children = elem "rt" attrs children
let ruby attrs children = elem "ruby" attrs children
let s attrs children = elem "s" attrs children
let samp attrs children = elem "samp" attrs children
let section attrs children = elem "section" attrs children
let select attrs children = elem "select" attrs children
let small attrs children = elem "small" attrs children
let source attrs = elem "source" attrs []
let span attrs children = elem "span" attrs children
let strong attrs children = elem "strong" attrs children
let sub attrs children = elem "sub" attrs children
let summary attrs children = elem "summary" attrs children
let sup attrs children = elem "sup" attrs children
let table attrs children = elem "table" attrs children
let tbody attrs children = elem "tbody" attrs children
let td attrs children = elem "td" attrs children
let textarea attrs children = elem "textarea" attrs children
let tfoot attrs children = elem "tfoot" attrs children
let th attrs children = elem "th" attrs children
let thead attrs children = elem "thead" attrs children
let time attrs children = elem "time" attrs children
let tr attrs children = elem "tr" attrs children
let track attrs = elem "track" attrs []
let u attrs children = elem "u" attrs children
let ul attrs children = elem "ul" attrs children
let var attrs children = elem "var" attrs children
let video attrs children = elem "video" attrs children
let wbr attrs = elem "wbr" attrs []

(* Extra constructors. *)
module Elem = struct
  type t = html

  let empty _ = ()

  let fmt format =
    let kmsg _ = text (Format.flush_str_formatter ()) in
    Format.kfprintf kmsg Format.str_formatter format

  let raw string = add_string string

  let of_some to_html option =
    match option with
    | Some x -> to_html x
    | None -> empty

  let of_ok to_html result =
    match result with
    | Ok x -> to_html x
    | Error _ -> empty
end

module Head = struct
  let title attrs children = elem "title" attrs children
  let style attrs children = elem "style" attrs children
  let body attrs children = elem "body" attrs children
  let link attrs = elem "link" attrs []
  let noscript attrs children = elem "noscript" attrs children
  let script attrs children = elem "script" attrs children
  let template attrs children = elem "template" attrs children
end

(* Output generation *)
let to_string self =
  let buf = Buffer.create 65525 in
  self buf;
  Buffer.contents buf

let write_to_channel chan self =
  let buf = Buffer.create 65525 in
  self buf;
  Buffer.output_buffer chan buf

let write_to_buffer buf self = self buf
