type out = Buffer.t -> unit
type html = Buffer.t -> unit

module Out = struct
  let[@inline] char char buf = Buffer.add_char buf char
  let[@inline] string string buf = Buffer.add_string buf string

  let[@inline] string_escaped x buf =
    let len = String.length x in
    let max_idx = len - 1 in
    let flush b start i =
      if start < len then Buffer.add_substring b x start (i - start)
    in
    let rec loop start i =
      if i > max_idx then flush buf start i
      else
        let next = i + 1 in
        match String.get x i with
        | '&' ->
          flush buf start i;
          string "&amp;" buf;
          loop next next
        | '<' ->
          flush buf start i;
          string "&lt;" buf;
          loop next next
        | '>' ->
          flush buf start i;
          string "&gt;" buf;
          loop next next
        | '\'' ->
          flush buf start i;
          string "&apos;" buf;
          loop next next
        | '\"' ->
          flush buf start i;
          string "&quot;" buf;
          loop next next
        | '@' ->
          flush buf start i;
          string "&commat;" buf;
          loop next next
        | _c -> loop start next
    in
    loop 0 0

  let[@inline] many outs : out = fun buf -> List.iter (fun out -> out buf) outs
end

let empty _ = ()

let[@inline] elem name (attrs : out list) (children : out list) : out =
 fun buf ->
  let attrs =
    match attrs with
    | [] -> empty
    | attrs ->
      fun buf ->
        Out.char ' ' buf;
        List.iter (fun attr -> attr buf) attrs
  in
  Out.char '<' buf;
  Out.string name buf;
  attrs buf;
  Out.char '>' buf;
  List.iter (fun child -> child buf) children;
  Out.string "</" buf;
  Out.string name buf;
  Out.char '>' buf

type flag = bool

type 'attrs global =
  ?class_flags:(string * bool) list ->
  ?class_list:string list ->
  ?class':string ->
  ?aria:(string * string) list ->
  ?attr:(string * string) list ->
  ?data:(string * string) list ->
  ?spellcheck:bool ->
  ?slot:string ->
  ?translate:[ `yes | `no ] ->
  ?title:string ->
  ?tabindex:int ->
  ?style:string ->
  ?role:string ->
  ?itemprop:string ->
  ?lang:string ->
  ?id:string ->
  ?hidden:flag ->
  ?dir:[ `ltr | `rtl | `auto ] ->
  ?contextmenu:string ->
  ?contenteditable:bool ->
  ?autocapitalize:[ `off | `none | `on | `sentences | `words | `characters ] ->
  ?accesskey:string ->
  ?draggable:flag ->
  'attrs

type t = html

module Attr = struct
  let[@inline] make ~name value : out =
   fun buf ->
    Out.string name buf;
    Out.string "=\"" buf;
    Out.string value buf;
    Out.char '\"' buf

  let multivalue ~name ~sep values : out =
   fun buf ->
    Out.string name buf;
    Out.string "=\"" buf;
    begin
      match values with
      | [] -> ()
      | [ v1 ] -> Out.string v1 buf
      | v1 :: vs ->
        Out.string v1 buf;
        List.iter
          (fun value ->
            Out.string sep buf;
            Out.string value buf)
          vs
    end;
    Out.char '\"' buf

  let list (attrs : out list) : out =
   fun buf ->
    match attrs with
    | [] -> ()
    | [ a1 ] -> a1 buf
    | a1 :: rest ->
      a1 buf;
      List.iter
        (fun attr ->
          Out.char ' ' buf;
          attr buf)
        rest

  let flag name = make ~name ""
  let bool ~name bool = make ~name (string_of_bool bool)
end

let[@inline] write_generic ?class_flags ?class_list ?class' ?aria ?attr ?data
    ?spellcheck ?slot ?translate ?title ?tabindex ?style ?role ?itemprop ?lang
    ?id ?hidden ?dir ?contextmenu ?contenteditable ?autocapitalize ?accesskey
    ?draggable =
  let class_flags =
    match class_flags with
    | Some class_flags ->
      let list =
        List.fold_left
          (fun acc (c, b) -> if b then c :: acc else acc)
          [] class_flags
      in
      Attr.multivalue ~name:"class" ~sep:" " list
    | None -> empty
  in
  let class_list =
    match class_list with
    | Some class_list -> Attr.multivalue ~name:"class" ~sep:" " class_list
    | None -> empty
  in
  let class' =
    match class' with
    | Some class' -> Attr.make ~name:"class" class'
    | None -> empty
  in
  let aria =
    match aria with
    | Some aria ->
      fun buf ->
        List.iter
          (fun (name, value) ->
            Out.char ' ' buf;
            (Attr.make ~name:("aria-" ^ name) value) buf)
          aria
    | None -> empty
  in
  let attr =
    match attr with
    | Some attr ->
      fun buf ->
        List.iter
          (fun (name, value) ->
            Out.char ' ' buf;
            (Attr.make ~name value) buf)
          attr
    | None -> empty
  in
  let data =
    match data with
    | Some data ->
      fun buf ->
        List.iter
          (fun (name, value) ->
            Out.char ' ' buf;
            (Attr.make ~name:("data-" ^ name) value) buf)
          data
    | None -> empty
  in
  let spellcheck =
    match spellcheck with
    | Some spellcheck -> Attr.bool ~name:"spellcheck" spellcheck
    | None -> empty
  in
  let slot =
    ignore slot;
    empty
  in
  let translate =
    ignore translate;
    empty
  in
  let title =
    ignore title;
    empty
  in
  let tabindex =
    ignore tabindex;
    empty
  in
  let style =
    ignore style;
    empty
  in
  let role =
    ignore role;
    empty
  in
  let itemprop =
    ignore itemprop;
    empty
  in
  let lang =
    ignore lang;
    empty
  in
  let id =
    ignore id;
    empty
  in
  let hidden =
    ignore hidden;
    empty
  in
  let dir =
    ignore dir;
    empty
  in
  let contextmenu =
    ignore contextmenu;
    empty
  in
  let contenteditable =
    ignore contenteditable;
    empty
  in
  let autocapitalize =
    ignore autocapitalize;
    empty
  in
  let accesskey =
    ignore accesskey;
    empty
  in
  let draggable =
    ignore draggable;
    empty
  in
  Out.many
    [ class_flags;
      class_list;
      class';
      aria;
      attr;
      data;
      spellcheck;
      slot;
      translate;
      title;
      tabindex;
      style;
      role;
      itemprop;
      lang;
      id;
      hidden;
      dir;
      contextmenu;
      contenteditable;
      autocapitalize;
      accesskey;
      draggable
    ]

let[@inline] a ?class_flags ?class_list ?class' ?aria ?attr ?data ?spellcheck
    ?slot ?translate ?title ?tabindex ?style ?role ?itemprop ?lang ?id ?hidden
    ?dir ?contextmenu ?contenteditable ?autocapitalize ?accesskey ?draggable
    ?target ?rel ?referrerpolicy ?ping ?media ?hreflang ?href ?download children
    =
  (*  *)
  let _target = target in
  let _rel = rel in
  let _referrerpolicy = referrerpolicy in
  let _ping = ping in
  let _media = media in
  let _hreflang = hreflang in
  let href =
    match href with
    | Some href -> Attr.make ~name:"href" href
    | None -> empty
  in
  let _download = download in

  let global =
    write_generic ?class_flags ?class_list ?class' ?aria ?attr ?data ?spellcheck
      ?slot ?translate ?title ?tabindex ?style ?role ?itemprop ?lang ?id ?hidden
      ?dir ?contextmenu ?contenteditable ?autocapitalize ?accesskey ?draggable
  in
  let attrs = global :: [ href ] in
  elem "a" attrs children

let to_string self =
  let buf = Buffer.create 65525 in
  self buf;
  Buffer.contents buf
