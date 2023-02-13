type flag = bool
(** The type for boolean attributes. *)

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
(** The generic constructor function for elements with global attributes. *)

type t
(** The type for HTML elements or character data. *)

val a :
  (?target:string ->
  ?rel:string ->
  ?referrerpolicy:string ->
  ?ping:string ->
  ?media:string ->
  ?hreflang:string ->
  ?href:string ->
  ?download:string ->
  t list ->
  t)
  global
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a} a}. *)

(* val applet :
     (?codebase:string -> ?code:string -> ?alt:string -> ?align:string -> t) global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/applet}
       applet}. *)

   val area :
     (?target:string ->
     ?shape:string ->
     ?rel:string ->
     ?referrerpolicy:string ->
     ?ping:string ->
     ?media:string ->
     ?hreflang:string ->
     ?href:string ->
     ?download:string ->
     ?coords:string ->
     ?alt:string ->
     t)
     global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/area}
       area}. *)

   val audio :
     (?src:string ->
     ?preload:flag ->
     ?muted:flag ->
     ?loop:flag ->
     ?crossorigin:[ `anonymous | `use_credentials ] ->
     ?controls:flag ->
     ?buffered:string ->
     ?autoplay:flag ->
     t)
     global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/audio}
       audio}. *)

   val base : (?target:string -> ?href:string -> t) global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/base}
       base}. *)

   val bgsound : (?loop:flag -> t) global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bgsound}
       bgsound}. *)

   val blockquote : (?cite:string -> t) global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/blockquote}
       blockquote}. *)

   val button :
     (?value:string ->
     ?type':string ->
     ?name:string ->
     ?formtarget:string ->
     ?formnovalidate:string ->
     ?formmethod:string ->
     ?formenctype:string ->
     ?formaction:string ->
     ?form:string ->
     ?disabled:flag ->
     ?autofocus:flag ->
     t)
     global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/button}
       button}. *)

   val canvas : (?width:int -> ?height:int -> t) global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/canvas}
       canvas}. *)

   val caption : (?align:string -> t) global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/caption}
       caption}. *)

   val col : (?span:int -> ?align:string -> t) global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/col} col}. *)

   val colgroup : (?span:int -> ?align:string -> t) global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/colgroup}
       colgroup}. *)

   val data : (?value:string -> t) global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/data}
       data}. *)

   val del : (?datetime:string -> ?cite:string -> t) global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/del} del}. *)

   val details : (?open':flag -> t) global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/details}
       details}. *)

   val dialog : (?open':flag -> t) global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dialog}
       dialog}. *)

   val embed :
     (?width:int -> ?type':string -> ?src:string -> ?height:int -> t) global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/embed}
       embed}. *)

   val fieldset : (?name:string -> ?form:string -> ?disabled:flag -> t) global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/fieldset}
       fieldset}. *)

   val form :
     (?target:string ->
     ?novalidate:flag ->
     ?name:string ->
     ?method':string ->
     ?enctype:string ->
     ?autocomplete:string ->
     ?action:string ->
     ?accept_charset:string ->
     ?accept:string ->
     t)
     global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/form}
       form}. *)

   val hr : (?align:string -> t) global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/hr} hr}. *)

   val iframe :
     (?width:int ->
     ?srcdoc:string ->
     ?src:string ->
     ?sandbox:string ->
     ?referrerpolicy:string ->
     ?name:string ->
     ?height:int ->
     ?allow:string ->
     ?align:string ->
     t)
     global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/iframe}
       iframe}. *)

   val img :
     (?width:int ->
     ?usemap:string ->
     ?srcset:string ->
     ?src:string ->
     ?sizes:string ->
     ?referrerpolicy:string ->
     ?ismap:flag ->
     ?height:int ->
     ?decoding:[ `sync | `async | `auto ] ->
     ?crossorigin:[ `anonymous | `use_credentials ] ->
     ?alt:string ->
     ?align:string ->
     t)
     global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/img} img}. *)

   val input :
     (?width:int ->
     ?value:string ->
     ?usemap:string ->
     ?type':string ->
     ?step:int ->
     ?src:string ->
     ?size:int ->
     ?required:flag ->
     ?readonly:flag ->
     ?placeholder:string ->
     ?pattern:string ->
     ?name:string ->
     ?multiple:flag ->
     ?min:int ->
     ?minlength:int ->
     ?maxlength:int ->
     ?max:int ->
     ?list:string ->
     ?height:int ->
     ?formtarget:string ->
     ?formnovalidate:string ->
     ?formmethod:string ->
     ?formenctype:string ->
     ?formaction:string ->
     ?form:string ->
     ?disabled:flag ->
     ?dirname:string ->
     ?checked:flag ->
     ?capture:[ `user | `environment ] ->
     ?autofocus:flag ->
     ?autocomplete:string ->
     ?alt:string ->
     ?accept:string ->
     t)
     global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input}
       input}. *)

   val ins : (?datetime:string -> ?cite:string -> t) global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ins} ins}. *)

   val keygen :
     (?name:string ->
     ?keytype:string ->
     ?form:string ->
     ?disabled:flag ->
     ?challenge:string ->
     ?autofocus:flag ->
     t)
     global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/keygen}
       keygen}. *)

   val label : (?form:string -> ?for':string -> t) global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/label}
       label}. *)

   val li : (?value:string -> t) global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li} li}. *)

   val link :
     (?type':string ->
     ?sizes:string ->
     ?rel:string ->
     ?referrerpolicy:string ->
     ?media:string ->
     ?integrity:string ->
     ?hreflang:string ->
     ?href:string ->
     ?crossorigin:[ `anonymous | `use_credentials ] ->
     t)
     global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/link}
       link}. *)

   val map : (?name:string -> t) global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/map} map}. *)

   val marquee : (?loop:flag -> t) global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/marquee}
       marquee}. *)

   val menu : (?type':string -> t) global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menu}
       menu}. *)

   val meta :
     (?name:string ->
     ?http_equiv:string ->
     ?content:string ->
     ?charset:string ->
     t)
     global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/meta}
       meta}. *)

   val meter :
     (?value:string ->
     ?optimum:int ->
     ?min:int ->
     ?max:int ->
     ?low:int ->
     ?high:int ->
     ?form:string ->
     t)
     global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/meter}
       meter}. *)

   val object' :
     (?width:int ->
     ?usemap:string ->
     ?type':string ->
     ?name:string ->
     ?height:int ->
     ?form:string ->
     ?data:string ->
     t)
     global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/object}
       object}. *)

   val ol : (?type':string -> ?start:int -> ?reversed:flag -> t) global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ol} ol}. *)

   val optgroup : (?label:string -> ?disabled:flag -> t) global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/optgroup}
       optgroup}. *)

   val option :
     (?value:string -> ?selected:flag -> ?label:string -> ?disabled:flag -> t)
     global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/option}
       option}. *)

   val output : (?name:string -> ?form:string -> ?for':string -> t) global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/output}
       output}. *)

   val param : (?value:string -> ?name:string -> t) global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/param}
       param}. *)

   val progress : (?value:string -> ?max:int -> ?form:string -> t) global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/progress}
       progress}. *)

   val q : (?cite:string -> t) global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/q} q}. *)

   val script :
     (?type':string ->
     ?src:string ->
     ?referrerpolicy:string ->
     ?integrity:string ->
     ?defer:flag ->
     ?crossorigin:[ `anonymous | `use_credentials ] ->
     ?charset:string ->
     ?async:flag ->
     t)
     global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/script}
       script}. *)

   val select :
     (?size:int ->
     ?required:flag ->
     ?name:string ->
     ?multiple:flag ->
     ?form:string ->
     ?disabled:flag ->
     ?autofocus:flag ->
     ?autocomplete:string ->
     t)
     global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/select}
       select}. *)

   val source :
     (?type':string ->
     ?srcset:string ->
     ?src:string ->
     ?sizes:string ->
     ?media:string ->
     t)
     global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/source}
       source}. *)

   val style : (?type':string -> ?media:string -> t) global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/style}
       style}. *)

   val table : (?align:string -> t) global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table}
       table}. *)

   val tbody : (?align:string -> t) global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tbody}
       tbody}. *)

   val td :
     (?rowspan:int -> ?headers:string -> ?colspan:int -> ?align:string -> t) global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/td} td}. *)

   val textarea :
     (?wrap:string ->
     ?rows:int ->
     ?required:flag ->
     ?readonly:flag ->
     ?placeholder:string ->
     ?name:string ->
     ?minlength:int ->
     ?maxlength:int ->
     ?inputmode:
       [ `none | `text | `decimal | `numeric | `tel | `search | `email | `url ] ->
     ?form:string ->
     ?disabled:flag ->
     ?dirname:string ->
     ?cols:int ->
     ?autofocus:flag ->
     ?autocomplete:string ->
     t)
     global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea}
       textarea}. *)

   val tfoot : (?align:string -> t) global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tfoot}
       tfoot}. *)

   val th :
     (?scope:[ `row | `col | `rowgroup | `colgroup ] ->
     ?rowspan:int ->
     ?headers:string ->
     ?colspan:int ->
     ?align:string ->
     t)
     global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/th} th}. *)

   val thead : (?align:string -> t) global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/thead}
       thead}. *)

   val time : (?datetime:string -> t) global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time}
       time}. *)

   val tr : (?align:string -> t) global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tr} tr}. *)

   val track :
     (?srclang:string ->
     ?src:string ->
     ?label:string ->
     ?kind:string ->
     ?default:string ->
     t)
     global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/track}
       track}. *)

   val video :
     (?width:int ->
     ?src:string ->
     ?preload:flag ->
     ?poster:string ->
     ?playsinline:flag ->
     ?muted:flag ->
     ?loop:flag ->
     ?height:int ->
     ?crossorigin:[ `anonymous | `use_credentials ] ->
     ?controls:flag ->
     ?buffered:string ->
     ?autoplay:flag ->
     t)
     global
   (** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/video}
       video}. *) *)

val to_string : t -> string
(** [to_string html] is the string representation of [html]. *)
