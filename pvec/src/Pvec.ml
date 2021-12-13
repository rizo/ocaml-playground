(* Persistent bit-partitioned Vector Trie *)

let array_copy_and_add a x =
  let a_len = Array.length a in
  let new_a = Array.make (a_len + 1) x in
  Array.blit a 0 new_a 0 a_len;
  new_a


type node

type 'a t = {
  root : node array;
  (* Root node links. *)
  tail : 'a array;
  (* A pointer to the tail of the vector. *)
  len : int;
  (* The len of the vector. *)
  shift : int; (* Current maximum index shift. *)
}

let shift_by = 5
let trie_len = int_of_float (2.0 ** float_of_int shift_by)

let make_link = Obj.magic
let make_data = Obj.magic

let as_link = Obj.magic

let as_data = Obj.magic

(* O(1) *)
let empty : 'a t = { root = [||]; len = 0; tail = [||]; shift = shift_by }

(* O(1) *)
let singleton a = { root = [||]; len = 1; shift = shift_by; tail = [| a |] }

(* O(1) *)
let len v = v.len

let tail_offset len =
  if len < 32 then 0 else ((len - 1) lsr shift_by) lsl shift_by


let node_with_index v idx =
  if idx >= tail_offset v.len then make_data v.tail
  else
    let rec loop level node =
      if level = 0 then node
      else
        let sub_idx = (idx lsr level) land (trie_len - 1) in
        loop (level - shift_by) (Array.get (as_link node) sub_idx)
    in
    loop (v.shift - shift_by) (Array.get v.root (idx lsr v.shift))


(* O(log32(n)) ~ O(1) *)
let idx v idx =
  let node = node_with_index v idx in
  Array.get (as_data node) (idx land (trie_len - 1))


(* O(log32(n)) ~ O(1) *)
let rec new_path level tail =
  if level = 0 then make_data tail
  else make_link [| new_path (level - shift_by) tail |]


(* Pushes the tail array to its correct location in the tree.
   If the parent is a leaf node, add the tail as a Data node.
   If index maps to the existing child, extend the child with a Link to tail.
   Otherwise add a new child to parent with a tail node. *)
let rec push_tail len level parent tail =
  let sub_idx = ((len - 1) lsr level) land (trie_len - 1) in
  (* Parent is a leaf node. *)
  if level = shift_by then
    let target = make_data tail in
    let parent' = array_copy_and_add parent target in
    parent'
    (* Maps to existing child.
       Replace the child with a link to target. *)
  else if sub_idx < Array.length parent then (
    let child = Array.get parent sub_idx in
    let target =
      make_link (push_tail len (level - shift_by) (as_link child) tail)
    in
    let parent' = Array.copy parent in
    Array.set parent' sub_idx target;
    parent'
    (* Does not map to existing child.
       Create a link and add path. *))
  else
    let target = new_path (level - shift_by) tail in
    let parent' = array_copy_and_add parent target in
    parent'


(* O(log32(n)) ~ O(1) *)
let add v x =
  if v.len = 0 then singleton x
  else if
    (* Tail update.
       Tail node has room for another element.
       Duplicate the old tail and add a new element.
       Return the updated vector with incremented len and a new tail. *)
    v.len land (trie_len - 1) <> 0
  then { v with len = v.len + 1; tail = array_copy_and_add v.tail x }
    (* Root overflow
       The current len requires another shift.
       Replace the current root with a new one and add the tail to the tree. *)
  else if v.len lsr shift_by > 1 lsl v.shift then
    {
      len = v.len + 1;
      shift = v.shift + shift_by;
      tail = [| x |];
      root = [| make_link v.root; new_path v.shift v.tail |];
    }
    (* Update the tree.
       Push the tail to the root. *)
  else
    {
      len = v.len + 1;
      shift = v.shift;
      tail = [| x |];
      root = push_tail v.len v.shift v.root v.tail;
    }


let of_list l = List.fold_left (fun v x -> add v x) empty l

let get indexable i =
  let len = len indexable in
  let i = if i < 0 then len + i else i in
  if i < 0 || i >= len then None else Some (idx indexable i)


let get_exn indexable i =
  let len = len indexable in
  let i = if i < 0 then len + i else i in
  if i < 0 || i >= len then invalid_arg "get_exn" else Some (idx indexable i)
