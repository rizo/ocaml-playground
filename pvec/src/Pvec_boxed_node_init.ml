(* Persistent bit-partitioned Vector Trie *)

let ipow x y =
  let rec loop r x y =
    match y with
    | 0 -> r
    | _ -> loop (r * x) x (y - 1)
  in
  loop 1 x y


let array_copy_and_add array x =
  let len = Array.length array in
  let out = Array.make (len + 1) x in
  Array.blit array 0 out 0 len;
  out


type 'a t = {
  (* Root node links. *)
  root : 'a node array;
  (* A pointer to the tail of the vector. *)
  tail : 'a array;
  (* The len of the vector. *)
  len : int;
  (* Current maximum index shift. *)
  shift : int;
}

and 'a node =
  (* Branch node with links to other nodes. *)
  | Link of 'a node array
  (* Leaf node holds the actual data values. *)
  | Data of 'a array

let shift_by = 2

let node_size = ipow 2 shift_by

let empty : 'a t = { root = [||]; len = 0; tail = [||]; shift = shift_by }

let singleton a = { root = [||]; len = 1; shift = shift_by; tail = [| a |] }

let len t = t.len

let link = function
  | Link a -> a
  | Data _ -> failwith "link request on data node"


let data = function
  | Data a -> a
  | Link _ -> failwith "data request on link node"


let of_array array =
  let len = Array.length array in
  if len = node_size then { root = [||]; tail = array; len; shift = shift_by }
  else if len < node_size then
    { root = [||]; tail = array; len; shift = shift_by }
  else
    let rec loop idx = failwith "" in
    loop 0


(* mask(t::BitmappedTrie, i::Int) = (((i - 1) >>> shift(t)) & (trielen - 1)) + 1 *)

(* Apply a mask to translate a global index into an index in a tree sub-node. *)
let sub_idx_mask ~level idx = (idx lsr level) land (node_size - 1)

let tail_offset len =
  if len < node_size then 0 else ((len - 1) lsr shift_by) lsl shift_by


let node_with_index idx t =
  if idx >= tail_offset t.len then Data t.tail
  else
    let rec loop level node =
      if level = 0 then node
      else
        let sub_idx = sub_idx_mask ~level idx in
        loop (level - shift_by) (Array.get (link node) sub_idx)
    in
    loop (t.shift - shift_by) (Array.get t.root (idx lsr t.shift))


let idx t idx =
  let node = node_with_index idx t in
  Array.get (data node) (idx land (node_size - 1))


let rec new_path level tail =
  if level = 0 then Data tail else Link [| new_path (level - shift_by) tail |]


(* Pushes the tail array to its correct location in the tree.
   If the parent is a leaf node, add the tail as a Data node.
   If index maps to the existing child, extend the child with a Link to tail.
   Otherwise add a new child to parent with a tail node. *)
let rec push_tail len level (parent : 'a node array) tail : 'a node array =
  let sub_idx = ((len - 1) lsr level) land (node_size - 1) in
  (* Parent is a leaf node. *)
  if level = shift_by then
    let target = Data tail in
    let parent' = array_copy_and_add parent target in
    parent'
    (* Maps to existing child.
       Replace the child with a link to target. *)
  else if sub_idx < Array.length parent then (
    let child = Array.get parent sub_idx in
    let target = Link (push_tail len (level - shift_by) (link child) tail) in
    let parent' = Array.copy parent in
    Array.set parent' sub_idx target;
    parent'
    (* Does not map to existing child.
       Create a link and add path. *))
  else
    let target = new_path (level - shift_by) tail in
    let parent' = array_copy_and_add parent target in
    parent'


let add t x =
  if t.len = 0 then (* Tree is empty. Return a singleton vec. *)
    singleton x
  else if t.len land (node_size - 1) <> 0 then
    (* Tail update.
       Tail node has room for another element.
       Duplicate the old tail and add a new element.
       Return the updated vector with incremented len and a new tail. *)
    { t with len = t.len + 1; tail = array_copy_and_add t.tail x }
  else if t.len lsr shift_by > 1 lsl t.shift then
    (* Root overflow
       The current len requires another shift.
       Replace the current root with a new one and add the tail to the tree. *)
    {
      len = t.len + 1;
      shift = t.shift + shift_by;
      tail = [| x |];
      root = [| Link t.root; new_path t.shift t.tail |];
    }
  else
    (* Update the tree.
       Push the tail to the root. *)
    {
      len = t.len + 1;
      shift = t.shift;
      tail = [| x |];
      root = push_tail t.len t.shift t.root t.tail;
    }


let of_list l = List.fold_left (fun t x -> add t x) empty l

let iota n =
  let rec loop i acc = if i = n then acc else loop (i + 1) (add acc i) in
  loop 0 empty


let get indexable i =
  let len = len indexable in
  let i = if i < 0 then len + i else i in
  if i < 0 || i >= len then None else Some (idx indexable i)
