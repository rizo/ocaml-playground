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


module Config = struct
  let shift_bits = 2

  let node_size = ipow 2 shift_bits
end

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

let empty : 'a t =
  { root = [||]; len = 0; tail = [||]; shift = Config.shift_bits }


let singleton a =
  { root = [||]; len = 1; shift = Config.shift_bits; tail = [| a |] }


let len t = t.len

let get_link = function
  | Link a -> a
  | Data _ -> failwith "link request on data node"


let get_data = function
  | Data a -> a
  | Link _ -> failwith "data request on link node"


(* Translate a global index into an index for a link node, where [shift] is the
   current shift of the node. *)
let get_link_idx ~shift idx = (idx lsr shift) land (Config.node_size - 1)

(* Translate a global index into an index for a data node. *)
let get_data_idx idx = idx land (Config.node_size - 1)

(* Translate a global index into an index for the root node, where [shift] is
   the maximum shift of the tree. *)
let get_root_idx ~shift idx = idx lsr shift

(* The offest of the tail node in the tree. *)
let tail_offset len =
  if len < Config.node_size then 0
  else ((len - 1) lsr Config.shift_bits) lsl Config.shift_bits


(* Find the leaf data node for a given global index in the tree. *)
let data_node_for_index idx t =
  if idx >= tail_offset t.len then t.tail
  else
    let rec loop shift node =
      if shift = 0 then get_data node
      else
        let link_idx = get_link_idx ~shift idx in
        loop (shift - Config.shift_bits) (Array.get (get_link node) link_idx)
    in
    let root_idx = get_root_idx ~shift:t.shift idx in
    loop (t.shift - Config.shift_bits) (Array.get t.root root_idx)


let idx t idx =
  let data = data_node_for_index idx t in
  let data_idx = get_data_idx idx in
  Array.get data data_idx


let rec new_path level tail =
  if level = 0 then Data tail
  else Link [| new_path (level - Config.shift_bits) tail |]


(* Pushes the tail array to its correct location in the tree.
   If the parent is a leaf node, add the tail as a Data node.
   If index maps to the existing child, extend the child with a Link to tail.
   Otherwise add a new child to parent with a tail node. *)
let rec push_tail len level (parent : 'a node array) tail : 'a node array =
  let sub_idx = ((len - 1) lsr level) land (Config.node_size - 1) in
  (* Parent is a leaf node. *)
  if level = Config.shift_bits then
    let target = Data tail in
    let parent' = array_copy_and_add parent target in
    parent'
    (* Maps to existing child.
       Replace the child with a link to target. *)
  else if sub_idx < Array.length parent then (
    let child = Array.get parent sub_idx in
    let target =
      Link (push_tail len (level - Config.shift_bits) (get_link child) tail)
    in
    let parent' = Array.copy parent in
    Array.set parent' sub_idx target;
    parent'
    (* Does not map to existing child.
       Create a link and add path. *))
  else
    let target = new_path (level - Config.shift_bits) tail in
    let parent' = array_copy_and_add parent target in
    parent'


let add t x =
  if t.len = 0 then (* Tree is empty. Return a singleton vec. *)
    singleton x
  else if t.len land (Config.node_size - 1) <> 0 then
    (* Tail update.
       Tail node has room for another element.
       Duplicate the old tail and add a new element.
       Return the updated vector with incremented len and a new tail. *)
    { t with len = t.len + 1; tail = array_copy_and_add t.tail x }
  else if t.len lsr Config.shift_bits > 1 lsl t.shift then
    (* Root overflow
       The current len requires another shift.
       Replace the current root with a new one and add the tail to the tree. *)
    {
      len = t.len + 1;
      shift = t.shift + Config.shift_bits;
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


let of_array input =
  let len = Array.length input in
  if len = Config.node_size then
    { root = [||]; tail = input; len; shift = Config.shift_bits }
  else if len < Config.node_size then
    { root = [||]; tail = input; len; shift = Config.shift_bits }
  else
    let loop parent idx =
      (* XXX: check array bounds *)
      let data = Data (Array.sub input idx Config.node_size) in

      failwith ""
    in
    loop 0


let of_list l = List.fold_left (fun t x -> add t x) empty l

let iota n =
  let rec loop i acc = if i = n then acc else loop (i + 1) (add acc i) in
  loop 0 empty


let get indexable i =
  let len = len indexable in
  let i = if i < 0 then len + i else i in
  if i < 0 || i >= len then None else Some (idx indexable i)
