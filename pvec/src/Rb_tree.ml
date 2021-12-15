(* Persistent bit-partitioned Vector Trie *)

let log fmt = Format.kasprintf print_endline fmt

let pp_bin formatter x =
  let rec loop i y acc =
    if y <> 0 then loop (i + 1) (y / 2) ((y mod 2) :: acc) else acc
  in
  let out = loop 0 x [] in
  List.iter (fun bit -> Format.pp_print_int formatter bit) out


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
  let bits_per_shift = 2
  let branch_factor = 4
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
  | Branch of 'a node array
  (* Leaf node holds the actual data values. *)
  | Leaf of 'a array

let empty : 'a t =
  { root = [||]; len = 0; tail = [||]; shift = Config.bits_per_shift }


let singleton a =
  { root = [||]; len = 1; shift = Config.bits_per_shift; tail = [| a |] }


let len t = t.len

let unwrap_branch = function
  | Branch a -> a
  | _ -> failwith "node is not a branch"


let unwrap_leaf = function
  | Leaf a -> a
  | _ -> failwith "node is not a leaf"


let is_level_with_leaves shift = shift = Config.bits_per_shift

let is_leaf_shift shift = shift = 0

let leaf_index key = key land (Config.branch_factor - 1)

let root_index ~shift key = key lsr shift

let branch_index ~shift key = (key lsr shift) land (Config.branch_factor - 1)

let leaf_lookup ~key leaf = Array.get leaf (leaf_index key)

let root_lookup ~key t = Array.get t.root (root_index ~shift:t.shift key)

let branch_lookup ~shift ~key branch =
  Array.get branch (branch_index ~shift key)


let leaf_update ~key v leaf =
  let copy = Array.copy leaf in
  Array.set copy (leaf_index key) v;
  copy


(* The offest of the tail node in the tree. *)
let tail_offset len =
  if len < Config.branch_factor then 0
  else ((len - 1) lsr Config.bits_per_shift) lsl Config.bits_per_shift


(* Find the leaf data node for a given global index in the tree. *)
let find_leaf_for_key key t =
  if key >= tail_offset t.len && key < t.len then t.tail
  else
    let rec loop shift node =
      if is_leaf_shift shift then unwrap_leaf node
      else
        let node' = branch_lookup ~shift ~key (unwrap_branch node) in
        loop (shift - Config.bits_per_shift) node'
    in
    let node = root_lookup ~key t in
    loop (t.shift - Config.bits_per_shift) node


let idx key t =
  (* log "idx: key_dec=%d key_bin=%a t.shift=%d" key pp_bin key t.shift; *)
  let leaf = find_leaf_for_key key t in
  leaf_lookup ~key leaf


let set key v t =
  if key >= tail_offset t.len && key < t.len then
    (* The element is in the tail *)
    { t with tail = leaf_update ~key v t.tail }
  else
    (* The element is in the root. *)
    let rec loop shift node =
      if is_leaf_shift shift then unwrap_leaf node
      else
        let node' = branch_lookup ~shift ~key (unwrap_branch node) in
        let node'_copy = Array.copy node' in
        loop (shift - Config.bits_per_shift) node'
    in
    let t_copy = { t with root = Array.copy t.root } in
    let node = root_lookup ~key t in
    let leaf = loop (t.shift - Config.bits_per_shift) node in
    Array.set leaf (leaf_index key) v;
    t_copy


let rec new_path level tail =
  if level = 0 then Leaf tail
  else Branch [| new_path (level - Config.bits_per_shift) tail |]


(* Pushes the tail array to its correct location in the tree.
   If the parent is a leaf node, add the tail as a Data node.
   If index maps to the existing child, extend the child with a Link to tail.
   Otherwise add a new child to parent with a tail node. *)
let rec push_tail len level (parent : 'a node array) tail : 'a node array =
  let sub_idx = ((len - 1) lsr level) land (Config.branch_factor - 1) in
  (* Parent is a leaf node. *)
  if level = Config.bits_per_shift then
    let target = Leaf tail in
    let parent' = array_copy_and_add parent target in
    parent'
    (* Maps to existing child.
       Replace the child with a link to target. *)
  else if sub_idx < Array.length parent then (
    let child = Array.get parent sub_idx in
    let target =
      Branch
        (push_tail len
           (level - Config.bits_per_shift)
           (unwrap_branch child) tail)
    in
    let parent' = Array.copy parent in
    Array.set parent' sub_idx target;
    parent'
    (* Does not map to existing child.
       Create a link and add path. *))
  else
    let target = new_path (level - Config.bits_per_shift) tail in
    let parent' = array_copy_and_add parent target in
    parent'


let push t x =
  if t.len = 0 then (* Tree is empty. Return a singleton vec. *)
    singleton x
  else if t.len land (Config.branch_factor - 1) <> 0 then
    (* Tail update.
       Tail node has room for another element.
       Duplicate the old tail and add a new element.
       Return the updated vector with incremented len and a new tail. *)
    { t with len = t.len + 1; tail = array_copy_and_add t.tail x }
  else if t.len lsr Config.bits_per_shift > 1 lsl t.shift then
    (* Root overflow
       The current len requires another shift.
       Replace the current root with a new one and add the tail to the tree. *)
    {
      len = t.len + 1;
      shift = t.shift + Config.bits_per_shift;
      tail = [| x |];
      root = [| Branch t.root; new_path t.shift t.tail |];
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
  if len <= Config.branch_factor then
    { root = [||]; tail = input; len; shift = Config.bits_per_shift }
  else
    let loop idx parent =
      if idx >= len then (* Exhausted input, return parent. *)
        parent
      else
        (* Reached leaf, fill and append to parent. *)
        let data = Leaf (Array.sub input idx Config.branch_factor) in
        let _parent' = array_copy_and_add parent data in
        assert false
    in
    let root = loop 0 [||] in
    { root; tail = input; len; shift = Config.bits_per_shift }


let of_list l = List.fold_left (fun t x -> push t x) empty l

let iota n =
  let rec loop i acc = if i = n then acc else loop (i + 1) (push acc i) in
  loop 0 empty


let get i t = if i < 0 || i >= t.len then None else Some (idx i t)
