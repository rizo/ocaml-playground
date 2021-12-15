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

let copy_node = function
  | Branch arr -> Branch (Array.copy arr)
  | Leaf arr -> Leaf (Array.copy arr)


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

let leaf_get ~key leaf = Array.get leaf (leaf_index key)

let root_get ~key t = Array.get t.root (root_index ~shift:t.shift key)

let branch_get ~shift ~key branch = Array.get branch (branch_index ~shift key)

let branch_set ~shift ~key v branch =
  Array.set branch (branch_index ~shift key) v


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
        let node' = branch_get ~shift ~key (unwrap_branch node) in
        loop (shift - Config.bits_per_shift) node'
    in
    let node = root_get ~key t in
    loop (t.shift - Config.bits_per_shift) node


let idx key t =
  (* log "idx: key_dec=%d key_bin=%a t.shift=%d" key pp_bin key t.shift; *)
  let leaf = find_leaf_for_key key t in
  leaf_get ~key leaf


let set key value self =
  if key >= tail_offset self.len && key < self.len then
    (* Tail mutation *)
    { self with tail = leaf_update ~key value self.tail }
  else
    (* Root mutation *)
    let rec loop shift node_copy =
      if is_leaf_shift shift then unwrap_leaf node_copy
      else
        (* Find and copy the child for key updating the branch. *)
        let children_copy = unwrap_branch node_copy in
        let child_copy = copy_node (branch_get ~shift ~key children_copy) in
        branch_set ~shift ~key child_copy children_copy;
        loop (shift - Config.bits_per_shift) child_copy
    in
    let copy = { self with root = Array.copy self.root } in
    let leaf = loop self.shift (Branch copy.root) in
    Array.set leaf (leaf_index key) value;
    copy


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


let push x self =
  if self.len = 0 then (* Tree is empty. Return a singleton vec. *)
    singleton x
  else if self.len land (Config.branch_factor - 1) <> 0 then
    (* Tail update.
       Tail node has room for another elemenself.
       Duplicate the old tail and add a new elemenself.
       Return the updated vector with incremented len and a new tail. *)
    { self with len = self.len + 1; tail = array_copy_and_add self.tail x }
  else if self.len lsr Config.bits_per_shift > 1 lsl self.shift then
    (* Root overflow
       The current len requires another shifself.
       Replace the current root with a new one and add the tail to the tree. *)
    {
      len = self.len + 1;
      shift = self.shift + Config.bits_per_shift;
      tail = [| x |];
      root = [| Branch self.root; new_path self.shift self.tail |];
    }
  else
    (* Update the tree.
       Push the tail to the rooself. *)
    {
      len = self.len + 1;
      shift = self.shift;
      tail = [| x |];
      root = push_tail self.len self.shift self.root self.tail;
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


let of_list l = List.fold_left (fun self x -> push x self) empty l

let iota n =
  let rec loop i self = if i = n then self else loop (i + 1) (push i self) in
  loop 0 empty


let get i t = if i < 0 || i >= t.len then None else Some (idx i t)
