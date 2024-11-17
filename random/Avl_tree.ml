module Belt_id = struct
  type ('a, 'id) hash = 'a -> int
  type ('a, 'id) eq = 'a -> 'a -> bool
  type ('a, 'id) cmp = 'a -> 'a -> int

  module type Comparable = sig
    type identity
    type t
    val cmp : (t, identity) cmp
  end

  type ('key, 'id) comparable =
    (module Comparable with type t = 'key and type identity = 'id)

  module MakeComparableU (M : sig
    type t
    val cmp : t -> t -> int
  end) =
  struct
    type identity
    include M
  end

  module MakeComparable (M : sig
    type t
    val cmp : t -> t -> int
  end) =
  struct
    type identity
    type t = M.t

    (* see https://github.com/rescript-lang/rescript-compiler/pull/2589/files/5ef875b7665ee08cfdc59af368fc52bac1fe9130#r173330825 *)
    let cmp =
      let cmp = M.cmp in
      fun a b -> cmp a b
  end

  let comparableU (type key) ~cmp =
    (module MakeComparableU (struct
      type t = key
      let cmp = cmp
    end) : Comparable
      with type t = key)


  let comparable (type key) ~cmp =
    let module N = MakeComparable (struct
      type t = key
      let cmp = cmp
    end) in
    (module N : Comparable with type t = key)


  module type Hashable = sig
    type identity
    type t
    val hash : (t, identity) hash
    val eq : (t, identity) eq
  end

  type ('key, 'id) hashable =
    (module Hashable with type t = 'key and type identity = 'id)

  module MakeHashableU (M : sig
    type t
    val hash : t -> int
    val eq : t -> t -> bool
  end) =
  struct
    type identity
    include M
  end

  module MakeHashable (M : sig
    type t
    val hash : t -> int
    val eq : t -> t -> bool
  end) =
  struct
    type identity
    type t = M.t
    let hash =
      let hash = M.hash in
      fun a -> hash a


    let eq =
      let eq = M.eq in
      fun a b -> eq a b
  end

  let hashableU (type key) ~hash ~eq =
    (module MakeHashableU (struct
      type t = key
      let hash = hash
      let eq = eq
    end) : Hashable
      with type t = key)


  let hashable (type key) ~hash ~eq =
    let module N = MakeHashable (struct
      type t = key
      let hash = hash
      let eq = eq
    end) in
    (module N : Hashable with type t = key)


  module Int_comparator = MakeComparable (struct
    type t = int
    let cmp x y = Int.compare x y
  end)
  let cmp_int : (int, Int_comparator.identity) comparable =
    (module Int_comparator)


  module Int_comparator_inv = MakeComparable (struct
    type t = int
    let cmp x y = Int.compare y x
  end)
  let cmp_int_inv : (int, Int_comparator_inv.identity) comparable =
    (module Int_comparator_inv)
end

module Tree = struct
  type ('k, 'v) node = {
    mutable key : 'k;
    mutable value : 'v;
    mutable height : int;
    mutable left : ('k, 'v) t;
    mutable right : ('k, 'v) t;
  }

  and ('key, 'a) t = ('key, 'a) node option

  type ('k, 'id) cmp = ('k, 'id) Belt_id.cmp

  let treeHeight (n : _ t) =
    match n with
    | None -> 0
    | Some n -> n.height


  let rec copy n =
    match n with
    | None -> n
    | Some n -> Some { n with left = copy n.left; right = copy n.right }


  let create l x d r =
    let hl, hr = (treeHeight l, treeHeight r) in
    Some
      {
        left = l;
        key = x;
        value = d;
        right = r;
        height = (if hl >= hr then hl + 1 else hr + 1);
      }


  let singleton x d =
    Some { left = None; key = x; value = d; right = None; height = 1 }


  let heightGe l r =
    match (l, r) with
    | _, None -> true
    | Some hl, Some hr -> hl.height >= hr.height
    | None, Some _ -> false


  let _updateValue n newValue =
    if n.value == newValue then n
    else
      {
        left = n.left;
        right = n.right;
        key = n.key;
        value = newValue;
        height = n.height;
      }


  let bal l x d r =
    let hl =
      match l with
      | None -> 0
      | Some n -> n.height
    in
    let hr =
      match r with
      | None -> 0
      | Some n -> n.height
    in
    if hl > hr + 2 then
      match l with
      | None -> assert false
      | Some ({ left = ll; right = lr; _ } as l) -> (
        if treeHeight ll >= treeHeight lr then
          create ll l.key l.value (create lr x d r)
        else
          match lr with
          | None -> assert false
          | Some lr ->
            create
              (create ll l.key l.value lr.left)
              lr.key lr.value (create lr.right x d r))
    else if hr > hl + 2 then
      match r with
      | None -> assert false
      | Some ({ left = rl; right = rr; _ } as r) -> (
        if treeHeight rr >= treeHeight rl then
          create (create l x d rl) r.key r.value rr
        else
          match rl with
          | None -> assert false
          | Some rl ->
            create (create l x d rl.left) rl.key rl.value
              (create rl.right r.key r.value rr))
    else
      Some
        {
          left = l;
          key = x;
          value = d;
          right = r;
          height = (if hl >= hr then hl + 1 else hr + 1);
        }


  let rec minKey0Aux n =
    match n.left with
    | None -> n.key
    | Some n -> minKey0Aux n


  let minKey n =
    match n with
    | None -> None
    | Some n -> Some (minKey0Aux n)


  let rec maxKey0Aux n =
    match n.right with
    | None -> n.key
    | Some n -> maxKey0Aux n


  let maxKey n =
    match n with
    | None -> None
    | Some n -> Some (maxKey0Aux n)


  let rec minKV0Aux n =
    match n.left with
    | None -> (n.key, n.value)
    | Some n -> minKV0Aux n


  let minimum n =
    match n with
    | None -> None
    | Some n -> Some (minKV0Aux n)


  let rec maxKV0Aux n =
    match n.right with
    | None -> (n.key, n.value)
    | Some n -> maxKV0Aux n


  let maximum n =
    match n with
    | None -> None
    | Some n -> Some (maxKV0Aux n)


  let rec each n f =
    match n with
    | None -> ()
    | Some n ->
      each n.left f;
      f n.key n.value;
      each n.right f


  let rec map n f =
    match n with
    | None -> None
    | Some n ->
      let newLeft = map n.left f in
      let newD = f n.value in
      let newRight = map n.right f in
      Some
        {
          left = newLeft;
          key = n.key;
          value = newD;
          right = newRight;
          height = n.height;
        }


  let rec fold m accu f =
    match m with
    | None -> accu
    | Some n ->
      let { left = l; key = v; value = d; right = r; _ } = n in
      fold r (f (fold l accu f) v d) f


  let updateValue n newValue =
    if n.value == newValue then n
    else
      {
        left = n.left;
        right = n.right;
        key = n.key;
        value = newValue;
        height = n.height;
      }


  let rec set t newK newD ~cmp =
    match t with
    | None -> singleton newK newD
    | Some n ->
      let k = n.key in
      let c = cmp newK k in
      if c = 0 then Some (updateValue n newD)
      else
        let l, r, v = (n.left, n.right, n.value) in
        if c < 0 then bal (set ~cmp l newK newD) k v r
        else bal l k v (set ~cmp r newK newD)


  let rec addMinElement n k v =
    match n with
    | None -> singleton k v
    | Some n -> bal (addMinElement n.left k v) n.key n.value n.right


  let rec addMaxElement n k v =
    match n with
    | None -> singleton k v
    | Some n -> bal n.left n.key n.value (addMaxElement n.right k v)


  (* Same as create and bal, but no assumptions are made on the
     relative heights of l and r. *)

  let rec join ln v d rn =
    match (ln, rn) with
    | None, _ -> addMinElement rn v d
    | _, None -> addMaxElement ln v d
    | Some l, Some r ->
      let { left = ll; key = lv; value = ld; right = lr; height = lh } = l in
      let { left = rl; key = rv; value = rd; right = rr; height = rh } = r in
      if lh > rh + 2 then bal ll lv ld (join lr v d rn)
      else if rh > lh + 2 then bal (join ln v d rl) rv rd rr
      else create ln v d rn


  let rec len_node n =
    let { left = l; right = r; _ } = n in
    let sizeL =
      match l with
      | None -> 0
      | Some l -> len_node l
    in
    let sizeR =
      match r with
      | None -> 0
      | Some r -> len_node r
    in
    1 + sizeL + sizeR


  let len n =
    match n with
    | None -> 0
    | Some n -> len_node n


  let rec to_list_acc n accu =
    match n with
    | None -> accu
    | Some n ->
      let { left = l; right = r; key = k; value = v; _ } = n in
      to_list_acc l ((k, v) :: to_list_acc r accu)


  let to_list s = to_list_acc s []

  let rec get n x ~cmp =
    match n with
    | None -> None
    | Some n (* Node(l, v, d, r, _) *) ->
      let v = n.key in
      let c = cmp x v in
      if c = 0 then Some n.value
      else get ~cmp (if c < 0 then n.left else n.right) x


  let rec has n x ~cmp =
    match n with
    | None -> false
    | Some n (* Node(l, v, d, r, _) *) ->
      let v = n.key in
      let c = cmp x v in
      c = 0 || has ~cmp (if c < 0 then n.left else n.right) x


  let empty = None

  (* TODO: use kv ref *)
  let rec removeMinAuxWithRef n kr vr =
    match n.left with
    | None ->
      kr.contents <- n.key;
      vr.contents <- n.value;
      n.right
    | Some ln -> bal (removeMinAuxWithRef ln kr vr) n.key n.value n.right


  let concat t1 t2 =
    match (t1, t2) with
    | None, _ -> t2
    | _, None -> t1
    | _, Some t2n ->
      let kr, vr = (ref t2n.key, ref t2n.value) in
      let t2r = removeMinAuxWithRef t2n kr vr in
      join t1 kr.contents vr.contents t2r


  let rec keepMap n p =
    match n with
    | None -> None
    | Some n -> (
      (* call `p` in the expected left-to-right order *)
      let { key = v; value = d; _ } = n in
      let newLeft = keepMap n.left p in
      let pvd = p v d in
      let newRight = keepMap n.right p in
      match pvd with
      | None -> concat newLeft newRight
      | Some d -> join newLeft v d newRight)


  let concatOrJoin t1 v d t2 =
    match d with
    | Some d -> join t1 v d t2
    | None -> concat t1 t2


  let rec splitAuxPivot n x pres ~cmp =
    let { left = l; key = v; value = d; right = r; _ } = n in
    let c = (cmp x v [@bs]) in
    if c = 0 then begin
      pres.contents <- Some d;
      (l, r)
    end
    else if c < 0 then
      match l with
      | None -> (None, Some n)
      | Some l ->
        let ll, rl = splitAuxPivot ~cmp l x pres in
        (ll, join rl v d r)
    else
      match r with
      | None -> (Some n, None)
      | Some r ->
        let lr, rr = splitAuxPivot ~cmp r x pres in
        (join l v d lr, rr)


  let rec merge s1 s2 f ~cmp =
    match (s1, s2) with
    | None, None -> None
    | Some _, None -> keepMap s1 (fun [@bs] k v -> (f k (Some v) None [@bs]))
    | None, Some _ -> keepMap s2 (fun [@bs] k v -> (f k None (Some v) [@bs]))
    | Some s1n, Some s2n ->
      if s1n.height >= s2n.height then
        let { left = l1; key = v1; value = d1; right = r1; _ } = s1n in
        let d2 = ref None in
        let l2, r2 = splitAuxPivot ~cmp s2n v1 d2 in
        let d2 = d2.contents in
        let newLeft = merge ~cmp l1 l2 f in
        let newD = (f v1 (Some d1) d2 [@bs]) in
        let newRight = merge ~cmp r1 r2 f in
        concatOrJoin newLeft v1 newD newRight
      else
        let { left = l2; key = v2; value = d2; right = r2; _ } = s2n in
        let d1 = ref None in
        let l1, r1 = splitAuxPivot ~cmp s1n v2 d1 in
        let d1 = d1.contents in
        let newLeft = merge ~cmp l1 l2 f in
        let newD = (f v2 d1 (Some d2) [@bs]) in
        let newRight = merge ~cmp r1 r2 f in
        concatOrJoin newLeft v2 newD newRight
end

module Map = struct
  type ('key, 'id) id = ('key, 'id) Belt_id.comparable

  type ('k, 'v, 'id) t = { cmp : ('k, 'id) Belt_id.cmp; data : ('k, 'v) Tree.t }

  let mk (type key idx) ~(id : (key, idx) id) =
    let module M = (val id) in
    { cmp = M.cmp; data = Tree.empty }


  let set key d t = { cmp = t.cmp; data = Tree.set ~cmp:t.cmp t.data key d }

  let get x t = Tree.get ~cmp:t.cmp t.data x

  let map f t = { cmp = t.cmp; data = Tree.map t.data f }

  let concat : ('a, 'b, 'id) t -> ('a, 'b, 'id) t -> ('a, 'b, 'id) t =
   fun t0 t1 -> { cmp = t0.cmp; data = Tree.concat t0.data t1.data }


  let merge t0 t1 f =
    { cmp = t0.cmp; data = Tree.merge t0.data t1.data f ~cmp:t0.cmp }
end