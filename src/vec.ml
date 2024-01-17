type ('a, -'p) t =
  { mutable data: 'a array
  ; mutable length: int
  }

let growth_rate = 1.5

let[@inline] array_uninit n = Array.make n (Obj.magic 0)

let[@inline] make_unsafe capacity length = { data = array_uninit capacity; length }

let[@inline] make ?capacity:(c=0) () =
  if c < 0 then
    raise (Invalid_argument "capacity < 0")
  else
    make_unsafe c 0

let as_read_only v = (v :> ('a, [`R]) t)

let as_write_only v = (v :> ('a, [`W]) t)

let[@inline] length v = v.length
let[@inline] capacity v = Array.length v.data

let[@inline] clear v =
  v.length <- 0;
  v.data <- [||]

let[@inline] get v i =
  if i < 0 || i >= v.length then
    raise (Invalid_argument "Index out of range")
  else
    v.data.(i)

let[@inline] set v i a =
  if i < 0 || i >= v.length then
    raise (Invalid_argument "Index out of range")
  else
    v.data.(i) <- a

let try_get v i =
  if i < 0 || i >= v.length then
    None
  else
    Some v.data.(i)

let[@inline] try_set v i a = i >= 0 && i < v.length && (v.data.(i) <- a; true)

let reserve c v =
  let cap = capacity v in
  if c > cap then
    let cap = ref (if cap = 0 then growth_rate else float_of_int cap) in
    let c = float_of_int c in

    while !cap < c do
      cap := !cap *. growth_rate
    done;

    let data = array_uninit (int_of_float !cap) in
    Array.blit v.data 0 data 0 v.length;
    v.data <- data

let[@inline] shrink_to_fit v =
  if capacity v > v.length then
    let data = array_uninit v.length in
    Array.blit v.data 0 data 0 v.length;
    v.data <- data

let[@inline] push a v =
  let old_length = v.length in
  let new_length = old_length + 1 in
  reserve new_length v;
  v.length <- new_length;
  v.data.(old_length) <- a

let[@inline] pop v =
  if v.length = 0 then
    None
  else
    let last = v.length - 1 in
    let a = v.data.(last) in
    v.data.(last) <- Obj.magic 0;
    v.length <- last;
    Some a

let[@inline] singleton a =
  { data = [|a|]
  ; length = 1
  }

let try_find f v =
  let rec go i =
    if i = v.length then
      None
    else
      let e = v.data.(i) in
      if f e then
        Some e
      else
        go (i + 1)
  in
  go 0

let[@inline] find f v =
  match try_find f v with
  | None -> raise Not_found
  | Some a -> a

let try_insert_at i a v =
  if i < 0 || i > v.length then
    false
  else
    let new_length = v.length + 1 in
    reserve new_length v;

    Array.blit v.data i v.data (i + 1) (v.length - i);
    v.data.(i) <- a;
    v.length <- new_length;

    true

let[@inline] insert_at i a v =
  if not (try_insert_at i a v) then
    raise (Invalid_argument "Index out of range")

let try_remove_at i v =
  if i < 0 || i >= v.length then
    None
  else
    let a = v.data.(i) in

    Array.blit v.data (i + 1) v.data i (v.length - i - 1);
    v.length <- v.length - 1;
    v.data.(v.length) <- Obj.magic 0;

    Some a

let[@inline] remove_at i v =
  match try_remove_at i v with
  | None -> raise (Invalid_argument "Index out of range")
  | Some a -> a

let map f v =
  let v2 = make_unsafe v.length v.length in

  for i = 0 to v.length - 1 do
    v2.data.(i) <- f v.data.(i)
  done;

  v2

let mapi f v =
  let v2 = make_unsafe v.length v.length in

  for i = 0 to v.length - 1 do
    v2.data.(i) <- f i v.data.(i)
  done;

  v2

let map_in_place f v =
  for i = 0 to v.length - 1 do
    v.data.(i) <- f v.data.(i)
  done

let map2 f v1 v2 =
  let total_length = v1.length * v2.length in
  let v = make_unsafe total_length total_length in
  let idx = ref 0 in

  for i = 0 to v1.length - 1 do
    for j = 0 to v2.length - 1 do
      v.data.(!idx) <- f v1.data.(i) v2.data.(j);
      incr idx
    done
  done;

  v

let[@inline] apply f v = map2 (@@) f v

let flatten vs =
  let total_length = ref 0 in

  for i = 0 to vs.length - 1 do
    total_length := !total_length + vs.data.(i).length
  done;

  let vec = make_unsafe !total_length !total_length in
  let idx = ref 0 in

  for i = 0 to vs.length - 1 do
    let v = vs.data.(i) in

    Array.blit v.data 0 vec.data !idx v.length;
    idx := !idx + v.length
  done;

  vec

let[@inline] append_in_place v v2 =
  let total_length = v.length + v2.length in
  reserve total_length v;

  Array.blit v2.data 0 v.data v.length v2.length;
  v.length <- total_length

let flat_map f v =
  let v2 = make_unsafe v.length 0 in

  for i = 0 to v.length - 1 do
    append_in_place v2 (f v.data.(i))
  done;

  v2

let[@inline] cartesian_product a b = map2 (fun a b -> a, b) a b

let iter f v =
  for i = 0 to v.length - 1 do
    f v.data.(i)
  done

let iteri f v =
  for i = 0 to v.length - 1 do
    f i v.data.(i)
  done

let filter f v =
  let v2 = make_unsafe v.length 0 in
  let l = ref 0 in

  for i = 0 to v.length - 1 do
    let a = v.data.(i) in
    if f a then
      (v2.data.(!l) <- a; incr l)
  done;

  v2.length <- !l;
  v2

let filteri f v =
  let v2 = make_unsafe v.length 0 in
  let l = ref 0 in

  for i = 0 to v.length - 1 do
    let a = v.data.(i) in
    if f i a then
      (v2.data.(!l) <- a; incr l)
  done;

  v2.length <- !l;
  v2

let filter_in_place f v =
  let old_l = v.length in
  let l = ref 0 in

  for i = 0 to old_l - 1 do
    let e = v.data.(i) in
    if f e then
      (v.data.(!l) <- e; incr l)
  done;

  for i = !l to old_l - 1 do
    v.data.(i) <- Obj.magic 0
  done;

  v.length <- !l

let[@inline] of_array_unsafe a =
  { data = a
  ; length = Array.length a
  }

let[@inline] to_array_unsafe v = v.data

let[@inline] of_array a = of_array_unsafe (Array.copy a)
let[@inline] to_array v = Array.sub v.data 0 v.length

let[@inline] of_list l = of_array_unsafe (Array.of_list l)

let to_list v =
  let rec go acc = function
    | -1 -> acc
    | i -> go (v.data.(i) :: acc) (i - 1)
  in
  go [] (v.length - 1)

let[@inline] copy v = of_array_unsafe (to_array v)

let[@inline] append v v2 =
  let v' = copy v in
  append_in_place v' v2;
  v'

let rev_in_place v =
  let rec go i j =
    if i < j then
      let tmp = v.data.(i) in
      v.data.(i) <- v.data.(j);
      v.data.(j) <- tmp;
      go (i + 1) (j - 1)
  in
  go 0 (v.length - 1)

let[@inline] rev v =
  let v' = copy v in
  rev_in_place v';
  v'

let[@inline] exists f v =
  let rec go i = i < v.length && (f v.data.(i) || go (i + 1))
  in go 0

let[@inline] for_all f v =
  let rec go i = i = v.length || (f v.data.(i) && go (i + 1))
  in go 0

let[@inline] mem e = exists ((=) e)
let[@inline] memq e = exists ((==) e)

let fold_left f z v =
  let rec go acc i =
    if i = v.length then
      acc
    else
      go (f acc v.data.(i)) (i + 1)
  in
  go z 0

let fold_right f v z =
  let rec go acc i =
    if i <= 0 then
      acc
    else
      go (f v.data.(i) acc) (i - 1)
  in
  go z (v.length - 1)

let zip_with f v1 v2 =
  let min_length = min v1.length v2.length in
  let v = make_unsafe min_length min_length in

  for i = 0 to min_length - 1 do
    v.data.(i) <- f v1.data.(i) v2.data.(i)
  done;

  v

let[@inline] zip v1 v2 = zip_with (fun a b -> (a, b)) v1 v2

let[@inline] sort_by f v =
  shrink_to_fit v;
  Array.fast_sort f v.data

let[@inline] sort v = sort_by compare v

let[@inline] equal_by f a b =
  if a.length <> b.length then
    false
  else
    let rec go i = i = a.length || (f a.data.(i) b.data.(i) && go (i + 1))
    in go 0

let[@inline] equal a b = equal_by (=) a b

let compare_by f a b =
  let min_l, min_l_ord =
    match a.length - b.length with
    | 0 -> a.length, 0
    | l when l < 0 -> a.length, -1
    | _ -> b.length, 1
  in
  let rec go i =
    if i = min_l then
      min_l_ord
    else
      let ord = f a.data.(i) b.data.(i) in
      if ord <> 0 then
        ord
      else
        go (i + 1)
  in
  go 0

let[@inline] compare a b = compare_by compare a b

let pretty_print fmt v =
  if v.length = 0 then
    "[]"
  else
    let buf = Buffer.create 2 in

    Buffer.add_char buf '[';
    Buffer.add_string buf (fmt v.data.(0));

    for i = 1 to v.length - 1 do
      Buffer.add_string buf "; ";
      Buffer.add_string buf (fmt v.data.(i))
    done;

    Buffer.add_char buf ']';
    Buffer.contents buf

let[@inline] range start end' =
  let l = (abs (end' - start) + 1) in
  let d = if start <= end' then 1 else -1 in
  of_array_unsafe (Array.init l (fun i -> start + i * d))

module Infix = struct
  let (.![]) = get
  let (.![]<-) = set

  let (.?[]) = try_get
  let (.?[]<-) = try_set

  let[@inline] (let+) v f = map f v
  let (and+) = cartesian_product

  let[@inline] (let*) v f = flat_map f v
  let (and*) = cartesian_product

  let (@) = append

  let (=|<) = map
  let[@inline] (>|=) v f = f =|< v

  let (<$>) = map
  let (<*>) = apply

  let (=<<) = flat_map
  let (>>=) v f = f =<< v

  let (--) = range
end
