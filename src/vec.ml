type ('a, -'p) t =
  { mutable growth_rate: float
  ; mutable length: int
  ; mutable data: 'a array
  }

let default_growth_rate = 2.

let[@inline] array_uninit n = Array.make n (Obj.magic 0)

let make ?growth_rate:(gr=default_growth_rate) ?capacity:(c=0) () =
  if gr <= 1. then
    raise (Invalid_argument "growth_rate <= 1")
  else if c < 0 then
    raise (Invalid_argument "capacity < 0")
  else
    { growth_rate = gr
    ; length = 0
    ; data = array_uninit c
    }

external as_read_only: ('a, [> `R]) t -> ('a, [`R]) t = "%identity"

external as_write_only: ('a, [> `W]) t -> ('a, [`W]) t = "%identity"

let[@inline] length v = v.length
let[@inline] capacity v = Array.length v.data

let[@inline] growth_rate v = v.growth_rate
let set_growth_rate gr v =
  if gr <= 1.
  then raise (Invalid_argument "growth_rate <= 1")
  else v.growth_rate <- gr

let[@inline] clear v =
  v.length <- 0;
  v.data <- [||]

let[@inline] get_exn v idx = v.data.(idx)

let[@inline] set_exn v idx val' = v.data.(idx) <- val'

let get v idx =
  if idx < 0 || idx >= v.length
  then None
  else Some v.data.(idx)

let[@inline] set v idx val' = idx >= 0 && idx < v.length && (v.data.(idx) <- val'; true)

let ensure_capacity c v =
  let capacity = capacity v in
  if c < 0 then
    raise (Invalid_argument "capacity < 0")
  else if c > capacity then begin
    let cap = ref (if capacity = 0 then v.growth_rate else float_of_int capacity) in
    let c = float_of_int c in
    while !cap < c do
      cap := !cap *. v.growth_rate
    done;

    let data = array_uninit (int_of_float !cap) in
    Array.blit v.data 0 data 0 v.length;
    v.data <- data
  end

let reserve c v =
  if c < 0
  then raise (Invalid_argument "amount_to_reserve < 0")
  else ensure_capacity (capacity v + c) v

let shrink_to_fit v =
  if capacity v > v.length then
    let data = array_uninit v.length in
    Array.blit v.data 0 data 0 v.length;
    v.data <- data

let push val' v =
  ensure_capacity (v.length + 1) v;
  let length = v.length in
  v.length <- length + 1;
  v.data.(length) <- val'

let pop v =
  if v.length = 0 then
    None
  else
    let val' = v.data.(v.length - 1) in
    v.data.(v.length - 1) <- Obj.magic 0;
    v.length <- v.length - 1;
    Some val'

let[@inline] singleton a =
  { growth_rate = default_growth_rate
  ; length = 1
  ; data = [|a|]
  }

let map f v =
  let v2 = make ~growth_rate:v.growth_rate ~capacity:v.length () in
  v2.length <- v.length;

  for i = 0 to v.length - 1 do
    v2.data.(i) <- f v.data.(i)
  done;

  v2

let mapi f v =
  let v2 = make ~growth_rate:v.growth_rate ~capacity:v.length () in
  v2.length <- v.length;

  for i = 0 to v.length - 1 do
    v2.data.(i) <- f i v.data.(i)
  done;

  v2

let map_in_place f v =
  for i = 0 to v.length - 1 do
    v.data.(i) <- f v.data.(i)
  done

let map2 f v1 v2 =
  let total_l = v1.length * v2.length in
  let max_gr = max v1.growth_rate v2.growth_rate in

  let v = make ~growth_rate:max_gr ~capacity:total_l () in
  v.length <- total_l;

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
  let max_gr = ref 0. in
  let total_l = ref 0 in

  for i = 0 to vs.length - 1 do
    let crr_v = vs.data.(i) in
    let v_gr = crr_v.growth_rate in
    if !max_gr < v_gr then
      max_gr := v_gr;

    total_l := !total_l + crr_v.length
  done;

  let v = make ~growth_rate:!max_gr ~capacity:!total_l () in
  v.length <- !total_l;

  let idx = ref 0 in

  for i = 0 to vs.length - 1 do
    let crr_v = vs.data.(i) in

    for j = 0 to crr_v.length - 1 do
      v.data.(!idx) <- crr_v.data.(j);
      incr idx
    done
  done;

  v

let append v v2 =
  let l = v.length + v2.length in
  ensure_capacity l v;

  for i = 0 to v2.length - 1 do
    v.data.(i + v.length) <- v2.data.(i)
  done;

  v.length <- l

let flat_map f v =
  let v2 = make ~growth_rate:v.growth_rate ~capacity:v.length () in

  for i = 0 to v.length - 1 do
    append v2 (f v.data.(i))
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
  let v2 = make ~growth_rate:v.growth_rate ~capacity:v.length () in
  let l = ref 0 in

  for i = 0 to v.length - 1 do
    let e = v.data.(i) in
    if f e then
      (v2.data.(!l) <- e; incr l)
  done;

  v2.length <- !l;
  v2

let filteri f v =
  let v2 = make ~growth_rate:v.growth_rate ~capacity:v.length () in
  let l = ref 0 in

  for i = 0 to v.length - 1 do
    let e = v.data.(i) in
    if f i e then
      (v2.data.(!l) <- e; incr l)
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

let[@inline] of_array_steal a =
  { growth_rate = default_growth_rate
  ; length = Array.length a
  ; data = a
  }

let[@inline] of_array a = of_array_steal (Array.copy a)
let[@inline] to_array v = Array.sub v.data 0 v.length

let[@inline] of_list l = of_array_steal (Array.of_list l)

let to_list v =
  let rec go acc = function
    | -1 -> acc
    | i -> go (v.data.(i) :: acc) (i - 1)
  in
  go [] (v.length - 1)

let[@inline] copy v = of_array_steal (to_array v)

let rev_in_place v =
  let[@inline] swap i j =
    let temp = v.data.(i) in
    v.data.(i) <- v.data.(j);
    v.data.(j) <- temp
  in
  let rec go i j =
    if i < j
    then (swap i j; go (i + 1) (j - 1))
  in
  go 0 (v.length - 1)

let[@inline] rev v =
  let v' = copy v in
  rev_in_place v';
  v'

let exists f v =
  let rec go i = i <> v.length && (f v.data.(i) || go (i + 1))
  in go 0

let for_all f v =
  let rec go i = i = v.length || (f v.data.(i) && go (i + 1))
  in go 0

let[@inline] mem e = exists ((=) e)
let[@inline] memq e = exists ((==) e)

let fold_left f z v =
  let rec go acc i =
    if i = v.length
    then acc
    else go (f acc v.data.(i)) (i + 1)
  in
  go z 0

let fold_right f z v =
  let rec go acc = function
    | 0 -> acc
    | i -> go (f v.data.(i) z) (i - 1)
  in
  go z (v.length - 1)

let zip_with f v1 v2 =
  let min_length = min v1.length v2.length in
  let max_gr = max v1.growth_rate v2.growth_rate in

  let v = make ~growth_rate:max_gr ~capacity:min_length () in
  v.length <- min_length;

  for i = 0 to min_length - 1 do
    v.data.(i) <- f v1.data.(i) v2.data.(i)
  done;

  v

let[@inline] zip v1 v2 = zip_with (fun a b -> (a, b)) v1 v2

let[@inline] sort_by f v =
  shrink_to_fit v;
  Array.fast_sort f v.data

let[@inline] sort v = sort_by compare v

let pretty_print fmt v =
  if v.length = 0 then
    "[]"
  else
    let buf = Buffer.create 2 in

    Buffer.add_char buf '[';
    Buffer.add_string buf @@ fmt v.data.(0);

    for i = 1 to v.length - 1 do
      Buffer.add_string buf ", ";
      Buffer.add_string buf (fmt v.data.(i))
    done;

    Buffer.add_char buf ']';
    Buffer.contents buf

let iota start end' =
  let v = make ~capacity:(abs (end' - start)) () in
  let rec inc i crr =
    if crr <= end' then begin
      v.data.(i) <- crr;
      inc (i + 1) (crr + 1)
    end
  in
  let rec dec i crr =
    if crr >= end' then begin
      v.data.(i) <- crr;
      dec (i + 1) (crr - 1)
    end
  in

  if start > end'
  then inc 0 start
  else dec 0 start;

  v

module Infix = struct
  let (.![]) = get_exn
  let (.![]<-) = set_exn

  let (.?[]) = get
  let (.?[]<-) = set

  let (=|<) = map
  let[@inline] (>|=) v f = f =|< v

  let (<$>) = map
  let (<*>) = apply

  let (=<<) = flat_map
  let (>>=) v f = f =<< v

  let (--) = iota
end

module Let_syntax = struct
  let[@inline] (let+) v f = map f v
  let (and+) = cartesian_product

  let[@inline] (let*) v f = flat_map f v
  let (and*) = cartesian_product
end
