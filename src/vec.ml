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
  if gr <= 1. then
    raise (Invalid_argument "growth_rate <= 1")
  else
    v.growth_rate <- gr

let[@inline] get_exn v idx = v.data.(idx)

let[@inline] set_exn v idx val' = v.data.(idx) <- val'

let get v idx =
  if idx < 0 || idx >= v.length then
    None
  else
    Some v.data.(idx)

let set v idx val' =
  if idx < 0 || idx >= v.length then
    false
  else
    (v.data.(idx) <- val'; true)

let ensure_capacity c v =
  let capacity = capacity v in
  if c < 0 then
    raise (Invalid_argument "capacity < 0")
  else if c <= capacity then
    ()
  else begin
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
  if c < 0 then
    raise (Invalid_argument "amount_to_reserve < 0")
  else
    ensure_capacity (capacity v + c) v

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

let singleton a =
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

let apply f v = map2 (@@) f v

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

let flat_map f v = flatten (map f v)

let cartesian_product a b = map2 (fun a b -> a, b) a b

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

  for i = 0 to v.length - 1 do
    let e = v.data.(i) in
    if f e then
      push e v2
  done;

  v2

let filteri f v =
  let v2 = make ~growth_rate:v.growth_rate ~capacity:v.length () in

  for i = 0 to v.length - 1 do
    let e = v.data.(i) in
    if f i e then
      push e v2
  done;

  v2

let of_array_steal a =
  { growth_rate = default_growth_rate
  ; length = Array.length a
  ; data = a
  }

let steal v =
  let data = v.data in
  v.length <- 0;
  v.data <- [||];
  data

let of_array a = of_array_steal (Array.copy a)
let to_array v = Array.sub v.data 0 v.length

let of_list l = of_array_steal (Array.of_list l)

let to_list v =
  let l = ref [] in
  for i = v.length - 1 downto 0 do
    l := v.data.(i) :: !l
  done;

  !l

let copy v = of_array_steal (to_array v)

let rev_in_place v =
  let i = ref 0 in
  let j = ref (v.length - 1) in

  while !i < !j do
    let i' = !i in
    let j' = !j in

    let temp = v.data.(i') in
    v.data.(i') <- v.data.(j');
    v.data.(j') <- temp;

    incr i;
    decr j
  done

let rev v =
  let v' = copy v in
  rev_in_place v';
  v'

let append v v2 =
  reserve v2.length v;

  for i = 0 to v2.length - 1 do
    push v2.data.(i) v
  done

let any f v =
  let done' = ref false in
  let i = ref 0 in

  while not !done' && !i < v.length do
    if f v.data.(!i) then
      done' := true
  done;

  !done'

let all f v =
  let done' = ref true in
  let i = ref 0 in

  while !done' && !i < v.length do
    if not (f v.data.(!i)) then
      done' := false
  done;

  !done'

let[@inline] mem e = any ((=) e)
let[@inline] memq e = any ((==) e)

let fold_left f z v =
  let z = ref z in

  for i = 0 to v.length - 1 do
    z := f !z v.data.(i)
  done;

  !z

let fold_right f z v =
  let z = ref z in

  for i = v.length - 1 downto 0 do
    z := f v.data.(i) !z
  done;

  !z

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

let sort_by f v =
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
  if start > end' then
    for i = start downto end' do
      push i v
    done
  else
    for i = start to end' do
      push i v
    done;
  v

module Infix = struct
  let[@inline] (.![]) = get_exn
  let[@inline] (.![]<-) = set_exn

  let[@inline] (.?[]) = get
  let[@inline] (.?[]<-) = set

  let[@inline] (=|<) = map
  let[@inline] (>|=) v f = f =|< v

  let[@inline] (<$>) = map
  let[@inline] (<*>) = apply

  let[@inline] (=<<) = flat_map
  let[@inline] (>>=) v f = f =<< v

  let[@inline] (--) = iota
end

module Let_syntax = struct
  let[@inline] (let+) v f = map f v
  let[@inline] (and+) = cartesian_product

  let[@inline] (let*) v f = flat_map f v
  let[@inline] (and*) = cartesian_product
end
