type ('a, -'p) t =
  { mutable growth_rate: float
  ; mutable length: int 
  ; mutable capacity: int
  ; mutable data: 'a array
  }

let default_growth_rate = 2.
let default_capacity = 0
let default_length = 0

let array_uninit n = Array.make n (Obj.magic 0)

let array_copy n a b =
  for i = 0 to n - 1 do
    a.(i) <- b.(i)
  done

let make ?growth_rate:(gr=default_growth_rate) ?capacity:(c=default_capacity) () =
  if gr <= 1. then
    raise (Invalid_argument "growth_rate <= 1.")
  else if c < 0 then
    raise (Invalid_argument "capacity < 0")
  else
    { growth_rate = gr
    ; length = default_length
    ; capacity = c
    ; data = array_uninit c
    }

external as_read_only : ('a, [> `R]) t -> ('a, [`R]) t = "%identity"

external as_write_only : ('a, [> `W]) t -> ('a, [`W]) t = "%identity"

let length v = v.length
let capacity v = v.capacity

let growth_rate v = v.growth_rate 
let set_growth_rate gr v =
  if gr <= 1. then
    raise (Invalid_argument "growth_rate <= 1.")
  else
    v.growth_rate <- gr 

let unchecked_get v = Array.get v.data
let unchecked_set v = Array.set v.data

let get_exn v idx =
  if idx < 0 || idx >= v.length then
    raise (Invalid_argument "Index out of range.")
  else
    unchecked_get v idx

let set_exn v idx val' =
  if idx < 0 || idx >= v.length then
    raise (Invalid_argument "Index out of range.")
  else
    unchecked_set v idx val'

let get v idx =
  if idx < 0 || idx >= v.length then
    None
  else
    Some (unchecked_get v idx)

let set v idx val' =
  if idx < 0 || idx >= v.length then
    false
  else
    (unchecked_set v idx val'; true) 

let ensure_capacity c v =
  if c < 0 then
    raise (Invalid_argument "capacity < 0")
  else if c <= v.capacity then
    ()
  else begin
    let cap = ref (if v.capacity = 0 then v.growth_rate else float_of_int v.capacity) in
    let c = float_of_int c in
    while !cap < c do
      cap := !cap *. v.growth_rate
    done;

    v.capacity <- int_of_float !cap;

    let data = array_uninit v.capacity in
    array_copy v.length data v.data;

    v.data <- data 
  end

let reserve c v =
  if c < 0 then
    raise (Invalid_argument "amount_to_reserve < 0")
  else
    ensure_capacity (v.capacity + c) v

let shrink_to_fit v =
  if v.capacity > v.length then 
    let data = array_uninit v.length in
    array_copy v.length data v.data;
  
    v.capacity <- v.length;
    v.data <- data

let push val' v =
  ensure_capacity (v.length + 1) v;
  let length = v.length in
  v.length <- length + 1;
  unchecked_set v length val'

let pop v =
  if v.length = 0 then
    None
  else
    let val' = unchecked_get v (v.length - 1) in
    unchecked_set v (v.length - 1) (Obj.magic 0);
    v.length <- v.length - 1;
    Some val'

let return a =
  let v = make ~capacity:1 () in
  push a v;
  v

let map f v =
  let v2 = make ~growth_rate:v.growth_rate ~capacity:v.length () in

  for i = 0 to v.length - 1 do
    unchecked_set v2 i (f (unchecked_get v i))
  done;

  v2

let mapi f v =
  let v2 = make ~growth_rate:v.growth_rate ~capacity:v.length () in

  for i = 0 to v.length - 1 do
    unchecked_set v2 i (f i (unchecked_get v i))
  done;

  v2

let map_in_place f v =
  for i = 0 to v.length - 1 do
    unchecked_set v i (f (unchecked_get v i))
  done

let map2 f v1 v2 =
  let total_l = v1.length * v2.length in
  let max_gr = max v1.growth_rate v2.growth_rate in

  let v = make ~growth_rate:max_gr ~capacity:total_l () in
  v.length <- total_l;

  let idx = ref 0 in

  for i = 0 to v1.length - 1 do
    for j = 0 to v2.length - 1 do
      unchecked_set v !idx (f (unchecked_get v1 i) (unchecked_get v2 j));
      incr idx
    done
  done;

  v

let apply f v = map2 (@@) f v

let flatten vs =
  let max_gr = ref 0. in
  let total_l = ref 0 in

  for i = 0 to vs.length - 1 do
    let crr_v = unchecked_get vs i in
    let v_gr = crr_v.growth_rate in
    if !max_gr < v_gr then
      max_gr := v_gr;

    total_l := !total_l + crr_v.length
  done;

  let v = make ~growth_rate:!max_gr ~capacity:!total_l () in
  v.length <- !total_l;

  let idx = ref 0 in 

  for i = 0 to vs.length - 1 do
    let crr_v = unchecked_get vs i in
    
    for j = 0 to crr_v.length - 1 do
      unchecked_set v !idx (unchecked_get crr_v j);
      incr idx
    done
  done;

  v

let flat_map f v = flatten (map f v)

let cartesian_product a b = map2 (fun a b -> a, b) a b

let iter f v =
  for i = 0 to v.length - 1 do
    f (unchecked_get v i)
  done

let iteri f v =
  for i = 0 to v.length - 1 do
    f i (unchecked_get v i)
  done

let filter f v =
  let v2 = make ~growth_rate:v.growth_rate ~capacity:v.length () in

  for i = 0 to v.length - 1 do
    let e = unchecked_get v i in
    if f e then
      push e v2
  done;

  v2

let of_list l =
  let rec go v = function
    | [] -> ()
    | a :: rest -> push a v; go v rest
  in
  let v = make () in
  go v l;
  v

let to_list v =
  let l = ref [] in
  for i = v.length - 1 downto 0 do
    l := (unchecked_get v i) :: !l
  done;

  !l

let of_array_steal a =
  let length = Array.length a in
  { growth_rate = default_growth_rate
  ; length = length
  ; capacity = length
  ; data = a
  }
  
let steal v =
  let data = v.data in
  v.length <- 0;
  v.capacity <- 0;
  v.data <- [||];
  data

let of_array a = of_array_steal (Array.copy a)
let copy v = of_array v.data

let to_array v =
  let a = array_uninit v.length in
  array_copy v.length a v.data;
  a

let rev_in_place v =
  let i = ref 0 in
  let j = ref (v.length - 1) in

  while !i < !j do
    let i' = !i in
    let j' = !j in

    let temp = unchecked_get v i' in
    unchecked_set v i' (unchecked_get v j');
    unchecked_set v j' temp;

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
    push (unchecked_get v2 i) v
  done

let any f v =
  let done' = ref false in
  let i = ref 0 in

  while not !done' && !i < v.length do
    if f (unchecked_get v !i) then
      done' := true
  done;

  !done'

let all f v =
  let done' = ref true in
  let i = ref 0 in

  while !done' && !i < v.length do
    if not (f (unchecked_get v !i)) then
      done' := false
  done;

  !done'

let mem e = any ((=) e)
let memq e = any ((==) e)

let fold_left f z v =
  let z = ref z in

  for i = 0 to v.length - 1 do
    z := f !z (unchecked_get v i)
  done;

  !z

let fold_right f z v =
  let z = ref z in

  for i = v.length - 1 downto 0 do
    z := f (unchecked_get v i) !z
  done;

  !z

let zip_with f v1 v2 =
  let min_length = min v1.length v2.length in
  let max_gr = max v1.growth_rate v2.growth_rate in
  let v = make ~growth_rate:max_gr ~capacity:min_length () in

  for i = 0 to min_length do
    push (f (unchecked_get v1 i) (unchecked_get v2 i)) v
  done;

  v

let zip v1 v2 = zip_with (fun a b -> (a, b)) v1 v2

let sort_by f v =
  shrink_to_fit v;
  Array.fast_sort f v.data

let sort v = sort_by compare v

let pretty_print fmt v =
  if v.length = 0 then
    "[]"
  else
    let buf = Buffer.create 2 in

    Buffer.add_char buf '[';
    Buffer.add_string buf @@ fmt (unchecked_get v 0);

    for i = 1 to v.length do
      Buffer.add_string buf ", ";
      Buffer.add_string buf (fmt (unchecked_get v i))
    done;

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
  let (.![]) = get_exn
  let (.![]<-) = set_exn

  let (.?[]) = get
  let (.?[]<-) = set

  let (=|<) = map
  let (>|=) v f = f =|< v

  let (<$>) = map
  let (<*>) = apply

  let (=<<) = flat_map
  let (>>=) v f = f =<< v

  let (--) = iota
end

module Let_syntax = struct
  let (let+) v f = map f v
  let (and+) = cartesian_product

  let (let*) v f = flat_map f v
  let (and*) = cartesian_product
end
