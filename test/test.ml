open OUnit2
open Vec.Infix

let make _ =
  let v = Vec.make () in
  assert_equal 0 (Vec.length v);
  assert_equal Vec.default_growth_rate (Vec.growth_rate v);

  let v = Vec.make ~growth_rate:3. () in
  assert_equal 3. (Vec.growth_rate v)

let capacity _ =
  let v = Vec.make () in

  Vec.ensure_capacity 30 v;
  assert_equal 32 (Vec.capacity v);

  let old_cap = Vec.capacity v in

  Vec.ensure_capacity 20 v;
  assert_equal old_cap (Vec.capacity v);

  Vec.push 2 v;
  Vec.shrink_to_fit v;
  assert_equal 1 (Vec.capacity v);

  let v = Vec.make ~growth_rate:1.5 () in

  Vec.ensure_capacity 30 v;
  assert_equal 38 (Vec.capacity v)

let get_set _ =
  let v = Vec.make ~capacity:8 () in

  assert_raises
    ~msg:"get_exn"
    (Invalid_argument "Index out of range")
    (fun () -> v.![0]);

  assert_raises
    ~msg:"set_exn"
    (Invalid_argument "Index out of range")
    (fun () -> v.![0] <- 0);

  assert_equal None v.?[0];
  assert_bool "set_safe out of range" (not @@ v.?[0] <- 0);

  Vec.push 1 v;

  assert_equal () (v.![0] <- 2);
  assert_equal 2 v.![0];

  assert_bool "set_safe in range" (v.?[0] <- 3);
  assert_equal (Some 3) v.?[0]

let find _ =
  let v = 1 -- 5 in

  assert_equal (Some 3) (Vec.find ((=) 3) v);
  assert_equal None (Vec.find ((<) 5) v);

  assert_raises
  ~msg:"find_exn"
  Not_found
  (fun () -> Vec.find_exn ((<) 5) v)

let add_remove_at _ =
  let v = 1 -- 5 in

  assert_equal (Some 3) (Vec.remove_at 2 v);
  assert_equal [1; 2; 4; 5] (Vec.to_list v);

  assert_equal (Some 1) (Vec.remove_at 0 v);
  assert_equal [2; 4; 5] (Vec.to_list v);

  assert_equal (Some 5) (Vec.remove_at 2 v);
  assert_equal [2; 4] (Vec.to_list v);

  assert_bool "add_at 0" (Vec.add_at 0 1 v);
  assert_equal [1; 2; 4] (Vec.to_list v);

  assert_bool "add_at 2" (Vec.add_at 2 3 v);
  assert_equal [1; 2; 3; 4] (Vec.to_list v);

  assert_bool "add_at 4" (Vec.add_at 4 5 v);
  assert_equal [1; 2; 3; 4; 5] (Vec.to_list v)

let push_pop _ =
  let v = Vec.make () in

  Vec.push 1 v;
  assert_equal 1 (Vec.length v);

  Vec.push 3 v;
  assert_equal 2 (Vec.length v);

  assert_equal (Some 3) (Vec.pop v);
  assert_equal (Some 1) (Vec.pop v);
  assert_equal None (Vec.pop v)

let map _ =
  let v = 0 -- 4 in

  let v' = succ <$> v in
  assert_equal [1; 2; 3; 4; 5] (Vec.to_list v');

  Vec.map_in_place succ v';
  assert_equal [2; 3; 4; 5; 6] (Vec.to_list v');

  let v'' = Vec.mapi (fun i _ -> i) v' in
  assert_equal [0; 1; 2; 3; 4] (Vec.to_list v'')

let iter _ =
  let v = 0 -- 5 in
  let expected = List.fold_left (+) 0 [0; 1; 2; 3; 4; 5] in
  let actual = ref 0 in

  Vec.iter (fun i -> actual := !actual + i) v;
  assert_equal expected !actual;

  let actual = ref true in
  Vec.iteri (fun i a -> actual := !actual && i = a) v;
  assert_bool "iteri" !actual

let cartesian_product _ =
  let a = Vec.of_list [1; 2; 3] in
  let b = Vec.of_list [10; 20; 30] in
  let expected = [1, 10; 1, 20; 1, 30; 2, 10; 2, 20; 2, 30; 3, 10; 3, 20; 3, 30] in

  assert_equal expected (Vec.to_list @@ Vec.cartesian_product a b)

let monad_ops _ =
  let list = [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]] in
  let a = Vec.of_list <$> Vec.of_list list in
  assert_equal (List.flatten list) (Vec.to_list @@ Vec.flatten a);

  let expected = [1; 1; 1; 2; 2; 2; 3; 3; 3; 4; 4; 4] in
  let f i = Vec.of_list [i; i; i] in
  let a = 1 -- 4 in
  assert_equal expected @@ Vec.to_list (a >>= f)

let filter _ =
  let v = 0 -- 10 in
  let even i = i mod 2 = 0 in
  assert_equal [0; 2; 4; 6; 8; 10] (Vec.to_list @@ Vec.filter even v);
  assert_equal (Vec.length v) (Vec.length @@ Vec.filteri (=) v);

  Vec.filter_in_place even v;
  assert_equal 6 (Vec.length v)

let conversions _ =
  let l = [1; 2; 3; 4; 5] in
  let v = Vec.of_list l in
  assert_equal l (Vec.to_list v);

  Vec.ensure_capacity 100 v;
  let a = Vec.to_array v in
  assert_equal [|1; 2; 3; 4; 5|] a;
  assert_equal 5 (Array.length a)

let rev _ =
  let l = [1; 2; 3; 4; 5; 6] in
  let v = Vec.of_list l in
  assert_equal (List.rev l) (Vec.to_list @@ Vec.rev v);

  Vec.rev_in_place v;
  assert_equal (List.rev l) (Vec.to_list v)

let append _ =
  let l1 = [1; 2; 3; 4; 5] in
  let l2 = [6; 7; 8; 9; 10] in
  let l = List.append l1 l2 in

  let v = Vec.of_list l1 in
  let v' = Vec.copy v in
  let v2 = Vec.of_list l2 in

  Vec.append_in_place v' v2;
  assert_equal l (Vec.to_list v');
  assert_equal 10 (Vec.length v');

  assert_equal l (Vec.to_list @@ v @ v2)

let exists _ =
  let v = Vec.of_list [1; 2; 3; 4; 5] in
  assert_bool "exists" (Vec.exists ((=) 4) v);
  assert_bool "not exists" (not @@ Vec.exists ((=) 6) v);
  assert_bool "exists empty" (not @@ Vec.exists (fun _ -> true) @@ Vec.make ())

let for_all _ =
  let v = 5 -- 1 in
  assert_bool "for_all" (Vec.for_all ((<=) 1) v);
  assert_bool "not for_all" (not @@ Vec.for_all ((<=) 3) v);
  assert_bool "for_all empty" (Vec.for_all (fun _ -> false) @@ Vec.make ())

let mem _ =
  let v = 1 -- 100 in
  assert_bool "mem" (Vec.mem 95 v);
  assert_bool "mem not" (not @@ Vec.mem 101 v);

  let a = [|1; 2|] in
  let b = [|1; 2|] in
  let v = Vec.of_list [[|1; 2|]; a; [|1; 2|]] in
  assert_bool "memq" (Vec.memq a v);
  assert_bool "memq" (not @@ Vec.memq b v)

let folds _ =
  let v = 1 -- 100 in
  let l = Vec.to_list v in

  let expected = List.fold_left (+) 0 l in
  let actual = Vec.fold_left (+) 0 v in
  assert_equal expected actual;

  let expected = List.fold_right Int.mul l 1 in
  let actual = Vec.fold_right Int.mul v 1 in
  assert_equal expected actual;

  let z = -50 in
  let empty = Vec.make () in
  assert_equal z (Vec.fold_left (+) z empty);
  assert_equal z (Vec.fold_right (+) empty z)

let zip _ =
  let a = 1 -- 3 in
  let b = 4 -- 6 in

  let expected = [1, 4; 2, 5; 3, 6] in
  assert_equal expected (Vec.to_list @@ Vec.zip a b);

  let expected = [5; 7; 9] in
  assert_equal expected (Vec.to_list @@ Vec.zip_with (+) a b)

let equal _ =
  assert_bool "equal empty" @@ Vec.equal (Vec.make ()) (Vec.make ());

  let a = Vec.of_list [1; 2; 3; 4; 5] in
  let b = 1 -- 5 in

  assert_bool "equal non-empty" (Vec.equal a b);

  Vec.ensure_capacity 10 a;
  assert_bool "equal diff capacity" (Vec.equal a b);

  Vec.push 6 a;
  assert_bool "equal diff length" (not @@ Vec.equal a b)

let compare _ =
  assert_equal 0 @@ Vec.compare (Vec.make ()) (Vec.make ());

  let a = Vec.of_list ['a'; 'b'; 'c'] in
  let b = Vec.of_list ['a'; 'b'; 'd'] in

  assert_equal 0 (Vec.compare a a);
  assert_equal (-1) (Vec.compare a b);
  assert_equal 1 (Vec.compare b a);

  let a = Vec.of_list ['a'; 'b'; 'c'] in
  let b = Vec.of_list ['a'; 'b'; 'c'; 'd'; 'e'] in
  assert_equal (-1) (Vec.compare a b);

  let a = Vec.of_list ['a'; 'b'; 'c'; 'd'] in
  let b = Vec.of_list ['e'; 'f'] in
  assert_equal (-1) (Vec.compare a b)

let pretty_print _ =
  let pp = Vec.pretty_print Int.to_string in

  assert_equal "[]" (pp @@ Vec.make ());
  assert_equal "[2]" (pp @@ Vec.singleton 2);
  assert_equal "[1; 2; 3; 4; 5]" (pp @@ 1 -- 5);

  let pp = Vec.pretty_print (fun s -> s) in
  assert_equal "[abc; def]" (pp @@ Vec.of_list ["abc"; "def"]);

  let pp = Vec.pretty_print (fun s -> Int.to_string @@ String.length s) in
  assert_equal "[3; 4; 5]" (pp @@ Vec.of_list ["aaa"; "abcd"; "abcde"])

let test_suite =
  "Tests" >:::
    [ "make" >:: make
    ; "capacity" >:: capacity
    ; "get_set" >:: get_set
    ; "find" >:: find
    ; "add_remove_at" >:: add_remove_at
    ; "push_pop" >:: push_pop
    ; "map" >:: map
    ; "iter" >:: iter
    ; "cartesian_product" >:: cartesian_product
    ; "monad_ops" >:: monad_ops
    ; "filter" >:: filter
    ; "conversions" >:: conversions
    ; "rev" >:: rev
    ; "append" >:: append
    ; "exists" >:: exists
    ; "for_all" >:: for_all
    ; "mem" >:: mem
    ; "folds" >:: folds
    ; "zip" >:: zip
    ; "equal" >:: equal
    ; "compare" >:: compare
    ; "pretty_print" >:: pretty_print
    ]

let () = run_test_tt_main test_suite
