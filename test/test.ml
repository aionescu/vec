open OUnit2

let make _ =
  let v = Vec.make () in
  assert_equal 0 (Vec.length v);
  assert_equal Vec.default_growth_rate (Vec.growth_rate v);

  let v = Vec.make ~growth_rate:3. () in
  assert_equal 3. (Vec.growth_rate v)

let test_suite =
  "Tests" >:::
    [ "make" >:: make
    ; "make2" >:: make
    ]

let () = run_test_tt_main test_suite
