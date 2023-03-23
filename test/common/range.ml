open! Core
open Memtrace_viewer_common.Range

module _ = struct
  open Make (Int)

  let%expect_test "inter" =
    let test_inter range1 range2 =
      match inter range1 range2 with
      | Non_empty { lower_bound; upper_bound } ->
        print_s
          [%message
            "" ~range:((lower_bound, upper_bound) : Point.t Bound.t * Point.t Bound.t)]
      | Empty -> printf "empty"
    in
    test_inter (range No_bound No_bound) (range (Open 4) No_bound);
    [%expect {| (range ((Open 4) No_bound)) |}];
    test_inter (range No_bound (Open 3)) (range No_bound (Closed 3));
    [%expect {| (range (No_bound (Open 3))) |}];
    test_inter (range No_bound (Open 3)) (range (Closed 1) No_bound);
    [%expect {| (range ((Closed 1) (Open 3))) |}];
    test_inter (range (Open 3) No_bound) (range (Closed 3) No_bound);
    [%expect {| (range ((Open 3) No_bound)) |}];
    test_inter (range (Closed 0) No_bound) (range (Closed 1) (Open 4));
    [%expect {| (range ((Closed 1) (Open 4))) |}];
    test_inter (range (Closed 0) (Open 4)) (range (Open 1) (Closed 5));
    [%expect {| (range ((Open 1) (Open 4))) |}];
    test_inter (range (Closed 0) (Open 4)) (range (Closed 5) No_bound);
    [%expect {| empty |}];
    test_inter (range (Closed 1) (Closed 1)) (range (Open 1) (Closed 2));
    [%expect {| empty |}];
    test_inter (range (Closed 1) (Closed 1)) (range (Closed 1) No_bound);
    [%expect {| (range ((Closed 1) (Closed 1))) |}]
  ;;
end

module _ = struct
  open Make (Int)

  let assert_equal_by ~equal ~sexp_of_t ans1 ans2 =
    if not (equal ans1 ans2)
    then
      raise_s
        [%message
          "should be equal" ~_:(sexp_of_t ans1 : Sexp.t) ~_:(sexp_of_t ans2 : Sexp.t)]
  ;;

  let test_cond ?shrinker ?shrink_attempts ?sexp_of ~cond generator =
    let generator =
      match cond with
      | Some cond -> Quickcheck.Generator.filter generator ~f:cond
      | None -> generator
    in
    Quickcheck.test ?shrinker ?shrink_attempts ?sexp_of generator
  ;;

  let run1 ?cond ~f () =
    test_cond
      ~cond
      ~sexp_of:[%sexp_of: t]
      ~shrinker:[%quickcheck.shrinker: t]
      [%quickcheck.generator: t]
      ~f
  ;;

  let run2 ?cond ~f () =
    test_cond
      ~cond
      ~sexp_of:[%sexp_of: t * t]
      ~shrinker:[%quickcheck.shrinker: t * t]
      [%quickcheck.generator: t * t]
      ~f
  ;;

  let run3 ?cond ~f () =
    test_cond
      ~cond
      ~sexp_of:[%sexp_of: t * t * t]
      ~shrinker:[%quickcheck.shrinker: t * t * t]
      [%quickcheck.generator: t * t * t]
      ~f
  ;;

  let run_eq ?cond ~run ~equal ~sexp_of_t ~f () =
    run
      ?cond
      ~f:(fun x ->
        let ans1, ans2 = f x in
        assert_equal_by ~equal ~sexp_of_t ans1 ans2)
      ()
  ;;

  let run1_eq = run_eq ~run:run1 ~equal ~sexp_of_t
  let run2_eq = run_eq ~run:run2 ~equal ~sexp_of_t
  let run3_eq = run_eq ~run:run3 ~equal ~sexp_of_t
  let%test_unit "commutativity of join" = run2_eq ~f:(fun (a, b) -> join a b, join b a) ()

  let%test_unit "associativity of join" =
    run3_eq ~f:(fun (a, b, c) -> join (join a b) c, join a (join b c)) ()
  ;;

  let%test_unit "all is zero of join" = run1_eq ~f:(fun a -> join all a, all) ()
  let run1_eq_oe = run_eq ~run:run1 ~equal:Or_empty.equal ~sexp_of_t:Or_empty.sexp_of_t
  let run2_eq_oe = run_eq ~run:run2 ~equal:Or_empty.equal ~sexp_of_t:Or_empty.sexp_of_t
  let run3_eq_oe = run_eq ~run:run3 ~equal:Or_empty.equal ~sexp_of_t:Or_empty.sexp_of_t

  let%test_unit "commutativity of inter" =
    run2_eq_oe ~f:(fun (a, b) -> inter a b, inter b a) ()
  ;;

  let%test_unit "associativity of inter" =
    run3_eq_oe
      ~f:(fun (a, b, c) ->
        Or_empty.inter (inter a b) (Non_empty c), Or_empty.inter (Non_empty a) (inter b c))
      ()
  ;;

  let%test_unit "all is identity of inter" =
    run1_eq_oe ~f:(fun a -> inter all a, Non_empty a) ()
  ;;

  let%test_unit "inter absorbs join" =
    run2_eq_oe ~f:(fun (a, b) -> inter a (join a b), Non_empty a) ()
  ;;

  let%test_unit "join absorbs inter" =
    run2_eq_oe ~f:(fun (a, b) -> Or_empty.join (Non_empty a) (inter a b), Non_empty a) ()
  ;;

  let%test_unit "conditional distributivity of inter over join" =
    run3_eq_oe
      ~cond:(fun (_, b, c) -> not (disjoint b c))
      ~f:(fun (a, b, c) -> inter a (join b c), Or_empty.join (inter a b) (inter a c))
      ()
  ;;

  let%test_unit "conditional distributivity of join over inter" =
    run3_eq_oe
      ~cond:(fun (a, b, c) -> (not (disjoint a b)) && not (disjoint a c))
      ~f:(fun (a, b, c) ->
        Or_empty.join (Non_empty a) (inter b c), inter (join a b) (join a c))
      ()
  ;;

  module _ = struct
    open! Or_empty

    let assert_equal ans1 ans2 =
      if not (equal ans1 ans2)
      then raise_s [%message "should be equal" ~_:(ans1 : t) ~_:(ans2 : t)]
    ;;

    let run1 ?cond ~f () =
      test_cond
        ~cond
        ~sexp_of:[%sexp_of: t]
        ~shrinker:[%quickcheck.shrinker: t]
        [%quickcheck.generator: t]
        ~f
    ;;

    let run2 ?cond ~f () =
      test_cond
        ~cond
        ~sexp_of:[%sexp_of: t * t]
        ~shrinker:[%quickcheck.shrinker: t * t]
        [%quickcheck.generator: t * t]
        ~f
    ;;

    let run3 ?cond ~f () =
      test_cond
        ~cond
        ~sexp_of:[%sexp_of: t * t * t]
        ~shrinker:[%quickcheck.shrinker: t * t * t]
        [%quickcheck.generator: t * t * t]
        ~f
    ;;

    let run_eq ?cond ~run ~f () =
      run
        ?cond
        ~f:(fun x ->
          let ans1, ans2 = f x in
          assert_equal ans1 ans2)
        ()
    ;;

    let run1_eq = run_eq ~run:run1
    let run2_eq = run_eq ~run:run2
    let run3_eq = run_eq ~run:run3

    let%test_unit "Or_empty: commutativity of join" =
      run2_eq ~f:(fun (a, b) -> join a b, join b a) ()
    ;;

    let%test_unit "Or_empty: associativity of join" =
      run3_eq ~f:(fun (a, b, c) -> join (join a b) c, join a (join b c)) ()
    ;;

    let%test_unit "Or_empty: empty is identity of join" =
      run1_eq ~f:(fun a -> join empty a, a) ()
    ;;

    let%test_unit "Or_empty: all is zero of join" =
      run1_eq ~f:(fun a -> join all a, all) ()
    ;;

    let%test_unit "Or_empty: commutativity of inter" =
      run2_eq ~f:(fun (a, b) -> inter a b, inter b a) ()
    ;;

    let%test_unit "Or_empty: associativity of inter" =
      run3_eq ~f:(fun (a, b, c) -> inter (inter a b) c, inter a (inter b c)) ()
    ;;

    let%test_unit "Or_empty: all is identity of inter" =
      run1_eq ~f:(fun a -> inter all a, a) ()
    ;;

    let%test_unit "Or_empty: inter absorbs join" =
      run2_eq ~f:(fun (a, b) -> inter a (join a b), a) ()
    ;;

    let%test_unit "Or_empty: join absorbs inter" =
      run2_eq ~f:(fun (a, b) -> join a (inter a b), a) ()
    ;;

    let%test_unit "Or_empty: conditional distributivity of inter over join" =
      run3_eq
        ~cond:(fun (_, b, c) -> not (disjoint b c))
        ~f:(fun (a, b, c) -> inter a (join b c), join (inter a b) (inter a c))
        ()
    ;;

    let%test_unit "Or_empty: conditional distributivity of join over inter" =
      run3_eq
        ~cond:(fun (a, b, c) -> (not (disjoint a b)) && not (disjoint a c))
        ~f:(fun (a, b, c) -> join a (inter b c), inter (join a b) (join a c))
        ()
    ;;
  end
end
