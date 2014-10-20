open Mutability
open Assertions

TEST_UNIT "count_up_from_test1" = 
  let f = count_up_from 3 2 in
  let a = f () in 
  let b = f () in 
  let c = f () in 
  let d = f () in 
  let l = [a; b; c; d] in
  assert_true (l = [3; 5; 7; 9])

TEST_UNIT "count_up_from_test2" = 
  let f = count_up_from 3 (-2) in
  let a = f () in 
  let b = f () in 
  let c = f () in 
  let d = f () in 
  let l = [a; b; c; d] in
  assert_true (l = [3; 1; -1; -3])

TEST_UNIT "count_up_from_test3" = 
  let f = count_up_from (-3) (-2) in
  let a = f () in 
  let b = f () in 
  let c = f () in 
  let d = f () in 
  let l = [a; b; c; d] in
  assert_true (l = [-3; -5; -7; -9])

TEST_UNIT "count_up_from_test4" = 
  let f = count_up_from (-3) (2) in
  let a = f () in 
  let b = f () in 
  let c = f () in 
  let d = f () in 
  let l = [a; b; c; d] in
  assert_true (l = [-3; -1; 1; 3])

TEST_UNIT "count_up_from_test5" = 
  let f = count_up_from (-7) (3) in
  let a = f () in 
  let b = f () in 
  let c = f () in 
  let d = f () in 
  let l = [a; b; c; d] in
  assert_true (l = [-7; -4; -1; 2])

TEST_UNIT "tabulate_test1" =
  let l = Array.to_list (tabulate (fun x -> (x * x)) 3) in
  assert_true (l = [0; 1; 4])

TEST_UNIT "tabulate_test2" =
  let l = Array.to_list (tabulate (fun x -> (x * x)) 1) in
  assert_true (l = [0])

TEST_UNIT "tabulate_test3" =
  let l = Array.to_list (tabulate (fun x -> (x * x)) 0) in
  assert_true (l = [])

TEST_UNIT "tabulate_test4" =
  let l = Array.to_list (tabulate (fun x -> 23) 4) in
  assert_true (l = [23; 23; 23; 23])

TEST_UNIT "tabulate_test4" =
  let l = Array.to_list (tabulate (fun x -> "a") 4) in
  assert_true (l = ["a"; "a"; "a"; "a"])

TEST_UNIT "tabulate_test5" =
  let l = Array.to_list (tabulate (fun x -> (x/2)) 4) in
  assert_true (l = [0; 1; 0; 0])

TEST_UNIT "fold_left_imp_test1" = 
  let f = (fun a e -> a * e) in 
  let acc = 1 in 
  let l = [1; 2; 3] in 
  let alst = fold_left_imp f acc l in 
  let blst = List.fold_left f acc l in 
  assert_true (alst = blst) 

TEST_UNIT "fold_left_imp_test2" = 
  let f = (fun a e -> a * e) in 
  let acc = 1 in 
  let l = [] in 
  let alst = fold_left_imp f acc l in 
  let blst = List.fold_left f acc l in 
  assert_true (alst = blst) 

TEST_UNIT "fold_left_imp_test3" = 
  let f = (fun a e -> a * e) in 
  let acc = 1 in 
  let l = [2] in 
  let alst = fold_left_imp f acc l in 
  let blst = List.fold_left f acc l in 
  assert_true (alst = blst) 

TEST_UNIT "fold_left_imp_test4" = 
  let f = (fun a e -> a * e) in 
  let acc = 1 in 
  let l = [-1; -3; 4] in 
  let alst = fold_left_imp f acc l in 
  let blst = List.fold_left f acc l in 
  assert_true (alst = blst) 

TEST_UNIT "fold_left_imp_test5" = 
  let f = (fun a e -> "b") in 
  let acc = "z" in 
  let l = [-1; -3; 4] in 
  let alst = fold_left_imp f acc l in 
  let blst = List.fold_left f acc l in 
  assert_true (alst = blst) 

TEST_UNIT "fold_left_imp_test6" = 
  let f = (fun a e -> e::a) in 
  let acc = [] in 
  let l = [-1; -3; 4] in 
  let alst = fold_left_imp f acc l in 
  let blst = List.fold_left f acc l in 
  assert_true (alst = blst) 

TEST_UNIT "zardoz_test1" = 
  let a = List.map zardoz (List.rev lst) in
  let b = List.rev (List.map zardoz lst) in 
  assert_false (a = b)