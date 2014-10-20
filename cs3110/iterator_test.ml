open Iterator
open Assertions

TEST_UNIT "ListIterator_test1" =
  let a = ListIterator.create ["1"; "2"; "3"] in 
  assert_true (ListIterator.has_next a)

TEST_UNIT "ListIterator_test2" =
  let a = ListIterator.create [1] in 
  assert_true (ListIterator.has_next a)

TEST_UNIT "ListIterator_test3" =
  let a = ListIterator.create [] in 
  assert_false (ListIterator.has_next a)

TEST_UNIT "ListIterator_test4" =
  let a = ListIterator.create [1; 2; 3] in 
  let x = ListIterator.next a in
  let y = ListIterator.next a in 
  let z = ListIterator.next a in 
  assert_true ([x; y; z] = [1; 2; 3])

TEST_UNIT "ListIterator_test5" =
  let a = ListIterator.create ["a"; "b"; "c"] in 
  let x = ListIterator.next a in
  let y = ListIterator.next a in 
  let z = ListIterator.next a in 
  assert_true ([x; y; z] = ["a"; "b"; "c"])

TEST_UNIT "ListIterator_test6" =
  let a = ListIterator.create [] in 
  assert_raises(Some (ListIterator.NoResult)) ListIterator.next a

TEST_UNIT "ListIterator_test7" =
  let a = ListIterator.create [1] in 
  let f x = (ListIterator.next x); () in 
  f a;
  assert_raises(Some (ListIterator.NoResult)) ListIterator.next a 

TEST_UNIT "ListIterator_test8" =
  let a = ListIterator.create [1; 2] in 
  let f x = (ListIterator.next x); () in 
  f a;
  f a;
  assert_false (ListIterator.has_next a)

TEST_UNIT "ListIterator_test9" =
  let a = ListIterator.create [1; 2] in 
  let f x = (ListIterator.next x); () in 
  f a;
  assert_true (ListIterator.has_next a)


let arbre = Node (4, Node (2, Node (1, Leaf, Leaf), Node (3, Leaf, Leaf)), 
Node (6, Node (5, Leaf, Leaf), Node (7, Leaf, Leaf)))

let shu = Node(4, Leaf, Leaf)

let bare = Leaf

let lettertree = Node ("d", Node ("b", Node ("a", Leaf, Leaf), Node ("c", Leaf, Leaf)), 
  Node ("f", Node ("e", Leaf, Leaf), Node ("g", Leaf, Leaf)))

TEST_UNIT "InorderTreeIterator_test1" =
   let a = InorderTreeIterator.create arbre in 
   let one = InorderTreeIterator.next a in
   let two = InorderTreeIterator.next a in 
   let three = InorderTreeIterator.next a in
   let four = InorderTreeIterator.next a in
   let five = InorderTreeIterator.next a in
   let six = InorderTreeIterator.next a in
   let seven = InorderTreeIterator.next a in
   assert_true ([one; two; three; four; five; six; seven]=[1; 2; 3; 4; 5; 6; 7])

TEST_UNIT "InorderTreeIterator_test2" = 
  let a = InorderTreeIterator.create shu in 
  assert_true(InorderTreeIterator.has_next a)

TEST_UNIT "InorderTreeIterator_test3" = 
  let a = InorderTreeIterator.create bare in 
  assert_false (InorderTreeIterator.has_next a)

TEST_UNIT "InorderTreeIterator_test4" =
  let z = InorderTreeIterator.create lettertree in 
  let a = InorderTreeIterator.next z in
  let b = InorderTreeIterator.next z in 
  let c = InorderTreeIterator.next z in
  let d = InorderTreeIterator.next z in
  let e = InorderTreeIterator.next z in
  let f = InorderTreeIterator.next z in
  let g = InorderTreeIterator.next z in
  assert_true ([a; b; c; d; e; f; g]=["a"; "b"; "c"; "d"; "e"; "f"; "g"])

TEST_UNIT "InorderTreeIterator_test5" = 
  let a = InorderTreeIterator.create bare in 
  assert_raises(Some (InorderTreeIterator.NoResult)) InorderTreeIterator.next a

TEST_UNIT "InorderTreeIterator_test6" = 
  let a = InorderTreeIterator.create arbre in 
  let f x = (InorderTreeIterator.next x); () in 
  f a;
  f a;
  f a;
  f a;
  f a; 
  f a;
  f a;
  assert_raises(Some (InorderTreeIterator.NoResult)) InorderTreeIterator.next a

TEST_UNIT "InorderTreeIterator_test6" = 
  let a = InorderTreeIterator.create arbre in 
  let f x = (InorderTreeIterator.next x); () in 
  f a;
  f a;
  f a;
  f a;
  f a; 
  f a;
  f a;
  assert_false (InorderTreeIterator.has_next a)

module TIM = TakeIterator (ListIterator)

TEST_UNIT "TakeIterator_test1" = 
  let a = TIM.create 3 (ListIterator.create [1; 2; 3; 4]) in 
  let f x= (TIM.next x); () in 
  f a; 
  f a;
  f a;
  assert_raises (Some (TIM.NoResult)) TIM.next a

TEST_UNIT "TakeIterator_test2" = 
  let a = TIM.create 3 (ListIterator.create [1; 2; 3; 4]) in 
  let e = TIM.next a in 
  let f = TIM.next a in 
  let g = TIM.next a in
  assert_true ([e; f; g] = [1; 2; 3])

TEST_UNIT "TakeIterator_test3" = 
  let a = TIM.create 4 (ListIterator.create [1; 2; 3; 4]) in 
  let e = TIM.next a in 
  let f = TIM.next a in 
  let g = TIM.next a in
  let h = TIM.next a in 
  assert_true ([e; f; g; h] = [1; 2; 3; 4])

TEST_UNIT "TakeIterator_test4" = 
  let a = TIM.create 5 (ListIterator.create [1; 2; 3; 4]) in 
  let e = TIM.next a in 
  let f = TIM.next a in 
  let g = TIM.next a in
  let h = TIM.next a in 
  assert_true ([e; f; g; h] = [1; 2; 3; 4])

TEST_UNIT "TakeIterator_test5" = 
  let a = TIM.create 5 (ListIterator.create [1; 2; 3; 4]) in 
  let f x= (TIM.next x); () in 
  f a; 
  f a;
  f a;
  f a;
  assert_raises (Some (TIM.NoResult)) TIM.next a

TEST_UNIT "TakeIterator_test6" = 
  let a = TIM.create 5 (ListIterator.create [1; 2; 3; 4]) in 
  let f x= (TIM.next x); () in 
  f a; 
  f a;
  f a;
  f a;
  assert_false (TIM.has_next a)

TEST_UNIT "TakeIterator_test7" = 
  let a = TIM.create 3 (ListIterator.create [1; 2; 3; 4]) in 
  let f x= (TIM.next x); () in 
  f a; 
  f a;
  assert_true (TIM.has_next a)

TEST_UNIT "TakeIterator_test8" = 
  let a = TIM.create 3 (ListIterator.create [1; 2; 3; 4]) in 
  let f x= (TIM.next x); () in 
  f a; 
  f a;
  f a;
  assert_false (TIM.has_next a)

module IUM = IteratorUtilsFn (ListIterator)

TEST_UNIT "IteratorUtilsFn_test1" = 
  let a = ListIterator.create [1; 2; 3; 4] in 
  (IUM.advance 3 a);
  let x = ListIterator.next a in 
  assert_true (x = 4)

TEST_UNIT "IteratorUtilsFn_test2" = 
  let a = ListIterator.create [1; 2; 3; 4] in 
  (IUM.advance 2 a);
  let x = ListIterator.next a in 
  assert_true (x = 3)

TEST_UNIT "IteratorUtilsFn_test3" = 
  let a = ListIterator.create [1; 2; 3; 4] in 
  (IUM.advance 0 a);
  let x = ListIterator.next a in 
  assert_true (x = 1)

TEST_UNIT "IteratorUtilsFn_test4" = 
  let a = ListIterator.create [1; 2; 3; 4] in 
  assert_raises (Some (ListIterator.NoResult)) (IUM.advance 5) a

TEST_UNIT "IteratorUtilsFn_test5" = 
  let a = ListIterator.create [] in 
  (IUM.advance 0 a);
  assert_raises(Some (ListIterator.NoResult)) ListIterator.next a

TEST_UNIT "IteratorUtilsFn_test6" = 
  let a = ListIterator.create [] in 
  assert_raises(Some (ListIterator.NoResult)) (IUM.advance 1) a

TEST_UNIT "IteratorUtilsFn_test7" = 
  let a = ListIterator.create [1; 2; 3; 4; 5] in 
  let x = IUM.fold (fun acc e -> acc * e) 1 a in 
  let y = List.fold_left (fun acc e -> acc * e) 1 [1; 2; 3; 4; 5] in
  assert_true (x = y)

TEST_UNIT "IteratorUtilsFn_test8" = 
  let a = ListIterator.create [] in 
  let x = IUM.fold (fun acc e -> acc * e) 23 a in
  assert_true (x = 23) 

TEST_UNIT "IteratorUtilsFn_test7" = 
  let a = ListIterator.create [1; 2; 3; 4; 5] in 
  let x = IUM.fold (fun acc e -> "a") " " a in 
  assert_true (x = "a")

TEST_UNIT "IteratorUtilsFn_test8" = 
  let a = ListIterator.create[1; 2; 3; 4; 5] in 
  let f func acc m = (IUM.fold func acc m); () in 
  (f (fun acc e -> acc * e) 1 a);
  assert_false (ListIterator.has_next a)

TEST_UNIT "IteratorUtilsFn_test9" = 
  let a = ListIterator.create[1; 2; 3; 4; 5] in 
  let f func acc m = (IUM.fold func acc m); () in 
  (f (fun acc e -> acc * e) 1 a);
  assert_false (ListIterator.has_next a)

module RIM = RangeIterator (ListIterator)

TEST_UNIT "RangeIterator_test1" = 
  let a = RIM.create 3 5 (ListIterator.create [1; 2; 3; 4; 5]) in
  let x = RIM.next a in
  let y = RIM.next a in
  let z = RIM.next a in
  assert_true ([x; y; z] = [3; 4; 5])

(*Wrote the same test twice because first time I forgot to reset the counter
in create so the same test would pass the first time and fail the second.*)
TEST_UNIT "RangeIterator_test2" = 
  let a = RIM.create 3 5 (ListIterator.create [1; 2; 3; 4; 5]) in
  let x = RIM.next a in
  let y = RIM.next a in
  let z = RIM.next a in
  assert_true ([x; y; z] = [3; 4; 5])

TEST_UNIT "RangeIterator_test3" = 
  let a = RIM.create 5 3 (ListIterator.create [1; 2; 3; 4; 5]) in
  assert_raises (Some (RIM.NoResult)) RIM.next a

TEST_UNIT "RangeIterator_test4" =
  let a = RIM.create 5 3 (ListIterator.create [1]) in
  assert_raises (Some (RIM.NoResult)) RIM.next a

TEST_UNIT "RangeIterator_test5" =
  let a = RIM.create 5 3 (ListIterator.create []) in
  assert_raises (Some (RIM.NoResult)) RIM.next a

TEST_UNIT "RangeIterator_test6" =
  let a = RIM.create 3 6 (ListIterator.create [1; 2; 3]) in
  let f m = (RIM.next m); () in 
  f a;
  assert_raises (Some (RIM.NoResult)) RIM.next a

TEST_UNIT "RangeIterator_test7" = 
  let a = RIM.create 23 23 (ListIterator.create [1; 2; 3; 4; 5]) in
  assert_raises (Some (RIM.NoResult)) RIM.next a

TEST_UNIT "RangeIterator_test8" = 
  let a = RIM.create 23 25 (ListIterator.create [1; 2; 3; 4; 5]) in
  assert_raises (Some (RIM.NoResult)) RIM.next a

TEST_UNIT "RangeIterator_test9" = 
  let a = RIM.create 20 23 (ListIterator.create [1; 2; 3; 4; 5]) in
  assert_raises (Some (RIM.NoResult)) RIM.next a

TEST_UNIT "RangeIterator_test10" = 
  let a = RIM.create 2 2 (ListIterator.create [1; 2; 3; 4; 5]) in
  assert_true ((RIM.next a) = 2)

TEST_UNIT "RangeIterator_test11" = 
  let a = RIM.create 3 7 (ListIterator.create [1; 2; 3; 4; 5]) in
  let x = RIM.next a in
  let y = RIM.next a in
  let z = RIM.next a in
  assert_true ([x; y; z] = [3; 4; 5])