module type ITERATOR = sig
  type 'a t
  exception NoResult

  (* returns: true if there are more results to yield,
   *   otherwise returns false. *)
  val has_next: 'a t -> bool

  (* returns:  the next result, if there are more results
   *   to yield.  Otherwise raises NoResult.
   * effects:  modifies the state of the iterator to record
   *   the yield. *)
  val next: 'a t -> 'a
end

module type LIST_ITERATOR = sig
  include ITERATOR
  (* parameters:  a list l
   * returns:  an iterator that will yield the elements of l,the
   *   each exactly once, in the order that they appear in l,
   *   starting with the head.  *)
  val create: 'a list -> 'a t
end


module ListIterator : LIST_ITERATOR = struct
  type 'a t = 'a list ref
  exception NoResult

  let create (l: 'a list): 'a t =
    ref l

  let has_next (i: 'a t): bool = 
    if (!i) = [] then false else true

  let next (i: 'a t): 'a = 
    if has_next i then let h = List.hd (!i) in (i := List.tl (!i); h) 
    else raise NoResult

end


type 'a tree = Leaf | Node of ('a * 'a tree * 'a tree)

module type INORDER_TREE_ITERATOR = sig
  include ITERATOR
  (* parameters:  a tree t
   * returns:  an iterator that will yield the elements of t,
   *   each exactly once, in the order that would be produced
   *   by an in-order traversal of t. *)
  val create: 'a tree -> 'a t
end


module InorderTreeIterator : INORDER_TREE_ITERATOR = struct
  type 'a t = 'a list ref
  exception NoResult
  
  let rec in_order (t: 'a tree): 'a list =
    match t with
    |Node (x, left, right) -> (in_order left)@[x]@(in_order right)
    |Leaf -> []
  
  let create (t: 'a tree): 'a t =
    ref (in_order t)

  let has_next (i: 'a t): bool = 
    if (!i) = [] then false else true

  let next (i: 'a t): 'a = 
    if has_next i then let h = List.hd (!i) in (i := List.tl (!i);h) 
    else raise NoResult
end


module type TAKE_ITERATOR = functor (I: ITERATOR) -> sig
  include ITERATOR

  (* parameters:  an integer n and an iterator i
   * returns:  an iterator that behaves the same as i for
   *   exactly n calls to next, but afterwards
   *   raises NoResult. *)
  val create: int -> 'a I.t -> 'a t
end


module TakeIterator : TAKE_ITERATOR = functor (I : ITERATOR) -> struct
  type 'a t = 'a I.t
  let counter = ref 0
  exception NoResult
  
  (*requires n to be nonnegative*)
  let create (n: int) (i: 'a I.t): 'a t =
  counter := n; i 

  let has_next (i: 'a t): bool=
    (!counter) > 0 && I.has_next i
 
  let next (i: 'a t): 'a=
    if has_next i then ((counter := !counter - 1); I.next i) else raise NoResult 

end


module IteratorUtilsFn (I : ITERATOR) = struct
  open I (*is this for anything special or just shorthand?*)

  (* effects: causes i to yield n results, ignoring
   *   those results.  Raises NoResult if i does.  *)
  let advance (n: int) (iter: 'a I.t) : unit =
    for i = 0 to n-1 do
      I.next iter; ()
    done

  (* returns: the final value of the accumulator after
   *   folding f over all the results returned by i,
   *   starting with acc as the initial accumulator.
   * effects: causes i to yield all its results. *)
  let rec fold (f : ('a -> 'b -> 'a)) (acc : 'a) (iter: 'b I.t) : 'a =
    if I.has_next iter then fold f (f acc (I.next iter)) iter else acc
end

module type RANGE_ITERATOR = functor (I : ITERATOR) -> sig
  include ITERATOR

  (* parameters: integers n and m and an iterator i
   * returns: an iterator that behaves the way I would
   *   on the nth through mth calls to next, and
   *   afterwards raises NoResult.
   *
   *   If n > m the resulting iterator should always raise NoResult.
   *)
  val create : int -> int -> 'a I.t -> 'a t
end


module RangeIterator : RANGE_ITERATOR = functor (I : ITERATOR) -> struct
  type 'a t = 'a I.t
  exception NoResult
  
  let counter = ref 0
  let nx = ref 0
  let mx = ref 0
  
  (*returns: unit. 
  effects: causes iter to yield n results, ignoring those result.
  increments counter by n.*)
  let move_head (iter: 'a t) : unit = 
    try
      for i = 1 to (!nx - 1) do
        I.next iter;
        (counter := !counter + 1); ()
      done  
    with 
      I.NoResult -> raise NoResult
  
  (*requires n <= m*)
  let create (n: int) (m: int) (iter: 'a I.t): 'a t = 
    (counter:= 0);
    (nx:= n);
    (mx:= m);
    iter
  
  let has_next (iter: 'a t): bool = I.has_next iter
  
  let next (iter: 'a t): 'a = 
    try
      if ((!counter >= !mx) || (mx < nx)) then raise NoResult
      else if (!counter < (!nx-1)) then ((move_head iter); I.next iter)
      else I.next iter
    with 
      I.NoResult -> raise NoResult
end

