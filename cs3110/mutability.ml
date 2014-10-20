(*count_up_from n k returns a function f of type unit -> int such that for the
i-th time f is called, f returns n+(i-1)*k. Requires n and k to be integers.*)
let count_up_from n k = 
  let c = ref n in
  fun () -> (c := (!c) + k;!c - k)

(*tabulate f n returns an array of length n where the element at index i in
the array is equal to f i. Requires that f is a pure function defined for all
integers greater than or equal to 0, and n is an integer greater than or equal
to 0. *)
let tabulate f n = 
	let a = Array.make n (f 0) in 
	  let rec tabulate_helper f n a= 
	    match n with
	    |(-1) -> ()
	    | _ -> (Array.set a n (f n)); 
	          (tabulate_helper f (n-1) a)
	  in tabulate_helper f (n-1) a; a

(*fold_left_imp (f:'a->'b->'a) (acc:'a) ([b1; b2; b3; ...; bn]:'b list) returns  
f (...(f (f (f acc b1) b2) b3)...) bn. *)
let fold_left_imp f acc xs = 
  let accref = ref acc in 
    let lst = ref xs in
      while (!lst != [] ) do
        match !lst with
        |h::t -> accref := f !accref h; lst := t
        |[] -> ()
      done; !accref

type t = int ref  (*TODO: change unit to whatever you want *)
type u = int  (*TODO: change unit to whatever you want *)

let lst : t list = let a = ref 0 in let x = a in [x; a; ref 1] 

(* Zardoz (x: int ref): int returns the int that x points to. 
Effects: changes the value that x points to to 23. Requires that x has type 
int ref.*)
let zardoz (x:t) : u = 
  let m= !x in 
  (x:=23); m