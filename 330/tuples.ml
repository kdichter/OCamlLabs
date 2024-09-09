(* CSCI 330: OCaml Lab 3
 * misc3.ml
 *)

(* ***** DOCUMENT ALL FUNCTIONS YOU WRITE OR COMPLETE ***** *)

(* assoc : 'a * 'b * ('b * 'a) list -> 'a
 * (assoc (d,k,l) accepts a tuple of values
 where d is the default value, k is the key,
 and l is a list of key-value pairs, and returns
 the value of that of a pair if k matches. Otherwise
 returns d
 * e.g. (assoc (-1,"jeff", [("sorin",85);("jeff",23);("moose",44)]) is 23)
 *)
let rec assoc (d,k,l) = 
    match l with
    | [] -> d
    | (x, y)::t -> if x = k then y else assoc (d, k, t)

(* removeDuplicates : 'a list -> 'a list
 * (removeDuplicates l) returns a list of elements 
 where all duplicate elements are removed and all
 the remaining elements maintain their order
 * e.g. (removeDuplicates [1;6;2;4;12;2;13;6;9] is [1;6;2;4;12;13;9])
 *)
let removeDuplicates l = 
  let rec helper (seen,rest) = 
      match rest with 
      |  [] -> seen
      | h::t -> 
        let seen' = if List.mem h seen then seen else h::seen in
        let rest' = t in
	  helper (seen',rest') 
  in
      List.rev (helper ([],l))

let f x =
  let xx = x * x * x
  in (xx, xx < 100);;

(* wwhile : ('a -> 'a * bool) * 'a -> 'a
 * (wwhile (f,b)) takes in a tuple (f,b) and calls
 the function f on input b to get a pair (b', c').
 wwhile continues to call f on b' as long as c' is true
 * e.g. (wwhile (f, 2) is 512) 
 *)
let rec wwhile (f,b) = 
    let result = f b in
    match result with
    | (_, false) -> let (x, y) = result in x
    | _ -> let (x, y) = result in wwhile (f, x)

(* fill in the code wherever it says : failwith "to be written" *)
let g x = truncate (1e6 *. cos (1e-6 *. float x));;
let h x = x * x;;

(* fixpoint : ('a -> 'a) * 'a -> 'a
 * (fixpoint (f,b)) takes in a tuple (f,b) and
 updates updates b with f(b) until b = f(b)
 * e.g. (fixpoint (g,0) is 739085)
 *)
let fixpoint (f,b) = wwhile ((fun x -> if f x = x then (f x, false) else (f x, true)),b)


(* ffor: int * int * (int -> unit) -> unit
   Applies the function f to all the integers between low and high
   inclusive; the results get thrown away.
 *)

let rec ffor (low,high,f) = 
  if low>high 
  then () 
  else let _ = f low in ffor (low+1,high,f)
