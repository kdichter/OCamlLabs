(* CSCI 330: Programming Assignment 5
 * misc5.ml
 *)

(* For this assignment, you may use the following library functions:

   List.map
   List.fold_left
   List.fold_right
   List.split
   List.combine
   List.length
   List.append
   List.rev

   See http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html for
   documentation.
*)



(* Do not change the skeleton code! The point of this assignment is to figure
 * out how the functions can be written this way (using fold). You may only
 * replace the   failwith "to be implemented"   part. *)



(*****************************************************************)
(******************* 1. Warm Up   ********************************)
(*****************************************************************)

(* sqsum : int list -> int
 * (sqsum xs) takes a list of integers
 * as input and returns an integer, which
 * is the sum of each value in the list squared
 * e.g. (sqsum [1;2;3;4] is 30)
 *)
let sqsum xs = 
  let f a x = a + x * x in 
  let base = 0 in
    List.fold_left f base xs

(* pipe : ('a -> 'a) list -> 'a -> 'a
 * (pipe fs s) takes a list of functions and a value
 * and applies the set of functions to the value to 
 * return an int
 * e.g. (pipe [(fun x-> 2*x);(fun x -> x + 3)] 3 is 9)
 *)
let pipe fs s = 
  let f a x = a |> x in
  let base = s in
    List.fold_left f base fs

(* pipec : ('a -> 'a) list -> 'a -> 'a
 * (pipec fs) takes a list of functions and uses
 * function currying to expect a second parameter
 * as the value to be applied to the list of functions
 * e.g. (pipec [(fun x-> 2*x);(fun x -> x + 3)] 3 is 9)
 *)
let pipec fs = 
  let f a x = fun z -> z |> a |> x in
  let base = fun y -> y in
    List.fold_left f base fs

(* sepConcat : string -> string list -> string
 * (sepConcat sep sl) accepts a list separator
 * and a list of strings and returns a string
 * where each element is separatred by the separator
 * e.g. (sepConcat ", " ["foo";"bar";"baz"] is "foo, bar, baz")
 *)
let rec sepConcat sep sl = match sl with 
  | [] -> ""
  | h :: t -> 
      let f a x = a ^ sep ^ x in
      let base = h in
      let l = t in
        List.fold_left f base l

(* stringOfList : ('a -> string) -> 'a list -> string
 * (stringOfList f l) accepts a function and a list
 * and converts that list to a string with the function
 * applied to each element in the list
 * e.g. (stringOfList string_of_int [1;2;3;4;5;6] is "[1; 2; 3; 4; 5; 6]")
 *)
let stringOfList f l = "[" ^ sepConcat "; " (List.map f l) ^ "]"

(* prodLists : int list -> int list -> int list
 * (prodLists l1 l2) takes two lists of integers
 * and returns a list of integers with the product
 * of each pair of numbers
 * e.g. (prodLists [3] [4] is 12)
 *)
let prodLists l1 l2 =
  let f a x = let (y, z) = x in a @ [y * z] in
  let base = [] in
  let args = List.combine l1 l2 in
    List.fold_left f base args

(*****************************************************************)
(******************* 2. Big Numbers ******************************)
(*****************************************************************)

(* clone : 'a -> int -> 'a list 

clone takes as input x and an integer n. The result is a list of length n, where each element is x. 
If n is 0 or negative, clone will return the empty list. 

# clone 3 5;;
- : int list = [3; 3; 3; 3; 3] 
# clone "foo" 2;;
- : string list = ["foo"; "foo"]
# clone clone (-3);;
- : ('_a -> int -> '_a list) list = [])
*)
let rec clone x n = if (n <= 0) then [] else x::(clone x (n - 1))

(*
padZero : int list -> int list -> int list * int list 

padZero takes two lists: [x1,...,xn] [y1,...,ym] and adds zeros in front to make the lists equal in length. 

# padZero [9;9] [1;0;0;2];;
- : int list * int list = ([0;0;9;9],[1;0;0;2]) 
# padZero [1;0;0;2] [9;9];;
- : int list * int list = ([1;0;0;2],[0;0;9;9]) 
*)
let rec padZero l1 l2 = 
   let l1len = List.length l1 in
   let l2len = List.length l2 in
    ((clone 0 (l2len-l1len)@l1), (clone 0 (l1len-l2len)@l2))

(*
removeZero : int list -> int list 

removeZero takes a list and removes a prefix of trailing zeros. 

# removeZero [0;0;0;1;0;0;2];;
- : int list = [1;0;0;2] 
# removeZero [9;9];;
- : int list = [9;9] 
# removeZero [0;0;0;0];;
- : int list = [] 
*)

let rec removeZero l = 
  match l with
  | 0::t -> removeZero t
  | _ -> l

let bigAdd l1 l2 = 
  let add (l1, l2) = 
    let f a x = 
    let (c, r) = a in
    let (y, z) = x in 
    ((c + y + z) / 10, ((c + y + z) mod 10) :: r) in
    let base = (0, []) in
    let args =  List.rev (List.combine l1 l2) in
    let (carry, res) = List.fold_left f base args in
      carry::res
  in 
    removeZero (add (padZero l1 l2))

(* EXTRA CREDIT BELOW *)

let rec mulByDigit i l =
  let mult (l1, l2) = 
    let f a x = failwith "to be implemented" in
    let base = failwith "to be implemented" in
    let args = failwith "to be implemented" in
    let (carry, res) = List.fold_left f base args in
      carry::res
  in 
    failwith "to be implemented"

let bigMul l1 l2 = 
  let f a x = failwith "to be implemented" in
  let base = failwith "to be implemented" in
  let args = failwith "to be implemented" in
  let (_, res) = List.fold_left f base args in
    res
