(* CSCI 330: Lab 2
 * misc.ml
 *)

(* 
	***** PROVIDE COMMENT BLOCKS AND IMPLEMENTATIONS FOR THE FOLLOWING FUNCTIONS ***** 
	***** INCLUDE TYPE SIGNATURES ***** 
*)

(* average : float -> float -> float
 * (average a b) accepts two floats and
 * returns the new average of the two that
 * can be used for determing the new sqrt
 * e.g. (average 12. 6.) is 4.
 *)
let next a b = (b +. (a /. b)) *. 0.5;;

(* sqrt : float -> float -> float
 * (sqrt tol x) accepts a tolerance and a float and
 * returns the square root on that float based
 * on the tolerance
 * e.g. (sqrt 0.00001 5.) is 2.236
 *)
let sqrt tol x = 
    let rec helperSqrt tol x y = 
        if abs_float (y *. y -. x) < tol then y (*abs_float ((average x y) -. y) < tol then y*)
        else helperSqrt tol x (next x y)
    in helperSqrt tol x x;;

(* sqrt2 : float -> float
 * (sqrt2 x) accepts a float and returns 
 * the square root of that float with a
 * tolerance of .00001
 * e.g. (sqrt2 5.) is 2.236
 *)
let rec sqrt2 = fun x -> sqrt 0.00001 x ;;

(* factorial1 : int -> int
 * (factorial1 x) accepts an int and
 * returns the factorial of that number
 * using an if-then-else statement
 * e.g. (fctorial1 5) is 120
 *)
let rec factorial1 x = if x = 0 then 1 else x * (factorial1 (x - 1));;

(* factorial2 : int -> int
 * (factorial2 x) accepts an int and
 * returns the factorial of that number
 * using a match statement
 * e.g. (fctorial2 5) is 120
 *)
let rec factorial2 x = 
    match x with
    | 0 -> 1
    | _ -> x * (factorial2 (x - 1));;

(* factorial3 : int -> int
 * (factorial3 x) accepts an int and
 * returns the factorial of that number
 * using tail recursion
 * e.g. (fctorial1 5) is 120
 *)
let factorial3 x =
    let rec helper3 x acc = 
        match x with
        | 0 -> acc
        | _ -> (helper3 (x - 1)) (acc * x)
    in helper3 x 1;;

(* fibonacci : int -> int
 * (fibonacci x) accepts an int and
 * returns the nth fibonacci number
 * in the sequence
 * e.g. (fibonacci 3) is 2
 *)
let fibonacci x = 
    let rec helperFib x prev curr =
        match x with
        | 0 -> prev
        | 1 -> curr
        | _ -> helperFib (x - 1) (curr) (prev + curr)
    in helperFib x 0 1;;

(* rev : 'a list -> 'a list
 * (rev lst) accepts a list and 
 * reverses the order of the elements
 * in the list
 * e.g. (rev [1;2;4;5;7;8]) is [8;7;5;4;2;1]
 *)
let rev lst = 
    let rec helperRev lst acc = 
        match lst with
        | [] -> acc
        | h::t -> (helperRev t) (h::acc)
    in helperRev lst [];; 

(* map : ('a -> 'b) -> 'a list -> 'b list
 * (map func lst) accepts a function and
 * a list and returns a list with the
 * function applied to each element in
 * the list
 * e.g. (map factorial3 [3;4;5]) is [6;24;120]
 *)
let rec map func lst = 
    match lst with
    | [] -> []
    | h::t -> (func h)::(map func t);;

(* map2 : ('a -> 'b) -> 'a list -> 'b list
 * (map2 func lst) accepts a function and
 * a list and returns a list with the
 * function applied to each element in
 * the list using tail recursion
 * e.g. (map2 factorial3 [3;4;5]) is [6;24;120]
 *)
let map2 func lst = 
    let rec helperMap func lst acc = 
    match lst with
    | [] -> acc
    | h::t -> helperMap func t (acc @ [func h])
    in helperMap func lst [];;

(* range : int -> int -> int list
 * (map func lst) accepts two ints
 * that represents a range of ints from
 * the first to the second and returns a list
 * of every int between the two ints including them
 * e.g. (range 2 5) is [2;3;4;5]
 *)
let rec range a b = if a > b then [] else a::(range (a + 1) b);;

let roots : float list = map2 sqrt2 (map2 float_of_int (range 1 20));;



