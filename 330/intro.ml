(* Author:      Kevin Dichter
 * Instructor:  Dr. Schwartz
 * Date:        LAST MODIFICATION DATE
 * Assignment:  Lab 1 - Introduction to OCaml
 * Description: DESCRIPTION OF LAB GOES HERE
 *)

(* BEGIN PROVIDED FUNCTIONS *)

(* explode : string -> char list
 * (explode s) is the list of characters in the string s in the order in
 *   which they appear
 * e.g.  (explode "Hello") is ['H';'e';'l';'l';'o']
 *)
let explode s =
        let rec _exp i =
                if i >= String.length s then [] else (s.[i])::(_exp (i+1)) in _exp 0;;

(* END PROVIDED FUNCTIONS *)


(* For ALL of the following method stubs (those with failwith "to be written"),
   add documentation comments including expected behavior *)

(* sumList : int list -> int 
 * (sumList l) accepts a list of integers and returns a sum of
 * the elements in the list
 * e.g. (numList [1;2;3;4]) is 10
 *)
let rec sumList l = 
        match l with
        | [] -> 0
        | h::t -> h + sumList t;;

(* digitsOfInt : int -> int list
 * (digitsOfInt) accepts a positive integer and returns a list of
 * all digits of the integer value
 * e.g. (numList 1234) is [1;2;3;4]
 *)
let rec digitsOfInt n = 
     (* if n = 0 then [] else (n mod 10)::(digitsOfInt (n / 10)) *)
        match n with
        | 0 -> []
        (* | _ -> (n mod 10)::(digitsOfInt (n / 10));; *)
        | _ -> (digitsOfInt (n / 10)) @ [(n mod 10)];;

(* digitsOfInt : int -> int
 * (digitsOfInt) accepts an integer, sums the digits together,
 * repeats until only 1 digit remains, and returns a value that
 * represents the number of additions required to get to 1 digit
 * e.g. (additivePersistence 9876) is 2
 *)
let rec additivePersistence n = if n < 10 then 0 else 1 + (additivePersistence (sumList (digitsOfInt n)));;

(* digitalRoot : int -> int
 * (digitalRoot) accepts an integer, sums the digits together,
 * repeats until only 1 digit remains, and returns that 
 * last digit
 * e.g. (digitalRoot 1256) is 5
 *)
let rec digitalRoot n = if n < 10 then n else (digitalRoot (sumList (digitsOfInt n)));;;;

(* listReverse : 'a list -> 'a list
 * (listReverse) reverses the elements in a list
 * e.g. (listReverse 1234) is [1;2;3;4]
 *)
let rec listReverse l =
       match l with
        | [] -> []
        | h::t -> listReverse t @ [h];;

(* palindrome : string -> bool
 * (palindrome) checks whether a string is a palindrome, 
 * which means the string is the same from left-to-right
 * as it is right-to-left
 * e.g. (palindrome "racecar") is true
        (palindrome "computer") is false
 *)
let palindrome w = if (explode (w)) = (listReverse (explode w)) then true else false;;



(* BEGIN PROVIDED FUNCTIONS *)

(* digits : int -> int list
 * (digits n) is the list of digits of n in the order in which they appear
 * in n
 * e.g. (digits 31243) is [3,1,2,4,3]
 *      (digits (-23422) is [2,3,4,2,2]
 *)
let digits n = digitsOfInt (abs n);;

(* END PROVIDED FUNCTIONS *)

(************** Add Testing Code Here ***************)


