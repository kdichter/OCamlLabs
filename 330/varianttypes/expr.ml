(* 
 * Name: Kevin Dichter
 * Date: YOUR LAST DATE MODIFIED HERE
 * Course: CSCI 330 - Programming Languages
 * Assignment: Variant Types
 *
 * Assignment Attribution:
 *   This lab is based on code by Chris Stone (lab from CSE 130 by Sorin Lerner at UCSD)
 *
 * Description: YOUR DESCRIPTION HERE
 *)

let pi = 4.0 *. (atan 1.0)

type expr = 
    VarX
  | VarY
  | Sine     of expr
  | Cosine   of expr
  | Average  of expr * expr
  | Times    of expr * expr
  | Thresh   of expr * expr * expr * expr	
  | TimesDiv2Plus  of expr * expr * expr
  | Divide10XPlus1  of expr
(* TODO: add two new "types" of expressions *)

type rng = int * int -> int
type builder_fun = rng * int -> expr

let rec exprToString e =
  match e with
  | VarX -> "x"
  | VarY -> "y" 
  | Sine x -> "sin(pi*" ^ (exprToString x) ^ ")"
  | Cosine x -> "cos(pi*" ^ (exprToString x) ^ ")"
  | Average (x, y) -> "((" ^ (exprToString x) ^ "+" ^ (exprToString y) ^ ")/2)"
  | Times (x, y) -> (exprToString x) ^ "*" ^ (exprToString y)
  | Thresh (x, y, a, b) -> "(" ^ (exprToString x) ^ "<" ^ (exprToString y) ^ "?" ^ (exprToString a) ^ ":" ^ (exprToString b) ^ ")"
  | TimesDiv2Plus (x, y, a) -> "((" ^ (exprToString x) ^ "*" ^ (exprToString y) ^ ")/(2+" ^ (exprToString a) ^ "))"
  | Divide10XPlus1 x -> "(" ^ (exprToString x) ^ "/((10*x)+1))"


(* build functions:
     Use these helper functions to generate elements of the expr
     datatype rather than using the constructors directly.  This
     provides a little more modularity in the design of your program *)

let buildX()                       = VarX
let buildY()                       = VarY
let buildSine(e)                   = Sine(e)
let buildCosine(e)                 = Cosine(e)
let buildAverage(e1,e2)            = Average(e1,e2)
let buildTimes(e1,e2)              = Times(e1,e2)
let buildThresh(a,b,a_less,b_less) = Thresh(a,b,a_less,b_less)
let buildTimesDiv2Plus(e1, e2, e3) = TimesDiv2Plus (e1,e2,e3)
let buildDivide10XPlus1(e)         = Divide10XPlus1(e)

(* TODO: add two new buildXXXXXXX functions *)


let rec eval (e, x, y) =
  match e with
  | VarX -> x
  | VarY -> y
  | Sine a -> sin (pi *. eval (a,x,y))
  | Cosine a -> cos (pi *. eval(a,x,y))
  | Average (a, b) -> ((eval (a,x,y)) +. (eval (b,x,y))) /. 2.
  | Times (a, b) -> (eval (a,x,y)) *. (eval (b,x,y))
  | Thresh (a, b, c, d) -> if (eval (a,x,y)) < (eval (b,x,y)) then (eval (c,x,y)) else (eval (d,x,y))
  | TimesDiv2Plus (a, b, c) -> (eval (a,x,y) *. eval (b,x,y)) /. (eval (c,x,y) +. 2.)
  | Divide10XPlus1 a -> (eval (a,x,y) /. ((10. *. x) +. 1.))

  (* code breaks with doRandomColor2 (5, 60, 118);; *)

(* (eval_fn e (x,y)) evaluates the expression e at the point (x,y) and then
 * verifies that the result is between -1 and 1.  If it is, the result is returned.  
 * Otherwise, an exception is raised.
 *)
let eval_fn e (x,y) = 
  let rv = eval (e,x,y) in
  assert (-1.0 <= rv && rv <= 1.0);
  rv

let sampleExpr =
      buildCosine(buildSine(buildTimes(buildCosine(buildAverage(buildCosine(
      buildX()),buildTimes(buildCosine (buildCosine (buildAverage
      (buildTimes (buildY(),buildY()),buildCosine (buildX())))),
      buildCosine (buildTimes (buildSine (buildCosine
      (buildY())),buildAverage (buildSine (buildX()), buildTimes
      (buildX(),buildX()))))))),buildY())))

let sampleExpr2 =
  buildThresh(buildX(),buildY(),buildSine(buildX()),buildCosine(buildY()))




(******************* Functions you need to write **********)

(* build: (int*int->int) * int -> Expr 
   Build an expression tree.  The second argument is the depth, 
   the first is a random function.  A call to rand(2,5) will give
   you a random number in the range [2,5)  
   (2 inclusive, and 5 exclusive).

   Your code should call buildX, buildSine, etc. to construct
   the expression.
*)

let rec build (rand,depth) = 
if depth = 0 then
  let random = rand (1, 3) in
    match random with
    | 1 -> buildX()
    | _ -> buildY()
else 
  let random = rand (1, 9) in
  match random with
  | 1 -> buildSine(build (rand, depth - 1))
  | 2 -> buildCosine(build (rand, depth - 1))
  | 3 -> buildAverage(build (rand, depth - 1), build (rand, depth - 1))
  | 4 -> buildAverage(build (rand, depth - 1), build (rand, depth - 1))
  | 5 -> buildTimes(build (rand, depth - 1), build (rand, depth - 1))
  | 6 -> buildTimes(build (rand, depth - 1), build (rand, depth - 1))
  | _ -> buildThresh(build (rand, depth - 1), build (rand, depth - 1), build (rand, depth - 1), build (rand, depth - 1))

let rec build2 (rand,depth) = 
if depth = 0 then
  let random = rand (1, 3) in
    match random with
    | 1 -> buildX()
    | _ -> buildY()
else 
  let random = rand (1, 11) in
  match random with
  | 1 -> buildSine(build (rand, depth - 1))
  | 2 -> buildCosine(build (rand, depth - 1))
  | 3 -> buildAverage(build (rand, depth - 1), build (rand, depth - 1))
  | 4 -> buildAverage(build (rand, depth - 1), build (rand, depth - 1))
  | 5 -> buildTimes(build (rand, depth - 1), build (rand, depth - 1))
  | 6 -> buildTimes(build (rand, depth - 1), build (rand, depth - 1))
  | 7 -> buildTimesDiv2Plus(build (rand, depth - 1), build (rand, depth - 1), build (rand, depth - 1))
  | 8 -> buildDivide10XPlus1(build (rand, depth - 1))
  | _ -> buildThresh(build (rand, depth - 1), build (rand, depth - 1), build (rand, depth - 1), build (rand, depth - 1))

(* g1,c1 : unit -> ((int*int->int) * int -> Expr) * int * int * int
 * these functions should return the parameters needed to create your 
 * top color / grayscale pictures.
 * they should return (function,depth,seed1,seed2)
 * Function should be build or build2 (whichever you used to create
 * the image)
 *)

let g1 () = (build, 8, 5, 10);; 

let c1 () = (build, 8, 10, 15);; 