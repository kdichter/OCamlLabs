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
  | Pythagorean  of expr * expr * expr
  | DivideX  of expr
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
  | Pythagorean (x, y, a) -> "((" ^ (exprToString x) ^ "*" ^ (exprToString x) ^ ")+(" ^ (exprToString y) ^ "*" ^ (exprToString y) ^ "))/(" ^ (exprToString a) ^ "*" ^ (exprToString a) ^ ")"
  | DivideX x -> "(" ^ (exprToString x) ^ "/x)"

  (* (x<y?x:sin(pi*x)*cos(pi*((x+y)/2))) *)

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
let buildPythagorean(e1, e2, e3)   = Pythagorean (e1,e2,e3)
let buildDivideX(e)                = DivideX(e)

(* TODO: add two new buildXXXXXXX functions *)


let rec eval (e, x, y) =
  let clamp v = if v < -1. || v > 1. then 0. else v in
  match e with
  | VarX -> x
  | VarY -> y
  | Sine a -> sin (pi *. eval (a,x,y))
  | Cosine a -> cos (pi *. eval(a,x,y))
  | Average (a, b) -> ((eval (a,x,y)) +. (eval (b,x,y))) /. 2.
  | Times (a, b) -> (eval (a,x,y)) *. (eval (b,x,y))
  | Thresh (a, b, c, d) -> if (eval (a,x,y)) < (eval (b,x,y)) then (eval (c,x,y)) else (eval (d,x,y))
  | Pythagorean (a, b, c) -> clamp (((eval (a,x,y) *. eval (a,x,y)) +. (eval (b,x,y) *. eval (b,x,y))) /. (eval (c,x,y) *. eval (c,x,y)))
  | DivideX a -> clamp (eval (a,x,y) /. x)

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
  let random = rand (1, 8) in
  match random with
  | 1 -> buildSine(build (rand, depth - 1))
  | 2 -> buildCosine(build (rand, depth - 1))
  | 3 -> buildAverage(build (rand, depth - 1), build (rand, depth - 1))
  | 4 -> buildTimes(build (rand, depth - 1), build (rand, depth - 1))
  | _ -> buildThresh(build (rand, depth - 1), build (rand, depth - 1), build (rand, depth - 1), build (rand, depth - 1))

let rec build2 (rand,depth) = 
if depth = 0 then
  let random = rand (1, 3) in
    match random with
    | 1 -> buildX()
    | _ -> buildY()
else 
  let random = rand (1, 8) in
  match random with
  | 1 -> buildSine(build (rand, depth - 1))
  | 2 -> buildCosine(build (rand, depth - 1))
  | 3 -> buildAverage(build (rand, depth - 1), build (rand, depth - 1))
  | 4 -> buildTimes(build (rand, depth - 1), build (rand, depth - 1))
  | 5 -> buildPythagorean(build (rand, depth - 1), build (rand, depth - 1), build (rand, depth - 1))
  | 6 -> buildDivideX(build (rand, depth - 1))
  | _ -> buildThresh(build (rand, depth - 1), build (rand, depth - 1), build (rand, depth - 1), build (rand, depth - 1))

(* g1,c1 : unit -> ((int*int->int) * int -> Expr) * int * int * int
 * these functions should return the parameters needed to create your 
 * top color / grayscale pictures.
 * they should return (function,depth,seed1,seed2)
 * Function should be build or build2 (whichever you used to create
 * the image)
 *)

let g1 () = failwith "to be implemented"  

let c1 () = failwith "to be implemented"