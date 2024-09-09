


(* Non tail recursive *)
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

(* Tail recursive *)
let map2 func lst = 
    let rec helperMap func lst acc = 
    match lst with
    | [] -> acc
    | h::t -> helperMap func t (acc @ [func h])
    in helperMap func lst [];;

let complex l = l
    |> map2 float_of_int
    |> map2 string_of_float

let complex2 l =
    map2 (fun x -> float_of_int x |> string_of_float) l

let centroid list =
    let rec average list count (xsum, ysum) = (*((xsum, ysum) as sums) = *)
        match list with
        | [] -> if count = 0 then (0.0, 0.0) else (xsum /. float_of_int count, ysum /. float_of_int count)
        | (x,y)::t -> average t (count + 1) (xsum +. x, ysum +. y)
        in average list 0 (0.0, 0.0)

let f x =
  let xx = x * x * x
  in (xx, xx < 100);;

let rec wwhile (f,b) = 
    match f b with
    | (_, false) -> let (x, y) = f b in x
    | _ -> let (x, y) = f b in wwhile (f, x)

let memCheck list value = if List.mem value list then 1 else 0

(* returns true if item in list is found, false otherwise *)
let rec memCheck2 (list, value) = 
    match list with
    | [] -> false
    | h::t -> if List.mem value list then true else false
