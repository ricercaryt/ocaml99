open! Core

(* Flatten a list*)


type 'a node =
  | One of 'a
  | Many of 'a node list 

let rec flatten (nodes : 'a node list) = match nodes with
  | [] -> []
  | x::xs -> match x with
    | One node -> node :: (flatten xs)
    | Many ys -> (flatten ys) @ (flatten xs)

(*elimite duplicates*)

let rec compress (s : string list) (a:string) = match s with 
  | [] -> []
  | x::xs -> if (String.equal x a) then compress xs a else a :: (compress xs x)

(* run length encoding *)
let encode (s : string list) : (int * string) list = 
  let rec helper (ss : string list) (a : string) (n : int) = 
    match ss with 
    | [] -> []
    | x :: xs -> if (String.equal x a) then helper xs a (n+1)
    else if n = 0 then helper xs a 1
    else (n, x) :: (helper xs a 1) 
  in (helper s "" 0)

(* modified run length encoding *)

type 'a rle =
  One of 'a 
  | Many of int * 'a

let encode2 (s : string list) : (string rle list) = 
  let rec helper (ss : string list) (a : string) (n : int) =
    match ss with 
    [] -> []
    | x :: xs -> if not (String.equal x a) then 
                    if (n = 1) then (One a) :: helper xs x 1
                    else if (n = 0) then helper xs x 1
                    else (Many (n, a)) :: helper xs x 1
                  else helper xs x (n + 1) 
  in (helper s "" 0) 

(* decode a run-length encoded list*)

let rec repeat a n = match n with 
  0 -> []
  | k -> a :: repeat a (k-1)

let rec decode (s : string rle list) : string list =
  match s with 
  | [] -> []
  | x :: xs -> match x with 
    | One a -> a :: (decode xs)
    | Many (n, a) -> (repeat a n) @ decode xs 

let replicate s =
  List.map (fun a -> [a, a]) s |> List.flatten 

let rec drop nth s =
  if nth = 0 then List.tl s 
  else (List.hd s) :: drop (nth - 1) (List.tl s) 

let rec split s fst lst length : string list list =
  if length = 0 then [fst] @ [lst] 
  else split (List.tl s) (fst @ [List.hd s]) (List.tl s) (length - 1) 


let rotate s n = 
  let sr = split s [] [] n 
  in (List.rev (List.hd sr)) @ (List.hd (List.rev sr))

let random_select (lst: 'a list) : 'a list =
  let open Random in 
  let rec select n =
    if Random.bool () then [n] else []
  in List.map select lst |> List.flatten 

let random_num n max_num =
  let open Random in 
  let rec select k = 
    if k = 0 then []
    else (Random.int max_num) :: select (k - 1)
  in select n   

let shuffle lst : 'a list =
  List.fast_sort (fun _ -> fun _ -> (Random.int (3) - 1)) lst  

let rec extract (n: int) (lst : 'a list) : 'a list list =
  if List.length lst < n then []
  else if n = 1 then List.map (fun x -> [x]) lst
  else if List.length lst = n then [lst]
  else let l1 = extract n (List.tl lst) 
      in let l2 = extract (n - 1) (List.tl lst) 
  in l1 @ (List.map (fun l -> (List.hd lst) :: l) l2) 

let is_prime (n:int) : bool =
  let rec helper m k = if m * m > k then true 
                       else if m * (k / m) = k then false 
                       else helper (m + 1) k 
                      in helper 2 n 

let rec gcd m n = if m < n then gcd n m 
else if n = 0 then m
else gcd n (m mod n)

let coprime m n = (gcd m n) = 1 

let euler_quotient n =
  let phi = Array.init (n + 1) (fun i -> i) in 
  for i = 2 to n do 
    if phi.(i) = i then 
      for j = 2 to (n/i) do 
        phi.(i*j) <- phi.(i*j) * (i-1) / i 
      done
  done ;
  phi.(n) 

let prime_factors n = 
  Array.fold_left (fun xs -> fun x -> 
    if is_prime x then x :: xs 
    else xs) [] (Array.init (n) (fun i -> i)) 

type bool_expr =
   Var of string 
  | Not of bool_expr 
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr

let rec table2_helper (a: string) (a_bool: bool) (b: string) (b_bool: bool) (expr: bool_expr) =
  let eval exp = table2_helper a a_bool b b_bool exp in 
    match expr with 
    Var x -> if x = a then a_bool else b_bool 
    | Not exp -> not (eval exp)
    | And (x, y) -> (eval x) && (eval y)
    | Or (x, y) -> (eval x) || (eval y) 
  
let table2 (a: string) (b: string) (expr: bool_expr) = 
  [(true, true, table2_helper a true b true expr),
  (true, false, table2_helper a true b false expr),
  (false, true, table2_helper a false b true expr),
  (false, false, table2_helper a false b false expr)]

let rec table_helper (tbl: (string, bool) Hashtbl.t) (expr: bool_expr) =
  match expr with 
  Var x -> Hashtbl.find tbl x 
  | Not exp -> table_helper tbl exp |> not 
  | And (x, y) -> (&&) (table_helper tbl x) (table_helper tbl y)
  | Or (x, y) -> (||) (table_helper tbl x) (table_helper tbl y) 

let table (ss: string list) (expr: bool_expr) = 
  let rec _table prelst (tbl: (string, bool) Hashtbl.t) lst =
    match lst with 
    | [] -> [prelst @ [table_helper tbl expr]]
    | x::xs -> Hashtbl.add tbl x true; 
     let t1 = _table (prelst@[true]) tbl xs in 
     Hashtbl.add tbl x false; 
     let t2 = _table (prelst@[false]) tbl xs in 
     t1 @ t2
  in _table [] (Hashtbl.create (List.length ss)) ss  

let rec graycode n : string list=
  if n = 0 then []
  else if n = 1 then ["0"; "1"]
  else let res = graycode (n-1)
  in (List.map (fun ss -> ["0" ^ ss; "1" ^ ss]) res) |> List.flatten



