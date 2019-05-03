let rec pow x n = 
  powHelp x x n
  and powHelp x c = function
    | 0 -> 1
    | 1 -> x
    | n -> if (n < 0) 
           then 0 
           else powHelp (x*c) c (n-1);;

let rec float_pow x n =
  float_powHelp x x n
  and float_powHelp x c = function
    | 0 -> 1.0
    | 1 -> x
    | -1 -> 1.0/.x
    | n -> if (n < 0)
           then float_powHelp (x *. c) c (n+1)
           else float_powHelp (x *. c) c (n-1);;

let reverse lst = 
  let rec reverseHelp result = function
    | [] -> result
    | h :: t -> reverseHelp (h :: result) t
  in reverseHelp [] lst;;

let compress lst =
  let rec compressHelp result = function
    | [] -> reverse result
    | h :: t -> if (result = [])
                then compressHelp (h :: result) t
                else if (h = (List.hd result))
                     then compressHelp result t
                     else compressHelp (h :: result) t
  in compressHelp [] lst;;

let cluster lst =
  let rec clusterHelp result hold = function
    | [] -> reverse (hold::result)
    | h :: t -> if (hold = [])
                then clusterHelp result [h] t
                else if (h = (List.hd hold))
                     then clusterHelp result (h::hold) t
                     else clusterHelp (hold::result) [h] t
  in clusterHelp [] [] lst;;

let slice lst i j = 
  let rec sliceHelp lst c hold = match lst with
    | [] -> reverse hold
    | h :: t -> if (c < i)
                then sliceHelp t (c+1) hold
                else if (j <= c)
                     then reverse hold
                     else sliceHelp t (c+1) (h::hold)
  in sliceHelp lst 0 [];;

let composition f g = fun x -> f (g x);;

let rec equiv_on f g lst = match lst with
  | [] -> true
  | h :: t -> if ((f h) = (g h))
              then equiv_on f g t
              else false;;

let pairwisefilter fxn lst =
  let rec pairwisefilterHelp fxn lst hold = match lst with
    | [] -> reverse hold
    | [_] -> reverse ((List.hd lst) :: hold)
    | h :: t -> pairwisefilterHelp fxn (List.tl t) ((fxn (List.hd t) h)::hold)
  in pairwisefilterHelp fxn lst [];;

let polynomial lst = 
  let rec polynomialHelp lst fxn = match lst with
    | [] -> fxn
    | h :: t -> polynomialHelp t (fun x -> fxn x + ((fst h) * int_of_float ( float_of_int x ** float_of_int (snd h))))
  in polynomialHelp lst (fun x -> 0);;

type bool_expr = 
  | Lit of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr

let rec truth_tableHelp a boolA b boolB expr = match expr with
  | Lit l -> if l = a
             then boolA
             else boolB
  | Not ex -> not(truth_tableHelp a boolA b boolB ex)
  | And (ex1, ex2) -> truth_tableHelp a boolA b boolB ex1 && truth_tableHelp a boolA b boolB ex2
  | Or (ex1, ex2) -> truth_tableHelp a boolA b boolB ex1 || truth_tableHelp a boolA b boolB ex2;;

let truth_table a b expr= 
  [(true, true, truth_tableHelp a true b true expr);
   (true, false, truth_tableHelp a true b false expr);
   (false, true, truth_tableHelp a false b true expr);
   (false, false, truth_tableHelp a false b false expr)];;

type 'a binary_tree = 
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

let rec tree2str binTree = match binTree with
  | Empty -> ""
  | Node (r, Empty, Empty) -> string_of_int r
  | Node (r, Empty, c2) -> (string_of_int r) ^ "(" ^ "," ^ (tree2str c2) ^ ")"
  | Node (r, c1, Empty) -> (string_of_int r) ^ "(" ^ (tree2str c1) ^ "," ^ ")"
  | Node (r, c1, c2) -> (string_of_int r) ^ "(" ^ (tree2str c1) ^ "," ^ (tree2str c2) ^ ")";; 