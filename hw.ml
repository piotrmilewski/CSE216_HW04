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

(* get rid of later*)
let square x = x*x;;
let increment x = x+1;;
let square_of_increment = composition square increment;;

let f i = i * i;;
let g i = 3 * i;;

let shorter a1 a2 = if (String.length a1 < String.length a2)
                    then a1
                    else if (String.length a1 = String.length a2)
                         then a1
                         else a2;;

pow 2 5;;
pow 2 (-5);;
float_pow 2.5 5;;
float_pow 2.5 (-5);;
reverse ["a"; "b"; "c"];;
reverse [1; 2];;
compress ["a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e"];;
cluster ["a";"a";"a";"b";"c";"c";"a";"a"];;
slice ["a";"b";"c";"d";"e";"f";"g";"h"] 2 6;;
slice ["a";"b";"c";"d";"e";"f";"g";"h"] 3 2;;
slice ["a";"b";"c";"d";"e";"f";"g";"h"] 3 20;;
square_of_increment 4;;
equiv_on f g [3];;
equiv_on f g [1;2;3];;
equiv_on f g [3;3;3];;
pairwisefilter min [14; 11; 20; 25; 10; 11];;
pairwisefilter min [14; 11; 20; 25; 10; 11; 9];;
pairwisefilter shorter ["and"; "this"; "makes"; "shorter"; "strings"; "always"; "win"];;
let f = polynomial [3, 3; -2, 1; 5, 0];;
f 2;;