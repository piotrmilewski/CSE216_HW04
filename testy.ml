(* part 1 *)
pow 2 5;;
pow 2 (-5);;
pow 0 1;;
pow 1 0;;
pow 0 0;;
pow (-2) 2;;
pow (-2) 3;;

float_pow 2.5 5;;
float_pow 2.5 (-5);;
float_pow 0.0 1;;
float_pow 1.0 0;;
float_pow 0.0 0;;

reverse ["a"; "b"; "c"];;
reverse [1; 2];;
reverse [];;
reverse [1; 1];;

compress ["a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e"];;
compress ["a";"a";"a";"a"];;
compress [];;
compress [1;1;1];;
compress [1;2;2;3;2;2;3;3;5];;

cluster ["a";"a";"a";"b";"c";"c";"a";"a"];;
cluster [];;
cluster [1;1;1;1;2;2;1;1;3;8];;

slice ["a";"b";"c";"d";"e";"f";"g";"h"] 2 6;;
slice ["a";"b";"c";"d";"e";"f";"g";"h"] 3 2;;
slice ["a";"b";"c";"d";"e";"f";"g";"h"] 3 20;;
slice [] 8 20;;
slice [] 0 0;;
slice [1;2;3;5;6] 3 3;;
slice [1;1;5;5;5;3;5;2] 18 19;;
slice [1;1;5;5;5;3;5;2] 3 1;;

(* part 2 *)
let square x = x*x;;
let increment x = x+1;;
let square_of_increment = composition square increment;;
square_of_increment 4;;

let decrease x = x-1;;
let cube x = x*x*x;;
let cube_of_decrease = composition cube decrease;;
cube_of_decrease 4;;

let f i = i * i;;
let g i = 3 * i;;
equiv_on f g [3];;
equiv_on f g [1;2;3];;
equiv_on f g [3;3;3];;
let h x = 27 / x;;
let j x = 28 / x;;
equiv_on h j [3;5;6];;
equiv_on h j [];;

let shorter a1 a2 = if (String.length a1 < String.length a2)
                    then a1
                    else if (String.length a1 = String.length a2)
                         then a1
                         else a2;;
pairwisefilter min [14; 11; 20; 25; 10; 11];;
pairwisefilter min [14; 11; 20; 25; 10; 11; 9];;
pairwisefilter shorter ["and"; "this"; "makes"; "shorter"; "strings"; "always"; "win"];;
pairwisefilter min [];;
pairwisefilter min [1;1;1;1;1;1;1;1];;
pairwisefilter min [1;1;1;1;1;1;1;1;1];;

let f = polynomial [3, 3; -2, 1; 5, 0];;
f 2;;
f 0;;
f (-1);;
let g = polynomial [];;
g 1;;

(* part 3 *)
truth_table "a" "b" (And(Lit("a"), Lit("b")));;
truth_table "a" "b" (And(Or(Lit("a"), Lit("b")), Not(Lit("a"))));;
truth_table "a" "b" (Lit("a"));;
truth_table "a" "b" (Not(Not(Lit("a"))));;

let a_tree = Node(1, Node(2, Node(4, Empty, Empty), Node(5, Empty, Empty)), Node(3, Node(6, Empty, Empty), Node(7, Empty, Empty)));;
let b_tree = Node(0, Node(1, Node(3, Empty, Empty), Node(4, Empty, Empty)), Node(2, Empty, Node(5, Node(6, Empty, Empty), Empty)));;
let c_tree = Node(0, Empty, Empty);;
let d_tree = Empty;;
tree2str a_tree;;
tree2str b_tree;;
tree2str c_tree;;
tree2str d_tree;;