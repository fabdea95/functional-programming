(*GRAPHS INITIALIZATION*)

(*grafo con cricca da 3*)
let f = function
 1 -> [2;5]
|2 -> [1;3;5]
|3 -> [2;4;6]
|4 -> [3]
|5 -> [1;2;6]
|6 -> [3;5;7]
|7 -> [6]
|_ -> [];;

(*grafo con cricca da 4*)
let f = function
 1 -> [2]
|2 -> [3;5;6]
|3 -> [2;5;6]
|4 -> [3]
|5 -> [2;3;6]
|6 -> [2;3;5]
|7 -> [6]
|_ -> [];;

(*grafo con cricca da 5*)
let f = function 
 1 -> [2;3;4;5]
|2 -> [1;3;4;5]
|3 -> [1;2;4;5;6]
|4 -> [1;2;3;5]
|5 -> [1;2;3;4]
|6 -> [3;7]
|7 -> [6]
|_ -> [];;


(*grafo senza cricca*)
let f = function
 1 -> [2;3]
|2 -> [1;3;4]
|3 -> [1;2;4]
|4 -> [2;3]
|_ -> [];;

type ’a graph = Graph of (’a -> ’a list);;
let g = Graph f;;
