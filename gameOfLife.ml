(* 2 - The game *)

(*
--------------------------------------------
AUXILIARY FUNCTIONS
--------------------------------------------
*)

#use "list_tools.ml"

open Random

let nbofcells l =
  let rec aux n = function
    |[]  -> n
    |1::l -> aux (n+1) l
    |0::l -> aux n l
    |_ -> failwith "wrong element inside the list"
  in 
  aux 0 l;; 

let remaining l =
  let rec aux = function
    |[] -> 0
    |e::l -> nbofcells e + aux l
  in
  aux l;;


(* 
---------------------------------------------
END OF AUXILIARY FUNCTIONS
--------------------------------------------
*)


(* Rules *)

let new_cell = 1 ;;
let empty = 0 ;;
let is_alive cell = cell <> empty ;;

let rules0 cell near =
  if cell = 1 then
    if near = 3 || near = 2 then 1 else 0
  else if near = 3 then 1 else 0;;

let count_neighbours (x,y) board size =
  let auxneigh (a,b) =
    if a < 0 || b < 0 then 0
    else
    if a >= size || b >= size then 0
    else get_cell (a,b) board
  in
  ((auxneigh (x,y+1))
  +(auxneigh (x,y-1))
  +(auxneigh (x-1,y))
  +(auxneigh (x-1,y-1))
  +(auxneigh (x-1,y+1))
  +(auxneigh (x+1,y))
  +(auxneigh (x+1,y-1))
  +(auxneigh (x+1,y+1))) ;;

(* Life *)

let rec seed_life board size nb_cell =
if nb_cell = 0 then board
else
  let a = Random.int size and b = Random.int size in
if get_cell (a,b) board = 1 then seed_life board size nb_cell
else
  let c = put_cell 1 (a,b) board in  seed_life c size (nb_cell - 1) ;;

let new_board size nb =
  let m = init_board (size,size) 0
  in seed_life m size nb ;;

let next_generation board size =
  let rec aux (x,y) board2 =
    match (x,y) with
      | (x,y) when x = size && y = size -> board2
      | (x,y)  when y = size -> aux (x+1,0) board2
      | (_,_) -> aux (x,y+1) (put_cell (rules0 (get_cell (x,y) board) (count_neighbours (x,y) board size)) (x,y) board2)
  in aux (0,0) board ;;

let rec game board size n =
 match n with
  | 0 -> ()
  | n -> draw_board board size ;
    game (next_generation board size) size (n - 1) ;;

let new_game size nb n =
  open_window size ;
game (new_board size nb) size n ;;
