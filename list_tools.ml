(* 1 - Toolbox *)

(* Lists of Lists *)

let rec length = function
  |[] -> 0
  |e::l -> 1 + length l;;

(* Function 1 : init_board *)

let init_list n x =
  if n < 0 then invalid_arg "init_list : n must be a natural"
  else
    let rec aux n x l = if n = 0 then l
      else
        aux (n-1) x (x::l)
    in aux n x [] ;;

let rec init_board (l,c) v =
if l < 0 || c < 0 then failwith "init_board = l,c must be > 0"
 else
  let l2 = init_list c v in
  match (l,c) with
  | (0,c) -> []
  | (l,c) -> l2::init_board (l-1,c) v ;;

(* Function 2 : get_cell *)

let get_element e l = 
  let rec aux n = function
    |[] -> failwith "out of bounds : not inside the list"
    |h::t -> 
      if n = e then h 
      else aux (n+1) t
  in aux 1 l;;

let get_cell (x,y) board = 
 let rec aux n = function
    |[] -> failwith "out of bound : not on the board"
    |h::t -> 
      if n = x then get_element y h
      else aux (n+1) t
  in
  aux 1 board;;

(* Function 3 : put_cell*)

let put_element value element l = 
  let rec aux n = function
    |[] -> failwith "out of bounds : not inside Ze list"
    |h::t -> 
      if n = element then value::t
      else h::aux(n+1) t
  in aux 1 l;;

let put_cell value (x,y) board =  
  let rec aux n = function
    |[] -> failwith "out of bounds : not on the board"
    |e::l -> 
      if n = x then (put_element value y e)::l
      else e::aux(n+1) l
  in aux 1 board;;

(* Graphic Functions *)

#use "topfind";;
#require "graphics";;

open Graphics;;

(* Function open_graph *)

let open_window size = open_graph (" " ^ string_of_int size ^ "x" ^
                                   string_of_int (size+20)) ;;

let draw_square (x,y) size = 
  moveto x y;
  lineto (x+size) y;
  lineto (x+size) (y+size);
  lineto x (y+size);
  lineto x y;;

let draw_fill (x,y) size color =
  set_color color;
  let finalX = x+size in 
  let rec aux x y = 
    moveto x y;
    match x with
    |x when x = finalX -> lineto x (y+size)
    |x -> lineto x (y+size); aux (x+1) y
  in
  aux x y;;

let draw_cell (x,y) size cell = 
  match cell with
    |0 -> draw_square (x,y) size
    |1 -> draw_fill (x,y) size black
    |_ -> failwith "invalid cell";;

let rec draw_line line size (x,y) =
  match line with
    |[] -> ()
    |e::l -> draw_cell (y,x) size e; draw_line l size  (x+size,y);; 

let draw_board board size =
  let (x,y) = (0,0) in
  let rec aux (x,y) = function
    |[] -> ()
    |e::l -> draw_line e size (x,y); aux (x,y+size) l
  in
  aux (x,y) board;;

let board = [[1;1;1;1;1;1;1;1;1;1];
             [0;0;0;0;0;0;0;0;0;0];
             [1;0;1;0;1;0;1;0;1;0];
             [0;1;0;1;0;1;0;1;0;1];
             [0;0;0;0;0;0;0;0;0;0];
             [1;1;1;1;1;1;1;1;1;1];
             [0;0;0;0;0;0;0;0;0;0];
             [1;0;1;0;1;0;1;0;1;0];
             [0;1;0;1;0;1;0;1;0;1];
             [0;0;0;0;0;0;0;0;0;0]];;

let test_display board cell_size =
  open_window (length board * cell_size + 40) ;
  draw_board board cell_size ;;

test_display board 50;;
