type image = float list list ;;
(* images are lists of lists of floats between 0. (white) and 1. (black) *)
type size = int * int ;;
open Graphics ;;

let window img =
  List.length (List.hd img), List.length img ;;

(* show the image *)
let depict img =
  open_graph ""; clear_graph ();
  let x, y = (fst (window img)), (snd (window img)) in resize_window x y;
  let depict_pix v r c = 
  let lvl = int_of_float (255. *. (1. -. v)) in set_color (rgb lvl lvl lvl);
  plot c (y - r) in
  List.iteri (fun r row -> List.iteri (fun c pix -> depict_pix pix r c) row) img;
  Unix.sleep 2; close_graph () ;;

(* dither max image -- dithered image *)
(* threshold thershold image -- image where pixels above the threshold
value are black *)
let temp img b = 
  List.map (fun row -> List.map (fun v -> if v <= b then 0. else 1.) row) img
    
let mona = Monalisa.image ;;
let mona_threshold = temp mona 0.75 ;;
let mona_dither = temp mona (Random.float 1.) ;;
  
let () = 
depict mona ;
depict mona_threshold ;
depict mona_dither ;;
           
