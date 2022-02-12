open Graphics
open Terrain

let char_of_case c =
  match c with
  | Eau -> ' '
  | Terrain -> '.'
  | Obstacle -> '*'
  | Mur -> '#'

let gw,gh = 8, 13
(* Bas de l'ecran de la carte *)
(* Taille de l'ecran *)
let tw, th = 80, 24
let sbh = 2
let sby = 0
let messh = 1
let messy = th - 1
let mw, mh = tw, th - sbh - messh
let cx, cy = 0, sbh

let putchar backgroundcolor color x y c = 
  moveto (cx+gw*x) (cy+gh*(mh-y-1));
  set_color backgroundcolor;
  fill_rect (cx+gw*x) (cy+gh*(mh-y-1)) gw gh;
  set_color color;
  draw_char c 

exception Fin

let main =
  open_graph (Printf.sprintf " %dx%d" (16+tw*gw) (50+th*gh));
  let t = genere_terrain (32, 32) 1234 in
  for i = 0 to Array.length t - 1 do
    for j = 0 to Array.length t.(0) - 1 do
      putchar white black i j (char_of_case t.(i).(j))
    done
  done;
  while true do
    (* on quitte dès qu'une touche est pressée *)
    let s = wait_next_event [Key_pressed] in
    if s.key = 'q' then raise Fin
  done











let () = print_endline "Bonjour, Monde!"

type Piece  = Bombardier | Cavalier | Bouclier | Unite | General | Officier | Lancier | Archer | Ouvrier | Souverain | Garde | Chambellan

type cases = Eau | Sol of Piece option | Pont | Mur | Arbre | Rocher | Camp

let creer_plateau_basique =
  let t = Array.make_matrix 34 34 None in
  
  for i = 0 to Array.length t-1 do
    for j = 0 to Array.length t-1 do
      t.(i).(j) <- Some Sol None
    done
  done; 
 
  for i = 0 to 4 do
    for j = 0 to 4-i do
      t.(i).(j) <- None
    done
  done;
  
  t.(9).(15) <- Some Mur;
  t.(9).(14) <- Some Mur;
  t.(9).(13) <- Some Mur;
  t.(9).(11) <- Some Mur;
  t.(10).(10) <- Some Mur;
  t.(11).(9) <- Some Mur;
  t.(12).(8) <- Some Mur;
  t.(13).(8) <- Some Mur;
  t.(14).(8) <- Some Mur;
  t.(15).(8) <- Some Mur;

  for i = 0 to (Array.length t - 1)/2 do
    for j = 0 to (Array.length t - 1)/2 do
      t.(Array.length t-1-i).(j) <- t.(i).(j);
      t.(i).(Array.length t-1-j) <- t.(i).(j);
      t(Array.length t-1-i).(Array.length t-1-j) <- t.(i).(j)
    done
  done;


let deplacements_possibles plateau i j =
  let l = [] in
  match plateau.(i).(j) with
  |Sol None -> l
  |Sol Bombardier -> (i+3,j)::(i-3::j)::(i,j+3)::(i,j-3)::(i+1,j)::(i-1::j)::(i,j+1)::(i,j-1)::l 
  (*il faut tester l'orientation de la pièce pour savoir dans quelle direction elle peut avancer de 3 et dans quelles de 1 *)
  |Sol Unite -> (i+1,j)::(i-1::j)::(i,j+1)::(i,j-1)::l 
  |Sol Cavalier -> for k = 1 to 4 do
                     for m = 1 to 4 do
                       (i-k,j)::(i-m,j)::(i+k,j)::(i+m,j)::(i-k,j-k)::(i-m,j-k)::(i+k,j-k)::(i+m,j-k)::(i-k,j+k)::(i-m,j+k)::(i+k,j+k)::(i+m,j+k)::(i-k,j-m)::(i-m,j-m)::(i+k,j-m)::(i+m,j-m)::(i-k,j+m)::(i-m,j+m)::(i+k,j+m)::(i+m,j+m)::(i,j+k)::(i,j-k)::(i,j+m)::(i,j-m)::l
                     done
                   done
  |Sol Bouclier ->(i+2,j)::(i-2::j)::(i,j+2)::(i,j-2)::l
  |Sol Lancier -> (i+2,j)::(i-2::j)::(i,j+2)::(i,j-2)::l
  |(*flemme de faire la suite*)