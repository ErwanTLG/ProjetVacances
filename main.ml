open Graphics
open Terrain

let char_of_case c =
  match c with
  | Eau -> ' '
  | Sol -> '.'
  | Arbre -> '*'
  | Rocher -> '@'
  | Mur -> '#'
  | Pont -> '='
  | Camp -> '^'

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
