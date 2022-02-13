open Graphics
open Terrain
open Pieces

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

(* convertit les coordonnées d'images (en pixels) en coordonées de grille (par rapport au tableau de jeu) *)
(* ATTENTION: peut dépasser la taille limite du tableau *)
let to_grid_coords (x, y) =
  (x / gw, y / gh)

(* vérifie si le point d'indices (i, j) est bien valide dans le tableau t *)
let est_valide (i, j) t =
  i >= 0 && j >= 0 && i < Array.length t && j < Array.length t.(0)

exception Fin

let piece_selectionnee : piece ref = ref None

let main =
  open_graph (Printf.sprintf " %dx%d" (16+tw*gw) (50+th*gh));
  let t = genere_terrain (32, 32) 1234 in
  while true do
    for i = 0 to Array.length t - 1 do
      for j = 0 to Array.length t.(0) - 1 do
        putchar white black i j (char_of_case t.(i).(j))
      done
    done;
    (* dessine un rectangle autour de la case sous la souris *)
    let m_x, m_y = to_grid_coords (mouse_pos ()) in
    if est_valide (m_x, m_y) t then draw_rect (m_x * gw - 1) (m_y * gh - 1) (gw + 1) (gh + 1);
    if button_down () && est_valide (m_x, m_y) pieces then piece_selectionnee := pieces.(m_x).(m_y)
  done

(* il y a actuellement de gros problèmes d'affichage, il faudrait paufiner la façon dont le rendu est fait *) 
(* mais la logique du jeu peut néanmoins être appliquée et fonctionner *)

