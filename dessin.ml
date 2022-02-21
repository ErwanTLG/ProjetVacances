open Graphics
open Terrain
open Pieces
open Utils

(* Bas de l'ecran de la carte *)
(* Taille de l'ecran *)
let tw, th = 32, 32
let sbh = 2
let sby = 0
let messh = 1
let messy = th - 1
let mw, mh = tw, th - sbh - messh
let cx, cy = 0, sbh
(* taille d'une case *)
let gw, gh = 8, 13

let putchar backgroundcolor color x y c =
  moveto (cx+gw*x) (cy+gh*(mh-y-1));
  set_color backgroundcolor;
  fill_rect (cx+gw*x) (cy+gh*(mh-y-1)) gw gh;
  set_color color;
  draw_char c

(* ----- FONCTIONS DE DESSIN DU TERRAIN ----- *)
let char_of_case c =
  match c with
  | Eau -> ' '
  | Sol -> '.'
  | Arbre -> '*'
  | Rocher -> '@'
  | Mur -> '#'
  | Camp -> '^'

let dessine_terrain () =
  set_color black;
    for i = 0 to Array.length terrain - 1 do
      for j = 0 to Array.length terrain.(0) - 1 do
        putchar white black i j (char_of_case terrain.(i).(j))
      done
    done

(* ----- FONCTIONS DE DESSIN DES PIECES ----- *)
(** dessine la pièce aux coordonnées i,j *)
let dessine_piece i j =
  match pieces.(j).(i) with
  | None -> ()
  | Some pc -> begin
    set_color white;
    fill_rect (i * gw) (j * gh) gw gh;
    let color = if pc.attaquant then red else blue in
    set_color color;
    match pc.t with
      | Unite -> moveto (i*gw+3) (j*gh+1); lineto (i*gw+3) (j*gh+11); moveto (i*gw+4) (j*gh+11); lineto (i*gw+4) (j*gh+1)
      | Officier -> moveto (i*gw+2) (j*gh+1); lineto (i*gw+2) (j*gh+11); moveto (i*gw+5) (j*gh+11); lineto (i*gw+5) (j*gh+1)
      | General -> moveto (i*gw+1) (j*gh+1); lineto (i*gw+1) (j*gh+11); moveto (i*gw+3) (j*gh+11); lineto (i*gw+3) (j*gh+1); moveto (i*gw+4) (j*gh+1); lineto (i*gw+4) (j*gh+11); moveto (i*gw+6) (j*gh+11); lineto (i*gw+6) (j*gh+1)
      | Lancier -> moveto (i*gw+1) (j*gh+11); lineto (i*gw+6) (j*gh+1); lineto (i*gw+5) (j*gh+1); moveto (i*gw+6) (j*gh+1); lineto (i*gw+6) (j*gh+3)
      | Cavalier -> moveto (i*gw+1) (j*gh+11); lineto (i*gw+1) (j*gh+1); lineto (i*gw+6) (j*gh+1); lineto (i*gw+6) (j*gh+11)
      | Bombardier -> draw_rect (i*gw+3) (j*gh+10) 4 4; moveto (i*gw+4) (j*gh+7); lineto (i*gw+4) (j*gh+4)
      | Archer -> moveto (i*gw+3) (j*gh+1); lineto (i*gw+3) (j*gh+12); lineto (i*gw+6) (j*gh+7); lineto (i*gw+3) (j*gh+1)
      | Bouclier -> moveto (i*gw+4) (j*gh+11); lineto (i*gw+6) (j*gh+9); lineto (i*gw+6) (j*gh+1); lineto (i*gw+1) (j*gh+1); lineto (i*gw+1) (j*gh+9); lineto (i*gw+3) (j*gh+11)
      | Ouvrier -> moveto (i*gw+4) (j*gh+11); lineto (i*gw+4) (j*gh+6); draw_rect (i*gw+1) (j*gh+6) 6 4
      | Chambellan -> moveto (i*gw+1) (j*gh+6); lineto (i*gw+6) (j*gh+6)
      | Garde -> moveto (i*gw+1) (j*gh+4);lineto (i*gw+6) (j*gh+4);moveto (i*gw+6) (j*gh+8); lineto (i*gw+1) (j*gh+8)
      | Souverain -> moveto (i*gw+1) (j*gh+3); lineto (i*gw+6) (j*gh+3); moveto (i*gw+6) (j*gh+6); lineto (i*gw+1) (j*gh+6); moveto (i*gw+1) (j*gh+9); lineto (i*gw+6) (j*gh+9)
    end

let dessine_pieces () =
  for x = 0 to fst dimensions - 1 do
    for y = 0 to snd dimensions - 1 do
      dessine_piece x y
    done
  done
