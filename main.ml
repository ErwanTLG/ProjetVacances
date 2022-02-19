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

let deplacement_valide p mouv = 
  match mouv with
  |(i,j)-> if deplacement_existant p i j (deplacements_possibles i j )
           then p.(i).(j) <> Eau || p.(i).(j) <> Mur || p.(i).(j) <> Arbre || p.(i).(j) <> Rocher
           else false


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

let piece_selectionnee : piece option ref = ref None

let tour_attaquant = ref true (* c'est l'attaquant qui joue en premier *)

(* vérifie si la piece aux coordonnées x, y appartient au joueur actif *)
let appartient_joueur_actif x y =
  let p = pieces.(x).(y) in
  match p with
  | None -> false
  | Some pc -> pc.attaquant = !tour_attaquant


let dessine_piece i j p =
  match p.(i).(j) with
  |Unite -> moveto (i*gw+3) (j*gh+1); lineto (i*gw+3) (j*gh+11); moveto (i*gw+4) (j*gh+11); lineto (i*gw+4) (j*gh+1)
  |Officier -> moveto (i*gw+2) (j*gh+1); lineto (i*gw+2) (j*gh+11); moveto (i*gw+5) (j*gh+11); lineto (i*gw+5) (j*gh+1)
  |General -> moveto (i*gw+1) (j*gh+1); lineto (i*gw+1) (j*gh+11); moveto (i*gw+3) (j*gh+11); lineto (i*gw+3) (j*gh+1); moveto (i*gw+4) (j*gh+1); lineto (i*gw+4) (j*gh+11); moveto (i*gw+6) (j*gh+11); lineto (i*gw+6) (j*gh+1)
  |Lancier -> moveto (i*gw+1) (j*gh+11); lineto (i*gw+6) (j*gh+1); lineto (i*gw+5) (j*gh+1); moveto (i*gw+6) (j*gh+1); lineto (i*gw+6) (j*gh+3)
  |Cavalier -> moveto (i*gw+1) (j*gh+11); lineto (i*gw+1) (j*gh+1); lineto (i*gw+6) (j*gh+1); lineto (i*gw+6) (j*gh+11)
  |Bombardier -> draw_rect (i*gw+3) (j*gh+10) 4 4; moveto (i*gw+4) (j*gh+7); lineto (i*gw+4) (j*gh+4)
  |Archer -> moveto (i*gw+3) (j*gh+1); lineto (i*gw+3) (j*gh+12); lineto (i*gw+6) (j*gh+7); lineto (i*gw+3) (j*gh+1)
  |Bouclier -> moveto (i*gw+4) (j*gh+11); lineto (i*gw+6) (j*gh+9); lineto (i*gw+6) (j*gh+1); lineto (i*gw+1) (j*gh+1); lineto (i*gw+1) (j*gh+9); lineto (i*gw+3) (j*gh+11)
  |Ouvrier -> moveto (i*gw+4) (j*gh+11); lineto (i*gw+4) (j*gh+6); draw_rect (i*gw+1) (j*gh+6) 6 4
  |Chambellan -> moveto (i*gw+1) (j*gh+6); lineto (i*gw+6) (j*gh+6)
  |Garde -> moveto (i*gw+1) (j*gh+4);lineto (i*gw+6) (j*gh+4);moveto (i*gw+6) (j*gh+8); lineto (i*gw+1) (j*gh+8)
  |Souverain -> moveto (i*gw+1) (j*gh+3); lineto (i*gw+6) (j*gh+3); moveto (i*gw+6) (j*gh+6); lineto (i*gw+1) (j*gh+6); moveto (i*gw+1) (j*gh+9); lineto (i*gw+6) (j*gh+9)
  

let main =
  (* TODO supprimer la ligne qui suit, juste pour le test*)
  pieces.(0).(0) <- Some {t = Archer; x = 0; y = 0; attaquant = true};
  open_graph (Printf.sprintf " %dx%d" (16+tw*gw) (50+th*gh));
  set_window_title "Projet Vacances";
  let t = genere_terrain (32, 32) 1234 in
  while true do
    
    (* dessine le terrain *)
    set_color black;
    for i = 0 to Array.length t - 1 do
      for j = 0 to Array.length t.(0) - 1 do
        putchar white black i j (char_of_case t.(i).(j))
      done
    done;
    
    (*J'ai fait une fonction dessine_piece au dessus *)

    (* dessine un rectangle rouge autour de la case de l'unité selectionnée *)
    (* je ne sais pas pourquoi, mais si on écrit ce match après le prochain, rien ne marche. CA M'A PRIS QUASI 1H A TROUVER CE BUG J'EN PEUX PLUS *)
    set_color red;
    (match !piece_selectionnee with
    | None -> ()
    | Some pc -> draw_rect (pc.x * gw - 1) (pc.y * gh - 1) (gw + 1) (gh + 1));

    let status = wait_next_event [Button_down] in
    let m_x, m_y = to_grid_coords(status.mouse_x, status.mouse_y) in

    (* si on clique sur l'unité sélectionnée, ça la déselectionne *)
    (match !piece_selectionnee with
    | None -> if est_valide (m_x, m_y) pieces && appartient_joueur_actif m_x m_y then piece_selectionnee := pieces.(m_x).(m_y)
    | Some pc -> if pc.x = m_x && pc.y = m_y then piece_selectionnee := None);
  
    (*J'ai essayé une fonction pour déplacer les pièces mais je sais pas quoi mettre dans le None *)
   (*match !piece_selectionnee with
    |None -> ()
    |Some pc -> wait_next_event [Button_down];
                let m_x, m_y = to_grid_coords(status.mouse_x, status.mouse_y) in
                if deplacement_valide t (m_x, m_y) 
                then deplace !piece_selectionnee m_x m_y*)
  done

 
