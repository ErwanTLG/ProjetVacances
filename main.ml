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

exception Fin of string
exception FinTour

let piece_selectionnee : piece option ref = ref None

let tour_attaquant = ref true (* c'est l'attaquant qui joue en premier *)

let annonce_victoire () =
  if !tour_attaquant then "Victoire de l'attaquant !"
  else "Victoire du défenseur !"

(* vérifie si la piece aux coordonnées x, y appartient au joueur actif *)
let appartient_joueur_actif x y =
  let p = pieces.(x).(y) in
  match p with
  | None -> false
  | Some pc -> pc.attaquant = !tour_attaquant

let commence_nouveau_tour () =
  piece_selectionnee := None;
  tour_attaquant := not !tour_attaquant

(* TODO compléter cette fonction *)
let check_win () =
  false

let help = "chess [-ia <difficulté : int>] [-ia_def <bool>]"
let diff = ref 0
let ia = ref false
let ia_def = ref true (* par défaut, l'ia joue en tant que défenseur *)

let set_ia_def b =
  ia_def := b

let handle_ia_arg i =
  if i <= 0 then failwith "Le niveau de difficulté de l'ia doit être un entier strictement positif.";
  diff := i;
  ia := true

let anon_fun _ =
  failwith "Erreur: arguments incorrects."

let speclist = [("-ia", Arg.Int handle_ia_arg, "Active l'ia avec un niveau de difficulté"); 
("-ia_def", Arg.Bool set_ia_def, "Précise si l'ia joue en tant que défenseur ou non. (default: true)")]

let main =
  Arg.parse speclist anon_fun help;
  
  (* TODO supprimer la ligne qui suit, juste pour le test*)
  pieces.(0).(0) <- Some {t = Archer; x = 0; y = 0; attaquant = true};
  open_graph (Printf.sprintf " %dx%d" (16+tw*gw) (50+th*gh));
  set_window_title "Projet Vacances";
  genere_terrain 1234;
  while true do
    
    (* dessine le terrain *)
    set_color black;
    for i = 0 to Array.length terrain - 1 do
      for j = 0 to Array.length terrain.(0) - 1 do
        putchar white black i j (char_of_case terrain.(i).(j))
      done
    done;
    
    (*J'allais faire le dessin des pièce mais je sais pas combien il y a de pixel par cases *)

    (* dessine un rectangle rouge autour de la case de l'unité selectionnée *)
    (* je ne sais pas pourquoi, mais si on écrit ce match après le prochain, rien ne marche. CA M'A PRIS QUASI 1H A TROUVER CE BUG J'EN PEUX PLUS *)
    set_color red;
    (match !piece_selectionnee with
    | None -> ()
    | Some pc -> draw_rect (pc.x * gw - 1) (pc.y * gh - 1) (gw + 1) (gh + 1));

    let status = wait_next_event [Button_down] in
    let m_x, m_y = to_grid_coords(status.mouse_x, status.mouse_y) in

    try
      (match !piece_selectionnee with
      | None -> if est_valide (m_x, m_y) pieces && appartient_joueur_actif m_x m_y then piece_selectionnee := pieces.(m_x).(m_y)
      | Some pc -> if pc.x = m_x && pc.y = m_y then piece_selectionnee := None 
        else if est_valide (m_x, m_y) pieces && appartient_joueur_actif m_x m_y then piece_selectionnee := pieces.(m_x).(m_y)
        else if deplacement_valide t (m_x, m_y) then begin 
          deplace pc m_x m_y;
          if pieces.(m_x).(m_y) <> None then raise FinTour
        end);
      with FinTour -> begin
        if check_win () then raise (Fin (annonce_victoire ()));
        commence_nouveau_tour ()
      end
  done
