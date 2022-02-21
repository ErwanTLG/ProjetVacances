open Graphics
open Dessin
open Terrain
open Pieces
open Utils

(** convertit les coordonnées d'images (en pixels) en coordonées de grille (par rapport au tableau de jeu)
  ATTENTION: peut dépasser la taille limite du tableau *)
let to_grid_coords (x, y) =
  (x / gw, y / gh)

exception Fin of string
exception FinTour

let piece_selectionnee : piece option ref = ref None

let tour_attaquant = ref true (* c'est l'attaquant qui joue en premier *)

let annonce_victoire () =
  if !tour_attaquant then "Victoire de l'attaquant !"
  else "Victoire du défenseur !"

(** vérifie si la piece aux coordonnées x, y appartient au joueur actif *)
let appartient_joueur_actif x y =
  let p = pieces.(y).(x) in
  match p with
  | None -> false
  | Some pc -> pc.attaquant = !tour_attaquant

(** Renvoie true si le déplacement de (x1, y1) vers (x2, y2) est valide *)
let deplacement_valide (x1, y1) (x2, y2) =
  if est_valide x2 y2 && marchable x2 y2 && not (appartient_joueur_actif x2 y2) then
    contains (x2, y2) (deplacements x1 y1)
  else false

(** renvoie la liste des déplacements possibles depuis la case x, y *)
let deplacements_possibles x y =
  let l = ref [] in
  List.iter (fun (x', y') -> if deplacement_valide (x, y) (x', y') then l := (x', y') :: !l) (deplacements x y);
  !l

(* TODO ajouter les conditions de victoire restantes *)
let check_win _ =
  let flag_Gen = ref false in
  let flag_Souv = ref false in
  let count_attackers = ref 0 in
  for x = 0 to fst dimensions - 1 do
    for y = 0 to snd dimensions - 1 do
      match pieces.(y).(x) with
      |None -> ()
      |Some p -> if p.t = General
                 then flag_Gen := true;
                 if p.t = Souverain
                 then flag_Souv := true;
                 if p.attaquant = true
                 then count_attackers := !count_attackers +1
    done
  done;
  if !flag_Gen = false || !count_attackers <= 5 || !flag_Souv = false
  then true
  else false

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
("-ia_def", Arg.Bool set_ia_def, "Précise si l'ia joue en tant que défenseur ou non. (défaut: true)")]

let rec commence_nouveau_tour () =
  piece_selectionnee := None;
  tour_attaquant := not !tour_attaquant;
  if !ia then begin
    if !ia_def = not !tour_attaquant then
    Ia.joue !tour_attaquant !diff;
    commence_nouveau_tour ()
  end

let main =
  Arg.parse speclist anon_fun help;

  open_graph (Printf.sprintf " %dx%d" (16+tw*gw) (50+th*gh));
  set_window_title "Projet Vacances";
  genere_terrain 1234;
  place_pieces ();

  while true do
    dessine_terrain ();
    dessine_pieces ();

    (* dessine un rectangle rouge autour de la pièce sélectionnée*)
    set_color red;
    (match !piece_selectionnee with
    | None -> ()
    | Some pc -> begin
      draw_rect (pc.x * gw - 1) (pc.y * gh - 1) (gw + 1) (gh + 1);
      (* dessine des rectangles verts aux positions où la pièce sélectionnée peut se déplacer *)
      set_color green;
      List.iter (fun (x, y) ->
        draw_rect (x * gw - 1) (y * gh - 1) (gw + 1) (gh + 1)
        ) (deplacements_possibles pc.x pc.y)
    end);

    let status = wait_next_event [Button_down] in
    let m_x, m_y = to_grid_coords (status.mouse_x, status.mouse_y) in

    try
      match !piece_selectionnee with
      | None -> if est_valide m_x m_y && appartient_joueur_actif m_x m_y then piece_selectionnee := pieces.(m_y).(m_x)
      | Some pc -> 
        (* si on clique sur la pièce sélectionnée, ça la déselectionne *)
        if pc.x = m_x && pc.y = m_y then piece_selectionnee := None 
        (* si on clique sur une autre pièce qui nous appartient, ça la sélectionne *)
        else if est_valide m_x m_y && appartient_joueur_actif m_x m_y then piece_selectionnee := pieces.(m_y).(m_x)
        (* on se déplace si on clique à un endroit valide *)
        else if deplacement_valide (pc.x, pc.y) (m_x, m_y) then begin
          deplace pc m_x m_y;
          raise FinTour
        end
      with FinTour -> begin
        if check_win pieces then raise (Fin (annonce_victoire ()));
        commence_nouveau_tour ()
      end
  done
