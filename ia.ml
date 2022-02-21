open Pieces
open Terrain
open Utils

(* vérifie s'il est possible de déplacer une pièce aux coordonnées x, y *)
(* tour_att indique si c'est le tour de l'attaquant *)
let est_valide tour_att (x, y) =
  if x >= 0 && x < fst dimensions && y >= 0 && y < snd dimensions then
    match pieces.(x).(y) with
    | Some p -> p.attaquant <> tour_att
    | None -> match terrain.(x).(y) with
      | Sol | Camp -> true
      | _ -> false
  else false

let coups_possibles tour_att : ((int * int) * (int * int)) list =
  (* pour chaque pièce active, on récupère la liste des déplacements possibles et on garde que ceux qui sont valides *)
  let coups = Array.map (fun a ->
    Array.fold_left (fun ls pc ->
      match pc with
      | None -> ls
      | Some p ->
        let depl = [] (*deplacements_possibles p.x p.y*) in
        let valides = List.fold_left (fun l d ->
          if est_valide tour_att d then ((p.x, p.y), d) :: l else l
      ) [] depl
      in
      valides :: ls
    ) [] a
  ) pieces
  in
  List.concat (List.concat (Array.to_list coups))

(* calcule le score que rapporte la piece p *)
let calcule_score p =
  match p.t with
  | Bouclier -> 2
  | Unite -> 1
  | Bombardier -> 3
  | Ouvrier -> 3
  | Archer -> 5
  | Cavalier -> 5
  | Chambellan -> 5 (* TODO faire en sorte que la distance par rapport à l'objectif impacte le score *)
  | Lancier -> 5
  | General -> 7
  | Souverain -> 7
  | Officier -> 6
  | Garde -> 4

(* évalue la position dans laquelle le joueur se trouve en lui assignant un score arbitraire *)
let evalue_position tour_att plateau =
  (* pour chaque piece, on ajoute / soustrait son score en fonction de son camp *)
  let score = ref 0 in
  Array.iter (fun a ->
    Array.iter (fun p ->
      match p with
      | None -> ()
      | Some pc -> let s = calcule_score pc in
        if pc.attaquant = tour_att then score := !score + s
        else score := !score - s
    ) a
  ) plateau;
  !score

let joue_coup plateau ((x0, y0), (x1, y1)) =
  plateau.(x1).(y1) <- plateau.(x0).(y0);
  plateau.(x0).(y0) <- None

exception Stop of int * ((int * int) * (int * int))

let rec cherche_coup_opti tour_att p prof alpha beta =
  if prof = 0 then evalue_position tour_att p, ((0, 0), (0, 0)) else begin
    let plateau = Array.copy p in
  	let dejoue_coup ((x0, y0), (x1, y1)) =
  	  plateau.(x0).(y0) <- p.(x0).(y0);
  	  plateau.(x1).(y1) <- p.(x1).(y1)
  	in
  	let coups = coups_possibles tour_att in
  	let meilleur = ref alpha in
  	let meilleur_coup = ref ((0, 0), (0, 0)) in
  	try
      List.iter (fun c ->
        joue_coup plateau c;
        let score, _ = cherche_coup_opti tour_att plateau (prof - 1) alpha beta in
        dejoue_coup c;
        if score >= beta then
          (* le coup était trop bon, l'adversaire évitera de se retrouver dans cette position *)
          raise (Stop (beta, c))
        else
        if score > alpha then (meilleur := score; meilleur_coup := c)
      ) coups;
  	  !meilleur, !meilleur_coup
  	with Stop (x, c) -> x, c
  end

let joue tour_att diff =
  let evaluation = evalue_position tour_att pieces in
  let _, coup = cherche_coup_opti tour_att pieces diff (-evaluation) evaluation in
  joue_coup pieces coup
