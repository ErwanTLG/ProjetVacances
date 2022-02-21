open Utils
open Terrain

type t_piece = Bombardier | Cavalier | Bouclier | Unite | General | Officier | Lancier | Archer | Ouvrier | Souverain | Garde | Chambellan
type piece = {t: t_piece; x: int; y: int; attaquant: bool}

let pieces : piece option array array = Array.make_matrix (fst dimensions) (snd dimensions) None

(* TODO compléter ces listes *)
(* liste les pièces de l'attaquant (type, (x, y)) *)
let pieces_attaquant =
  [|(General, (15, 0)); (Officier, (25,3)); (Officier,(16,3)); (Officier,(6,3)); (Unite,(15,2)); (Unite,(16,2)); (Unite,(17,2));
    (Unite,(14,2)); (Unite,(13,2)); (Unite,(18,2));(Unite,(19,2)); (Unite,(12,2)); (Lancier,(15,3)); (Lancier,(14,3));
    (Lancier,(17,3)); (Lancier,(18,3)); (Cavalier,(7,3)); (Cavalier,(5,3)); (Cavalier,(26,3)); (Cavalier,(24,3))|]

(* liste les pièces du défenseur sous forme (type, (x, y)) *)
let pieces_defenseur =
  [|(Archer, (18, 7)); (Archer,(13,7)); (Lancier,(17,7)); (Lancier,(15,7)); (Lancier,(16,7)); (Lancier,(14,7)); (Bouclier,(14,6));
    (Bouclier,(15,6)); (Bouclier,(16,7)); (Bouclier,(17,7)); (Souverain, (16,19)); (Garde,(16, 17)); (Garde,(14, 19)); (Garde,(18, 19))|]

(* remplit la matrice pieces avec les pieces spécifiées ci-dessus *)
let place_pieces () =
  let aux att piece =
    let t, (x, y) = piece in
    (match Terrain.terrain.(y).(x) with
    | Sol | Camp -> ()
    | _ -> Terrain.terrain.(y).(x) <- Sol);
    pieces.(y).(x) <- Some {t = t; x = x; y = y; attaquant = att}
  in
  Array.iter (aux true) pieces_attaquant;
  Array.iter (aux false) pieces_defenseur

(** Renvoie la liste des déplacements possibles pour la pièce située aux coordonnées x,y *)
let deplacements i j =
  match pieces.(j).(i) with
  | None -> []
  | Some pc -> match pc.t with
    | Bombardier | Archer | Unite -> [(i+1, j); (i-1, j); (i, j+1); (i, j-1)]
    | Cavalier ->[(i+1, j); (i-1, j); (i, j+1); (i, j-1); (i+1, j+1); (i-1, j-1); (i-1, j+1); (i+1, j-1);(i+2, j); (i-2, j); (i, j+2); (i, j-2); (i+2, j+2); (i-2, j-2); (i-2, j+2); (i+2, j-2);(i+3, j); (i-3, j); (i, j+3); (i, j-3); (i+3, j+3); (i-3, j-3); (i-3, j+3); (i+3, j-3);(i+1, j+2); (i+1, j-2); (i-1, j+2); (i-1, j-2); (i+2, j+1); (i-2, j+1); (i+2, j-1); (i-2, j-1)]
    | Bouclier -> [(i+2, j); (i-2, j); (i, j+2); (i, j-2)]
    | Lancier -> [(i+1, j+2); (i+1, j-2); (i-1, j+2); (i-1, j-2)]
    | General | Officier | Garde -> [(i+3, j); (i, j+3); (i-  3, j); (i, j-3); (i+2, j+2); (i-2, j-2); (i-2, j+2); (i+2, j-2)]
    | Chambellan -> [(i+3,j+3); (i-3, j-3); (i-3, j+3); (i+3, j-3)]
    | Ouvrier | Souverain -> [(i+1, j); (i-1, j); (i, j+1); (i, j-1); (i+1, j+1); (i-1, j-1); (i-1, j+1); (i+1, j-1)]

let deplace p x y =
  pieces.(p.y).(p.x) <- None;
  pieces.(y).(x) <- Some {t = p.t; x = x; y = y; attaquant = p.attaquant}
