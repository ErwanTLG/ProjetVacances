(** taille du terrain : longueur, hauteur *)
let dimensions = 32, 32

(** vérifie si le point d'indices (x, y) est bien valide (dans la zone de jeu) *)
let est_valide x y =
  x >= 0 && y >= 0 && x < fst dimensions && y < snd dimensions

(** contains v l vérifie si la liste l contient l'élément v *)
let contains v l =
  List.fold_left (fun b v' -> b || v = v') false l
