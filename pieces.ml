open Params

type t_piece = Bombardier | Cavalier | Bouclier | Unite | General | Officier | Lancier | Archer | Ouvrier | Souverain | Garde | Chambellan
type piece = {t: t_piece; x: int; y: int; attaquant: bool}

(* ne pas oublier de reporter les dimensions du jeu ici lorsqu'on les change *)
let pieces : piece option array array = Array.make_matrix (fst dimensions) (snd dimensions) None

let deplacements_possibles i j =
  let aux t =
    match t with
    | Bombardier | Archer | Unite -> [(i+1, j); (i-1, j); (i, j+1); (i, j-1)]
    | Cavalier ->[(i+1, j); (i-1, j); (i, j+1); (i, j-1); (i+1, j+1); (i-1, j-1); (i-1, j+1); (i+1, j-1);(i+2, j); (i-2, j); (i, j+2); (i, j-2); (i+2, j+2); (i-2, j-2); (i-2, j+2); (i+2, j-2);(i+3, j); (i-3, j); (i, j+3); (i, j-3); (i+3, j+3); (i-3, j-3); (i-3, j+3); (i+3, j-3);(i+1, j+2); (i+1, j-2); (i-1, j+2); (i-1, j-2); (i+2, j+1); (i-2, j+1); (i+2, j-1); (i-2, j-1)]
    | Bouclier -> [(i+2, j); (i-2, j); (i, j+2); (i, j-2)]
    | Lancier -> [(i+1, j+2); (i+1, j-2); (i-1, j+2); (i-1, j-2)]
    | General | Officier | Garde -> [(i+3, j); (i, j+3); (i-3, j); (i, j-3); (i+2, j+2); (i-2, j-2); (i-2, j+2); (i+2, j-2)]
    | Chambellan -> [(i+3,j+3); (i-3, j-3); (i-3, j+3); (i+3, j-3)]
    | Ouvrier | Souverain -> [(i+1, j); (i-1, j); (i, j+1); (i, j-1); (i+1, j+1); (i-1, j-1); (i-1, j+1); (i+1, j-1)]
  in
  let p = pieces.(i).(j) in
  match p with
  | None -> []
  | Some pc -> aux pc.t

let deplace p x y =
  pieces.(p.x).(p.y) <- None;
  pieces.(x).(y) <- Some {t = p.t; x = x; y = y; attaquant = p.attaquant}

let rec deplacement_existant p i j l =
  match l with
  |[] -> false
  |t::q -> if t = (i,j) then true else deplacement_existant p i j q



