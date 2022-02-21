open Graphics
open Params

type t_piece = Bombardier | Cavalier | Bouclier | Unite | General | Officier | Lancier | Archer | Ouvrier | Souverain | Garde | Chambellan
type piece = {t: t_piece; x: int; y: int; attaquant: bool}

let gw, gh = 8, 13

(* ne pas oublier de reporter les dimensions du jeu ici lorsqu'on les change *)
let pieces : piece option array array = Array.make_matrix 32 32 None

(* TODO compléter ces listes *)
(* liste les pièces de l'attaquant (type, (x, y)) *)
let pieces_attaquant = [|(General, (15, 0));(Officier, (25,3)); (Officier,(16,3)); (Officier,(6,3)); (Unite,(15,2)); (Unite,(16,2));(Unite,(17,2));(Unite,(14,2));(Unite,(13,2));(Unite,(18,2));(Unite,(19,2)); (Unite,(12,2)); (Lancier,(15,3));(Lancier,(14,3));(Lancier,(17,3));(Lancier,(18,3));(Cavalier,(7,3));(Cavalier,(5,3));(Cavalier,(26,3));(Cavalier,(24,3))|]

(* liste les pièces du défenseur sous forme (type, (x, y)) *)
let pieces_defenseur = [|(Archer, (18, 7)); (Archer,(13,7)); (Lancier,(17,7));(Lancier,(15,7));(Lancier,(16,7));(Lancier,(14,7)); (Bouclier,(14,6));(Bouclier,(15,6));(Bouclier,(16,7));(Bouclier,(17,7));(Souverain, (16,19));(Garde,(16, 17));(Garde,(14, 19));(Garde,(18, 19))|]

let place_pieces () =
  let aux arr att =
    for i = 0 to Array.length arr - 1 do
      let t, (x, y) = arr.(i) in
      match Terrain.terrain.(y).(x) with
      | Sol | Camp -> ()
      | _ -> Terrain.terrain.(y).(x) <- Sol;
      pieces.(y).(x) <- Some {t = t; x = x; y = y; attaquant = att}
    done
  in
  aux pieces_attaquant true;
  aux pieces_defenseur false

(** Renvoie la liste des déplacements possibles pour la pièce située aux coordonnées x,y *)
let deplacements i j =
  let aux t =
    match t with
    | Bombardier | Archer | Unite -> [(i+1, j); (i-1, j); (i, j+1); (i, j-1)]
    | Cavalier ->[(i+1, j); (i-1, j); (i, j+1); (i, j-1); (i+1, j+1); (i-1, j-1); (i-1, j+1); (i+1, j-1);(i+2, j); (i-2, j); (i, j+2); (i, j-2); (i+2, j+2); (i-2, j-2); (i-2, j+2); (i+2, j-2);(i+3, j); (i-3, j); (i, j+3); (i, j-3); (i+3, j+3); (i-3, j-3); (i-3, j+3); (i+3, j-3);(i+1, j+2); (i+1, j-2); (i-1, j+2); (i-1, j-2); (i+2, j+1); (i-2, j+1); (i+2, j-1); (i-2, j-1)]
    | Bouclier -> [(i+2, j); (i-2, j); (i, j+2); (i, j-2)]
    | Lancier -> [(i+1, j+2); (i+1, j-2); (i-1, j+2); (i-1, j-2)]
    | General | Officier | Garde -> [(i+3, j); (i, j+3); (i-  3, j); (i, j-3); (i+2, j+2); (i-2, j-2); (i-2, j+2); (i+2, j-2)]
    | Chambellan -> [(i+3,j+3); (i-3, j-3); (i-3, j+3); (i+3, j-3)]
    | Ouvrier | Souverain -> [(i+1, j); (i-1, j); (i, j+1); (i, j-1); (i+1, j+1); (i-1, j-1); (i-1, j+1); (i+1, j-1)]
  in
  match pieces.(i).(j) with
  | None -> []
  | Some pc -> aux pc.t


let dessine_piece i j =
  match pieces.(j).(i) with
  |None -> ()
  |Some pc -> begin
    set_color white;
    fill_rect (i * gw - 1) (j * gh - 1) (gw) (gh);
    set_color black;
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

let deplace p x y =
  pieces.(p.x).(p.y) <- None;
  pieces.(x).(y) <- Some {t = p.t; x = x; y = y; attaquant = p.attaquant}

(** Renvoie true si le déplacement de (x1, y1) vers (x2, y2) est valide *)
let deplacement_valide (x1, y1) dest =
  (* on suppose que les coordonnées sont valides, i.e. elles ne produisent pas de IndexOutOfBounds *)
  let depl = deplacements x1 y1 in
  List.fold_left (fun b pos -> b || pos = dest) false depl
