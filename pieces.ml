open Graphics


type t_piece = Bombardier | Cavalier | Bouclier | Unite | General | Officier | Lancier | Archer | Ouvrier | Souverain | Garde | Chambellan
type piece = {t: t_piece; x: int; y: int; attaquant: bool}

let gw,gh = 8, 13

(* ne pas oublier de reporter les dimensions du jeu ici lorsqu'on les change *)
let pieces : piece option array array = Array.make_matrix 32 32 None

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

let dessine_piece i j p =
  match p.(i).(j) with
  |None -> ()
  |Some pc -> match pc.t with
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


let deplace p x y =
  set_color white;
  fill_rect (x*gw+1) (y*gh+11) 6 11;
  set_color black;
  pieces.(p.x).(p.y) <- None;
  pieces.(x).(y) <- Some {t = p.t; x = x; y = y; attaquant = p.attaquant};
  dessine_piece x y pieces

let rec deplacement_existant p i j l =
  match l with
  |[] -> false
  |t::q -> if t = (i,j) then true else deplacement_existant p i j q



