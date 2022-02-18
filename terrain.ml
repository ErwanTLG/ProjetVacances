open Params

type cases = Eau | Sol | Pont | Mur | Arbre | Rocher | Camp

let freq_eau = 5
let freq_arbre = 10
let freq_rocher = 5

(* génère aléatoirement un terrai de taille x * y en utilisant la graine seed *)
let genere_terrain seed = 
  let t = Array.make_matrix (fst dimensions) (snd dimensions) Eau in
  let _ = Random.init seed in
  for i = 0 to fst dimensions - 1 do
    for j = 0 to snd dimensions - 1 do
      let r = Random.int 100 in
      if r < freq_eau then
        t.(i).(j) <- Eau
      else if r < freq_eau + freq_arbre then
        t.(i).(j) <- Arbre
      else if r < freq_eau + freq_arbre + freq_rocher then
        t.(i).(j) <- Rocher
      else
        t.(i).(j) <- Sol
    done
  done;

  (* place le rempart *)
  let positions_rempart = [(9, 15); (9, 14); (9, 13); (9, 11);
   (10, 10); (11, 9); (12, 8); (13, 8); (14, 8); (15, 8)] in
  List.iter (fun (x, y) -> t.(x).(y) <- Mur) positions_rempart;
  (* fait la symétrie du rempart *)
  let dim_y = Array.length t in
  for i = 0 to (dim_y - 1) / 2 do
    for j = 0 to (dim_y - 1) / 2 do
      if t.(i).(j) = Mur then begin
        t.(dim_y - 1 - i).(j) <- t.(i).(j);
        t.(i).(dim_y - 1 - j) <- t.(i).(j);
        t.(dim_y - 1 - i).(dim_y - 1 - j) <- t.(i).(j)
      end
    done
  done;
  t

