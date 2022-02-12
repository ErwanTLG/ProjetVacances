type cases = Eau | Sol | Pont | Mur | Arbre | Rocher | Camp

let freq_eau = 5
let freq_arbre = 10
let freq_rocher = 5

(* génère aléatoirement un terrai de taille x * y en utilisant la graine seed *)
let genere_terrain (x, y) seed = 
  let t = Array.make_matrix x y Eau in
  let _ = Random.init seed in
  for i = 0 to x - 1 do
    for j = 0 to y - 1 do
      let r = Random.int 100 in
      if r < freq_eau then
        t.(i).(j) <- Eau
      else if r < freq_arbre then
        t.(i).(j) <- Arbre
      else if r < freq_rocher then
        t.(i).(j) <- Rocher
      else
        t.(i).(j) <- Terrain
    done
  done;
  t.(9).(15) <- Some Mur;
  t.(9).(14) <- Some Mur;
  t.(9).(13) <- Some Mur;
  t.(9).(11) <- Some Mur;
  t.(10).(10) <- Some Mur;
  t.(11).(9) <- Some Mur;
  t.(12).(8) <- Some Mur;
  t.(13).(8) <- Some Mur;
  t.(14).(8) <- Some Mur;
  t.(15).(8) <- Some Mur;
  for i = 0 to (Array.length t - 1)/2 do
    for j = 0 to (Array.length t - 1)/2 do
      t.(Array.length t-1-i).(j) <- t.(i).(j);
      t.(i).(Array.length t-1-j) <- t.(i).(j);
      t(Array.length t-1-i).(Array.length t-1-j) <- t.(i).(j)
    done
  done;
  t

(* fonction de test *)
let affiche_terrain t =
  let open Printf in
  for i = 0 to Array.length t - 1 do
    for j = 0 to Array.length t.(0) - 1 do
      match t.(i).(j) with
      | Eau -> printf " "
      | Terrain -> printf "."
      | Obstacle -> printf "*"
      | Mur -> printf "#"
    done;
    printf "\n"
  done;
  flush stdout
