type case = Eau | Terrain | Obstacle | Mur

let freq_eau = 5
let freq_obstacle = 15

(* génère aléatoirement un terrai de taille x * y en utilisant la graine seed *)
let genere_terrain (x, y) seed = 
  let t = Array.make_matrix x y Eau in
  let _ = Random.init seed in
  for i = 0 to x - 1 do
    for j = 0 to y - 1 do
      let r = Random.int 100 in
      if r < freq_eau then
        t.(i).(j) <- Eau
      else if r < freq_obstacle then
        t.(i).(j) <- Obstacle
      else
        t.(i).(j) <- Terrain
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

