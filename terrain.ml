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
      else if r < freq_eau + freq_arbre then
        t.(i).(j) <- Arbre
      else if r < freq_eau + freq_arbre + freq_rocher then
        t.(i).(j) <- Rocher
      else
        t.(i).(j) <- Sol
    done
  done;
  (* place le rempart *)
  t.(9).(15) <- Mur;
  t.(9).(14) <- Mur;
  t.(9).(13) <- Mur;
  t.(9).(11) <- Mur;
  t.(10).(10) <- Mur;
  t.(11).(9) <- Mur;
  t.(12).(8) <- Mur;
  t.(13).(8) <- Mur;
  t.(14).(8) <- Mur;
  t.(15).(8) <- Mur;
  (* fait la symétrie du rempart *)
  for i = 0 to (Array.length t - 1) / 2 do
    for j = 0 to (Array.length t - 1) / 2 do
      if t.(i).(j) = Mur then begin
        t.(Array.length t - 1 - i).(j) <- t.(i).(j);
        t.(i).(Array.length t - 1 - j) <- t.(i).(j);
        t.(Array.length t - 1 - i).(Array.length t - 1 - j) <- t.(i).(j)
      end
    done
  done;
  t

