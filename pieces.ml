type piece  = None | Bombardier | Cavalier | Bouclier | Unite | General | Officier | Lancier | Archer | Ouvrier | Souverain | Garde | Chambellan

(* ne pas oublier de reporter les dimensions du jeu ici lorsqu'on les change *)
let pieces = Array.make_matrix 32 32 None

let deplacements_possibles i j =
  match pieces.(i).(j) with
  | None -> []
  | Bombardier | Archer | Unite -> [(i+1, j); (i-1, j); (i, j+1); (i, j-1)]
  | Cavalier ->(* for k = 1 to 4 do  TODO FIX THIS: ça ne peut pas marcher actuellement*)
               (*      for m = 1 to 4 do
                        (i-k,j)::(i-m,j)::(i+k,j)::(i+m,j)::(i-k,j-k)::(i-m,j-k)::(i+k,j-k)::(i+m,j-k)::(i-k,j+k)::(i-m,j+k)::(i+k,j+k)::(i+m,j+k)::(i-k,j-m)::(i-m,j-m)::(i+k,j-m)::(i+m,j-m)::(i-k,j+m)::(i-m,j+m)::(i+k,j+m)::(i+m,j+m)::(i,j+k)::(i,j-k)::(i,j+m)::(i,j-m)::[]
                     done
                   done*) [] (* je renvoie temporairement une liste vide pour pas me casser la tête et que le code compile *)
  | Bouclier -> [(i+2, j); (i-2, j); (i, j+2); (i, j-2)]
  | Lancier -> [(i+1, j+2); (i+1, j-2); (i-1, j+2); (i-1, j-2)]
  | General | Officier | Garde -> [(i+3, j); (i, j+3); (i-3, j); (i, j-3); (i+2, j+2); (i-2, j-2); (i-2, j+2); (i+2, j-2)]
  | Chambellan -> [(i+3,j+3); (i-3, j-3); (i-3, j+3); (i+3, j-3)]
  | Ouvrier | Souverain -> [(i+1, j); (i-1, j); (i, j+1); (i, j-1); (i+1, j+1); (i-1, j-1); (i-1, j+1); (i+1, j-1)]
