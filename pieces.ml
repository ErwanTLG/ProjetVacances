type piece  = Bombardier | Cavalier | Bouclier | Unite | General | Officier | Lancier | Archer | Ouvrier | Souverain | Garde | Chambellan

let pieces = Array.make_matrix (fst Main.dimensions) (snd Main.dimensions)

let deplacements_possibles i j =
  let l = [] in
  match pieces.(i).(j) with
  |None -> l
  |Bombardier |Archer |Unite -> (i+1,j)::(i-1::j)::(i,j+1)::(i,j-1)::l
  |Cavalier -> for k = 1 to 4 do
                     for m = 1 to 4 do
                       (i-k,j)::(i-m,j)::(i+k,j)::(i+m,j)::(i-k,j-k)::(i-m,j-k)::(i+k,j-k)::(i+m,j-k)::(i-k,j+k)::(i-m,j+k)::(i+k,j+k)::(i+m,j+k)::(i-k,j-m)::(i-m,j-m)::(i+k,j-m)::(i+m,j-m)::(i-k,j+m)::(i-m,j+m)::(i+k,j+m)::(i+m,j+m)::(i,j+k)::(i,j-k)::(i,j+m)::(i,j-m)::l
                     done
                   done
  |Bouclier ->(i+2,j)::(i-2::j)::(i,j+2)::(i,j-2)::l
  |Lancier -> (i+1,j+2)::(i+1::j-2)::(i-1,j+2)::(i-1,j-2)::l
  |General |Officier |Garde -> (i+3,j)::(i,j+3)::(i-3,j)::(i,j-3)::(i+2,j+2)::(i-2,j-2)::(i-2,j+2)::(i+2,j-2)::l
  |Chambellan -> (i+3,j+3)::(i-3,j-3)::(i-3,j+3)::(i+3,j-3)::l
  |Ouvrier |Souverain -> (i+1,j)::(i-1::j)::(i,j+1)::(i,j-1)::(i+1,j+1)::(i-1,j-1)::(i-1,j+1)::(i+1,j-1)::l
