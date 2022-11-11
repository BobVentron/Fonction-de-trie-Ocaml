(* liste_aleatoire b n
renvoie une liste de n entiers
compris entre 0 et (b - 1) inclus *)

let rec liste_aleatoire b n =
  if n <= 0
  then []
  else
    let k = (Random.int b) in
    k::(liste_aleatoire b (n - 1));;

let l = (* génération d’une liste aléatoire *)

let temps_debut = Sys.time () in
  let _ = (* tri de l *) in
  let temps_fin = Sys.time () in
  temps_fin -. temps_debut;;
