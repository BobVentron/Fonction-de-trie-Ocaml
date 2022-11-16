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
  
(*2éme fonction*)
(*Fonction de Tie pas sélection du minimum*)
let min_list ordre l =
  let rec min_list_aux ordre l a1 a2 =
    match l with
      [] -> failwith(" Erreur min_list: il n'y a pas d'élément dans la liste")
    | [x] -> (x, a2)
    | x::(y::r) -> if (ordre) x y || x = y
                   then min_list_aux ordre (x::r) (a1 + 1) a2
                   else min_list_aux ordre (y::r) (a1 + 1) (a1 +1)
  in
  min_list_aux ordre l 0 0;;

(*
a1 = postion du parcours de la liste
a2 = position du derniers element minimun trouver 
 *)

let min_list_indice ordre i l =
  let rec min_list_indice ordre i l a =
    match l with
      [] -> failwith(" Erreur min_list_indice: il n'y a pas d'élément dans la liste")
    | x::r -> if i = (a - 1)
              then let (e, indice) = min_list ordre (x::r) in
                   (e, indice + a)
              else min_list_indice ordre i r (a + 1)
  in
  min_list_indice ordre  i l 0;;

let rec echange e1 i1 e2 i2 l =
  if i1 = i2
  then l
  else match l with
         [] -> failwith("erreur")
       | x::r -> if i1 = 0
                 then e2::(echange e1 (i1 - 1) e2 (i2 - 1) r)
                 else
                   if i2 = 0
                   then e1::r
                   else x::(echange e1 (i1 - 1) e2 (i2 - 1) r);;

let rec nieme n l =
  match l with
    [] -> failwith("erreur")
  | x::r ->
     if n = 0
     then x
     else nieme (n - 1) r
