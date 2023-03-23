(* 1: Tri comptage *)

(* nb_occurences e l calcule le nombre d'occurences d'un élément dans une liste *)
let rec nb_occurences e l = 
  match l with
    [] -> 0
  |x::r -> 
      if x=e 
      then 1 + nb_occurences e r 
      else nb_occurences e r ;;

(* supprimer_tous e l supprime tous les éléments e de la liste l *)      
let rec supprimer_tous e l = 
  match l with
    [] -> []
  |x::r -> 
      if x=e
      then supprimer_tous e r 
      else x::supprimer_tous e r ;;



(* min_max ordre l renvoie, selon l'ordre, la valeur maximale (max), minimale(min) 
   et une opération ( (+) ou (-) ) dans un 3-uplet *)
let min_max ordre l = 
  let rec min_max_aux ordre l min max =
    match l with
      [] -> if min > max
            then (min , max, (-))    (* Le (-) réprésente l'opération à réaliser entre min et max *)
            else (min, max, (+))     (* Le (+) réprésente l'opération à réaliser entre max et min *)
    | x::r -> if ordre max x 
              then min_max_aux ordre r min x
              else if ordre x min 
              then min_max_aux ordre r x max
              else min_max_aux ordre r min max
  in
  match l with
    x::r -> min_max_aux ordre r x x
  | _ -> failwith("Erreur min_max: il est impossible de savoir le minimun et le maximum d'une liste vide");;
  (* Répresente avec une exception tous les autres cas non voulus pour min_max *)


(* nb_chaque crée une liste de 2-uplet composer d'un élément de l 
   et du nombre de fois que chaque élément apparait dans une liste l selon l'ordre *)
let nb_chaque ordre l = 
  let rec nb_chaque_aux ordre l i max operateur =
    if ordre max i
    then []
    else let nb = nb_occurences i l in
         if nb = 0
         then nb_chaque_aux ordre l (operateur i 1) max operateur
         else (i, nb)::(nb_chaque_aux ordre (supprimer_tous i l) (operateur i 1) max operateur)
  in
  let (min, max, operateur) = min_max ordre l in
  nb_chaque_aux ordre l min max operateur;;
(*
operateur = (+) ou (-)
max = maximum de la liste    
*)

(* liste_element nb e revoie une liste composée de nb fois l'élément e *)
let rec liste_element nb e = 
  if nb < 1 
  then []
  else e::liste_element (nb-1) e ;;


(* concat l1 l2 concatène deux listes l1 et l2 et renvoie le résultat *)         
let rec concat l1 l2 = 
  match l1 with 
    [] -> l2
  |x::r -> x::concat r l2;;


(* reconstituer l permet de reconstituer une liste à partir de la liste l résultat de nb_chaque 
   et renvoie alors la liste de départ *)  
let rec reconstituer l = 
  match l with 
    [] -> []
  | (e, nb_e)::r -> concat (liste_element nb_e e) (reconstituer r);;
(*
e = élément dans l
nb_e = nombre d'occurences de l'élémént e dans l    
*)
