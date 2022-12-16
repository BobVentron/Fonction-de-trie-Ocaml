(* liste_aleatoire b n
renvoie une liste de n entiers
compris entre 0 et (b - 1) inclus *)

let rec liste_aleatoire b n =
  if n <= 0
  then []
  else
    let k = (Random.int b) in
    k::(liste_aleatoire b (n - 1));;

let l = liste_aleatoire 200 31;;

(*let temps_debut = Sys.time () in
  let _ = min_list l in
  let temps_fin = Sys.time () in
  temps_fin -. temps_debut;;
 *)

(*Tri comptage*)
let rec nb_occurences e l =
  match l with
    [] -> 0
  |x::r -> 
      if x=e 
      then 1 + nb_occurences e r 
      else nb_occurences e r ;;

let rec supprimer_tous e l =
  match l with
    [] -> []
  |x::r -> 
      if x=e
      then supprimer_tous e r 
      else x::supprimer_tous e r ;;

let min_max ordre l = 
  let rec min_max_aux ordre l min max =
    match l with
      [] -> if min > max
            then (min , max, (-))
            else (min, max, (+))
    | x::r -> if ordre max x 
              then min_max_aux ordre r min x
              else if ordre x min 
              then min_max_aux ordre r x max
              else min_max_aux ordre r min max
  in
  match l with
    x::r -> min_max_aux ordre r x x
  | _ -> failwith("err");;

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

let rec liste_element nb e = 
  if nb<1 
  then []
  else e::liste_element (nb-1) e ;;
         
let rec concat l1 l2 =
  match l1 with 
    [] -> l2
  |x::r -> x::concat r l2;;

let rec reconstituer l =
  match l with 
    [] -> []
  | (e, nb_e)::r -> concat (liste_element nb_e e) (reconstituer r);;

let tri_comptage ordre l =
  let l1 = nb_chaque ordre l in
  reconstituer l1;;

(*Fonction de Trie pas sélection du minimum*)
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
  let rec min_list_indice_aux ordre i l a =
    match l with
      [] -> failwith(" Erreur min_list_indice : il n'y a pas d'élément dans la liste")
    | x::r -> if i = (a - 1)
              then let (e, indice) = min_list ordre (x::r) in
                   (e, indice + a)
              else min_list_indice_aux ordre i r (a + 1)
  in
  min_list_indice_aux ordre  i l 0;;
(*
a = accumulateur

 *)

let rec echange e1 i1 e2 i2 l =
  if i1 = i2
  then l
  else match l with
         [] -> failwith("Erreur echange : il n'y a pas d'élément dans la liste")
       | x::r -> if i1 = 0
                 then e2::(echange e1 (i1 - 1) e2 (i2 - 1) r)
                 else
                   if i2 = 0
                   then e1::r
                   else x::(echange e1 (i1 - 1) e2 (i2 - 1) r);;

(*
e1 = element 1
i1 = indice de l'element 1
e2 = element 2
i1 = indice de l'element 2 
 *)

let rec nieme n l =
  match l with
    [] -> failwith("Erreur nieme : il n'y a pas d'élément dans la liste")
  | x::r ->
     if n = 0
     then x
     else nieme (n - 1) r;;


let tri_selection_min ordre l =
  let rec tri_selction_min_aux ordre l a =
   if a = List.length l - 1
    then l
    else  let (e, i) = min_list_indice  ordre (a - 1) l in
          tri_selction_min_aux ordre (echange (nieme a l) a e i l) (a + 1)
  in
  tri_selction_min_aux ordre l 0;;

(*Tri de crêpes*)

let inserer_spatule ordre l n =
  let rec inserer_spatule_aux ordre l n i e_max i_max=
    if n = i
    then i_max
    else
      match l with
        x::r ->
         if ordre e_max x || x = e_max
         then inserer_spatule_aux ordre r n (i + 1) x (i + 1)
         else inserer_spatule_aux ordre r n (i + 1) e_max i_max
      | _ -> failwith("err1")
  in
  match l with
    x::r -> inserer_spatule_aux ordre r n 0 x 0
  | _ -> failwith("err2");;

let retourner_spatule l i =
  let rec retourner_spataule_rec l i i_parcours l_retourner=
    if i + 1 = i_parcours
    then concat l_retourner l
    else
      match l with
        x::r -> retourner_spataule_rec r i (i_parcours + 1) (x::l_retourner)
      | _ -> failwith("err3")
  in
  retourner_spataule_rec l i 0 [];;

let etape ordre l n =
  let i_max = inserer_spatule ordre l n in
  if n = i_max
  then l
  else retourner_spatule (retourner_spatule l i_max) n;;

let tri_crepes ordre l =
  let rec tri_crepes_aux ordre l n =
    if n = 0
    then l
    else tri_crepes_aux ordre (etape ordre l n) (n - 1)
  in
  tri_crepes_aux ordre l ((List.length l) -1);;
             
(*fonction de trie pas encerclement*)
let renverser l =
  let rec renverser_aux l l_final =
    match l with
      [] -> l_final
    | x::r -> renverser_aux r (x::l_final)
  in
  renverser_aux l [];;

let  encercler ordre l =
  let rec encercle_aux ordre l1 l2 acc nl=
    match (l1, l2) with
      x1::r1, x2::r2 -> if 1  >  acc
                        then  if x1 = x2
                              then (x1::nl)
                              else nl
                        else if ordre x1 x2
                        then x1::encercle_aux ordre r1 r2 (acc - 1)  (x2::nl)
                        else x2::encercle_aux ordre r1 r2 (acc - 1) (x1::nl)
    | _ -> failwith("err")
  in
  encercle_aux ordre l (renverser l) ((List.length l) / 2) [];;
    
let separer l =
  let rec separer l nl acc =
    match l with
      x::r -> if 1 > acc
              then (renverser(nl), (x::r))
              else separer r (x::nl) (acc-1)
    |_ -> failwith("err")
              
  in
  separer l [] ((List.length l)/2);;

let rec fusion ordre l1 l2 =
  match (l1, l2) with
    [], [] -> []
  | [], x -> x
  | x, [] -> x
  | x1::r1, x2::r2 -> if ordre x1 x2
                      then x1::fusion ordre r1 (x2::r2)
                      else x2::fusion ordre (x1::r1) r2;;

let rec tri_encerclement ordre l =
  if List.length l = 1
  then l
  else
    let (l1, l2) = separer (encercler  ordre l) in
          fusion ordre (tri_encerclement ordre l1)  (tri_encerclement ordre l2);;
