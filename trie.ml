(* liste_aleatoire b n
renvoie une liste de n entiers
compris entre 0 et (b - 1) inclus *)

let rec liste_aleatoire b n =
  if n <= 0
  then []
  else
    let k = (Random.int b) in
    k::(liste_aleatoire b (n - 1));;

let l = liste_aleatoire 5 100000 ;;

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


(* tri_comptage ordre l utilise les fonctions précédentes pour trier un liste l selon un ordre choisi *)
let tri_comptage ordre l = 
  let l1 = nb_chaque ordre l in
  reconstituer l1;;


(* 2: Fonction de Trie par sélection du minimum *)

(*min_list ordre l renvoie un 2-uplet composée du plus élément d'un liste l et son indice dans celle-ci*)
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


(* min_list ordre réutilise min_list mais ajoute la condition que l'on doit chercher à partir d'un indice i 
   dans l pour obtenir le minimum de la liste *)
let min_list_indice ordre i l =
  let rec min_list_indice_aux ordre i1 l i2 =
    match l with
      [] -> failwith(" Erreur min_list_indice : il n'y a pas d'élément dans la liste")
    | x::r -> if i = (i2 - 1)
              then let (e, indice) = min_list ordre (x::r) in
                   (e, indice + i2)
              else min_list_indice_aux ordre i1 r (i2 + 1)
  in
  min_list_indice_aux ordre  i l 0;;
(*
i1 = position de la fin de la liste 
i2 = position du parcours de la liste
 *)


(* echange e1 i1 e2 i2 l renvoie la liste l avec l'élément e1 se trouvant en l'indice i1 
   et le remplace par l'élément e2 en l'indice i2 *)
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
e1 = element numero 1
i1 = indice de l'element 1
e2 = element nuemro 2
i1 = indice de l'element 2 
 *)


(*nieme n l renvoie l’élément d’indice n de l 
   et renvoie un exception si la liste l est vide*)
let rec nieme n l =
  match l with
    [] -> failwith("Erreur nieme : il n'y a pas d'élément dans la liste")
  | x::r ->
     if n = 0
     then x
     else nieme (n - 1) r;;
(*
   n = indice d'un élément de l
*)


(* tri_selection_min ordre l utilise les fonctions vues précedemment 
   dont min_list_indice, echange et nieme *)
let tri_selection_min ordre l =
  let rec tri_selction_min_aux ordre l a =
   if a = List.length l - 1
    then l
    else  let (e, i) = min_list_indice  ordre (a - 1) l in
          tri_selction_min_aux ordre (echange (nieme a l) a e i l) (a + 1)
  in
  tri_selction_min_aux ordre l 0;;
(*
e = élément de l
i = indice d'un élément de l     
*)


(* 3: Tri de crêpes *)

(* inserer_spatule ordre l n renvoie l’indice de la plus grande crêpe de l selon ordre 
   (soit (<) ou (>)) pour un n de taille typique et l non vide *)
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
      | _ -> failwith("Erreur inserer_spatule_aux: n est trop grand")
  in
  match l with
    x::r -> inserer_spatule_aux ordre r n 0 x 0
  | _ -> failwith("Erreur inserer_spatule: il est impossible d'inserer la spatule dans une liste vide");;
(*
i = indice de l'élément n
e_max = élément maximal de l 
i_max = indice de l'élément maximal de l   
*)


(* (retourner_spatule : ’a list -> int -> ’a list)       
retourner_spatule l i renvoie la liste l, dans laquelle les crêpes situées au-dessus de
la spatule (donc de l’indice 0 à l’indice i inclus) ont été retournées *)

let retourner_spatule l i =
  let rec retourner_spataule_rec l i i_parcours l_retourner=
    if i + 1 = i_parcours
    then concat l_retourner l
    else
      match l with
        x::r -> retourner_spataule_rec r i (i_parcours + 1) (x::l_retourner)
      | _ -> failwith("Erreur retourner_spatule_rec: i est trop grand")
  in
  retourner_spataule_rec l i 0 [];;
(*
   i = indice d'un élément de l
   i_parcours = position du parcours de l 
   l_retourner = liste l retournée
*)


(* etape ordre l n renvoie la liste l obtenue après avoir réalisé l’étape d’indice n de l’al-
gorithme du tri de crêpes. On utilse alors ici inserer_spatule et retourner_spatule en succession*)
let etape ordre l n =
  let i_max = inserer_spatule ordre l n in
  if n = i_max
  then l
  else retourner_spatule (retourner_spatule l i_max) n;;
(*
i_max = indice de l'élément maximal de l 
*)

(* tri_crepes ordre l trie la liste l selon l’algorithme du tri de crêpes et l’ordre de tri
ordre passé en paramètre *)
let tri_crepes ordre l =
  let rec tri_crepes_aux ordre l n =
    if n = 0
    then l
    else tri_crepes_aux ordre (etape ordre l n) (n - 1)
  in
  tri_crepes_aux ordre l ((List.length l) -1);;
             

  (* 4: Fonction de Trie par Encerclement *)

(* renverser l renverse l’ordre des éléments de l *)
let renverser l =
  let rec renverser_aux l l_final =
    match l with
      [] -> l_final
    | x::r -> renverser_aux r (x::l_final)
  in
  renverser_aux l [];;
(*
l_final = est la liste l renversée   
*)


(* encercler ordre l renvoie la liste l après son encerclement selon ordre *)
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
(*
acc = accumulateur pour la condition d'arrêt 
nl = liste renversée que l'on compare à l1 et l2     
*)


(* separer l renvoie, sous la forme d’un couple, la liste des premiers éléments de l, et la
liste des derniers éléments de l *)
let separer l =
  let rec separer l nl acc =
    match l with
      x::r -> if 1 > acc
              then (renverser(nl), (x::r))
              else separer r (x::nl) (acc-1)
    |_ -> failwith("err")
              
  in
  separer l [] ((List.length l)/2);;

(*
acc = accumulateur pour la condition d'arrêt 
nl = liste renversée que l'on compare à l   
*)


(* fusion ordre l1 l2 fusionne deux listes l1 et l2 selon l'ordre *)
let rec fusion ordre l1 l2 =
  match (l1, l2) with
    [], [] -> []
  | [], x -> x
  | x, [] -> x
  | x1::r1, x2::r2 -> if ordre x1 x2
                      then x1::fusion ordre r1 (x2::r2)
                      else x2::fusion ordre (x1::r1) r2;;


(* tri_encerclement ordre l trie la liste l selon l’algorithme du tri par encerclement et
l’ordre de tri ordre passé en paramètre *)
let rec tri_encerclement ordre l =
  if List.length l = 1
  then l
  else
    let (l1, l2) = separer (encercler  ordre l) in
          fusion ordre (tri_encerclement ordre l1)  (tri_encerclement ordre l2);;

          
(* temps_debut retourne le temps, en secondes, 
que met le processeur de l’ordinateur pour faire le calcul d’un tri *)
let temps_debut = Sys.time () in
  let _ = tri_selection_min (<) l in
  let temps_fin = Sys.time () in
  temps_fin -. temps_debut;;
