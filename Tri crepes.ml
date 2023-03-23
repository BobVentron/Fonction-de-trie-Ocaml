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
