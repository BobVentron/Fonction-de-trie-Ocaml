(* 2: Fonction de Tri par sélection du minimum *)

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
