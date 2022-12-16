
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

let rec liste_element nb e = 
  if nb<1 
  then []
  else e::liste_element (nb-1) e ;;
         
let rec concat l1 l2 =
  match l1 with 
    [] -> l2
  |x::r -> x::concat r l2;;

let rec max_liste l =
  match l with
  | [] -> failwith "max_list"
  | [h] -> h
  | x::(y::r) -> 
      if x>y
      then max_liste (x::r)
      else max_liste (y::r);;

let rec min_liste l =
  match l with
  | [] -> failwith "min_list"
  | [h] -> h
  | x::(y::r) -> 
      if x<y
      then min_liste (x::r)
      else min_liste (y::r);;

(*let rec trois_uplet x y l=
   match l with 
     [] -> []
   |x::r ->
       if x=y
       then (x,nb_occurences(x))::trois_uplet x y r
       else trois_uplet x y r;;*)

let min_max ordre l =
  let rec mmax_aux ordre l min_liste max_liste =
    if l = []
    then failwith "liste vide"
    else
    if ordre (max_liste l) (min_liste l)
    then (max_liste l ,min_liste l, (+))
    else if ordre (min_liste l) (max_liste l)
    then (min_liste l, max_liste l, (-))
    else failwith "erreur valeur de la liste"
  in mmax_aux ordre l min_liste max_liste;; 

(*let nb_chaque ordre l=
  let rec aux ordre l acc =
    match l with
      [] -> []
    |[x] -> acc
    |x::y::r -> 
        if ordre x (min_liste l)
        then aux ordre (supprimer_tous x r) ((x,nb_occurences(x))::acc)
        else if ordre y (min_liste l)
        then aux ordre (supprimer_tous x r) ((y,nb_occurences(y))::acc)
        else if ordre x (max_liste l)
        then aux ordre r acc
        else if ordre y (max_liste l)
        then aux ordre r acc
        else acc
  in aux ordre l [];;*)
  
let reconstituer l = 
  let rec cons l nb e acc =
    match l with
      [] -> []
    |[(x,y)]-> acc
    |(x,y)::r -> cons r y x (concat (liste_element y x) (acc)) 
  in cons l 0 0 [] ;;                

          (*let tri_comptage ordre l =  *)
  
(*let inserer_spatule ordre l n =
   let rec ins_aux ordre l n i =
     if l = []
     then []
     else 
     if i=String.length l 
     then l.[i]       
then ins_aux ordre l n (i+1)  *)


      
      



    
    
  
  







































