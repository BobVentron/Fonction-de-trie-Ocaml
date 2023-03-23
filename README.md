# Fonction-de-trie-Ocaml
Différente fonction de trie sur Ocaml avec l'analyse de leur efficacité.

  Voici quatre méthodes de tri de listes différentes. Ces algorithmes, à l’exception du premier, pourront trier des listes de n’importe quel type d’éléments, et dans n’importe quel ordre.

La permier méthode s'appelle Tri comptage :
  Cette méthode de tri commence par compter, dans l’ordre, pour chaque élément pouvant apparaître dans la liste à trier, le nombre de fois où il apparaît. Ceci fait, il suffira de reconstituer la liste, en répétant chaque élément le bon nombre de fois, du plus petit au plus grand. Notez que cet algorithme permet uniquement de trier des listes d’entiers, contenant au moins deux valeurs différentes, et seulement selon les ordres mathématiques < et >.
  
Ensuite nous avons le tri par sélection du minimum
Le principe de ce tri est simple : il s’agit de récupérer le plus petit élément de la liste, et de le placer en première position. Puis on recommence avec le second plus petit élément, qu’on place en deuxième position. Et ainsi de suite.
Plus précisément, pour trier une liste l dans n’importe quel ordre :
1. On récupère le plus petit élément de la liste entière.
2. On l’intervertit avec le premier élément de la liste.
3. Puisque le premier élément de la liste est maintenant à la bonne place :
  (a) On récupère le plus petit élément de la liste, à partir de son deuxième élément.
  (b) On l’intervertit avec le deuxième élément de la liste.
4. Puisque les deux premiers éléments de la liste sont maintenant à la bonne place :
  (a) On récupère le plus petit élément de la liste, à partir de son troisième élément.
  (b) On l’intervertit avec le troisième élément de la liste.
5. On continue ainsi jusqu’à ce que tous les éléments soient correctement placés.

