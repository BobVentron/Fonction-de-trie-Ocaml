# Fonction-de-trie-Ocaml
# Différente fonction de trie sur Ocaml avec l'analyse de leur efficacité.

  Voici quatre méthodes de tri de listes différentes. Ces algorithmes, à l’exception du premier, pourront trier des listes de n’importe quel type d’éléments, et dans n’importe quel ordre.

## **La permier méthode s'appelle tri comptage :**
  Cette méthode de tri commence par compter, dans l’ordre, pour chaque élément pouvant apparaître dans la liste à trier, le nombre de fois où il apparaît. Ceci fait, il suffira de reconstituer la liste, en répétant chaque élément le bon nombre de fois, du plus petit au plus grand. Notez que cet algorithme permet uniquement de trier des listes d’entiers, contenant au moins deux valeurs différentes, et seulement selon les ordres mathématiques < et >.
  
## **Ensuite nous avons le tri par sélection du minimum**
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

## **Puis il y a le tri de crêpes**
Le tri de crêpes considère les éléments à trier comme étant des crêpes de diamètre plus ou moins grand, proportionnellement à leur valeur. La liste à trier est alors représentée comme une pile de crêpes (le haut de la pile contenant le premier élément de la liste). L’idée est de retourner intelligemment, à l’aide d’une spatule, une partie des crêpes du dessus de la pile, et de répéter le processus jusqu’à obtenir une pile de crêpes triées par taille.

## **Enfin nous avons le tri par encerclement**
Dans cette méthode de tri, on commence par encercler la liste à trier, i.e. :
1. on compare le premier élément de la liste avec le dernier, et on les échange s’ils ne respectent pas l’ordre ;
2. puis on fait de même avec le deuxième élément de la liste et l’avant-dernier ;
3. puis avec le troisième élément de la liste et l’avant-avant-dernier, etc.
Une fois l’encerclement terminé, on sépare en deux les éléments de la liste obtenue en récupérant d’une part ceux du début, et d’autre part ceux de la fin. On recommence le processus sur ces deux listes, en les enclerclant puis en les divisant en deux.
Quand la liste considérée ne contient plus qu’un seul élément, le processus s’arrête et la renvoie telle quelle (car elle est triée).
La liste triée globale est obtenue en fusionnant, à chaque fois, le résultat du tri par encerclement des deux listes.

Il est possible de convertir un fichier .tex en .pdf avec la commande (a exécuter dans un terminal): pdflatex nondufichier.tex 
Attention a bien avoir pdflatex
