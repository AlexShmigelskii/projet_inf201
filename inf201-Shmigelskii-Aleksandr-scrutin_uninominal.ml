(* -----------------------------------------------------------------------
   inf201-Shmigelskii-Aleksandr-scrutin_uninominal.ml : compte-rendu projet scrutin_uninominal
   Shmigelskii Aleksandr <Aleksandr.Shmigelskii@univ-grenoble-alpes.fr> 
   ----------------------------------------------------------------------- *)

(* Question 1 *)
type candidat = string;;

(* 
D'après consigne:
Un bulletin sera représenté par une chaîne de caractères (qui représente un candidat)
*)
type bulletin = candidat;;
type urne = bulletin list;;
type score = int;;
type panel = candidat list;;
type depouillement = (candidat * score) list;;


let lc1: panel = ["Eric";"Kyle";"Stan"];;
let u1: urne = ["Eric";"Kyle";"Stan";"Kyle";"Kyle";"Stan";"Eric";"Eric";
         "Kyle";"Eric";"Stan";"Eric";"Eric";"Eric";"Stan";"Stan"];;
let dep1: depouillement = [("Eric", 7); ("Kyle", 4); ("Stan", 5)];;

(* Question 2 *)

(* 
Je vais utiliser une méthode récursive pour compter le nombre d'occurrences
d'une chaîne de caractères dans la liste 

Équations de récurrence:

i)  compte _ [] = 0

ii) compte с pr::fin = ça depend
    
    compte c pr::fin = 1 + compte c fin, si c = pr
    compte c pr::fin = compte c fin, sinon
*)

let rec compte (c: candidat) (u: urne): score =
   match u with
   | [] -> 0
   | pr :: fin -> if (c = pr) then 1 + compte c fin else compte c fin
;;

assert (compte "Eric" u1 = 7);;

(* Question 3 *)

(* 
Nous allons parcourir la liste des candidats (panel) et compter l'apparition de chacun
d'entre eux sur la liste de l'urne.

Équations de récurrence:

i)  depouiller [] _ = []

ii) depouiller c::reste u = (c, compte c u) :: depouiller reste u

Si le candidat n'a pas de voix, la fonction compte renverra 0
et le depouillement contiendra la paire (c, 0).
*)

let rec depouiller (lc: panel) (u: urne): depouillement = 
   match lc with
   | [] -> []
   | c :: reste -> (c, compte c u) :: depouiller reste u
;;

assert (depouiller lc1 u1 = [("Eric", 7); ("Kyle", 4); ("Stan", 5)]);;


(* Question 4 *)

(* 
Nous utiliserons une fonction supplémentaire (ajout_candidate c s r) qui reçoit un candidat,
son score et une liste à laquelle il doit être ajouté.

Équations de récurrence:

i)  ajout_candidat c s [] -> [(c, s)]

ii) ajout_candidat c s (c2, s2) :: fin -> ça depend

                                          (c, s + s2) :: fin (si c = c2)
                                          (c2, s2) :: ajout_candidat c s fin (sinon)

*)

let rec ajout_candidat (c: candidat) (s: score) (r: depouillement): depouillement =
   match r with
   | [] -> [(c, s)]
   | (c2, s2) :: fin -> if (c = c2) then (c, s + s2) :: fin 
                        else (c2, s2) :: ajout_candidat c s fin
;;

(* 
Traitons récursivement la liste r2, en ajoutant chaque paire à r1 avec ajout_candidat.
*)

let rec union (r1: depouillement) (r2: depouillement): depouillement =
   match r2 with
   | [] -> r1
   | (c, s) :: fin -> let new_r1 = ajout_candidat c s r1 in
                      union new_r1 fin
;;

let dep1 = [("Eric", 7); ("Kyle", 4); ("Stan", 5)];;
let dep2 = [("Eric", 2); ("Stan", 1)];;
let dep3 = [("Alice", 3)];;

assert (union dep1 dep2 = [("Eric", 9); ("Kyle", 4); ("Stan", 6)]);;
assert (union dep1 dep3 = [("Eric", 7); ("Kyle", 4); ("Stan", 5); ("Alice", 3)]);;


(* Question 5 *)

(* 
Équations de récurrence:

i)  max_depouiller [] -> ??

Nous supposerons que la liste ne peut pas être vide

i) max_depouiller [x] -> x

Posons (c_max, s_max) = max_depouiller depouillement 

ii) max_depouiller (c, s) :: fin -> (c_max, s_max) si (s_max > s)
                                    (c, s) sinon
*)

let rec max_depouiller (d: depouillement) : candidat * score =
   match d with
   | [x] -> x
   | (c, s) :: rest -> let (c_max, s_max) = max_depouiller rest in
                       if s_max > s then (c_max, s_max) else (c, s)
;;

let dep1 = [("Eric", 7); ("Kyle", 4); ("Stan", 5)];;
assert (max_depouiller dep1 = ("Eric", 7));;


(* Question 6 *)

(* 
Ce n'est pas une fonction récursive.
1.	Tout d'abord, pour chaque candidat du panel, on compte le nombre de
   voix dans l'urne à l'aide de la fonction dépouiller.
2.	Ensuite, nous trouvons la paire (candidat, nombre de votes) avec le
   nombre maximum de votes en utilisant la fonction max_depouiller.
3. Étant donné que la condition ne permet pas d'obtenir une égalité, nous
   pouvons sans risque retirer le candidat de la paire obtenue.
*)

let vainqueur_scrutin_uninominal (u : urne) (lc : panel) : candidat =
   let d = depouiller lc u in
   let (c, _) = max_depouiller d in c
;;

let lc1: panel = ["Eric";"Kyle";"Stan"];;
let u1: urne = ["Eric";"Kyle";"Stan";"Kyle";"Kyle";"Stan";"Eric";"Eric";
                "Kyle";"Eric";"Stan";"Eric";"Eric";"Eric";"Stan";"Stan"];;
assert (vainqueur_scrutin_uninominal u1 lc1 = "Eric");;

(* Question 7 *)

(* 
Nous procéderons selon l'algorithme suivant:
1. Trouver le candidat ayant le plus grand nombre de voix.
2. Le supprimons du dépouillement.
3. Trouver le candidat ayant le plus grand nombre de voix parmi les candidats restants
4. Retourner la liste de ces deux paires. 

Pour cela, mettons en œuvre une fonction supplémentaire
pour supprimer un candidat de la liste

*)

let rec supprimer_candidat (c: candidat) (d: depouillement) : depouillement =
   match d with
   | [] -> []
   | (c2, s2) :: fin -> if (c = c2) then fin
                        else (c2, s2) :: supprimer_candidat c fin
;;

let deux_premiers (d: depouillement) : depouillement =
   let (c1, s1) = max_depouiller d in
   let d_sans_premier = supprimer_candidat c1 d in
   let (c2, s2) = max_depouiller d_sans_premier in [(c1, s1); (c2, s2)]
;;

let dep1 = [("Eric", 7); ("Kyle", 4); ("Stan", 5)];;
assert (deux_premiers dep1 = [("Eric", 7); ("Stan", 5)]);;
