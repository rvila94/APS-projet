# RAPPORT DE PROJET APS

## 1. Avancement
<!-- APS1a WIP -->

---

## 2. État des lieux des fonctionnalités

### Fonctionnalités implémentées avec succès
- **APS0**  
Lors du développement d'APS1, nous avons renforcé notre base de tests, ce qui nous as permis de détecter certaines erreurs dans le typeur. Ces erreurs ont été corrigées dans le dossier `APS1` mais le dossier `APS0` n'a pas été mis à jour en conséquence.  
**En résumé :** l'évaluateur et le typeur fonctionnent correctement pour APS0 mais uniquement dans le dossier `APS1`.
- **APS1**  
L'evaluateur et le typeur fonctionnent correctement selon l'ensemble de nos tests
- **APS1a**  
[WIP] 
### Fonctionnalités non encore fonctionnelles ou à corriger
- **APS1a**  
[WIP]

---

## 3. Choix d'implémentation

### Gestion des primitives
La gestion des primitives est faite directement dans l'environnement initial,  
contrairement à ce qui fait dans le cours, nous ne passons pas par un prim1 et prim2.


### Gestion de la mémoire et du flux de sortie
<!-- l'adresse est un entier, a chaque nouvelle adresse on veut que l'entier qui correspond a l'adresse cree soit incrementé pour que adresse(i+1) = adresse(i) + 1
     etant donné que la memoire est une liste de paire (adresse, valeur), on utilise la taille de cette liste comme entier pour la nouvelle adresse  -->

### Environnement de typage
<!-- inspirer de l'exemple prof donné en tme -->

---


## 4. Répartition du travail

La majorité du travail à été fait lors des tme où nous travaillions sur la même machine (sauf quelques rares occasions)  
En dehors du cadre des tme, nous avons travaillé individuellement tout en communiquant à distance pour s'assurer de ne pas travailler sur la même chose ainsi que s'entraider.  

On ne s'est pas assigné de taches à chacun, la repartition s'est faites de maniere plus naturelle.


---

## 5. Sources d'inspiration

Nous avons essayé d'avoir une implémentation au plus proche possible du cours,  
ainsi, nos "sources d'inspirations ont donc été les formulaire d'APS, les notes du cours, ainsi que les exemples donnés en tme.  
     
Nous avons aussi utilisé chatGPT comme outil d'assistance:
- Pour nous débloquer lors de certaines erreurs Prolog, notament au debut quand on n'etait pas assez familiers avec ce dernier.
- Pour accélérer la profuction des fichiers `README.md`, `WHO.md`, `RAPPORT.md`, principalement pour la syntaxe Markdown et les fautes.



