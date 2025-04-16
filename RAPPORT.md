# RAPPORT DE PROJET APS

## 1. Avancement

À ce stade du projet, nous avons bien entamé la version APS2. 
Nous avons un typeur fonctionnel mais notre évaluateur ne l'est que partiellement.

---

## 2. État des lieux des fonctionnalités

### Fonctionnalités implémentées avec succès

- **APS0**  
  Lors du développement d'APS1, nous avons renforcé notre base de tests, ce qui nous a permis de détecter certaines erreurs dans le typeur. Ces erreurs ont été corrigées dans le dossier `APS1`, mais le dossier `APS0` n'a pas été mis à jour en conséquence.  
  **En résumé :** l'évaluateur et le typeur fonctionnent correctement pour APS0, mais uniquement dans le dossier `APS1`.

- **APS1**  
  L’évaluateur et le typeur fonctionnent correctement selon l'ensemble de nos tests.

- **APS1a**  
  L’évaluateur et le typeur fonctionnent correctement selon l'ensemble de nos tests.  
  
- **APS2**  
  Le typeur fonctionne correctement selon l'ensemble de nos tests. 

### Fonctionnalités non encore fonctionnelles ou à corriger

- **APS2**  
  L'évaluateur n'est pas terminé, il ne passe pas nos tests 22 à 25.

---

## 3. Choix d'implémentation

### Gestion des primitives

La gestion des primitives est faite directement dans l'environnement initial.  
Contrairement à ce qui est présenté dans le cours, nous ne passons pas par `prim1` et `prim2`.

### Gestion de la mémoire et du flux de sortie

La mémoire est modélisée comme une liste d’associations entre adresses et valeurs.  
Pour `APS1`, l’adresse est un entier unique. À chaque nouvelle allocation, nous utilisons la taille actuelle de la mémoire comme nouvelle adresse, garantissant ainsi que chaque adresse est distincte. Ce choix simplifie l’implémentation tout en assurant une croissance linéaire de l’espace mémoire.  
De plus, lors de l’allocation, une valeur par défaut est associée à l’adresse, nous avons choisi `0` comme valeur par défaut.

Le flux de sortie est géré de manière simple : les valeurs à afficher lors des instructions `echo` sont accumulées dans une liste d'entiers.

### Environnement de typage

L’environnement de typage est représenté comme une liste d’associations entre identifiants et types.  
L’environnement initial contient les constantes booléennes (`true`, `false`) ainsi que les primitives (`add`, `sub`, `not`, etc.) avec leurs types respectifs.  
Chaque définition ajoute une nouvelle entrée en tête de l’environnement, ce qui permet un parcours et une vérification des types simples.

---

## 4. Répartition du travail

La majorité du travail a été réalisée lors des TME, où nous travaillions sur la même machine (sauf quelques rares occasions).  
En dehors du cadre des TME, nous avons travaillé individuellement, tout en communiquant à distance pour nous assurer de ne pas travailler sur la même chose, et pour nous entraider.

Nous ne nous sommes pas assigné de tâches fixes, la répartition s’est faite de manière naturelle.

---

## 5. Sources d'inspiration

Nous avons essayé d’avoir une implémentation au plus proche possible du cours.  
Nos principales sources d’inspiration ont donc été les formulaires d’APS, les notes de cours, ainsi que les exemples donnés en TME.

Nous avons également utilisé ChatGPT comme outil d’assistance :

- Pour nous débloquer lors de certaines erreurs Prolog, notamment au début, lorsque nous n’étions pas encore familiers avec ce langage.
- Pour accélérer la production des fichiers `README.md`, `WHO.md`, `RAPPORT.md`, principalement pour la syntaxe Markdown et la correction orthographique.
