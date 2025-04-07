# Projet APS — Analyse des Programmes et Sémantiques

Projet réalisé dans le cadre du cours **Analyse des Programmes et Sémantiques** (MU4IN503) à Sorbonne Université.

## Structure du projet

```
aps-projet/
├── APS0/
│   ├── printer.ml
│   └── ... (meme fichiers que APS1)
├── APS1/
│   ├── ast.ml              # Définition de la grammaire
│   ├── eval.ml             # Évaluateur OCaml (WIP)
│   ├── lexer.mll           # Analyseur lexical
│   ├── Makefile            # Compilation des différents exécutables
│   ├── parser.mly          # Analyseur syntaxique (grammaire)
│   ├── prologTerm.ml       # Génère le Prolog à partir des fichiers test .aps
│   ├── run.sh              # Script de test automatique (typeur + eval sur l'ensemble des tests)
│   └── typeur.pl           # Typeur en Prolog
└── Samples/
    └── prog*.aps           # Fichiers de tests (corrects & erronés)
```


---

## Build

Placez-vous dans le dossier **APS1** puis exécute :

```bash
make
```

Cela compile deux exécutables :
- `prologTerm` : transforme un fichier `.aps` en un terme Prolog compatible avec le typeur `typeur.pl`
- `eval` : évalue le programme


---

## Run

1. Assurez-vous d'avoir les droits d'exécution sur `run.sh` :

```bash
chmod +x run.sh
```

2. Puis lancez les tests :

```bash
./run.sh
```

Ce script :
1. Parcourt tous les fichiers `.aps` du dossier `Samples/`
2. Exécute `prologTerm` sur chaque fichier
3. Passe le terme généré à `typeur.pl`
4. Lance `eval` si le typage est correct


---

## Fichiers de tests `.aps`

Chaque fichier `.aps` est un programme écrit dans un langage simplifié :

| Fichier | Description |
|--------|-------------|
| `prog01.aps` | Test `ECHO` avec une constante entière |
| `prog01-err1.aps` | Erreur de typage : `ECHO` d'une chaîne non supportée |
| `prog02.aps`, `prog03.aps`,`prog04.aps` | Test d'applications avec `add`, `mul`, `sub`, `div` |
| `prog02-err1.aps` | Erreur de typage : opérande non entier |
| `prog05.aps`, `prog06.aps` | Test `if` booléen correctement typé avec `or` et `and`|
| `prog05-err1.aps` | Erreur de typage : `if` avec une condition entière (non booléenne) |
| `prog07.aps` | Test `FUN` avec paramètres bien typés (*annale ER1 2023, ex1 P1* ) |
| `prog07-err*.aps` | Erreurs de typage : const mal typée, mauvais appel de fonction, ...|
| `prog08.aps` | Test `FUN REC` correctement typée (*annale ER1 2023, ex2 P3* ) |
| `prog09.aps` | Test `FUN REC` avec abstractions (*annale ER1 2025, ex1 P1* ) |
| `prog09-err1.aps` | Erreur de typage |
| `prog11.aps` | Test `VAR` et `SET` |
| `prog11-err1.aps` | Erreurs de typage : affectation d'un entier à une variable booléenne |
| `prog12.aps`, `prog13.aps`, `prog14.aps` | Test `IF` et `WHILE` |
| `prog12-err1.aps`, `prog13-err1.aps`, `prog14.aps` | Erreurs de typage : condition entiere (non booléenne) |
| `prog15.aps`, `prog16.aps`, `prog17.aps` | Test `PROC` et `PROC REC`|
| `prog15-err1.aps`, `prog16-err1.aps` | Erreurs de typage : mauvais nombre d'arguments |


---

## Work in Progress

- Completer/corriger `eval.ml`
- Affichage des valeurs retournées pour chaque test
- Écriture de tests supplémentaires afin de fortifiser notre base de tests


---

## Auteurs

- [VILA Rodrigo](https://github.com/rvila94)
- Arlais Julien

---


