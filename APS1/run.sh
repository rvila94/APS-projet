#!/bin/bash

SAMPLES_DIR="../Samples"

echo "Lancement des tests pour APS1..."

for file in "$SAMPLES_DIR"/*.aps
do
    echo "____"
    echo "Test du fichier " $file ": " 
    echo "Lancement du typeur..."
    type_check=$(./prologTerm $file | swipl typeur.pl)
    echo $type_check
    if [[ $type_check = *"Type Error"* ]]; then
        continue
    fi
    echo "Lancement de l'evaluateur..."
    res=$(./eval < $file)
    echo $res
done