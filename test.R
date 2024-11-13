#moyenne des catégories
moyennes_visuel <- apply(stats2[, c("Esthetique", "Couleur", "Finitions")], 1, mean, na.rm = TRUE)
moyennes_tactile <- apply(stats2[, c("PriseEnMain", "Texture", "Poids","Ergonomie")], 1, mean, na.rm = TRUE)
moyennes_auditif <- apply(stats2[, c("Songeneral", "Frottement", "Cliquetis","Intensite")], 1, mean, na.rm = TRUE)
moyennes_ecriture <- apply(stats2[, c("RepEncre", "IntensiteEncre", "Fluidite","Sechage")], 1, mean, na.rm = TRUE)

#vecteur témoin
moyennes_hedonique <- apply(stats2[, c("Appreciation", "Aisance", "RachatBic")], 1, mean, na.rm = TRUE)

#data
tableaux <- data.frame(
  moyennes_visuel ,
  moyennes_tactile ,
  moyennes_auditif ,
  moyennes_ecriture
)

# Trouver pour chaque élément du vecteur témoin la colonne avec la valeur la plus proche
resultat <- apply(tableaux, 1, function(ligne) {
  # Calculer la différence absolue entre chaque élément de vecteur_temoin et chaque colonne de la ligne
  diff_abs <- abs(moyennes_hedonique - ligne)
  
  # Identifier l'indice (ou nom) de la colonne avec la différence minimale
  colnames(tableaux)[which.min(diff_abs)]
})

resultat
