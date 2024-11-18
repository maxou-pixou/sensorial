moyennes_visuel <- apply(stats2[, c("Esthetique", "Couleur", "Finitions")], 1, mean, na.rm = TRUE)
moyennes_tactile<- apply(stats2[, c("PriseEnMain", "Texture", "Poids", "Ergonomie")], 1, mean, na.rm = TRUE)
moyennes_auditif<- apply(stats2[, c("Songeneral", "Frottement", "Cliquetis","Intensite")], 1, mean, na.rm = TRUE)
moyennes_ecriture<- apply(stats2[, c("RepEncre", "IntensiteEncre", "Fluidite","Sechage")], 1, mean, na.rm = TRUE)
moyennes_hedonique<- apply(stats2[, c("Appreciation", "Aisance", "RachatBic")], 1, mean, na.rm = TRUE)

# Remplacer les NA dans moyennes par une valeur par défaut (ex : 0)
moyennes_visuel[is.na(moyennes_visuel)] <- 0
moyennes_tactile[is.na(moyennes_tactile)] <- 0
moyennes_auditif[is.na(moyennes_auditif)] <- 0
moyennes_ecriture[is.na(moyennes_ecriture)] <- 0
moyennes_hedonique[is.na(moyennes_hedonique)] <- 1  # Éviter les divisions par NA

# Créer le tableau (en s'assurant que toutes les colonnes sont numériques)
tableaux <- data.frame(
  moyennes_visuel = as.numeric(moyennes_visuel),
  moyennes_tactile = as.numeric(moyennes_tactile),
  moyennes_auditif = as.numeric(moyennes_auditif),
  moyennes_ecriture = as.numeric(moyennes_ecriture)
)

# Calculer la différence absolue entre chaque valeur et la moyenne hédonique correspondante
diff_abs_df <- as.data.frame(sapply(1:nrow(tableaux), function(i) {
  abs(tableaux[i, ] - moyennes_hedonique[i])  # Calcul des différences absolues pour chaque ligne
}))

# Inverser (transposer) les lignes et les colonnes du DataFrame diff_abs_df
diff_abs_df <- as.data.frame(t(diff_abs_df))  # Assurez-vous que la transposition est correcte

# Convertir explicitement les valeurs en numérique après la transposition
diff_abs_df[] <- lapply(diff_abs_df, as.numeric)

# Calculer la somme des différences absolues pour chaque type de moyenne
somme_diff_abs <- colSums(diff_abs_df)

# Définir de nouveaux noms pour les colonnes
noms_colonnes <- c("Visuel", "Tactile", "Auditif", "Écriture")

# Tracer l'histogramme des sommes des différences absolues
barplot_heights <- barplot(
  somme_diff_abs, 
  main = "Somme des différences absolues par type de moyenne", 
  col = "skyblue", 
  xlab = "Indices", 
  ylab = "Somme des différences absolues", 
  ylim = c(0, max(somme_diff_abs) + 5),  # Ajuster les limites de l'axe Y
  names.arg = noms_colonnes  # Utiliser les noms des colonnes comme labels
)

# Ajouter les valeurs des sommes au-dessus des barres, arrondies à la première décimale
text(barplot_heights, 
     somme_diff_abs + 1,  # Positionner le texte juste au-dessus de chaque barre
     labels = round(somme_diff_abs, 1),  # Arrondir à la première décimale
     col = "black",       # Couleur du texte
     cex = 0.8,           # Taille du texte
     pos = 3)             # Position du texte au-dessus de la barre

