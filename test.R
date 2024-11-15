# Remplacer les NA dans moyennes par une valeur par défaut (ex : 0)
moyennes_visuel[is.na(moyennes_visuel)] <- 0
moyennes_tactile[is.na(moyennes_tactile)] <- 0
moyennes_auditif[is.na(moyennes_auditif)] <- 0
moyennes_ecriture[is.na(moyennes_ecriture)] <- 0
moyennes_hedonique[is.na(moyennes_hedonique)] <- 1  # Éviter les divisions par NA

# Créer le tableau
tableaux <- data.frame(
  moyennes_visuel,
  moyennes_tactile,
  moyennes_auditif,
  moyennes_ecriture
)

# Calculer la différence absolue entre chaque valeur et la moyenne hédonique correspondante
diff_abs_df <- as.data.frame(sapply(1:nrow(tableaux), function(i) {
  abs(tableaux[i, ] - moyennes_hedonique[i])  # Calcul des différences absolues pour chaque ligne
}))

# Afficher le DataFrame des différences absolues
print(diff_abs_df)
