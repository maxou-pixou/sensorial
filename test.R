# Filtrer les hommes
stats_hommes <- stats2[stats2$Genre == "F", ]

# Calculer les moyennes par catégorie pour les hommes
moyennes_visuel_h <- rowMeans(stats_hommes[, c("Esthetique", "Couleur", "Finitions")], na.rm = TRUE)
moyennes_tactile_h <- rowMeans(stats_hommes[, c("PriseEnMain", "Texture", "Poids", "Ergonomie")], na.rm = TRUE)
moyennes_auditif_h <- rowMeans(stats_hommes[, c("Songeneral", "Frottement", "Cliquetis", "Intensite")], na.rm = TRUE)
moyennes_ecriture_h <- rowMeans(stats_hommes[, c("RepEncre", "IntensiteEncre", "Fluidite", "Sechage")], na.rm = TRUE)
moyennes_hedonique_h <- rowMeans(stats_hommes[, c("Appreciation", "Aisance", "RachatBic")], na.rm = TRUE)

# Remplacer les NA dans les moyennes par des valeurs par défaut
moyennes_visuel[is.na(moyennes_visuel)] <- 0
moyennes_tactile[is.na(moyennes_tactile)] <- 0
moyennes_auditif[is.na(moyennes_auditif)] <- 0
moyennes_ecriture[is.na(moyennes_ecriture)] <- 0
moyennes_hedonique[is.na(moyennes_hedonique)] <- 1  # Pour éviter les divisions par NA

# Créer un DataFrame pour contenir les moyennes
tableaux <- data.frame(
  Individu = 1:length(moyennes_visuel),  # Ajouter un identifiant pour chaque individu
  Visuel = moyennes_visuel,
  Tactile = moyennes_tactile,
  Auditif = moyennes_auditif,
  Ecriture = moyennes_ecriture,
  Hedonique = moyennes_hedonique
)

# Restructurer les données en format long pour l'ANOVA
data_long <- tableaux %>%
  pivot_longer(
    cols = c(Visuel, Tactile, Auditif, Ecriture, Hedonique),
    names_to = "Categorie",
    values_to = "Valeur"
  )

# Réaliser l'ANOVA
anova_result <- aov(Valeur ~ Categorie, data = data_long)

# Afficher le résumé des résultats de l'ANOVA
summary(anova_result)

# Effectuer un test post-hoc Tukey
tukey_result <- TukeyHSD(anova_result)

# Afficher les résultats du test de Tukey
print(tukey_result)

# Extraire les résultats de Tukey dans un DataFrame
tukey_df <- as.data.frame(tukey_result$Categorie)

# Ajouter une colonne pour indiquer si les p-values sont significatives
tukey_df$Significatif <- tukey_df$`p adj` < 0.05

# Visualisation des résultats du test de Tukey
ggplot(tukey_df, aes(x = rownames(tukey_df), y = `p adj`, fill = Significatif)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(
    title = "Résultats du test de Tukey",
    x = "Comparaison",
    y = "Valeur p ajustée"
  ) +
  scale_fill_manual(values = c("gray", "red")) +  # Rouge pour p < 0.05, gris sinon
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_text(aes(label = round(`p adj`, 3)), vjust = -0.5, color = "black", size = 3)
