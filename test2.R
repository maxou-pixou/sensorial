

# Créer un DataFrame avec les moyennes des colonnes
df_moyennes <- data.frame(Moyennes = moyennes_colonnes)



# Restructurer les données dans un format long pour les colonnes 8 à 25
data_long <- stats2[, c(11,23,24,25)] %>%
  pivot_longer(cols = everything(), names_to = "Categorie", values_to = "Valeurs")

# Réaliser l'ANOVA
anova_result <- aov(Valeurs ~ Categorie, data = data_long)

# Résumé des résultats de l'ANOVA
summary(anova_result)

# Appliquer le test de Tukey (post-hoc)
tukey_result <- TukeyHSD(anova_result)

# Résumé des résultats du test de Tukey
summary(tukey_result)

# Extraire les résultats du test Tukey et les convertir en DataFrame
tukey_df <- as.data.frame(tukey_result$Categorie)

# Ajouter une colonne pour indiquer si la p-value est significative (<0.05)
tukey_df$Significatif <- tukey_df$`p adj` < 0.05

# Créer un joli graphique en histogramme avec ggplot2
library(ggplot2)

ggplot(tukey_df, aes(x = rownames(tukey_df), y = `p adj`, fill = Significatif)) +
  geom_bar(stat = "identity", show.legend = FALSE) +  # Créer des barres
  labs(title = "Résultats du test de Tukey",
       x = "Comparaison", y = "Valeur p ajustée") +
  scale_fill_manual(values = c("gray", "red")) +  # Rouge pour p < 0.05, gris sinon
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotation des labels de l'axe x
  geom_text(aes(label = round(`p adj`, 3)), vjust = -0.5, color = "black", size = 3)  # Ajouter les valeurs des p-value sur les barres
