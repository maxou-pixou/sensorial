

# Calculer les moyennes par catégorie pour les hommes
moyennes_visuel_h <- rowMeans(stats2[, c("Esthetique", "Couleur", "Finitions")], na.rm = TRUE)
moyennes_tactile_h <- rowMeans(stats2[, c("PriseEnMain", "Texture", "Poids", "Ergonomie")], na.rm = TRUE)
moyennes_auditif_h <- rowMeans(stats2[, c("Songeneral", "Frottement", "Cliquetis", "Intensite")], na.rm = TRUE)
moyennes_ecriture_h <- rowMeans(stats2[, c("RepEncre", "IntensiteEncre", "Fluidite", "Sechage")], na.rm = TRUE)
moyennes_hedonique_h <- rowMeans(stats2[, c("Appreciation", "Aisance", "RachatBic")], na.rm = TRUE)

# Remplacer les NA dans les moyennes par des valeurs par défaut
moyennes_visuel[is.na(moyennes_visuel)] <- 0
moyennes_tactile[is.na(moyennes_tactile)] <- 0
moyennes_auditif[is.na(moyennes_auditif)] <- 0
moyennes_ecriture[is.na(moyennes_ecriture)] <- 0
moyennes_hedonique[is.na(moyennes_hedonique)] <- 1  # Pour éviter les divisions par NA




# Extraire les colonnes 8 à 25
columns_to_test <- stats2[, 8:25]

# Appliquer le test de Shapiro-Wilk pour chaque colonne
shapiro_results <- apply(columns_to_test, 2, function(col) {
  if (all(!is.na(col))) {  # Vérifier si la colonne n'est pas vide ou entièrement NA
    return(shapiro.test(col)$p.value)
  } else {
    return(NA)  # Retourner NA si la colonne est vide
  }
})

# Afficher les résultats du test pour chaque colonne
shapiro_results
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
data_long <- tidyr::pivot_longer(
  tableaux,
  cols = c(Visuel, Tactile, Auditif, Ecriture, Hedonique),
  names_to = "Categorie",
  values_to = "Valeur"
)

library(ggplot2)

#######################################


#######################################

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





########################################################
# Charger les bibliothèques nécessaires
library(readxl)
library(dplyr)
library(tidyr)
library(FSA)      # Pour les tests post-hoc de Dunn
library(ggplot2)  # Pour la visualisation

# Charger les données (remplacez le chemin par votre fichier)
# stats2 <- read_excel("path_to_your_file.xlsx")

# Sélection des colonnes à analyser (8 à 25)
columns_to_test <- colnames(stats2)[8:25]

# Restructurer les données en format long pour comparaison entre colonnes
data_long <- stats2 %>%
  pivot_longer(
    cols = all_of(columns_to_test),  # Prend les colonnes 8 à 25
    names_to = "Categorie",
    values_to = "Valeur"
  ) %>%
  filter(!is.na(Valeur))  # Retirer les NA pour éviter des erreurs

# **1. Test ANOVA (comparaison des colonnes)**
anova_result <- aov(Valeur ~ Categorie, data = data_long)
summary(anova_result)

# Test post-hoc Tukey
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

# Extraire les résultats de Tukey dans un DataFrame
tukey_df <- as.data.frame(tukey_result$Categorie)
tukey_df$Significatif <- tukey_df$`p adj` < 0.05

# Visualisation des résultats de Tukey
ggplot(tukey_df, aes(x = rownames(tukey_df), y = `p adj`, fill = Significatif)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(
    title = "Résultats du test de Tukey",
    x = "Comparaison",
    y = "Valeur p ajustée"
  ) +
  scale_fill_manual(values = c("gray", "red")) +  # Rouge pour significatif, gris sinon
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_text(aes(label = round(`p adj`, 3)), vjust = -0.5, color = "black", size = 3)




