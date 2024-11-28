

# Calculer une moyenne globale des scores pour chaque individu
stats2$MoyenneGlobale <- rowMeans(stats2[, c("Esthetique", "Couleur", "Finitions", 
                                             "PriseEnMain", "Texture", "Poids", 
                                             "Ergonomie", "Songeneral", "Frottement", 
                                             "Cliquetis", "Intensite", "RepEncre", 
                                             "IntensiteEncre", "Fluidite", "Sechage", 
                                             "Appreciation", "Aisance", "RachatBic")], 
                                  na.rm = TRUE)

# Vérifiez que les facteurs sont bien des variables catégoriques
stats2$Genre <- as.factor(stats2$Genre)
stats2$Activite <- as.factor(stats2$Activite)
stats2$Position <- as.factor(stats2$Position)
stats2$Age <- as.factor(stats2$Age)
stats2$VolEcriture <- as.factor(stats2$VolEcriture)




# Effectuer une ANOVA à deux facteurs (Genre et Activite)
anova_result <- aov(MoyenneGlobale ~ Genre + Activite + Position + Age + VolEcriture, data = stats2)
anova_summary <- summary(anova_result)

# Extraire les p-valeurs
p_genre <- anova_summary[[1]]["Genre", "Pr(>F)"]
p_activite <- anova_summary[[1]]["Activite", "Pr(>F)"]
p_position <- anova_summary[[1]]["Position", "Pr(>F)"]
p_age <- anova_summary[[1]]["Age", "Pr(>F)"]
p_volecriture <- anova_summary[[1]]["VolEcriture", "Pr(>F)"]

# Créer un tableau des p-valeurs
resultats_tableau <- data.frame(
  Facteur = c( "Genre", "Age", "Activite","VolEcriture","Position"),
  P_value = c(p_genre, p_activite, p_position,p_age,p_volecriture)
)

# Ajouter une colonne pour indiquer les p-valeurs significatives
resultats_tableau$Significatif <- resultats_tableau$P_value < 0.05

# Créer un histogramme avec ggplot2
library(ggplot2)

ggplot(resultats_tableau, aes(x = Facteur, y = P_value, fill = Significatif)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(
    title = "Histogramme des p-valeurs",
    x = "Facteur",
    y = "P-valeur"
  ) +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "gray")) +
  theme_minimal() +
  geom_text(aes(label = round(P_value, 3)), vjust = -0.5, color = "black")
