# Sélectionner les colonnes nécessaires
data_to_analyze <- stats2[, c("PriseEnMain", "Activite", "Age", "VolEcriture", "Position")]

# Supprimer les valeurs manquantes
data_to_analyze <- data_to_analyze[!is.na(data_to_analyze$PriseEnMain), ]

# Initialiser un dataframe pour stocker les résultats
kruskal_results <- data.frame(Variable = character(), P_Value = numeric(), Significatif = logical())

# Variables catégoriques à analyser
variables_categorique <- c("Activite", "Age", "VolEcriture", "Position")

# Appliquer le test de Kruskal-Wallis pour chaque variable
for (var in variables_categorique) {
  # Effectuer le test de Kruskal-Wallis
  kruskal_test <- kruskal.test(PriseEnMain ~ get(var), data = data_to_analyze)
  
  # Stocker les résultats
  kruskal_results <- rbind(kruskal_results, data.frame(
    Variable = var,
    P_Value = kruskal_test$p.value,
    Significatif = kruskal_test$p.value < 0.01  # Comparer avec alpha = 0.01
  ))
  
  # Afficher les résultats dans la console
  print(paste("Test de Kruskal-Wallis pour :", var))
  print(kruskal_test)
}

# Visualisation des p-values
ggplot(kruskal_results, aes(x = Variable, y = P_Value, fill = Significatif)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(
    title = "P-values des tests de Kruskal-Wallis par variable",
    x = "Variable",
    y = "P-Value"
  ) +
  scale_fill_manual(values = c("red", "gray")) +  # Rouge si significatif
  geom_text(aes(label = round(P_Value, 3)), vjust = -0.5, size = 4, color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
