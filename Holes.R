library(unmarked)
library(ggplot2)
library(dplyr)
library(viridis)
library(tidyr)

# Charger les données
data <- read.csv("FM_onlydata.csv", head = TRUE, sep = ";", stringsAsFactors = TRUE)

# Vérifier et corriger les types de données
data$Holes <- as.numeric(as.character(data$Holes)) # Convertir Holes en numérique si nécessaire
data$Cats <- as.numeric(as.character(data$Cats))
data$Orientation <- as.numeric(as.character(data$Orientation))
data$Buildings <- as.numeric(as.character(data$Buildings))
data$Vegetation <- as.numeric(as.character(data$Vegetation))
data$Sun <- as.factor(data$Sun) # Garder Sun comme facteur

# Séparer les données pour les observations et les covariables
datali <- data[c("Passage1", "Passage2", "Passage3")]
Cats <- data[["Cats"]]
Holes <- data[["Holes"]]
Orientation <- data[["Orientation"]]
Buildings <- data[["Buildings"]]
Vegetation <- data[["Vegetation"]]
Sun <- data[["Sun"]]
Lizards <- data[["Lizards"]]
weather <- data[c("Weather1", "Weather2", "Weather3")]

# Créer un unmarkedFramePCount
umf <- unmarkedFramePCount(y = datali, 
                           siteCovs = data.frame(Cats = Cats, Holes = Holes, Orientation = Orientation, 
                                                 Buildings = Buildings, Vegetation = Vegetation, Sun = Sun))

# Résumé et structure de umf
summary(umf)
str(umf)

# Modélisation avec le modèle de régression Poisson pour prédire le nombre de lézards en fonction des trous
model2 <- pcount(~1 ~Holes, umf)
summary(model2)
# Résumé:
# Coefficients:
#   (Intercept)      Holes  
#      0.5416       0.2032

# Préparer les nouvelles données pour les prédictions
new.data.holes <- data.frame(Holes = seq(min(Holes, na.rm = TRUE), max(Holes, na.rm = TRUE), length.out = 100))

# Utiliser le modèle pour faire des prédictions
pred.abundance.holes <- predict(model2, newdata = new.data.holes, type = "state", append = TRUE)

ggplot() +
  geom_line(aes(x = new.data.holes$Holes, y = pred.abundance.holes$Predicted), color = "red", size = 1) +
  geom_ribbon(aes(x = new.data.holes$Holes, ymin = pred.abundance.holes$lower, ymax = pred.abundance.holes$upper), alpha = 0.2, fill = "red") +
  labs(title = "Prediction of number of lizards on holes",
       x = "Number of holes",
       y = "Predicted number of lizards") +
  theme_minimal()