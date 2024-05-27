library(unmarked)
library(ggplot2)
library(dplyr)
library(viridis)
library(tidyr)

# Charger les données
data <- read.csv("FM_onlydata.csv", head = TRUE, sep = ";", stringsAsFactors = TRUE)

# Vérifier et corriger les types de données
data$Vegetation <- as.numeric(as.character(data$Vegetation)) # Convertir Vegetation en numérique si nécessaire
data$Cats <- as.numeric(as.character(data$Cats))
data$Holes <- as.numeric(as.character(data$Holes))
data$Orientation <- as.numeric(as.character(data$Orientation))
data$Buildings <- as.numeric(as.character(data$Buildings))
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

# Modélisation avec le modèle de régression Poisson pour prédire le nombre de lézards en fonction de l'orientation
model_orientation <- pcount(~1 ~Orientation, umf)
summary(model_orientation)

# Préparer les nouvelles données pour les prédictions
new.data.orientation <- data.frame(Orientation = seq(min(Orientation, na.rm = TRUE), max(Orientation, na.rm = TRUE), length.out = 100))

# Utiliser le modèle pour faire des prédictions
pred.abundance.orientation <- predict(model_orientation, newdata = new.data.orientation, type = "state", append = TRUE)

# Visualiser les résultats
ggplot() +
  geom_line(aes(x = new.data.orientation$Orientation, y = pred.abundance.orientation$Predicted), color = "blue", size = 1) +
  geom_ribbon(aes(x = new.data.orientation$Orientation, ymin = pred.abundance.orientation$lower, ymax = pred.abundance.orientation$upper), alpha = 0.2, fill = "blue") +
  labs(title = "Prediction of number of lizards on orientation",
       x = "Orientation",
       y = "Predicted number of lizards") +
  theme_minimal()