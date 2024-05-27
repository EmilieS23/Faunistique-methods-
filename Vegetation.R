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

# Résumé et structure de umf
summary(umf)
str(umf)

# Modélisation avec le modèle de régression Poisson pour prédire le nombre de lézards en fonction de la végétation
model5 <- pcount(~1 ~Vegetation, umf)
summary(model5)
# Résumé:
# Coefficients:
#   (Intercept)      Vegetation  
#      x.xxxx       x.xxxx

# Préparer les nouvelles données pour les prédictions
new.data.vegetation <- data.frame(Vegetation = seq(min(Vegetation, na.rm = TRUE), max(Vegetation, na.rm = TRUE), length.out = 100))

# Utiliser le modèle pour faire des prédictions
pred.abundance.vegetation <- predict(model5, newdata = new.data.vegetation, type = "state", append = TRUE)

# Visualiser les résultats
ggplot() +
  geom_line(aes(x = new.data.vegetation$Vegetation, y = pred.abundance.vegetation$Predicted), color = "green", size = 1) +
  geom_ribbon(aes(x = new.data.vegetation$Vegetation, ymin = pred.abundance.vegetation$lower, ymax = pred.abundance.vegetation$upper), alpha = 0.2, fill = "green") +
  labs(title = "Prediction of number of lizards on vegetation",
       x = "Percentage of vegetation",
       y = "Predicted number of lizards") +
  theme_minimal()