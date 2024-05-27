#Mettez en commentaire les résultats ! 

install.packages("unmarked")
library(unmarked)
library(ggplot2)
install.packages("tidyverse")
library(tidyverse)
install.packages("hrbrthemes")
library(hrbrthemes)
install.packages("viridis")
library(viridis)
library(forcats)
library(dplyr)
library(tidyr)

# Charger les données
data <- read.csv("FM_onlydata.csv", head = TRUE, sep = ";", stringsAsFactors = TRUE)

# Visualiser les données
# View(data)

# Séparer les données pour les observations et les covariables
datali <- data[c("Passage1", "Passage2", "Passage3")]
Cats <- data[["Cats"]]
Holes <- data[["Holes"]]
Orientation <- data[["Orientation"]]
Buildings <- data[["Buildings"]]
Vegetation <- data[["Vegetation"]]
Sun <- data[["Sun"]]

# Créer un unmarkedFramePCount
umf <- unmarkedFramePCount(y = datali, 
                           siteCovs = data.frame(Cats = Cats, Holes = Holes, Orientation = Orientation, 
                                                 Buildings = Buildings, Vegetation = Vegetation, Sun = Sun))

# Résumé et structure de umf
summary(umf)
str(umf)

# Modélisation et calcul de l'AIC pour chaque covariable
model1 <- pcount(~1 ~Cats, umf)
summary(model1)
# AIC = 154.3438

model2 <- pcount(~1 ~Holes, umf)
summary(model2)
# AIC = 166.0307

model3 <- pcount(~1 ~Orientation, umf)
summary(model3)
# AIC = 161.5061

model4 <- pcount(~1 ~Buildings, umf)
summary(model4)
# AIC = 162.3646

model5 <- pcount(~1 ~Vegetation, umf)
summary(model5)
# AIC = 162.6432

model6 <- pcount(~1 ~Sun, umf)
summary(model6)
# AIC = 152.9226

modelall <- pcount(~1 ~(Cats + Holes + Orientation + Buildings + Vegetation + Sun), umf)
summary(modelall)
# AIC = 159.3415

model7 <- pcount(~1 ~Holes + Orientation, umf)
summary(model7)
# AIC = 164.1759

model8 <- pcount(~1 ~Orientation + Vegetation, umf)
summary(model8)
# AIC = 163.2236

# Transformation des paramètres en échelle logarithmique
p.m1 = backTransform(model1, type = "det")
p.m2 = backTransform(model2, type = "det")
p.m3 = backTransform(model3, type = "det")
p.m4 = backTransform(model4, type = "det")
p.m5 = backTransform(model5, type = "det")
p.m6 = backTransform(model6, type = "det")
p.m7 = backTransform(model7, type = "det")
p.m8 = backTransform(model8, type = "det")
p.mall = backTransform(modelall, type = "det")

# Calcul des intervalles de confiance pour les modèles
confint(backTransform(model1, type = "det"))
confint(backTransform(model2, type = "det"))
confint(backTransform(model3, type = "det"))
confint(backTransform(model4, type = "det"))
confint(backTransform(model5, type = "det"))
confint(backTransform(model6, type = "det"))
confint(backTransform(model7, type = "det"))
confint(backTransform(model8, type = "det"))
confint(backTransform(modelall, type = "det"))

# Prédictions d'abondance en fonction de la variable Sun
new.data.sunny <- data.frame(Orientation = seq(-3, 3, length.out = 100), Sun = "sunny")
new.data.shaded <- data.frame(Orientation = seq(-3, 3, length.out = 100), Sun = "shaded")
new.data.morningsun <- data.frame(Orientation = seq(-3, 3, length.out = 100), Sun = "morning sun")
new.data.middaysun <- data.frame(Orientation = seq(-3, 3, length.out = 100), Sun = "midday sun")
new.data.afternoonsun <- data.frame(Orientation = seq(-3, 3, length.out = 100), Sun = "afternoon sun")

pred.abundance.sunny <- predict(model6, newdata = new.data.sunny, type = "state", append = TRUE)
pred.abundance.shaded <- predict(model6, newdata = new.data.shaded, type = "state", append = TRUE)
pred.abundance.morningsun <- predict(model6, newdata = new.data.morningsun, type = "state", append = TRUE)
pred.abundance.middaysun <- predict(model6, newdata = new.data.middaysun, type = "state", append = TRUE)
pred.abundance.afternoonsun <- predict(model6, newdata = new.data.afternoonsun, type = "state", append = TRUE)

# Visualiser les prédictions
par(cex.lab = 1.5)
plot(pred.abundance.sunny$Orientation, pred.abundance.sunny$Predicted, type = "l", lwd = 2, ylim = c(0, 25), col = "blue", xlab = "Orientation", ylab = "Estimated Abundance")
lines(pred.abundance.sunny$Orientation, pred.abundance.sunny$lower, type = "l", lty = 2, col = "blue")
lines(pred.abundance.sunny$Orientation, pred.abundance.sunny$upper, type = "l", lty = 2, col = "blue")

lines(pred.abundance.shaded$Orientation, pred.abundance.shaded$Predicted, type = "l", lwd = 2, col = "red")
lines(pred.abundance.shaded$Orientation, pred.abundance.shaded$lower, type = "l", lty = 2, col = "red")
lines(pred.abundance.shaded$Orientation, pred.abundance.shaded$upper, type = "l", lty = 2, col = "red")

lines(pred.abundance.morningsun$Orientation, pred.abundance.morningsun$Predicted, type = "l", lwd = 2, col = "green")
lines(pred.abundance.morningsun$Orientation, pred.abundance.morningsun$lower, type = "l", lty = 2, col = "green")
lines(pred.abundance.morningsun$Orientation, pred.abundance.morningsun$upper, type = "l", lty = 2, col = "green")

lines(pred.abundance.middaysun$Orientation, pred.abundance.middaysun$Predicted, type = "l", lwd = 2, col = "orange")
lines(pred.abundance.middaysun$Orientation, pred.abundance.middaysun$lower, type = "l", lty = 2, col = "orange")
lines(pred.abundance.middaysun$Orientation, pred.abundance.middaysun$upper, type = "l", lty = 2, col = "orange")

lines(pred.abundance.afternoonsun$Orientation, pred.abundance.afternoonsun$Predicted, type = "l", lwd = 2, col = "purple")
lines(pred.abundance.afternoonsun$Orientation, pred.abundance.afternoonsun$lower, type = "l", lty = 2, col = "purple")
lines(pred.abundance.afternoonsun$Orientation, pred.abundance.afternoonsun$upper, type = "l", lty = 2, col = "purple")

# Alternative visualisation
new.sundata <- data.frame(Lizards = mean(data$Lizards), Sun = c("sunny", "shaded", "morning sun", "midday sun", "afternoon sun"))
pred.abundance.Sun <- predict(model6, newdata = new.sundata, type = "state", append = TRUE)
pred.abundance.Sun$Sun <- factor(pred.abundance.Sun$Sun)

x11()
par(cex.lab = 1.5)
plot(1:5, pred.abundance.Sun$Predicted, xaxt = "n", xlim = c(0.5, 5.5), ylim = c(0, 5), xlab = "Sun", ylab = "Estimated Abundance", col = "chocolate", pch = 16)
axis(1, at = 1:5, labels = levels(pred.abundance.Sun$Sun))
segments(1:5, pred.abundance.Sun$lower, 1:5, pred.abundance.Sun$upper, col = "chocolate")



#ce graphe semble mieux et singificatif mais pas hyper beau (donc encore à améliorer!)


lizards_holes <- ggplot(data, aes(x=as.factor(Holes), fill=Lizards)) +
  geom_bar(position="dodge") +
  labs(title="",
       x="Number of holes", y="Count of observations") + 
  theme(axis.text.x = element_text(face="bold", size=7, angle=90))

print(lizards_holes)

###

data_long <- data %>%
  gather(key="variable", value="value", Holes, Vegetation, Buildings, Orientation) %>%
  mutate(value = as.numeric(value)) # S'assurer que les valeurs sont numériques

# Plot
p <- data_long %>%
  #mutate(variable = fct_reorder(variable, value)) %>%
  ggplot(aes(x=value, color=variable, fill=variable)) +
  geom_histogram(alpha=0.6, binwidth=5) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  #theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size=8)
  ) +
  xlab("") +
  ylab("Abundance of lizards") +
  facet_wrap(~variable, scales = "free")

print(p)
