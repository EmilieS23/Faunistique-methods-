# Installer et charger le package nécessaire
install.packages("unmarked")
library(unmarked)

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

#Mettez en commentaire les résultats ! 

install.packages("unmarked")
library(unmarked) 

data <- read.csv("FM_onlydata.csv", head =T, sep=";", stringsAsFactors = T)
View(data)
datali <- data[c("Passage1", "Passage2", "Passage3")]
#View(datali)
Cats <- data[c("Cats")]
Holes <- data[c("Holes")]
Orientation <- data[c("Orientation")]
Buildings <- data[c("Buildings")]
Vegetation <- data[c("Vegetation")] 
Sun <- data[c("Sun")]

#Weather et Time ne sont pas considérer comme covarible et devront être traité autrement

#def lézards et covariables
umf <- unmarkedFramePCount(y=datali, siteCovs=data.frame(Cats=Cats, Holes=Holes,Orientation=Orientation, Buildings=Buildings,Vegetation=Vegetation, Sun=Sun)) 

summary(umf) 
str(umf)

#AIC avec la plus faible valeur représente donc le modèle le plus significatif

model1 <- pcount(~1 ~Cats, umf) 
summary(model1) 
#AIC = 154.3438

model2 <- pcount(~1 ~Holes, umf)
summary(model2)
#AIC = 166.0307

model3 <- pcount(~1 ~Orientation, umf)
summary(model3)
#AIC = 161.5061

model4 <- pcount(~1 ~Buildings, umf)
summary(model4)
#AIC = 162.3646

model5 <- pcount(~1 ~Vegetation, umf)
summary(model5)
#AIC = 162.6432

model6 <- pcount(~1 ~Sun, umf)
summary(model6) 
#AIC = 152.9226 --> peut-être ici pas très significatif car pas des valeurs numériques ?

modelall <- pcount(~1 ~(Cats+Holes+Orientation+Buildings+Vegetation+Sun), umf)
summary(modelall)
#AIC = 159.3415 --> jsp si ça a du sens de tout compiler mais j'ai essayé ^^

model7 <- pcount(~1~Holes+Orientation, umf)
summary(model7)
#AIC = 164.1759

model8 <- pcount(~1~Orientation+Vegetation, umf)
summary(model8)
#AIC = 163.2236

#transformation des paramètres en échelle logarithmique --> cela dit je sais pas trop quoi faire de ces résultats...
(p.m1 = backTransform(model1, typ = "det") )
(p.m2 = backTransform(model2, typ = "det") ) 
(p.m3 = backTransform(model3, typ = "det") ) 
(p.m4 = backTransform(model4, typ = "det") ) 
(p.m5 = backTransform(model5, typ = "det") ) 
(p.m6 = backTransform(model6, typ = "det") ) 
(p.m7 = backTransform(model7, typ = "det") ) 
(p.m8 = backTransform(model8, typ = "det") ) 
(p.mall = backTransform(modelall, typ = "det") ) 
confint(model1, type='det') 
confint(backTransform(model1, type='det')) 
confint(model2, type='det') 
confint(backTransform(model2, type='det')) 
confint(model3, type='det') 
confint(backTransform(model3, type='det')) 
confint(model4, type='det') 
confint(backTransform(model4, type='det')) 
confint(model5, type='det') 
confint(backTransform(model5, type='det')) 
confint(model6, type='det') 
confint(backTransform(model6, type='det')) 
confint(model7, type='det') 
confint(backTransform(model7, type='det')) 
confint(model8, type='det') 
confint(backTransform(model8, type='det')) 
confint(modelall, type='det') 
confint(backTransform(modelall, type='det')) 


####
#Abundance
####

new.data.sunny = data.frame(Orientation = seq (-3,3,length.out=100), Sun = c("sunny")) 
new.data.shaded = data.frame(Orientation = seq (-3,3,length.out=100), Sun = c("shaded")) 
new.data.morningsun = data.frame(Orientation = seq (-3,3,length.out=100), Sun = c("morning sun")) 
new.data.middaysun = data.frame(Orientation = seq (-3,3,length.out=100), Sun = c("midday sun")) 
new.data.afternoonsun = data.frame(Orientation = seq (-3,3,length.out=100), Sun = c("afternoon sun"))  

pred.abundance.sunny = predict(model6, newdata = new.data.sunny, type = "state", append=T) 
pred.abundance.shaded = predict(model6, newdata = new.data.shaded, type = "state", append=T) 
pred.abundance.morningsun = predict(model6, newdata = new.data.morningsun, type = "state", append=T)
pred.abundance.middaysun = predict(model6, newdata = new.data.middaysun, type = "state", append=T) 
pred.abundance.afternoonsun = predict(model6, newdata = new.data.afternoonsun, type = "state", append=T) 


head(pred.abundance.sunny) # have a look at the stuff which was predicted 

####
#Plot
####

plot(pred.abundance.sunny$Lizards, pred.abundance.sunny$Predicted)  
plot(pred.abundance.shaded$Lizards, pred.abundance.shaded$Predicted) 
#etc

par(cex.lab = 1.5) 
x11() 
plot(pred.abundance.sunny$Lizards, pred.abundance.sunny$Predicted, ylim = c(0, 25), xlab = "x", ylab = "y", type = "l", lwd=2) 
points(pred.abundance.sunny$Lizards, pred.abundance.sunny$lower, type = "l", lty = 2) # add confidence intervals 
points(pred.abundance.sunny$Lizards, pred.abundance.sunny$upper, type = "l", lty = 2) 
points(pred.abundance.shaded$Lizards, pred.abundance.shaded$Predicted, ylim = c(0, 60), xlab = "Covariate", ylab = "Estimated abundance", type = "l", lwd=2, col = "red") 
points(pred.abundance.shaded$Lizards, pred.abundance.shaded$lower, type = "l", lty = 2, col = "red") # add confidence intervals 
points(pred.abundance.shaded$Lizards, pred.abundance.shaded$upper, type = "l", lty = 2, col = "red") 
points(pred.abundance.middaysun$Lizards, pred.abundance.middaysun$Predicted, ylim = c(0, 60), xlab = "Covariate", ylab = "Estimated abundance", type = "l", lwd=2, col = "blue") 
points(pred.abundance.middaysun$Lizards, pred.abundance.middaysun$lower, type = "l", lty = 2, col = "blue") # add confidence intervals 
points(pred.abundance.middaysun$Lizards, pred.abundance.middaysun$upper, type = "l", lty = 2, col = "blue") 

#bon il y a l'idée du code mais ça donne qqch de faut...

#Other way to plot

new.sundata = data.frame(Liards = mean(Lizards), Sun = c("sunny", "shaded", "morning sun", "midday sun", "afternoon sun") )  

pred.abundance.Sun = predict(model6, newdata = new.sundata, type = "state", append=T) 
pred.abundance.Sun$Sun = factor(pred.abundance.Sun$Sun) 

par(cex.lab = 1.5) 
plot(1:5, pred.abundance.Sun$Predicted, xaxt="n", xlim = c(0.5, 5.5), ylim = c(0,5), xlab = "Sun", ylab = "estimated abundance", col = "chocolate" ) 
axis(1, at = 1:5, labels = c("sunny", "shaded", "morning sun", "midday sun", "afternoon sun")) 
segments(1:5, pred.abundance.Sun$lower, 1:5, pred.abundance.Sun$upper, col = "chocolate") 

#ce graphe semble mieux et singificatif mais pas hyper beau (donc encore à améliorer!)
