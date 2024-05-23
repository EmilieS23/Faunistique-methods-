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
