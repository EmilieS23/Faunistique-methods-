model4 <- pcount(~1 ~Buildings, umf)
summary(model4)
# Résumé:
# Coefficients:
#   (Intercept)      Holes  
#      0.5416       0.2032

# Préparer les nouvelles données pour les prédictions
new.data.buildings <- data.frame(Buildings = seq(min(Buildings, na.rm = TRUE), max(Buildings, na.rm = TRUE), length.out = 100))

# Utiliser le modèle pour faire des prédictions
pred.abundance.buildings <- predict(model4, newdata = new.data.buildings, type = "state", append = TRUE)

ggplot() +
  geom_line(aes(x = new.data.buildings$Buildings, y = pred.abundance.buildings$Predicted), color = "grey", size = 1) +
  geom_ribbon(aes(x = new.data.buildings$Buildings, ymin = pred.abundance.buildings$lower, ymax = pred.abundance.buildings$upper), alpha = 0.2, fill = "grey") +
  labs(title = "Prediction of the number of lizards on buildings",
       x = "Percentage of buildings",
       y = "Predicted number of lizards") +
  theme_minimal()