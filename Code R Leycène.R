#charger les librairies
library(ggplot2)
library(dplyr)
library(plotly)

#mettre le fichier 
data = read.csv("C:/Users/axeli/Documents/phishing_attacks_sample_company_6.csv")


# Convertir la colonne 'success' en numérique (1 pour 'success' et 0 pour 'failure')
data <- data %>%
  mutate(success = ifelse(success == "success", 1, 0))


#1. Analyse statistique descriptive

pourcentages <- nrow(data) / 100
summary_stats <- data %>%
  group_by(channel) %>%
  summarise(
    Mean_Success_Rate = mean(success, na.rm = TRUE) * 100,
    Median_Success_Rate = median(success, na.rm = TRUE),
    Attack_Frequency = n()/pourcentages
  )


print (summary_stats)

# Visualisation des attaques par canal

ggplot(data, aes(channel, fill=channel)) +
  geom_bar() +
  labs(title="Fréquence des Attaques par Canal", x="Canal d'Attaque", y="Nombre d'Attaques") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
# 2. Analyse Bivariée (Canal vs Taux de Réussite)
# Calcul de la moyenne du taux de réussite par canal

bivariate_analysis <- data %>%
  group_by(channel) %>%
  summarise(Mean_Success_Rate = mean(success, na.rm = TRUE))

# Afficher l'analyse bivariée
print(bivariate_analysis)

# Visualisation du taux de réussite par canal

ggplot(bivariate_analysis, aes(x=channel, y=Mean_Success_Rate, fill=channel)) +
  geom_bar(stat="identity") +
  labs(title="Taux de Réussite par Canal d'Attaque", x="Canal d'Attaque", y="Taux de Réussite Moyenne") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
# 3. Estimation des Paramètres de Population
# Estimation de la proportion d'attaques réussies

z <- 1.96

confidence_intervals <- data %>%
  group_by(channel) %>%
  summarise(
    n = n(),  # Nombre total d'observations
    p_hat = mean(success, na.rm = TRUE),  # Proportion de succès
    SE = sqrt(p_hat* (1 - p_hat) / n), #Erreur standard
    CI_lower = p_hat - z * SE ,  # Limite inférieure
    CI_upper = p_hat + z * SE  # Limite supérieure
  )

  
# Affichage des résultats
print(confidence_intervals)

# 4. Test d'Hypothèses
# Hypothèse : plus de 25 % des attaques par email sont réussies

email_data <- data %>%
  filter(channel == "email")

p_hat <- mean(email_data$success, na.rm = TRUE)
n <- nrow(email_data)

p_0 <- 0.25
SE <- sqrt((p_0 * (1 - p_0)) / n)
z <- (p_hat - p_0) / SE

z_critical <- 1.645
print(z)

if (z > z_critical) {
  print("L'hypothèse nulle est rejetée : Plus de 25% des attaques par email sont réussies.")
} else {
  print("L'hypothèse nulle ne peut pas être rejetée : Pas suffisamment de preuves que plus de 25% des attaques par email sont réussies.")
}

# 5. Modélisation par Régression Linéaire
# Modélisation de la relation entre la durée de l'attaque et le taux de succès

data_clean <- data %>%
  filter(!is.na(detection_time_hours), !is.na(success))

model <- lm(success ~ detection_time_hours, data = data_clean)
summary(model)

# Visualisation de la régression
ggplot(data_clean, aes(x=detection_time_hours, y=success)) +
  geom_point() +
  geom_smooth(method="lm", col="blue") +
  labs(title="Relation entre la Durée de l'Attaque et le Taux de Réussite", 
       x="Durée de l'Attaque", y="Taux de Réussite")
