library(dplyr)
library(class)
library(caret)
library(ggplot2)

# Charger les données
data <- read.csv("C:/Users/user/Documents/DM/hotel_bookings.csv")

# Nettoyer et préparer données
data_clean <- data %>%
  select(hotel, arrival_date_month, lead_time, adr, is_canceled) %>%
  na.omit()

# Créer une variable "season" : haute saison = juin, juillet, août
data_clean <- data_clean %>%
  mutate(season = ifelse(arrival_date_month %in% c("June", "July", "August"), "High", "Low")) %>%
  mutate(season = as.factor(season))

# Transformer facteurs en numériques pour KNN (via one-hot encoding)
data_knn <- data_clean %>%
  mutate(hotel = as.factor(hotel))

# One-hot encoding pour la variable hotel
dummies <- model.matrix(~ hotel - 1, data=data_knn)
knn_data <- cbind(dummies, lead_time = data_knn$lead_time, adr = data_knn$adr)
knn_target <- data_knn$season

# Diviser train/test (80/20)
set.seed(123)
train_index <- createDataPartition(knn_target, p=0.8, list=FALSE)
train_x <- knn_data[train_index, ]
test_x <- knn_data[-train_index, ]
train_y <- knn_target[train_index]
test_y <- knn_target[-train_index]

# Appliquer KNN (k=7)
knn_pred <- knn(train = train_x, test = test_x, cl = train_y, k = 7)

# Évaluer la performance
print(confusionMatrix(knn_pred, test_y))

# Préparer dataframe pour visualisation avec lead_time et adr
df_vis <- data.frame(
  lead_time = test_x[ , "lead_time"],
  adr = test_x[ , "adr"],
  PredictedSeason = knn_pred
)

# Visualisation ggplot
ggplot(df_vis, aes(x = lead_time, y = adr, color = PredictedSeason)) +
  geom_point(alpha = 0.6) +
  labs(title = "KNN : Classification saisonnière (haute/basse saison)",
       x = "Lead time (jours avant arrivée)",
       y = "Tarif moyen par nuit (ADR)",
       color = "Saison prédite") +
  theme_minimal() 