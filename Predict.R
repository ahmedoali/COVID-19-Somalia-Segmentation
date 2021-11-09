
# Predictive modelling ----------------------------------------------------
library(caret)
library(tidyverse)

input_data <- data.frame(input_data,"Segments" = mclust_5$classification)


# LDA

# Split the data into training (80%) and test set (20%)
set.seed(123)
training <- input_data[,1] %>%
  createDataPartition(p = 0.8, list = FALSE)
train_data <- input_data[training, ]
test_data <- input_data[-training, ]

library(MASS)
# Fit the model
model <- lda(Segments ~ ., data = train_data[,-c(1:3)])
# Make predictions
predictions <- model %>% 
  predict(test_data[,-c(1:3)])
# Model accuracy
mean(predictions$class==test_data$Segments)

# Model summary
model

# Visualisation
plot(model)

plot_data <- cbind(train_data[,-c(1:3)], predict(model)$x)
ggplot(plot_data, aes(LD1, LD2)) +
  geom_point(aes(color = covid_feel_r_2tb))

