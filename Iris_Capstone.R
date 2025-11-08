# -------------------------------
# Iris Dataset Capstone Project
# -------------------------------

# Load Libraries
library(ggplot2)
library(dplyr)
library(caret)
library(GGally)
library(corrplot)
library(rpart)
library(rpart.plot)

# Step 1: Load Data
data(iris)
str(iris)
summary(iris)

# Step 2: Data Cleaning
sum(is.na(iris))        # No missing values
iris <- iris %>% mutate(Petal.Ratio = Petal.Length / Petal.Width)
sapply(iris, class)

# Step 3: Exploratory Data Analysis
# Histograms
ggplot(iris, aes(x = Sepal.Length, fill = Species)) +
  geom_histogram(bins = 20, alpha = 0.6) + theme_minimal()

# Boxplots
ggplot(iris, aes(x = Species, y = Petal.Length, fill = Species)) +
  geom_boxplot() + theme_minimal()

# Pairplot
GGally::ggpairs(iris, aes(color = Species))

# Correlation matrix
corrplot(cor(iris[, 1:4]), method = "color")

# Step 4: Model (Decision Tree)
set.seed(123)
trainIndex <- createDataPartition(iris$Species, p = 0.8, list = FALSE)
train <- iris[trainIndex, ]
test <- iris[-trainIndex, ]

model <- train(Species ~ ., data = train, method = "rpart")
pred <- predict(model, test)
confusionMatrix(pred, test$Species)

# Plot tree
rpart.plot(model$finalModel)

# Step 5: Export Cleaned Data for Power BI
write.csv(iris, "iris_cleaned.csv", row.names = FALSE)

