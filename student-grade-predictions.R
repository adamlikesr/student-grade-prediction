# -------------------------------
# 1. Load Packages, Set Seed
# -------------------------------

library(tidyverse)
library(here)
library(ggplot2)

set.seed(123)

# -------------------------------
# 2. Clean Data
# -------------------------------

# Read CSV
student_data <- read.csv("student-mat.csv", sep=";")

# Remove NA's, select relevant data, rename for readability
clean_data <- student_data %>% 
  na.omit() %>%
  transmute(
    classes_failed = failures,
            hours_studied = studytime,
            absences,
            finalgrade = G3
    )


# -------------------------------
# 3. Training and Testing Data
# -------------------------------

# Count rows
n <- nrow(clean_data)

# Create training indices
training_indicies <- sample(x = 1:n, size = 0.8 * n, replace = FALSE)

# Split data
training_data <- clean_data[training_indicies, ]
testing_data <- clean_data[-training_indicies, ]


# -------------------------------
# 4. Fit Regression Model
# -------------------------------

# Create linear regression model, fit to training data
model <- lm(finalgrade ~ classes_failed + hours_studied + absences,
            data = training_data)


# -------------------------------
# 5. Make Predictions
# -------------------------------

# Use model to predict grades in testing data
grade_predictions <- predict(model, newdata = testing_data)


# -------------------------------
# Assess Regression Accuracy
# -------------------------------

# Calculate Mean Standard Error
mse <- mean((testing_data$finalgrade - grade_predictions)^2)

# Calculate Root Mean Standard Error
rmse <- sqrt(mse)