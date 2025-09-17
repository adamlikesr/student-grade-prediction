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
            travel_time = traveltime,
            free_time = freetime,
            workday_drinking = Dalc,
            weekend_drinking = Walc,
            health,
            final_grade = G3
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
model <- lm(final_grade ~ classes_failed + hours_studied + absences + travel_time + free_time + weekend_drinking + workday_drinking + health,
            data = training_data)


# -------------------------------
# 5. Make Predictions
# -------------------------------

# Use model to predict grades in testing data
grade_predictions <- predict(model, newdata = testing_data)


# -------------------------------
# 6. Assess Regression Accuracy
# -------------------------------

# Calculate mean standard error
mse <- mean((testing_data$final_grade - grade_predictions)^2)

# Calculate root mean standard error
rmse <- sqrt(mse)


#-----------------------------
# 7. Build Function
#-----------------------------

inputPredict <- 
  function(user_classes_failed, user_hours_studied, user_absences, 
                         user_travel_time, user_free_time, user_weekend_drinking, 
                         user_workday_drinking, user_health)
  {
    # Create new data frame to store user inputs
    user_data <- data.frame(
      classes_failed = user_classes_failed,
      hours_studied = user_hours_studied,
      absences = user_absences,
      travel_time = user_travel_time,
      free_time = user_free_time,
      workday_drinking = user_workday_drinking,
      weekend_drinking = user_weekend_drinking,
      health = user_health
      )
    
    # Predict user's grade based off of inputs, removes vector name in output
    user_prediction = unname(predict(model, newdata = user_data))
    
    # Return user's grade prediction
    return(user_prediction)
}


