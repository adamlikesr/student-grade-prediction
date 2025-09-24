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

# Create function to predict final_grade from user inputs
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


predictGrade <- function() {
  # Greet user
  cat("* Thank you for using the GradePredictor!     *\n* You can stop the program by typing \"stop\"   *\n* Answer the questions truthfully, and enjoy! *")
  
  # Function to check validity of input
  isValid <- function(user_prompt, min_val, max_val) {
    repeat {
      input = readline(prompt = user_prompt)
      
      # Check if user wants to stop, return NULL to stopAndStore
      if (tolower(input) == "stop") {
        return(NULL)
      }
      
      # Checks if input is in range 
      num <-  suppressWarnings(as.numeric(input))
      if(!is.na(num) && num >= min_val && num <= max_val) {
        return(num)
      } 
      
      # Displays error message
      else {
        cat("Invalid input, please enter a number between ", min_val, " and ", max_val, ".\n")
      }
    }
  }
  
  # Helper function to store inputs, and stop program at user command
  stopAndStore <- function(user_prompt, min_val, max_val) {
    x <- isValid(user_prompt, min_val, max_val)
    
    # Check if user wants to stop
    if(is.null(x)) {
      cat("Terminating program. Have a nice day!")
      stop()
    }
    return(x)
  }
  
  
  # Ask user questions, store into user variables
  user_classes_failed <- stopAndStore("How many classes have you failed the past semester? If more than 3, enter 4: ", 0, 4)
  
  user_hours_studied <- stopAndStore("How much do you study a week on a scale from 1 to 4? 1 being not much (less than two hours a week), 4 being very frequently (more than 10 hours a week): ", 1, 4)
  
  user_absences <- stopAndStore("In that past 90 days, how many times have you missed school: ", 0, 90)
  
  user_travel_time <- stopAndStore("On a scale from 1 to 4, how long is your commute? 1 being very short (less than 15 minutes), 4 being very long (more than 1 hour): ", 1, 4)
  
  user_free_time <- stopAndStore("On a scale from 1 to 5, how much free time do you have after school? 1 being very little, 5 being very much: ", 1, 5)
  
  user_workday_drinking <- stopAndStore("On a scale from 1 to 5, how often do you drink during school days? 1 being little to never, 5 being very frequently: ", 1, 5)
  
  user_weekend_drinking <- stopAndStore("On a scale from 1 to 5, how often do you drink on the weekends? 1 being little to never, 5 being very frequently: ", 1, 5)
  
  user_health <- stopAndStore("On a scale from 1 to 5, how is your health status? 1 being very bad, 5 being extremley healthy: ", 1, 5)

  # Predict final grade
  output_grade <- inputPredict(user_classes_failed, user_hours_studied, user_absences,
                               user_travel_time, user_free_time, user_workday_drinking,
                               user_weekend_drinking, user_health)
  
  # Calculate grade as %, find lower and upper error bound of result
  output_grade_perc <- (output_grade / 20)
  output_grade_perc_lower_bound <- (output_grade_perc - (rmse / 20))
  output_grade_perc_upper_bound <- (output_grade_perc + (rmse / 20))
  
  # Output results
  cat("Using your data, the model predicts that your grade will likely be between", output_grade_perc_lower_bound, "% and ", output_grade_perc_upper_bound, "%.")
}

