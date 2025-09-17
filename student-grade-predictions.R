# Install packages
library(tidyverse)
library(here)

# Read data
student_data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/00320/student-mat.csv", sep=";")
