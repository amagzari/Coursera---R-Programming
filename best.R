# Coursera - Johns Hopkins Data Science Specialization
# Course 2 - R Programming - Week 4 - Assignment 3 
# https://www.coursera.org/learn/r-programming/supplement/w1c7p/programming-assignment-3-instructions-hospital-quality

# CHANGE WORKING DIRECTORY ACCORDINGLY:
# PLACE specdata FOLDER IN WORKING DIRECTORY

# PART 2

# Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
# outcome name. The function reads the outcome-of-care-measures.csv file and returns a character vector
# with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
# in that state.

# Acquire data from csv file

data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

# Select the necessary fields (variables/columns) in the following order:
# hospital name, state, 30-day mortality rate from heart attack, heart failure and pneumonia

data_new <- data[, c(2, 7, 11, 17, 23)]

# Note: Now, 2nd col becomes 1st, 7th 2nd, 11th 3rd, 17th 4th, 23rd 5th


# Create the function "best" that takes two arguments (state and outcome) to output the hospital with best/lowest outcome

best <- function(state, outcome) {
  
  # Select the desired state
  
  data_state <- data_new[which(data_new[, 2] == state), ]
  
  if (all(state != data_new[, 2])) {
    stop("invalid state")
  }
  
  # Select the desired outcome
  
  if (outcome == "heart attack") {
    data_updated <- data_state[, 1:3]
  } else if (outcome == "heart failure") {
    data_updated <- data_state[, c(1, 2, 4)]
  } else if (outcome == "pneumonia") {
    data_updated <- data_state[, c(1, 2, 5)]
  } else {
    stop("invalid outcome")
  }
  
  # Convert the outcome column to numeric type
  
  data_updated[, 3] <- suppressWarnings(as.numeric(data_updated[, 3]))
  
  # Delete NAs from the outcome column
  
  non_na_indices <- which(!is.na(data_updated[, 3]))
  data_updated <- data_updated[non_na_indices, ]
  
  # Order the data frame in ascending order according to the outcome column
  # In case of a tie, the candidates are ordered following the alphabetical order of the hospital name
  
  data_ordered <- data_updated[with(data_updated, order(data_updated[, 3], data_updated[, 1])), ]
  
  # Output the hospital name in the desired state with the lowest outcome
  
  data_ordered[1, 1]
  
}