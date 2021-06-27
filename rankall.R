# Coursera - Johns Hopkins Data Science Specialization
# Course 2 - R Programming - Week 4 - Assignment 3 
# https://www.coursera.org/learn/r-programming/supplement/w1c7p/programming-assignment-3-instructions-hospital-quality

# CHANGE WORKING DIRECTORY ACCORDINGLY:
# PLACE specdata FOLDER IN WORKING DIRECTORY

# PART 4

# Write a function called rankall that takes two arguments: an outcome name (outcome) and a hospital ranking (num). The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame
# containing the hospital in each state that has the ranking specified in num.

# Load the dplyr package to use the filter function later on

library(dplyr)

# Acquire data from csv file

data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

# Select the necessary fields (variables/columns) in the following order:
# hospital name, state, 30-day mortality rate from heart attack, heart failure and pneumonia

data_new <- data[, c(2, 7, 11, 17, 23)]

# Note: Now, 2nd col becomes 1st, 7th 2nd, 11th 3rd, 17th 4th, 23rd 5th


# Create the function "rankall" 

rankall <- function(outcome, num = "best") {
  
  # Select the desired outcome
  
  if (outcome == "heart attack") {
    data_updated <- data_new[, 1:3]
  } else if (outcome == "heart failure") {
    data_updated <- data_new[, c(1, 2, 4)]
  } else if (outcome == "pneumonia") {
    data_updated <- data_new[, c(1, 2, 5)]
  } else {
    stop("invalid outcome")
  }
  
  
  # Convert the outcome column to numeric type
  
  data_updated[, 3] <- suppressWarnings(as.numeric(data_updated[, 3]))
  
  # Delete NAs from the outcome column
  
  non_na_indices <- which(!is.na(data_updated[, 3]))
  data_complete <- data_updated[non_na_indices, ]
  
  # Order the data frame in ascending order by state, outcome then hospital name column
  
  data_ordered <- data_complete[with(data_complete, order(data_complete[, 2], data_complete[, 3], data_complete[, 1])), ]
  
  # Create a list of data frames, each one corresponding to a specif state
  
  data_by_state <- vector(mode = "list")
  
  unique_states <- unique(data_ordered[, 2])
  
  for (s in 1:length(unique_states)) {
    data_by_state[[s]] <- filter(data_ordered, data_ordered[, 2] == unique_states[s])
  }
  
  # Create a frame containing one hospital per state according to the desired rank ("best", "worst" or numerical value)
  
  ranking <- data.frame(nrow = length(unique_states), ncol = 2)
  colnames(ranking) <- c("hospital", "state")
  
  if (num == "best") {
    
    for (s in 1:length(unique_states)) {
      ranking[s, 1] <- data_by_state[[s]][1, 1]
      ranking[s, 2] <- data_by_state[[s]][1, 2] 
    }
    
  } else if (num == "worst") {
    
    for (s in 1:length(unique_states)) {
      ranking[s, 1] <- data_by_state[[s]][nrow(data_by_state[[s]]), 1]
      ranking[s, 2] <- data_by_state[[s]][nrow(data_by_state[[s]]), 2] 
    } 
    
  } else {
    
    for (s in 1:length(unique_states)) {

      if (nrow(data_by_state[[s]]) >= num) {

        ranking[s, 1] <- data_by_state[[s]][num, 1]
        ranking[s, 2] <- data_by_state[[s]][num, 2]

      } else {

        ranking[s, 1] <- "<NA>"
        ranking[s, 2] <- data_by_state[[s]][1, 2]
      }
    }
    
  }
  
  # Output the result: ranking data frame
    
  ranking
  
}