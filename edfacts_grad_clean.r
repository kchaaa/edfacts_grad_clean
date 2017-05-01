###########################
## TODO:-Clean it up ()
#http://stat545.com/block022_regular-expression.html
###########################
## Set Up 

setwd("/Users/crpe/Documents/Portfolio/edfacts_grad_clean")  
# MAC: 
# PC: setwd()

library(plyr)
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
library(stringi)

edfacts_12 <- read.csv("edfacts_grad_clean_data/Edfacts 2012.csv", stringsAsFactors = FALSE, na.strings=c("",".","PS","NA"))
edfacts_13 <- read.csv("edfacts_grad_clean_data/Edfacts 2013.csv", stringsAsFactors = FALSE, na.strings=c("",".","PS","NA"))
edfacts_14 <- read.csv("edfacts_grad_clean_data/Edfacts 2014.csv", stringsAsFactors = FALSE, na.strings=c("",".","PS","NA"))
edfacts_15 <- read.csv("edfacts_grad_clean_data/Edfacts 2015.csv", stringsAsFactors = FALSE, na.strings=c("",".","PS","NA"))

## Changes to the dataset size
# Get rid of date column(?)
edfacts_12$DATE_CUR <- NULL
edfacts_13$DATE_CUR <- NULL
edfacts_14$DATE_CUR <- NULL
edfacts_15$DATE_CUR <- NULL

###########################
## Functions 
# Function 1: gets rid of the GT,GE,LT,LE 
clean_up_gl_num <- function(df) {
  # Change the GT, GE, LT, LE
  df[7:ncol(df)] <- as.data.frame(sapply(df[7:ncol(df)], gsub, pattern="GE", replacement=""))
  df[7:ncol(df)] <- as.data.frame(sapply(df[7:ncol(df)], gsub, pattern="LE", replacement=""))
  df[7:ncol(df)] <- as.data.frame(sapply(df[7:ncol(df)], gsub, pattern="GT", replacement=""))
  df[7:ncol(df)] <- as.data.frame(sapply(df[7:ncol(df)], gsub, pattern="LT", replacement=""))
  return(df)
}

# Function 2: Calculate the midpoint of the range
calc_mid <- function(str) {
  num1 <- as.numeric(substr(str,1,2))
  num2 <- as.numeric(substr(str,4,5))
  midpoint <- (num1 + num2) / 2
  return(as.character(midpoint))
}

# Function 3: Changes the columns type to the right ones
right_type <- function(df) {
  for (i in 7:ncol(df)) {
    ifelse(i %% 2, df[,i] <- as.numeric(df[,i]), df[,i] <- as.character(df[,i]))
  }
  return(df)
}

# Function 4: All numeric
all_numeric <- function(df) {
  for (i in 7:ncol(df)) {
    ifelse(!i %% 2, df[,i] <- as.numeric(df[,i]), df[,i] <- df[,i])
  }
  return(df)
}

# Function 5: Changes the Ranges to Midpoints
range_2_mid <- function(df) {
  df[7:ncol(df)] <- as.data.frame(sapply(df[7:ncol(df)], 
                                                 gsub, 
                                                 pattern="^([a-zA-Z0-9]*-[a-zA-Z0-9]*)+$", 
                                                 replacement=calc_mid(df[])
                                                )
                                          )
  return(df)
}

###########################
## Clean Up
# Changes the dataset values
edfacts_12 <- clean_up_gl_num(edfacts_12)
edfacts_13 <- clean_up_gl_num(edfacts_13)
edfacts_14 <- clean_up_gl_num(edfacts_14)
edfacts_15 <- clean_up_gl_num(edfacts_15)

# Make sure the columns are the right class (odds=numeric, evens=character)
edfacts_12 <- right_type(edfacts_12)
edfacts_13 <- right_type(edfacts_13)
edfacts_14 <- right_type(edfacts_14)
edfacts_15 <- right_type(edfacts_15)

# Converts the ranges to midpoints
edfacts_12 <- range_2_mid(edfacts_12)
edfacts_13 <- range_2_mid(edfacts_13)
edfacts_14 <- range_2_mid(edfacts_14)
edfacts_15 <- range_2_mid(edfacts_15)

# Make sure every column is numeric
edfacts_12 <- right_type(edfacts_12)
edfacts_13 <- right_type(edfacts_13)
edfacts_14 <- right_type(edfacts_14)
edfacts_15 <- right_type(edfacts_15)
edfacts_12 <- all_numeric(edfacts_12)
edfacts_13 <- all_numeric(edfacts_13)
edfacts_14 <- all_numeric(edfacts_14)
edfacts_15 <- all_numeric(edfacts_15)

###########################
## Finish Up 
# Write to .csv files
write.csv(edfacts_12, file="clean_data/edfacts_12.csv")
write.csv(edfacts_13, file="clean_data/edfacts_13.csv")
write.csv(edfacts_14, file="clean_data/edfacts_14.csv")
write.csv(edfacts_15, file="clean_data/edfacts_15.csv")

