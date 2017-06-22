###########################
## TODO:-Clean it up ()
#http://stat545.com/block022_regular-expression.html
###########################
## Set Up 
rm(list=ls())

setwd("/Users/crpe/Documents/CityMetrics/edfacts_grad_clean")  
# MAC: 
# PC: setwd()

library(plyr)
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
library(stringi)

edfacts_11 <- read.csv("edfacts_grad_data/Edfacts 2011.csv", stringsAsFactors = FALSE, na.strings=c("",".","PS","NA"))
edfacts_12 <- read.csv("edfacts_grad_data/Edfacts 2012.csv", stringsAsFactors = FALSE, na.strings=c("",".","PS","NA"))
edfacts_13 <- read.csv("edfacts_grad_data/Edfacts 2013.csv", stringsAsFactors = FALSE, na.strings=c("",".","PS","NA"))
edfacts_14 <- read.csv("edfacts_grad_data/Edfacts 2014.csv", stringsAsFactors = FALSE, na.strings=c("",".","PS","NA"))
edfacts_15 <- read.csv("edfacts_grad_clean_data/Edfacts 2015.csv", stringsAsFactors = FALSE, na.strings=c("",".","PS","NA"))

## Changes to the dataset size
# Get rid of date column(?)
edfacts_11$DATE_CUR <- NULL
edfacts_12$DATE_CUR <- NULL
edfacts_13$DATE_CUR <- NULL
edfacts_14$DATE_CUR <- NULL
edfacts_14$INSERT_DATE <- NULL
#edfacts_15$DATE_CUR <- NULL

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
# calc_mid <- function(str) {
#   num1 <- as.numeric(substr(str,1,2))
#   num2 <- as.numeric(substr(str,4,5))
#   midpoint <- (num1 + num2) / 2
#   return(as.character(midpoint))
# }

# Function 3: Changes the columns type to the right ones
right_type <- function(df) {
  for (i in 7:ncol(df)) {
    ifelse(i %% 2, df[,i] <- as.numeric(df[,i]), df[,i] <- as.character(df[,i]))
  }
  return(df)
}

# Function 5: Change Ranges => Midpoints
midpoint_1011 <- function(df) {
  df$ALL_RATE_1011 <- sapply(strsplit(df$ALL_RATE_1011, "-"),
                             function(x) mean(as.numeric(x)))
  df$MAM_RATE_1011 <- sapply(strsplit(df$MAM_RATE_1011, "-"),
                             function(x) mean(as.numeric(x)))
  df$MAS_RATE_1011 <- sapply(strsplit(df$MAS_RATE_1011, "-"),
                             function(x) mean(as.numeric(x)))
  df$MBL_RATE_1011 <- sapply(strsplit(df$MBL_RATE_1011, "-"),
                             function(x) mean(as.numeric(x)))
  df$MHI_RATE_1011 <- sapply(strsplit(df$MHI_RATE_1011, "-"),
                             function(x) mean(as.numeric(x)))
  df$MTR_RATE_1011 <- sapply(strsplit(df$MTR_RATE_1011, "-"),
                             function(x) mean(as.numeric(x)))
  df$MWH_RATE_1011 <- sapply(strsplit(df$MWH_RATE_1011, "-"),
                             function(x) mean(as.numeric(x)))
  df$CWD_RATE_1011 <- sapply(strsplit(df$CWD_RATE_1011, "-"),
                             function(x) mean(as.numeric(x)))
  df$ECD_RATE_1011 <- sapply(strsplit(df$ECD_RATE_1011, "-"),
                             function(x) mean(as.numeric(x)))
  df$LEP_RATE_1011 <- sapply(strsplit(df$LEP_RATE_1011, "-"),
                             function(x) mean(as.numeric(x)))
  
  return(df)
}

midpoint_1112 <- function(df) {
  df$ALL_RATE_1112 <- sapply(strsplit(df$ALL_RATE_1112, "-"),
                             function(x) mean(as.numeric(x)))
  df$MAM_RATE_1112 <- sapply(strsplit(df$MAM_RATE_1112, "-"),
                             function(x) mean(as.numeric(x)))
  df$MAS_RATE_1112 <- sapply(strsplit(df$MAS_RATE_1112, "-"),
                             function(x) mean(as.numeric(x)))
  df$MBL_RATE_1112 <- sapply(strsplit(df$MBL_RATE_1112, "-"),
                             function(x) mean(as.numeric(x)))
  df$MHI_RATE_1112 <- sapply(strsplit(df$MHI_RATE_1112, "-"),
                             function(x) mean(as.numeric(x)))
  df$MTR_RATE_1112 <- sapply(strsplit(df$MTR_RATE_1112, "-"),
                             function(x) mean(as.numeric(x)))
  df$MWH_RATE_1112 <- sapply(strsplit(df$MWH_RATE_1112, "-"),
                             function(x) mean(as.numeric(x)))
  df$CWD_RATE_1112 <- sapply(strsplit(df$CWD_RATE_1112, "-"),
                             function(x) mean(as.numeric(x)))
  df$ECD_RATE_1112 <- sapply(strsplit(df$ECD_RATE_1112, "-"),
                             function(x) mean(as.numeric(x)))
  df$LEP_RATE_1112 <- sapply(strsplit(df$LEP_RATE_1112, "-"),
                             function(x) mean(as.numeric(x)))
  
  return(df)
}
midpoint_1213 <- function(df) {
  df$ALL_RATE_1213 <- sapply(strsplit(df$ALL_RATE_1213, "-"),
                             function(x) mean(as.numeric(x)))
  df$MAM_RATE_1213 <- sapply(strsplit(df$MAM_RATE_1213, "-"),
                             function(x) mean(as.numeric(x)))
  df$MAS_RATE_1213 <- sapply(strsplit(df$MAS_RATE_1213, "-"),
                             function(x) mean(as.numeric(x)))
  df$MBL_RATE_1213 <- sapply(strsplit(df$MBL_RATE_1213, "-"),
                             function(x) mean(as.numeric(x)))
  df$MHI_RATE_1213 <- sapply(strsplit(df$MHI_RATE_1213, "-"),
                             function(x) mean(as.numeric(x)))
  df$MTR_RATE_1213 <- sapply(strsplit(df$MTR_RATE_1213, "-"),
                             function(x) mean(as.numeric(x)))
  df$MWH_RATE_1213 <- sapply(strsplit(df$MWH_RATE_1213, "-"),
                             function(x) mean(as.numeric(x)))
  df$CWD_RATE_1213 <- sapply(strsplit(df$CWD_RATE_1213, "-"),
                             function(x) mean(as.numeric(x)))
  df$ECD_RATE_1213 <- sapply(strsplit(df$ECD_RATE_1213, "-"),
                             function(x) mean(as.numeric(x)))
  df$LEP_RATE_1213 <- sapply(strsplit(df$LEP_RATE_1213, "-"),
                             function(x) mean(as.numeric(x)))
  
  return(df)
}
midpoint_1314 <- function(df) {
  df$ALL_RATE_1314 <- sapply(strsplit(df$ALL_RATE_1314, "-"),
                             function(x) mean(as.numeric(x)))
  df$MAM_RATE_1314 <- sapply(strsplit(df$MAM_RATE_1314, "-"),
                             function(x) mean(as.numeric(x)))
  df$MAS_RATE_1314 <- sapply(strsplit(df$MAS_RATE_1314, "-"),
                             function(x) mean(as.numeric(x)))
  df$MBL_RATE_1314 <- sapply(strsplit(df$MBL_RATE_1314, "-"),
                             function(x) mean(as.numeric(x)))
  df$MHI_RATE_1314 <- sapply(strsplit(df$MHI_RATE_1314, "-"),
                             function(x) mean(as.numeric(x)))
  df$MTR_RATE_1314 <- sapply(strsplit(df$MTR_RATE_1314, "-"),
                             function(x) mean(as.numeric(x)))
  df$MWH_RATE_1314 <- sapply(strsplit(df$MWH_RATE_1314, "-"),
                             function(x) mean(as.numeric(x)))
  df$CWD_RATE_1314 <- sapply(strsplit(df$CWD_RATE_1314, "-"),
                             function(x) mean(as.numeric(x)))
  df$ECD_RATE_1314 <- sapply(strsplit(df$ECD_RATE_1314, "-"),
                             function(x) mean(as.numeric(x)))
  df$LEP_RATE_1314 <- sapply(strsplit(df$LEP_RATE_1314, "-"),
                             function(x) mean(as.numeric(x)))
  
  return(df)
}


###########################
## Clean Up
# Changes the dataset values
edfacts_11 <- clean_up_gl_num(edfacts_11)
edfacts_12 <- clean_up_gl_num(edfacts_12)
edfacts_13 <- clean_up_gl_num(edfacts_13)
edfacts_14 <- clean_up_gl_num(edfacts_14)
edfacts_15 <- clean_up_gl_num(edfacts_15)

# Make sure the columns are the right class (odds=numeric, evens=character)
edfacts_11 <- right_type(edfacts_11)
edfacts_12 <- right_type(edfacts_12)
edfacts_13 <- right_type(edfacts_13)
edfacts_14 <- right_type(edfacts_14)
edfacts_15 <- right_type(edfacts_15)

# Converts the ranges to midpoints
edfacts_11 <- midpoint_1011(edfacts_11)
edfacts_12 <- midpoint_1112(edfacts_12)
edfacts_13 <- midpoint_1213(edfacts_13)
edfacts_14 <- midpoint_1314(edfacts_14)
edfacts_15 <- midpoint_1415(edfacts_15)

# turn NAs into -99
# our dummy value for the database
edfacts_11[is.na(edfacts_11)] <- -99
edfacts_12[is.na(edfacts_12)] <- -99
edfacts_13[is.na(edfacts_13)] <- -99
edfacts_14[is.na(edfacts_14)] <- -99
edfacts_15[is.na(edfacts_15)] <- -99

###########################
## Finish Up 
# Write to .csv files
write.csv(edfacts_11, file="clean_data/edfacts_11.csv")
write.csv(edfacts_12, file="clean_data/edfacts_12.csv")
write.csv(edfacts_13, file="clean_data/edfacts_13.csv")
write.csv(edfacts_14, file="clean_data/edfacts_14.csv")
#write.csv(edfacts_15, file="clean_data/edfacts_15.csv")

