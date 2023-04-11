#Course: Introduction to Data Analytics
#Department: Information and Electronic Engineering
#Project: Recommender System in R/RStudio Environment 

#Created/programmed by:
#Oikonomou Alexandros 2019119
#=============================
#Main R file.

rm(list = ls())

#Loading libraries.
library(dplyr)
library(readxl)

#Loading Functions.
source("MyFunctions.R")

#(a)
#From xls files to dataframes.
train_data <- data.frame(read_xls('train_data.xls'))
test_data <- data.frame(read_xls('test_data.xls'))

#Τhe number of readers (U1,...) in train_data.
readers_training <- nrow(train_data)

#Τhe number of readers (NU1,...) in test_data.
readers_test <- nrow(test_data)

#Τhe number of columns in train_data.
columns_training <- ncol(train_data)

#The number of nearest neighbors.
k_NN <- 3

#Vector with book titles.
books <- colnames(train_data)[2:9]

#Dataframe for algorithm's recommendations.
recommendations <- data.frame(reader = character(0), book = character(0), rank = numeric(0))

#Vector for algorithm's value: Mean Absolute Error.
MAE <- c()

#(c)
#Algorithm's main().
similarities <- calculate_similarities()

for (i in 1:readers_test) {
  k_nearest <- get_k_nearest(i, k_NN) 
  predictions <- calculate_predictions(i, k_nearest) 
  need_recommendation <- spot_the_NAs(i) 
  recommendations <- calculate_recommendations(i,need_recommendation,predictions,recommendations) 
  MAE <- mean_absolute_error(i,predictions,MAE)
}
