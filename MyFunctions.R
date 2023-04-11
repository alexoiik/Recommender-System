#Course: Introduction to Data Analytics
#Department: Information and Electronic Engineering
#Project: Recommender System in R/RStudio Environment 

#Created/programmed by:
#Oikonomou Alexandros 2019119
#=============================
#Functions R file.

#(b)
calculate_similarities <- function() {
  
  #train_data and test_data without 1st column.
  x <- select(train_data, -c(TRAIN_READER))
  y <- select(test_data, -c(TEST_READER))
  
  #Calculating all similarities. 
  sim_NU1 <- sapply(1:nrow(as.matrix(x)), function(i) cor(as.matrix(x)[i,], as.matrix(y)[1,], method = "pearson", use = "complete.obs"))
  sim_NU2 <- sapply(1:nrow(as.matrix(x)), function(i) cor(as.matrix(x)[i,], as.matrix(y)[2,], method = "pearson", use = "complete.obs"))
  
  #The dataframe with similarities.
  similarities <- data.frame(Readers = train_data$TRAIN_READER, NU1 = sim_NU1, NU2 = sim_NU2)
  
  rm(x, y, sim_NU1, sim_NU2) #Deleting useless values, dataframes.
  
  return(similarities) #Returning dataframe.
}

#(d)
get_k_nearest <- function(i, k_NN) {
  
  #If reader (i) is NU1.
  if(i == 1) {
    #Dataframe with k-nearest neighbors for NU1.
    k_nearest_NU1 <- cbind(train_data, select(similarities, similarity = NU1)) %>%
                     arrange(desc(similarity)) %>% 
                     slice(1:k_NN)
    return(k_nearest_NU1)
  }
  
  #If reader (i) is NU2. 
  if(i == 2) {
    #Dataframe with k-nearest neighbors for NU2.
    k_nearest_NU2 <- cbind(train_data, select(similarities, similarity = NU2)) %>%
                     arrange(desc(similarity)) %>% 
                     slice(1:k_NN)
    return(k_nearest_NU2)
  }
}

#(e)
calculate_predictions <- function(i, k_nearest) {
  
  #Creating a new k_nearest, where NA=0.
  new_k_nearest <- k_nearest
  new_k_nearest[is.na(new_k_nearest)] <- 0
  
  #Creating Vector predictions.
  predictions <- c() 
  
  #Algorithm for calculating predictions.
  for(s in 2:(dim(new_k_nearest)[2]-1)) {
    arithmiths <- 0 
    paronomasths <- 0 
    for(g in 1:dim(new_k_nearest)[1]) {
      arithmiths <- arithmiths + (new_k_nearest[g,s] * new_k_nearest[g,10])
      if(new_k_nearest[g,s] == 0) 
        paronomasths <- paronomasths + 0 
      if(new_k_nearest[g,s] != 0) 
        paronomasths <- paronomasths + new_k_nearest[g,10]
    }
    predictions <- append(predictions, (arithmiths)/(paronomasths))
  }
  
  rm(new_k_nearest,s,g) #Deleting useless values and dataframe.
  
  return(predictions) #Returning vector.
}

#(f)
spot_the_NAs <- function(i) {
  
  #If reader (i) is NU1. 
  if(i == 1)
    need_recommendation <- which(is.na(test_data[1,2:9]))
  #If reader (i) is NU2. 
  if(i == 2)
    need_recommendation <- which(is.na(test_data[2,2:9]))
  
  return(need_recommendation) #Returning vector.
}

#(g)
calculate_recommendations <- function(i, need_recommendation, predictions, recommendations) {
  
  #Algorithm for calculating recommendations.
  for(k in 1:length(need_recommendation)) {
    addRow <- data.frame(reader = test_data[i,1], 
                         book = books[need_recommendation[k]], 
                         prating = predictions[need_recommendation[k]])
    
    recommendations <- rbind(recommendations, addRow)
  }
  
  rm(addRow,k) #Deleting useless dataframe and value.
  
  return(recommendations) #Returning dataframe.
}

#(h)
mean_absolute_error <- function(i,predictions,MAE) {
  
  #Calculating Absolute Error for NU1.
  AbsErr_NU1 <- c() #Creating vector.
  prediction_NU1 <- calculate_predictions(1,get_k_nearest(1, k_NN))
  AbsErr_NU1 <- append(AbsErr_NU1, test_data[1,2] - prediction_NU1[1]) %>% append(test_data[1,4] - prediction_NU1[3]) %>%
                append(test_data[1,5] - prediction_NU1[4]) %>% append(test_data[1,6] - prediction_NU1[5]) %>%
                append(test_data[1,7] - prediction_NU1[6]) %>% append(test_data[1,9] - prediction_NU1[8]) 
  
  #Calculating Absolute Error for NU2.
  AbsErr_NU2 <- c() #Creating vector.
  prediction_NU2 <- calculate_predictions(2,get_k_nearest(2, k_NN))
  AbsErr_NU2 <- append(AbsErr_NU2, test_data[2,3] - prediction_NU2[2]) %>% append(-(test_data[2,4]-prediction_NU2[3])) %>%
                append(test_data[2,5] - prediction_NU2[4]) %>% append(test_data[2,6] - prediction_NU2[5]) %>%
                append(-(test_data[2,8] - prediction_NU2[7])) %>% append(-(test_data[2,9] - prediction_NU2[8])) 
  
  #Final result: Mean Absolute Error.
  MAE <- c(NU1 = sum(AbsErr_NU1)/length(AbsErr_NU1), NU2 = sum(AbsErr_NU2)/length(AbsErr_NU2))
  
  rm(AbsErr_NU1,AbsErr_NU2,prediction_NU1,prediction_NU2) #Deleting useless vectors.
  
  return(MAE) #Returning vector.
}