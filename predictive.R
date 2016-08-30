#This scripts uses shiny and R to help doing text analytics and text mining
#This is the server side logic scripts for intefacing with ui.R. 
#
#Author: Eric Goh
#Department: Institutional Statistics
#Company: Nanyang Technological University
#
#License: LGPL, All rights reserved. 
#
#Reference: 
#1. http://www.svm-tutorial.com/2014/11/svm-classify-text-r/
#
#hosted at: https://gohminghui88.shinyapps.io/JATAS/



###############################################################################################
# IMPORTANT:                                                                                  #
###############################################################################################
#
#You will face the following error: 
#create_matrix error or Error in if (attr(weighting, "Acronym") == "tf-idf") weight <- 1e-09 : 
#argument is of length zero: 
#This is a bug in RTextTools packages. To solve it: 
#Run "trace("create_matrix",edit=T)"
#At "if (attr(weighting, "Acronym") == "tf-idf")"
#change Acronym to acronym and save the file.
#
###############################################################################################



library(tm);
library(SnowballC);
library(RColorBrewer);
library(ggplot2);
library(wordcloud);
library(biclust); 
#library(cluster); 
library(igraph); 
library(fpc);
library(nnet);
library(e1071);
library(RTextTools);



#data: the dataset in data frame object
#responseVar: column name for the column to be predicted
#docsVar: column name for the column containing all the docs


evaluateModel <- function(data, responseVar, docsVar) {
  
  #Get Docs and Categories
  docs <- data[, docsVar];
  categories <- data[, responseVar];
  
  #Get ProcessedData
  pData <- cbind(categories, docs);
  
  
  #Get testData and trainData from data
  index <- 1:nrow(pData)
  testindex <- sample(index, trunc(length(index)/3))
  testset <- pData[testindex,]
  trainset <- pData[-testindex,]
  
  #Get Response Variable and Classifier Type
  classifier_type <- 1;
  #selResponseVar <- trainset[, 1];  #column 1 is response variable as pData <- cbind(selData, docs[, 1]);
  
  results <- "";
  
  if(classifier_type == 1) {
    
    #Create dtm
    dtm <- create_matrix(trainset[, 2]);
    
    # Configure the training data
    container <- create_container(dtm, trainset[, 1], trainSize=1:nrow(trainset), virgin=FALSE);
    
    # train a SVM Model
    model <- train_model(container, "SVM", kernel="linear", cost=1);
    
    
    
    # create a prediction document term matrix
    predMatrix <- create_matrix(testset[, 2], originalMatrix=dtm);
    #create_matrix error: 
    #Run "trace("create_matrix",edit=T)"
    #At "if (attr(weighting, "Acronym") == "tf-idf")"
    #change Acronym to acronym and save the file.
    
    # create the corresponding container
    predSize = nrow(testset);
    predictionContainer <- create_container(predMatrix, labels=rep(0,predSize), testSize=1:predSize, virgin=FALSE);
    
    
    # predict
    results <- classify_model(predictionContainer, model);
    #results;
  }
  
  cfm <- table(pred = results[, 1], true = testset[, 1]); #column 1 is response variable as pData <- cbind(selData, docs[, 1]);
  
  return(cfm);
  
}


#data: the dataset in data frame object
#responseVar: column name for the column to be predicted
#docsVar: column name for the column containing all the docs
#docs2pred: data frame object, single column, containing only all the docs to be predicted. 

SVMPredict <- function(data, responseVar, docsVar, docs2pred) {
  
  #Get Docs and Categories
  docs <- data[, docsVar];
  categories <- data[, responseVar];
  
  #Get ProcessedData
  trainset <- cbind(categories, docs);
  
  #Get testset
  testset <- docs2pred;
  
  results <- "";
  
  ###TRAINING###
  
    #Create dtm
    dtm <- create_matrix(trainset[, 2]);
    
    # Configure the training data
    container <- create_container(dtm, trainset[, 1], trainSize=1:nrow(trainset), virgin=FALSE);
    
    # train a SVM Model
    model <- train_model(container, "SVM", kernel="linear", cost=1);
    

  ###PREDICTION###
  
  # create a prediction document term matrix
  predMatrix <- create_matrix(testset, originalMatrix=dtm);
  
  # create the corresponding container
  predSize = nrow(testset);
  predictionContainer <- create_container(predMatrix, labels=rep(0,predSize), testSize=1:predSize, virgin=FALSE);
  
  
  # predict
  results <- classify_model(predictionContainer, model);
  #results;
  
  results <- cbind(testset, results);
  
  return(results);
}



#EXAMPLE USAGE

#Model Evaluation
data <- read.csv("D:/EricGoh/NTU/R_Scripts/JATAS/testData2.csv");
responseVar <- "A4";
docsVar <- "A5";

cfm <- evaluateModel(data, responseVar, docsVar);
cfm;   #Confusion Matrix


#Prediction

data <- read.csv("D:/EricGoh/NTU/R_Scripts/JATAS/testData2.csv");
responseVar <- "A4";
docsVar <- "A5";

docs2pred <- read.csv("D:/EricGoh/NTU/R_Scripts/JATAS/testset.csv");

results <- SVMPredict(data, responseVar, docsVar, docs2pred);

View(results);


