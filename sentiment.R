#This scripts uses R to help do Sentiment Analysis
#This is the server side logic scripts for intefacing with ui.R. 
#
#Author: Eric Goh
#Department: Institutional Statistics
#Company: Nanyang Technological University
#
#License: LGPL, All rights reserved. 
#
#Reference: 
#1. https://sites.google.com/site/miningtwitter/questions/sentiment/sentiment
#
#hosted at: https://gohminghui88.shinyapps.io/JATAS/


#doc: data frame object, single column, containing all observations of text/comments and etc. 
#dict: data frame object, three columns, 1st column: words, 2nd column: postive, 3rd column: negative
#modified from http://www.wjh.harvard.edu/~inquirer/homecat.htm positive and negative wordlists. 
#sample modified dict file: https://drive.google.com/file/d/0Bz4dLRb7D8RUZVJiZzZ6RlBnNFE/view?usp=sharing


sentimentAnalysis <- function(docs, dict, numObs)
{
  
  #Dict: http://www.wjh.harvard.edu/~inquirer/homecat.htm
  #Dict_All: https://www.quora.com/Is-there-a-downloadable-database-of-positive-and-negative-words
  
  resData <- data.frame(sentiments=character(), stringsAsFactors=FALSE);
  
  for(j in 1:numObs) {
    
    curSentiment <- "";
    
    for(i in 1:nrow(dict)) {
      
      word <- dict[i, 1];
      pos <-  dict[i, 2];
      neg <-  dict[i, 3];
      
      if(grepl(as.character(word), as.character(docs[j]), ignore.case = TRUE) == TRUE)
      {
        if(pos != "") { curSentiment <- "positive"; } 
        else if(neg != "") { curSentiment <- "negative"; } 
        #else { curSentiment <- "none"; }
      }
    }
  
    resData[nrow(resData)+1,] <- c(curSentiment);
  }
  
  resData <- cbind(docs[1:numObs], resData);
  
  return(resData)
  
}


#Example usage
docs <- data.frame(observations = c("Abandon my stuffs", "Abide to rules", "a"));
numObs <- nrow(docs);
docs <- docs[, 1];

dict <- read.csv("D:/EricGoh/NTU/R_Scripts/JATAS/havard_sentimentAnalysis_Pos_Neg_Dict.csv"); #change to the location of your dict file
View(dict)

res <- sentimentAnalysis(docs, dict, numObs);
View(res)

