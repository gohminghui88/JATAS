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
#1. https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html
#2. http://stackoverflow.com/questions/24703920/r-tm-package-vcorpus-error-in-converting-corpus-to-data-frame
#3. http://www.r-bloggers.com/simple-text-mining-with-r/
#4. https://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Classification/SVM
#5. https://sites.google.com/site/miningtwitter/questions/sentiment/sentiment
#6. http://www.svm-tutorial.com/2014/11/svm-classify-text-r/
#7. http://www.slideshare.net/rdatamining/text-mining-with-r-an-analysis-of-twitter-data
#
#hosted at: https://gohminghui88.shinyapps.io/JATAS/


#import "shiny" library for easy web interface development
library(shiny);

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
library(Rgraphviz);
library(graph);
#Use the following command to install RGraphViz
#source("http://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz")


# Define server logic required to scrape data
shinyServer(function(input, output, session) {
  
  #Data Input
  output$dataTable = renderDataTable({ 
    
    data <- readData(input);
    
    
    updateSelectInput(session, "varSelect", label = "variables",
                      choices = colnames(data), selected = colnames(data)[2]
    );
    
    updateSelectInput(session, "varSelect2", label = "variables",
                      choices = colnames(data), selected = colnames(data)[1]
    );
    
    data
    
  })
  
  
  #Preprocess Text
  output$preprocessedTable = renderDataTable({ 
    
    data <- readData(input);
    selData <- data[, input$varSelect];
    
    #Convert selected data frame to corpuses
    d <- data.frame(selData);
    ds <- DataframeSource(d);
    docs <- Corpus(ds);
    
    #Remove Punctation
    if(input$rmPunctCB == TRUE)
    {
      docs <- tm_map(docs, removePunctuation);
    }
    
    if(input$rmNumCB == TRUE)
    {
      docs <- tm_map(docs, removeNumbers);
    }
    
    if(input$convLowercaseCB == TRUE)
    {
      docs <- tm_map(docs, tolower);
    }
    
    if(input$rmStopWordsCB == TRUE)
    {
      docs <- tm_map(docs, removeWords, stopwords("english"));
    }
    
    if(input$snowballCB == TRUE)
    {
      docs <- tm_map(docs, stemDocument);
    }
    
    if(input$rmWhiteSpaceCB == TRUE)
    {
      docs <- tm_map(docs, stripWhitespace);
    }
    
    
    #convert corpus to data frame
    res <- data.frame(text=unlist(sapply(docs, `[`, "content")), stringsAsFactors=F);
    
    res
    
  })
  
  #Data Exploration
  
  #Term Frequency Table 
  output$exploredTable = renderTable({ 
    
    if(input$expSelect == 1) {
      docs <- getProcessedData(input);
      maxWords <- input$minFreqText;
    
      dtm <- DocumentTermMatrix(docs);
      dtm_tfxidf <- weightTfIdf(dtm);
      
      freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE);
      #tfxidf <- sort(colsums(as.matrix(dtm_tfxidf)), decreasing=TRUE);
      
      freqTB <- data.frame(freq, 100);
      #tfTB <- data.frame()
      
      freqTB[1:maxWords,]
    }
  
  })
  
  #Word Cloud Plot
  #plotOutput
  output$plot1 <- renderPlot({
    if(input$expSelect == 2) {
      docs <- getProcessedData(input);
    
      dtm <- DocumentTermMatrix(docs);
      freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE);
    
      maxWords <- input$minFreqText;
    
      set.seed(142);   
      dark2 <- brewer.pal(6, "Dark2");   
      wordcloud(names(freq), freq, scale=c(4,0.5), max.words=maxWords, rot.per=0.2, colors=dark2)    
    }
    
    #freq Plot
    else if(input$expSelect == 3) {
      docs <- getProcessedData(input);
      
      dtm <- DocumentTermMatrix(docs);
      freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE);
      
      maxWords <- input$minFreqText;
      
      wf <- data.frame(word=names(freq), freq=freq);   
      p <- ggplot(wf[1:maxWords,], aes(word, freq));    
      p <- p + geom_bar(stat="identity");   
      p <- p + theme(axis.text.x=element_text(angle=45, hjust=1));   
      p
      
    }
    
    #Assoc Plot
    else if(input$expSelect == 4) {
      docs <- getProcessedData(input);
      
      dtm <- DocumentTermMatrix(docs);
      
      maxWords <- as.numeric(input$minFreqText);
      freq.terms <- findFreqTerms(dtm, lowfreq=maxWords);
      
      plot(dtm, term=freq.terms, corThreshold = 0.12, weighting = TRUE)
    }
    
  })
  

  
  #Clustering
  #plotOutput
  output$clusterPlot <- renderPlot({
    #if(input$expSelect == 2) {
      docs <- getProcessedData(input);
      
      dtm <- DocumentTermMatrix(docs);
      #dtms <- removeSparseTerms(dtm, 0.15); # Prepare the data (max 15% empty space)   
      #d <- dist(t(dtms), method="euclidian");   
      
      dtm_tfxidf <- weightTfIdf(dtm);
      
      #http://michael.hahsler.net/SMU/CSE7337/install/tm.R
      ### k-means (this uses euclidean distance)
      m <- as.matrix(dtm_tfxidf);
      rownames(m) <- 1:nrow(m);
      
      ### don't forget to normalize the vectors so Euclidean makes sense
      norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5);
      m_norm <- norm_eucl(m);
      
      
      ### cluster into 10 clusters
      cl <- kmeans(m_norm, input$minClusterText);
      cl;
      
      table(cl$cluster);
      
      ### show clusters using the first 2 principal components
      plot(prcomp(m_norm)$x, col=cl$cl)
      #kfit <- kmeans(d, input$minClusterText);   
      #clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)
      
      #clusplot(as.matrix(m_norm), cl$cluster, color=T, shade=T, labels=2, lines=0)
    #}
    
  })
  
  
  output$clustTableOutput = renderDataTable({
  
    #if(input$expSelect == 2) {
    docs <- getProcessedData(input);
    
    dtm <- DocumentTermMatrix(docs);
    #dtms <- removeSparseTerms(dtm, 0.15); # Prepare the data (max 15% empty space)   
    #d <- dist(t(dtms), method="euclidian");   
    
    dtm_tfxidf <- weightTfIdf(dtm);
    
    #http://michael.hahsler.net/SMU/CSE7337/install/tm.R
    ### k-means (this uses euclidean distance)
    m <- as.matrix(dtm_tfxidf);
    rownames(m) <- 1:nrow(m);
    
    ### don't forget to normalize the vectors so Euclidean makes sense
    norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5);
    m_norm <- norm_eucl(m);
    
    
    ### cluster into 10 clusters
    cl <- kmeans(m_norm, input$minClusterText);
    #as.data.frame(table(cl));
    
    as.data.frame(table(cl$cluster))
    
    #findFreqTerms(dtm[cl$cluster==1], 50);
    #inspect(docs[which(cl$cluster==1)])
    #as.data.frame(docs[which(cl$cluster==1)])
    
  })
  
  #Sentiment Analysis
  
  #DataTable
  output$sentiTable = renderTable({ 
    
    docs <- getProcessedData(input);
    #convert corpus to data frame
    docs <- data.frame(text=unlist(sapply(docs, `[`, "content")), stringsAsFactors=F);
    
    dict <- readDict(input);
    
    numObs <- input$maxObsText;
    
    
    sentimentAnalysis(docs, dict, numObs)
    
    
  })
  
  output$sentiPlot <- renderPlot({
  
    docs <- getProcessedData(input);
    #convert corpus to data frame
    docs <- data.frame(text=unlist(sapply(docs, `[`, "content")), stringsAsFactors=F);
    
    dict <- readDict(input);
    
    numObs <- input$maxObsText;
    
    
    res <- sentimentAnalysis(docs, dict, numObs);
    
    plot(factor(res$sentiments))
    
  })
  
  
  #Predictive Analytics
  
  #Model Evaluation
  output$evalTableOutput = renderTable({
    
    #Get data
    data <- readData(input);
    
    #Get Processed Documents
    docs <- getProcessedData(input);
    docs <- data.frame(text=unlist(sapply(docs, `[`, "content")), stringsAsFactors=F);
    
    #Get Response Variable Columns
    selData <- data[, input$varSelect2];
    
    
    #Get ProcessedData
    pData <- cbind(selData, docs[, 1]);
    
    
    #Get testData and trainData from data
    index <- 1:nrow(pData)
    testindex <- sample(index, trunc(length(index)/3))
    testset <- pData[testindex,]
    trainset <- pData[-testindex,]
    
    #Get Response Variable and Classifier Type
    classifier_type <- input$predSelect;
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
    
    cfm
    
    #trainset
    
  })
  
  #Prediction
  output$predTableOutput = renderDataTable({
    
    #input data for prediction
    inFile3 <- input$file3;
    
    if (is.null(inFile3))
      return(NULL);
    
    testset <- read.csv(inFile3$datapath, header = input$header3,
                          sep = input$sep3, quote = input$quote3);
    
    
    
    #Get training data
    
    #Get data
    data <- readData(input);
    
    #Get Processed Documents
    docs <- getProcessedData(input);
    docs <- data.frame(text=unlist(sapply(docs, `[`, "content")), stringsAsFactors=F);
    
    #Get Response Variable Columns
    selData <- data[, input$varSelect2];
    
    
    #Get ProcessedData
    trainset <- cbind(selData, docs[, 1]);
    
    
    #Get Response Variable and Classifier Type
    classifier_type <- input$predSelect;
    #selResponseVar <- trainData[, 1]; #column 1 is response variable as trainData <- cbind(selData, docs[, 1]);
    
    results <- "";
    
    
    #SVM
    if(classifier_type == 1)
    {
      
      #Create dtm
      dtm <- create_matrix(trainset[, 2]);
      
      # Configure the training data
      container <- create_container(dtm, trainset[, 1], trainSize=1:nrow(trainset), virgin=FALSE);
      
      # train a SVM Model
      model <- train_model(container, "SVM", kernel="linear", cost=1);
      
      
      
      # create a prediction document term matrix
      predMatrix <- create_matrix(testset, originalMatrix=dtm);
      
      # create the corresponding container
      predSize = nrow(testset);
      predictionContainer <- create_container(predMatrix, labels=rep(0,predSize), testSize=1:predSize, virgin=FALSE);
      
      
      # predict
      results <- classify_model(predictionContainer, model);
      #results;
      
      results <- cbind(testset, results);
    }
    
    results
    
  })
  
})



readData <- function(input) {
  
  inFile <- input$file1;
  
  if (is.null(inFile))
    return(NULL);
  
  data <- read.csv(inFile$datapath, header = input$header,
                   sep = input$sep, quote = input$quote);
  
  return(data);
  
}

readDict <- function(input) {
  
  inFile <- input$file2;
  
  if (is.null(inFile))
    return(NULL);
  
  data <- read.csv(inFile$datapath, header = input$header2,
                   sep = input$sep2, quote = input$quote2);
  
  return(data);
  
}

getProcessedData <- function(input) {
  data <- readData(input);
  selData <- data[, input$varSelect];
  
  #Convert selected data frame to corpuses
  d <- data.frame(selData);
  ds <- DataframeSource(d);
  docs <- Corpus(ds);
  
  #Remove Punctation
  if(input$rmPunctCB == TRUE)
  {
    docs <- tm_map(docs, removePunctuation);
  }
  
  if(input$rmNumCB == TRUE)
  {
    docs <- tm_map(docs, removeNumbers);
  }
  
  if(input$convLowercaseCB == TRUE)
  {
    docs <- tm_map(docs, tolower);
  }
  
  if(input$rmStopWordsCB == TRUE)
  {
    docs <- tm_map(docs, removeWords, stopwords("english"));
  }
  
  if(input$snowballCB == TRUE)
  {
    docs <- tm_map(docs, stemDocument);
  }
  
  if(input$rmWhiteSpaceCB == TRUE)
  {
    docs <- tm_map(docs, stripWhitespace);
  }
  
  
  #convert corpus to data frame
  #res <- data.frame(text=unlist(sapply(docs, `[`, "content")), stringsAsFactors=F);
  
  return(docs);
}



sentimentAnalysis <- function(docs, dict, numObs)
{
  
  #Dict: http://www.wjh.harvard.edu/~inquirer/homecat.htm
  #Dict_All: https://www.quora.com/Is-there-a-downloadable-database-of-positive-and-negative-words
  
  
  #dict <- havard_sentimentAnalysis_Pos_Neg_Dict;
  #data <- testData2;
  #docs <- data[, docVar];
  
  resData <- data.frame(sentiments=character(), stringsAsFactors=FALSE);
  
  for(j in 1:numObs) {
    
    curSentiment <- "";
    
    for(i in 1:nrow(dict)) {
      
      word <- dict[i, 1];
      pos <-  dict[i, 2];
      neg <-  dict[i, 3];
      
      if(grepl(as.character(word), as.character(docs[j, 1]), ignore.case = TRUE) == TRUE)
      {
        if(pos != "") { curSentiment <- "positive"; } 
        else if(neg != "") { curSentiment <- "negative"; } 
        #else { curSentiment <- "none"; }
      }
    }
    
    #resData <- rbind(resData, c(curSentiment));
    resData[nrow(resData)+1,] <- c(curSentiment);
  }
  
  resData <- cbind(docs[1:numObs, ], resData);
  
  return(resData)
  
}


