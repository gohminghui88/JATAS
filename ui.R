#This scripts uses shiny and R to help doing simple text analysis and mining, especially for feedbacks and surveys
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


shinyUI(navbarPage("JATAS v1.0 beta", 

      #Data Input
      tabPanel("Data", 
              sidebarLayout(
                  sidebarPanel(
                                
                                
                      h3("Data (CSV format, at least 2 columns, 1st categorical, 2nd text) >>"),
                      tags$br(),
                                
                      fileInput('file1', 'Choose file to upload',
                            accept = c(
                                'text/csv',
                                'text/comma-separated-values',
                                'text/tab-separated-values',
                                'text/plain',
                                '.csv',
                                '.tsv'
                              )
                      ), 
                                
                      tags$hr(),
                                
                      checkboxInput('header', 'Header', TRUE),
                                
                      radioButtons('sep', 'Separator',
                                    c(Comma=',',
                                      Semicolon=';',
                                      Tab='\t'),
                                    ','),
                                
                      radioButtons('quote', 'Quote',
                                    c(None='',
                                      'Double Quote'='"',
                                      'Single Quote'="'"),
                                    '"'),
                                
                      tags$hr() 
                                
                  ), 
                              
                  mainPanel(
                                
                        dataTableOutput("dataTable")  
                        #downloadButton("dataFileOutput", "Download")
                                
                    )
                  )
                            
        ), 
        
        #Text Preprocessing
        tabPanel("Text Preprocessing >>", 
              sidebarLayout(
                    sidebarPanel(
                      
                      h3("Preprocess Your Text"), 
                      tags$br(), 
                      
                      selectInput("varSelect", label = h3("Variables"), 
                                  choices = list("Choice 1" = 1, "Choice 2" = 2,
                                                 "Choice 3" = 3), selected = 1), 
                      tags$br(),
                      
                      checkboxInput('rmPunctCB', 'Remove Punctuation', FALSE),
                      checkboxInput('rmNumCB', 'Remove Numbers', FALSE),
                      checkboxInput('convLowercaseCB', 'Convert Lowercase', FALSE),
                      checkboxInput('rmStopWordsCB', 'Remove Stop Words', FALSE),
                      checkboxInput('snowballCB', 'Snowball Stemming', FALSE),
                      checkboxInput('rmWhiteSpaceCB', 'Remove White Spaces', FALSE), 
                      
                      tags$br()
                    ),
                    
                    mainPanel(
                      dataTableOutput("preprocessedTable")
                      
                    )
              )
        ), 
        
        #Data Exploration
        tabPanel("Data Exploration >>", 
              sidebarLayout(
                  sidebarPanel(
                    
                    h3("Words/Freq: "), 
                    textInput("minFreqText", label = h5("Amount"), value = "15"),
                    
                    selectInput("expSelect", label = h3("Methods"), 
                                choices = list("Term Freq Tables" = 1, "Wordcloud" = 2, "Freq Plot" = 3, "Word Assoc Plot" = 4
                                               ), selected = 1),
                    
                    tags$br()
                    
                  ),
                              
                  mainPanel(
                    tableOutput("exploredTable"), 
                    plotOutput("plot1", click = "plot_click") 
                    #plotOutput("plot2", click = "plot_click"), 
                    #plotOutput("plot3", click = "plot_click")
                  )
              )
        ), 
        
        #Clustering
        tabPanel("Clustering >>", 
              sidebarLayout(
                    sidebarPanel(
                      h3("Number of Clusters: "), 
                      textInput("minClusterText", label = h5("Clusters"), value = "10"), 
                      tags$br()
                    ),
                              
              mainPanel(
                    #dataTableOutput("clusterTable"), 
                    plotOutput("clusterPlot", click = "plot_click"), 
                    dataTableOutput("clustTableOutput")
                                
                    )
              )
        ), 
        
        #Sentiment Analysis
        tabPanel("Sentiment Analysis >>", 
                sidebarLayout(
                      sidebarPanel(
                        h3("Dictionary (CSV format, 1st Column: Word, 2nd Column: Positive, 3rd Column: Negative)"),
                        tags$br(),
                        
                        fileInput('file2', 'Choose file to upload',
                                  accept = c(
                                    'text/csv',
                                    'text/comma-separated-values',
                                    'text/tab-separated-values',
                                    'text/plain',
                                    '.csv',
                                    '.tsv'
                                  )
                        ), 
                        
                        tags$hr(),
                        
                        checkboxInput('header2', 'Header', TRUE),
                        
                        radioButtons('sep2', 'Separator',
                                     c(Comma=',',
                                       Semicolon=';',
                                       Tab='\t'),
                                     ','),
                        
                        radioButtons('quote2', 'Quote',
                                     c(None='',
                                       'Double Quote'='"',
                                       'Single Quote'="'"),
                                     '"'),
                        
                        tags$br(), 
                        tags$a(href="https://drive.google.com/file/d/0Bz4dLRb7D8RUZVJiZzZ6RlBnNFE/view?usp=sharing", "Sample Dictionary"), 
                        tags$hr(), 
                        
                        h3("Number of Observations to Analyze: "), 
                        textInput("maxObsText", label = h5("Max Number of Obs"), value = "10"), 
                        tags$hr(), 
                        
                        tags$p("Note: Sentiment Analysis can takes very long time, in particular when the dictionary or docs is very big. Use the following R Script to run it on your computer. "), 
                        tags$a(href="https://drive.google.com/file/d/0Bz4dLRb7D8RUTGJZcDRZYkZkMDQ/view?usp=sharing", "Sentiment Analysis R Code"), 
                        tags$hr()
                        
                      ),
                              
                      mainPanel(
                        
                        plotOutput("sentiPlot", click = "plot_click"),   
                        tableOutput("sentiTable")
                                
                      )
                  )
              ), 
          
          #Predictive Analytics
          tabPanel("Predictive Analytics", 
                  sidebarLayout(
                      sidebarPanel(
                        tags$br(),
                        h3("Predictive Analytics"),
                        tags$br(),
                        
                        selectInput("predSelect", label = h3("Classifiers"), 
                                    choices = list("SVM" = 1), selected = 1),
                        
                        selectInput("varSelect2", label = h3("Response Variable (Compare with All Variables)"), 
                                    choices = list("Choice 1" = 1, "Choice 2" = 2,
                                                   "Choice 3" = 3), selected = 1), 
                        
                        tags$br(), 
                        tags$hr(),
                        
                        fileInput('file3', 'upload file to predict',
                                  accept = c(
                                    'text/csv',
                                    'text/comma-separated-values',
                                    'text/tab-separated-values',
                                    'text/plain',
                                    '.csv',
                                    '.tsv'
                                  )
                        ), 
                        
                        tags$hr(),
                        
                        checkboxInput('header3', 'Header', TRUE),
                        
                        radioButtons('sep3', 'Separator',
                                     c(Comma=',',
                                       Semicolon=';',
                                       Tab='\t'),
                                     ','),
                        
                        radioButtons('quote3', 'Quote',
                                     c(None='',
                                       'Double Quote'='"',
                                       'Single Quote'="'"),
                                     '"'),
                        
                        tags$hr(), 
                        
                        tags$p("Note: Due to a bug in RTextTools, you will see error in the prediction. Use the following link to download the R File for prediction. You will need to follow the instructions to remove the bug in RTextTools. "), 
                        tags$a(href="https://drive.google.com/file/d/0Bz4dLRb7D8RUSFF5ME9jcG5PSlk/view?usp=sharing", "SVM Predict R Code"), 
                        tags$hr()
                        
                        
                      ),
                              
                      mainPanel(
                        
                        h3("Model Evaluation (trainset=66.67%, testset=33.33%)"), 
                        tags$br(), 
                        tags$p("Confusion Matrix: Predict vs Actual"), 
                        tableOutput("evalTableOutput"),
                        tags$br(),  
                        tags$hr(),
                        tags$br(),  
                        h3("Prediction"),
                        tags$br(),
                        dataTableOutput("predTableOutput")
                                
                      )
                  )
            ), 
          
          #About
          tabPanel("About", 
                            
                #verbatimTextOutput("summary", 
                            
                h3("Just Another Text Analytics System (JATAS) v1.0 beta"),
                tags$br(),
                tags$p("Created by Eric Goh M. H., Institutional Statistics, Nanyang Technological University"),
                tags$p("License: GNU GPL"),
                tags$p("Description: JATAS is an online Text Analytics system that enables quick text analytics and visualization. It is not meant to replace softwares like SPSS with Text Analytics plugins. System is in beta version and has been tested with survey Dataset of up to 500 observations. "),
                tags$br(), 
                tags$p("All Rights Reserved. Copyrighted 2016. ")
                            
                #)
                            
          )

))