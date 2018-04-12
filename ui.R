library(shiny)
library(ggplot2)
library(sentimentr)
library(tm)
library(wordcloud)
library(topicmodels)
library(memoise)

fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      textInput("text", label = h3("Text input"), value = "Sample Text"),
      actionButton("buttonFunction", "Update")
     # actionButton("update", "Change")
      
    ),
    
    mainPanel(
      verbatimTextOutput("value"),
      verbatimTextOutput("sentimentVal"),
      h3(textOutput("warning", container = span)),
      plotOutput("plot")
      )
  )
    
)
  
