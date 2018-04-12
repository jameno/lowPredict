function(input, output) {
  
  #Sentiment Calculation
  sentimentVal = reactive({
    
    cat(sentiment(variableText$text)$sentiment)
    
    
    })
  
  #Wordcloud calculation
  freqr=reactive({
    
    input$update
    
    docs=Corpus(VectorSource(variableText$text))
    
    docs <- tm_map(docs,content_transformer(tolower))
    
    docs = tm_map(docs, removePunctuation)
    
    docs = tm_map(docs, removeWords, stopwords("english"))
    
    dtm <- DocumentTermMatrix(docs)
    
    freqr <- colSums(as.matrix(dtm))
    
  })
  
  #Warning output
  
  getWarning = reactive({
    
    if(sum(which(sentiment(variableText$text)$sentiment < -.5)>0)){
      "DON'T SEND IT!!!!!!!!!!!!!!!!!!! AHGHGAHIDHJAIDSJFISAJ"
    }
    else{
      "ALL IS RIGHT IN THE WORLD"
    }
    
    
  })
  
  variableText <- reactiveValues(data = NULL)
  variableText$text <- "Sample Text"
  
  observeEvent(input$buttonFunction, {variableText$text <- input$text})
  
 
    output$value <-renderPrint({ variableText$text})
    
    output$sentimentVal = renderPrint({sentimentVal()})
    
    output$warning <- renderText({getWarning()})
    
    output$plot = renderPlot({
      if(is.null(variableText$text))return()
      freqr = freqr()
      wordcloud(names(freqr),freqr, min.freq=1)
    })
}