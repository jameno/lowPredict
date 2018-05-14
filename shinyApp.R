library(shiny)
library(forecast)
library(ggplot2)
library(caTools)
library(RCurl)
library(jsonlite)
library(twilio)

#load("YOUR_DATA_LOCATION/bgData2.RData")
Sys.setenv(TWILIO_SID = "YOUR TWILIO SID")
Sys.setenv(TWILIO_TOKEN = "YOUR TWILIO TOKEN")
if(FALSE){
drops <- c("_id", "date", "trend", "direction", "device", "type")

#Reverse the Data so we are going from Past to Present
alldata = rev(alldata)
alldata$sgv[is.na(alldata$sgv)] <- mean(alldata$sgv)
predictionData = c();
avg50 = c();
avg30 = c();
avg15 = c();
current = c();
future30 = c();
future60 = c();

for(i in 11:(nrow(alldata)-12)){
  #Create averages of 50, 30, and 15 minute periods
  avg50[i-10] = mean(alldata$sgv[i-10]:alldata$sgv[i]);
  avg30[i-10] = mean(alldata$sgv[i-6]:alldata$sgv[i]);
  avg15[i-10] = mean(alldata$sgv[i-3]:alldata$sgv[i]);
  current[i-10] = alldata$sgv[i];
  
  #Add predictor variables at 30 minutes and 60 minutes in the future
  future30[i-10] = alldata$sgv[i+6]
  future60[i-10] = alldata$sgv[i+12]
  
}

predictionData = data.frame(cbind(avg50, avg30, avg15, current, future30, future60))

ts_data=data.frame(c());
#Predicting set using 50 minutes of data
for(i in 11:(nrow(alldata)-12)){
  #Bind strip of 50min data and futureBG
  ts_data = rbind(ts_data, c(alldata$sgv[(i-10):i],alldata$sgv[i+6]))
  if(i%%1000==0){print(i)}
}

predictionData = ts_data

#TEST ON TRAINING SET
#split into 70% training and 30% testing
set.seed(1234)
sample = sample.split(predictionData, SplitRatio = .70)
train = subset(predictionData, sample == TRUE)
test  = subset(predictionData, sample == FALSE)

glm_model <- glm(future30~avg50+avg30+avg15+current,data=train,family="gaussian")
glm_model <- glm(X237~., data=train,family="gaussian")
predicted <- predict(glm_model, test)

}
lastFiftySGV <- alldata$sgv[1:10]

lastSGV = alldata$sgv[1]

lastFiftyTime <- alldata$dateString[1:10]

ui <- fluidPage(
  #(titlePanel("SGV Graph", windowTitle = "app")),
  
  sidebarPanel(
    textInput("text", label = h3("Nightscout URL"), value = "http://"),
    textInput("threshold", label = h3("Low Threshold"), value = "75"),
    textInput("phone", label = h3("Phone Number"), value = "+1"),
    actionButton("buttonFunction", "Start Predicting")
   ),
     mainPanel(
      plotOutput(outputId = "percent"),
      h3(textOutput("currentBG")),
      h3(textOutput("predictedBG")),
      h3(textOutput("warning"))
      #verbatimTextOutput("value")
  )
  
)

server <- function(input, output,session) {
  i <- reactiveValues(counter = 21790)
  flag <- reactiveValues(flag_num = 0)
  
  #Get latest glucose levels and prediction
  getWarning = reactive({
      paste("YOU CHANGED IT TO: ", fullURL,sep="")
    
  })
  
  variableText <- reactiveValues(data = NULL)
  #variableText$text <- "http://your-nightscout-url"
  thresholdVal <- reactiveValues(data = NULL)
  
  observeEvent(input$buttonFunction, {variableText$text <- input$text})
  observeEvent(input$buttonFunction, {thresholdVal$text <- input$threshold})
  
  output$percent <- renderPlot({
    #Don't start the autoupdates until a URL has been entered
    if(is.null(variableText$text))return()
    
    
    urldata <- getURL(fullURL)
    urldata <- fromJSON(urldata)
    urldata <- urldata[ , !(names(urldata) %in% drops)]
    
    invalidateLater(millis = 3000, session)
    i$counter <- isolate(i$counter) + 1
    flag$flag_num <- isolate(flag$flag_num)+1
    starttime=i$counter
    endtime=i$counter+49
    bg = predictionData$future30[(starttime):(endtime)]
    bg_predict = predict(glm_model,predictionData[starttime:endtime,1:4])
    
    bg = predictionData$X224.1[(starttime):(endtime)]
    bg_predict = predict(glm_model,predictionData[starttime:endtime,1:11])
    
    #Linear Predict
    bg_linear = (bg[50]-bg[48])*3+bg[50]
    
    #Scatterplot differences
    p<-ggplot() + geom_point(aes(x=1:50,y=bg),color='red', size = 5)
    p<- p + geom_point(aes(x=1:50,y=bg_predict[1:50]),color='blue',size=5)+xlab("Time (5 min)") +ylab("BG (mg/dL)")+ labs(title="Predicted BG (BLUE) vs Real (RED)")
    p<- p + geom_point(aes(x=56,y=bg_linear),color='blue',size=5,shape=1)+xlab("Time (5 min)") +ylab("BG (mg/dL)")+ labs(title="Predicted BG (BLUE) vs Real (RED)")
    p<- p+theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
    p
    p<-p+geom_hline(aes(yintercept=as.integer(thresholdVal$text)),color="red",size=1)
    
	your_phone = "YOUR PHONE NUMBER"
	twilio_num = "YOUR TWILIO NUMBER"
    output$currentBG <- renderText({paste("Current Blood Glucose: ", toString(bg[50]),sep="")})
    #output$predictedBG <- renderText({paste("Predicted BG in 30min: ", toString(round(bg_predict[50])),sep="")})
    output$predictedBG <- renderText({paste("Predicted BG in 30min: ", toString(round(bg_linear)),sep="")})  
    if(bg_linear<=as.integer(thresholdVal$text) && flag$flag_num > 0){
    #if(bg_predict[50]<=as.integer(thresholdVal$text) && flag$flag_num > 0){
      #tw_send_message(your_phone, twilio_num, paste("LOW PREDICTED! (",toString(round(bg_predict[50])),") in 30 min: ", (Sys.time()+1800),sep=""))
      tw_send_message(your_phone, twilio_num, paste("LOW PREDICTED! (",toString(round(bg_linear)),") in 30 min: ", (Sys.time()+1800),sep=""))
      output$warning <- renderText({paste("LOW PREDICTED! Warning sent: ", date(), sep="")})
      flag$flag_num = -12
    }
    print(flag$flag_num)
    p+ylim(1,400)
    
  })
  
  updateURL = reactive({
    fullURL <- paste(variableText$text,"/api/v1/entries/sgv.json",sep="")
    })
  
  #output$value <- renderPrint({ lastSGV })

  
}
shinyApp(ui, server) 