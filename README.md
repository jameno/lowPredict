#lowPredict

Using an R Shiny App to predict hypoglycemic events in real time from Nightscout CGM data

### WARNING
This application is meant for experimental purposes only. 
Do not use this application for medical decision or treatment.

### 1. Screen Startup

You will be asked for your Nightscout website url and the numeric threshold for when you want to be alerted.

### 2. Forecasting Display

Once your data is collected and personal model trained, you will begin to see live data of your CGM similar to nightscout with a 30 minute prediction window.

The display is updated in real time with your Nightscout site (every 5 minutes)

If your glucose level is predicted to drop below your threshold, the screen will display an alert.

