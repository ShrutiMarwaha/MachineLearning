library(caret)
library(shiny)
########################

#load("/Users/shruti/Desktop/WorkMiscellaneous/MachineLearning/ChronicKidneyDisease/app/svm_rbf_smallmodel.rda")
svm_rbf_smallmodel <- readRDS("/Users/shruti/Desktop/WorkMiscellaneous/MachineLearning/ChronicKidneyDisease/app/svm_rbf_smallmodel.rds")
svm_imp_features2 <- c("rbc.c","bu","sod","pc.abnormal","dm.no","sg.1.010","sg.1.020","sg.1.025","age","pe.no","wbcc","cad.no","ba.notpresent","bp")
source("helpers.R")
########################

shinyServer(
  function(input, output) {
    output$inputValue <- renderPrint({input$rbc_count})
    output$inputValue <- renderPrint({input$bu})
    output$inputValue <- renderPrint({input$sod})
    output$inputValue <- renderPrint({input$pc})
    output$inputValue <- renderPrint({input$dm})
    output$inputValue <- renderPrint({input$age})
    output$inputValue <- renderPrint({input$sg})
    output$inputValue <- renderPrint({input$pe})
    output$inputValue <- renderPrint({input$wbc_count})
    output$inputValue <- renderPrint({input$cad})
    output$inputValue <- renderPrint({input$ba})
    output$inputValue <- renderPrint({input$bp})
    output$user_input <- renderTable({create_input_matrix(input$rbc_count,input$bu,input$sod,input$pc,input$dm,input$age,input$sg,input$pe,input$wbc_count,input$cad,input$ba,input$bp)})
    output$prediction <- renderPrint({chronicKidneyDiseaseRisk(input$rbc_count,input$bu,input$sod,input$pc,input$dm,input$age,input$sg,input$pe,input$wbc_count,input$cad,input$ba,input$bp)})
    
  }
)