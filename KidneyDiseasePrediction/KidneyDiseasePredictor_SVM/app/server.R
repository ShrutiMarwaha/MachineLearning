library(caret)
library(kernlab)
library(shiny)
########################

source("helpers.R")
# It is important to store the rds file in same folder as ui and server files and do not provide complete path
svm_rbf_smallmodel <- readRDS("svm_rbf_smallmodel.rds")
print("loaded model file")
########################

shinyServer(
  function(input, output) 
  {
    output$inputValue <- renderPrint({input$rbc_count})
    output$inputValue <- renderPrint({input$bu})
    output$inputValue <- renderPrint({input$sod})
    output$inputValue <- renderPrint({input$pc})
    output$inputValue <- renderPrint({input$dm})
    output$inputValue <- renderPrint({input$age})
    output$inputValue <- renderPrint({input$sg})
    output$inputValue <- renderPrint({input$pe})
    output$inputValue <- renderPrint({input$bp})
    
    # build the input dataframe for model
    output$user_input <- renderTable({create_input_matrix(input$rbc_count,input$bu,input$sod,input$pc,input$dm,input$age,input$sg,input$pe,input$bp)})
    # predict the risk using the model
    output$prediction <- renderPrint({chronicKidneyDiseaseRisk(svm_rbf_smallmodel,input$rbc_count,input$bu,input$sod,input$pc,input$dm,input$age,input$sg,input$pe,input$bp)})   
  }
)