library(shiny)
#load("/Users/shruti/Desktop/WorkMiscellaneous/MachineLearning/ChronicKidneyDisease/app/svm_rbf_smallmodel.rda")

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Chronic Kidney Disease Prediction Tool"),
  
  fluidRow(
          column(12, 
                 h4(
                 helpText("This app helps health care providers to identify patients at the risk of  
                          Chronic Kidney Disease.") 
                    ) 
                 ) 
          ),

  fluidRow(
          column(4,
                  wellPanel(
                            h4("Blood & Urine Tests",style = "color: blue; font-family: 'times'; font-size:14pt;"),
                            numericInput("rbc_count", "Red Blood Cell Count (millions/cmm)", 5, min = 0, max = 20, step = 0.1),
                            numericInput("wbc_count", "White Blood Cell Count (cells/cumm)", 7000, min = 3000, max = 20000, step = 100),
                            numericInput("bu", "Blood Urea (mgs/dl)", 30, min = 1, max = 500, step = 1),
                            numericInput("sod", "Blood Sodium", 140, min = 100, max = 200, step = 1),
                            numericInput("sg", "Urinary specific gravity", 1.02, min = 1.010, max = 1.025, step = 0.005) ,
                            selectInput("pc", 
                                      label = "Abnormal Pus Cells",
                                      #choices = c("Normal", "Abnormal"),
                                      choices = c(TRUE, FALSE),
                                      selected = "FALSE"),
                            selectInput("ba", 
                                        label = "Bacterial Infection",
                                        #choices = c("Normal", "Abnormal"),
                                        choices = c(TRUE, FALSE),
                                        selected = "FALSE") 
                          ) 
                  ),
    
          column(4, offset = 0.5,
                  wellPanel(
                            h4("Medical Conditions",style = "color: blue; font-family: 'times'; font-size:14pt;"),
                            selectInput("dm", 
                                        label = "Diabetes Millitus",
                                        #choices = c("Yes", "No"),
                                        choices = c(TRUE, FALSE),
                                        selected = "FALSE"),
                            
                            selectInput("pe", 
                                        label = "Pedal Anemia",
                                        #choices = c("Yes", "No"),
                                        choices = c(TRUE, FALSE),
                                        selected = "FALSE"),
                        
                            selectInput("cad", 
                                        label = "Coronary Artery Disease",
                                        #choices = c("Yes", "No"),
                                        choices = c(TRUE, FALSE),
                                        selected = "FALSE")
                              ),
    
                  wellPanel(
                            h4("Other Vitals",style = "color: blue; font-family: 'times'; font-size:14pt;"),
                            numericInput("bp","Blood Pressure", 70, min=10, max=250, step=1),
                            numericInput("age","Age (years)", 40, min=0, max=150, step=1) 
                           ), 
           
                submitButton("Submit") 
              ),

          column(4, 
                  wellPanel(
                             #h4('Input Matrix'),
                             #tableOutput(outputId = "user_input"),
                             #h4('At Risk of Chronic Kidney Disease?',style = "color: blue; font-family: 'times'; font-size:14pt;"),
                             #verbatimTextOutput(outputId = "prediction")
                             h4('Probability that Patient is at High Risk for Chronic Kidney Disease',style = "color: blue; font-family: 'times'; font-size:14pt;"),
                             verbatimTextOutput(outputId = "prediction")
                          )
                )
          )
      )
)
