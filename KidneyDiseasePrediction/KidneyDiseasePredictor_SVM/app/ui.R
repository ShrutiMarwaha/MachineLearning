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
                            numericInput("bu", "Blood Urea (mgs/dl)", 30, min = 1, max = 500, step = 1),
                            numericInput("sod", "Blood Sodium (mEq/L)", 145, min = 100, max = 200, step = 1),
                            numericInput("sg", "Urinary specific gravity", 1.02, min = 1.010, max = 1.025, step = 0.005) ,

                            h4("Pus Cells",style="font-size:11pt"),
                            checkboxInput("pc", label = "Abnormal Pus Cells", value = FALSE)
                          ) 
                  ),
    
          column(4, offset = 0.5,
                  wellPanel(
                              h4("Medical Conditions",style = "color: blue; font-family: 'times'; font-size:14pt;"),
                              checkboxInput("dm", label = "Diabetes Millitus", value = FALSE),
                              checkboxInput("pe", label = "Pedal Edema", value = FALSE)
                            ),

                  wellPanel(
                              h4("Other Vitals",style = "color: blue; font-family: 'times'; font-size:14pt;"),
                              numericInput("bp","Blood Pressure (mmHg)", 70, min=10, max=250, step=1),
                              numericInput("age","Age (years)", 40, min=0, max=150, step=1) 
                           ), 
           
                submitButton("Submit") 
              ),

          column(4, 
                  wellPanel(
#                              h4('Input Matrix'),
#                              tableOutput(outputId = "user_input"),
                             h4('Probability that Patient is at High Risk for Chronic Kidney Disease',style = "color: blue; font-family: 'times'; font-size:14pt;"),
                             verbatimTextOutput(outputId = "prediction")
                          )
                )
          )
      )
)
