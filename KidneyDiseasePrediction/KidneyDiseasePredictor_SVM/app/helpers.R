# function to convert users input from app to matrix that can be fed to machine learning model
svm_imp_features <- c("rbc.c","bu","sod","pc.abnormal","dm.no","sg.1.010","sg.1.020","sg.1.025","age","pe.no","bp")

create_input_matrix <- function(rbc_count,bu,sod,pc,dm,age,sg,pe,bp) {
  input_df <- data.frame(matrix(data=NA,nrow=1,ncol=length(svm_imp_features),dimnames=list(c(),svm_imp_features)))
  
  input_df$age <- age
  input_df$rbc.c <- rbc_count
  input_df$bu <- bu
  input_df$sod <- sod
  input_df$bp <- bp
  input_df$dm.no <- as.numeric(!dm) # dm.no=1 means diabetes == FLASE
  input_df$pc.abnormal <- as.numeric((pc))  # pc.abnormal=1 menas Abnormal Pus Cells == TRUE
  input_df$pe.no <- as.numeric(!pe) # pe.no=1 means Pedal Edema == FLASE

  
  if(sg==1.010)
  {
    input_df$sg.1.010 <- 1
    input_df$sg.1.020 <- 0
    input_df$sg.1.025 <- 0
  }
  else if(sg==1.015)
  {
    input_df$sg.1.010 <- 0
    input_df$sg.1.020 <- 0
    input_df$sg.1.025 <- 0
  }
  else if(sg==1.020)
  {
    input_df$sg.1.010 <- 0
    input_df$sg.1.020 <- 1
    input_df$sg.1.025 <- 0
  }
  else if(sg==1.025)
  {
    input_df$sg.1.010 <- 0
    input_df$sg.1.020 <- 0
    input_df$sg.1.025 <- 1
  }
  
  return(input_df)
}

# function to predict outcome using svm model and above user's input data
chronicKidneyDiseaseRisk <- function(svm_rbf_smallmodel, rbc_count,bu,sod,pc,dm,age,sg,pe,bp)
{  
  new_sample <- create_input_matrix(rbc_count,bu,sod,pc,dm,age,sg,pe,bp)
  prediction <- predict(svm_rbf_smallmodel, newdata=new_sample, type = "prob")
  return(prediction$ckd)
}

# To deploy app
# setwd("/Users/shruti/Dropbox/SHRUTIM/Rscripts/MachineLearning/KidneyDiseasePrediction/KidneyDiseasePredictor_SVM/app")
# library(shinyapps)
# authorized to your accountby going to https://www.shinyapps.io/admin/#/tokens. click show 
# setAccountInfo(name='shruti-marwaha', token='5CE2F264C65E9C22C7812E2C0BB7467E', secret='IFvw73TseKuNan+P3bbEw4mRJUYszuPZRpZQW4g4')
# deployApp(appName="KidneyDiseasePredictor")
