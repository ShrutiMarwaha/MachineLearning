
# function to convert users input from app to matrix that can be fed to machine learning model
create_input_matrix <- function(rbc_count,bu,sod,pc,dm,age,sg,pe,wbc_count,cad,ba,bp) {
  input_df <- data.frame(matrix(data=NA,nrow=1,ncol=length(svm_imp_features2),dimnames=list(c(),svm_imp_features2 )))
  
  input_df$age <- age
  input_df$rbc.c <- rbc_count
  input_df$bu <- bu
  input_df$sod <- sod
  input_df$wbcc <- wbc_count
  input_df$bp <- bp
  #input_df$dm.no <- as.numeric(!dm) # dm.no=1 means diabetes == FLASE
  #input_df$pc.abnormal <- as.numeric(pc)  # pc.abnormal=1 menas Abnormal Pus Cells == TRUE
  #input_df$pe.no <- as.numeric(!pe) # pe.no=1 means Pedal Edema == FLASE
  if(pc == FALSE)
  {
    input_df$pc.abnormal <- 0
  }
  else
  {
    input_df$pc.abnormal <- 1
  }
  
  if(dm == FALSE)
  {
    input_df$dm.no <- 1
  }
  else
  {
    input_df$dm.no <- 0
  }
  
  if(pe == FALSE)
  {
    input_df$pe.no <- 1
  }
  else
  {
    input_df$pe.no <- 0
  }
  
  if(cad == FALSE)
  {
    input_df$cad.no <- 1
  }
  else
  {
    input_df$cad.no <- 0
  }
  
  if(ba == FALSE)
  {
    input_df$ba.notpresent <- 1
  }
  else
  {
    input_df$ba.notpresent <- 0
  }
  
  if(sg==1.010)
  {
    input_df$sg.1.010 <- 1
    #input_df$sg.1.015 <- 0
    input_df$sg.1.020 <- 0
    input_df$sg.1.025 <- 0
  }
  else if(sg==1.015)
  {
    input_df$sg.1.010 <- 0
    #input_df$sg.1.015 <- 1
    input_df$sg.1.020 <- 0
    input_df$sg.1.025 <- 0
  }
  else if(sg==1.020)
  {
    input_df$sg.1.010 <- 0
    #input_df$sg.1.015 <- 0
    input_df$sg.1.020 <- 1
    input_df$sg.1.025 <- 0
  }
  else if(sg==1.025)
  {
    input_df$sg.1.010 <- 0
    #input_df$sg.1.015 <- 0
    input_df$sg.1.020 <- 0
    input_df$sg.1.025 <- 1
  }
  
  return(input_df)
}

# function to predict outcome using svm model and above user's input data
chronicKidneyDiseaseRisk <- function(rbc_count,bu,sod,pc,dm,age,sg,pe,wbc_count,cad,ba,bp)
{
  new_sample <- create_input_matrix(rbc_count,bu,sod,pc,dm,age,sg,pe,wbc_count,cad,ba,bp)
  prediction <- predict(svm_rbf_smallmodel, newdata=new_sample, type = "prob")
  # 0 implies ckd; 1 implies notckd; convert them into true and false respectively
  #prediction <- (!as.logical (as.numeric (as.character(prediction)) ) )
  return(prediction$ckd)
}