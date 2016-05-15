library(caret)
library(ggplot2)
library(lubridate) # to extract date, month etc
library(ggmap) # to get zipcode from longitude and latitude

################################# Data Munging #################################
# load data
setwd("/Users/shruti/Desktop/WorkMiscellaneous/MachineLearning/SanFranciscoCrime/")
train_data <- read.csv("./train.csv") 
test_data <- read.csv("./test.csv") 

# data exploration
dim(train_data)
head(train_data)
summary(train_data)
levels(train_data$Category)
table(train_data$Category)
sum(complete.cases(train_data))

dim(test_data)
head(test_data)

# Feature Extraction
FeatureExtraction <- function(dataset,zip){
  year_of_crime <- as.factor(year(dataset$Dates))
  month_of_crime <- as.factor(month(dataset$Dates))
  date_of_crime <- as.factor(day(dataset$Dates))
  hour_of_crime <- as.factor(hour(dataset$Dates))
  new_dataframe <- data.frame(cbind(hour_of_crime,date_of_crime,month_of_crime,year_of_crime,droplevels(dataset[,c("DayOfWeek","PdDistrict","X","Y")])))
  
  return(new_dataframe)
}

# Feature engineering: get zip code from latitude and longitude
# install.packages("zipcode")
library(zipcode)
data(zipcode)
sf_zipcodes <- subset(zipcode,city=="San Francisco",)
sf_zipcodes$latitude <- as.numeric(format(sf_zipcodes$latitude,digits=8))
sf_zipcodes$longitude <- as.numeric(format(sf_zipcodes$longitude,digits=8))
dim(sf_zipcodes)
head(sf_zipcodes)

# function to convert latitude and longitude to zipcode
closest_zipcode <- function(latitude_longitude_matrix)   
# latitude_longitude_matrix : input matrix containing latitude and longitude from training or test set
{
  # find euclidean distance between user provided latitude and longitude and 
  # all latitudes, longitudes from sf_zipcodes and select the row from latter 
  # which has minimum distance. then extract its zip code
  return(sf_zipcodes[which.min (apply (sf_zipcodes[,c("latitude","longitude")],1,function(i) {
    dist( rbind(i,latitude_longitude_matrix) ) #euclidean distance
    } ) ),"zip"])
}

# Sanity Check: to validate some of the zipcodes with google api
train_data[1,]
closest_zipcode(c(train_data$Y[1],train_data$X[1]))
revgeocode(c(train_data$X[1], train_data$Y[1]))

zip_codes_training <- apply(train_data[,c("Y","X")],1,closest_zipcode)
save(zip_codes_training,file="zip_codes_training.rda")
#load("./zip_codes_training.rda")

zip_codes_test <- apply(test_data[,c("Y","X")],1,closest_zipcode)
save(zip_codes_test,file="zip_codes_test.rda")
#load("./zip_codes_test.rda")

training_features <- cbind( FeatureExtraction(train_data),zip=zip_codes_training, Category=train_data$Category )
test_features <- FeatureExtraction(test_data,zip=zip_codes_test)
# save(training_features,file="./training_features.rda")
# save(test_features,file="./test_features.rda")
sapply(training_features[1,],class)

################################# Modeling #################################
# TO DO: create dummy variables

# Data Partition
set.seed(1)
training_indices <- createDataPartition(y=training_features$Category,p=0.6,list=F)
training_set <- training_features[training_indices,]
test_set <- training_features[-training_indices,]
# dim(training_set); dim(test_set)
write.table(training_set,file="./training_set.txt")

# k-fold cross validation
train_control <- trainControl(method="cv", number=3, savePredictions = T)

set.seed(1)
svm_lm_model <- train(Category ~ ., data = training_set, trControl=train_control, method = "svmLinear")
varImp(svm_lm_model)
plot(varImp(svm_lm_model))

variable_imp <- varImp(svm_lm_model)$importance
apply(variable_imp,1,median)

set.seed(1)
svm_lm_model2 <- train(Category ~ year_of_crime + hour_of_crime + zip_codes + PdDistrict + DayOfWeek + date_of_crime, data = training_set, trControl=train_control, method = "svmLinear")

set.seed(1)
rpart_model <- train(Category ~ ., data = training_set, trControl=train_control, method="rpart" ,preProcess=c("scale","center"))
# plot classification trees
fancyRpartPlot(rpart_model$finalModel)

# random forest
set.seed(1)
rf_model <- train(Category ~ .,data = training_set, trControl=train_control, method = "rf",preProcess = c("center", "scale"),prox=T)

# boosting with tres
set.seed(1)
gbm_model <- train(Category ~ .,data = training_set, trControl=train_control, method = "gbm", preProcess = c("center", "scale"), verbose=F)

# evaluate on test set
test_pred_svm_lm <- predict(svm_lm_model, newdata=test_set[,-10])
confusionMatrix(data=test_pred_svm_lm, test_set$Category)

test_pred_svm_lm2 <- predict(svm_lm_model2, newdata=test_set[,-10])
confusionMatrix(data=test_pred_svm_lm2, test_set$Category)

test_pred_rpart <- predict(rpart_model, newdata=test_set[,-10])
confusionMatrix(data=test_pred_rpart, test_set$Category)

######################
# work on smaller data
set.seed(1)
sample_indices <- sample(1:nrow(training_features),10000)
sample_data <- droplevels(training_features[sample_indices,])
summary(sample_data)
head(sample_data)
sort(table(sample_data$Category))

y <- as.data.frame(sort(table(sample_data$Category)))
colnames(y) <- "crime"
rownames(y[y$crime<10,])
sample_data2 <- droplevels(subset(sample_data,!(Category=="PORNOGRAPHY/OBSCENE MAT" | Category=="EXTORTION" | Category=="GAMBLING" | Category=="SEX OFFENSES NON FORCIBLE"  | Category=="FAMILY OFFENSES"  | Category=="BAD CHECKS"), ))
sort(table(sample_data2$Category))

set.seed(1)
training_indices <- createDataPartition(y=sample_data2$Category,p=0.6,list=F)
training_set <- sample_data2[training_indices,]
test_set <- sample_data2[-training_indices,]

