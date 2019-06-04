#### 00. INCLUDES ####

#Load Libraries: p_load can install, load,  and update packages
if(require("pacman")=="FALSE"){
  install.packages("pacman")
}

pacman::p_load(rstudioapi,readr,dplyr,tidyr,ggplot2,caret,randomForest,e1071,reshape2,h2o,plotly,class,ggpubr)

# theme_pubr()

# install.packages("doParallel")
# library(doParallel)
# detectCores()
# cluster <- makeCluster(detectCores() - 1)
# registerDoParallel(cluster)
# fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, allowParallel = TRUE)


#Setting my Plotly API
# Sys.setenv("plotly_username"="kikusanchez")
# Sys.setenv("plotly_api_key"="Uky3F2ELrykJSTQHGBiP")
# library(plotly)
# packageVersion('plotly')


# Setwd (1º current wd where is the script, then we move back to the
# general folder)
current_path = getActiveDocumentContext()$path
setwd(dirname(current_path))
setwd("..")
rm(current_path)


#### 01. PRE-PROCESS ####
#### 01. 1 Training ####

# Import Data
Training <- read.csv(file="../Task3.IoT_wifi_locationing/datasets/trainingData.csv", header = TRUE)

# Extract WAPS columns
WAPS <- grep("WAP", colnames(Training), value = TRUE)
no_WAPS <- grep("WAP", colnames(Training), value = TRUE, invert = TRUE)

# Remove Feature with var = 0
f <- Training[,apply(Training[WAPS], 2, function(x) {var(x) != 0})]
f <- f[,grep("WAP", colnames(f), value = TRUE)]
Training <- cbind(f, Training[,no_WAPS])

# Remove row mean = 100
Training <- Training[apply(Training[grep("WAP", colnames(Training), value = TRUE)], 1, function(x){mean(x) < 100}),]

# Remove dupplicate rows
Training <- distinct(Training)
WAPS <- grep("WAP", colnames(Training), value = TRUE)

# Replace x <= 0 & x > -30 & x == 100 by -105
Training <- cbind(as.data.frame(apply(Training[WAPS], 2, function(x){ifelse(x <= 0 & x > -30 | x == 100 | x < -90, -105, x)})), Training[no_WAPS])

Building2 <- Training[Training$BUILDINGID == 2,]
Training<-Training[!Training$BUILDINGID == 2,]
Building2 <- cbind(as.data.frame(apply(Building2[WAPS], 2, function(x){ifelse(x < -80, -105, x)})), Building2[no_WAPS])
Training <- rbind(Training, Building2)

# # New feature for best value
# Training$MAX_W <- colnames(Training)[apply(Training[,grep("WAP", colnames(Training))],1,which.max)]

# Remove waps which appears in the three buildings
Training$WAP248 <- NULL
Training$WAP362 <- NULL
Training$WAP413 <- NULL

# Remove phone 19 (don't feed the troll)
Training <- Training[!(Training$PHONEID==19),]


#### 01. 2. Testing ####

# Import Data
Testing <- read.csv(file="../Task3.IoT_wifi_locationing/datasets/validationData.csv", header = TRUE)

# Replace outliers values by -105
Testing <- cbind(as.data.frame(apply(Testing[,grep("WAP", names(Testing), value = TRUE)], 2, function(x){ifelse(x <= 0 & x > -30 | x == 100 | x < -90, -105, x)})), Testing[,grep("WAP", names(Testing), value = TRUE, invert = TRUE)])

# Extract features names of training
Names <- colnames(Training)

# Keep only thoose feature in testing
Testing <- Testing[,grep(paste(Names, collapse="|"), names(Testing), value=TRUE)]

# Clean environment
rm(list=setdiff(ls(), c("Training", "Testing", "WAPS")))

# #Saving pre-processed training and testing
# write.csv(Training, file = "../Task3.IoT_wifi_locationing/datasets/01.training_preprocessed.csv", row.names=FALSE)
# write.csv(Testing, file = "../Task3.IoT_wifi_locationing/datasets/01.testing_preprocessed.csv", row.names=FALSE)


#### 02. MODELLING ####
#### 02. 1. Building ####

# #Loading training and testing ready for building prediction
# Training <- read.csv("../datasets/01.training_preprocessed.csv", na = c("N/A"))
# Testing <- read.csv("../datasets/01.testing_preprocessed.csv", na = c("N/A"))

# converting datasets into h2o environments and checking the names of the features
# train.h2o <- as.h2o(Training)
test.h2o <- as.h2o(Testing)
# names(train.h2o)
# names(test.h2o)

# # dependent variable as factor (BUILDINGID)
# train.h2o[,466] <- as.factor(train.h2o[,466])

# # setting dependent variable and independent ones
# y.dep <- 466
# x.indep <- c(1:462)

# # running the model random forest
# h2o_rf_model_building <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = train.h2o, ntrees = 5000, max_depth = 10, seed = 123)

# LOAD THE rf MODEL
h2o.init()
h2o_rf_model_building <- h2o.loadModel(path=paste0(getwd(),"/","models", "/", "h2o_rf_model_building"))
h2o_rf_model_building

# results of my model rf in training dataset
h2o.performance(h2o_rf_model_building)
h2o.varimp(h2o_rf_model_building)

# aplying the rf model to the validation set
Testing$BUILDINGID <- as.factor(Testing$BUILDINGID)
system.time(pred_rf_model_building <- as.data.frame(h2o.predict(h2o_rf_model_building, test.h2o)))
pred_rf_model_building$predict
postResample(pred_rf_model_building, Testing$BUILDINGID) # Accuracy 1 | Kappa 1

# confusion matrix of rf
table(pred_rf_model_building$predict, Testing$BUILDINGID)
cfm_rf_building_h2o <- table(pred_rf_model_building$predict, Testing$BUILDINGID)


# # gbt model
# system.time(
#   gbm_model_building <- h2o.gbm(y=y.dep, x=x.indep, training_frame = train.h2o, ntrees = 1000, max_depth = 4, learn_rate = 0.01, seed = 1122))

# # results of my gbt model in training dataset
# h2o.performance(gbm_model_building)
# h2o.varimp(gbm_model_building)

# # aplying the gbt model to the validation set
# Testing$BUILDINGID <- as.factor(Testing$BUILDINGID)
# system.time(pred_gbm_model_building <- as.data.frame(h2o.predict(gbm.model, test.h2o)))
# pred_gbm_model_building$predict
# postResample(pred_gbm_model_building, Testing$BUILDINGID)


# # confusion matrix of gbt model
# table(pred_gbm_model_building$predict, Testing$BUILDINGID)
# cfm_gbm_model_building_h2o <- table(pred_gbm_model_building$predict, Testing$BUILDINGID)


# #save the best model as a h2o model
# h2o.saveModel(object=h2o_rf_model_building, path=paste0(getwd(),"/", "models"), force=TRUE)
#(this will save it as the default name, rename it manually on the folder to loaded after)


# # Save results
# write.csv(Testing, file = "../datasets/02.testing_building_pred_ok.csv", row.names=FALSE)
# write.csv(Training, file = "../datasets/02.training_building_pred_ok.csv", row.names=FALSE)

# # Clean all the environment but Training and Testing
# rm(list=setdiff(ls(), c("Training", "Testing")))

# # Remove Phone 17
# #Training <- Training[!(Training$USERID == 17 & Training$BUILDINGID == 2),]

# BUILDING <- feature.selection(Training, Testing, features)
# 
# # Building prediction
# building.prediction <- function(Training, Testing) {
#   
#   Training$BUILDINGID <- as.factor(Training$BUILDINGID)
# 
#   Final <- list()
#   Predictions <- list()
#   Metrics <- list()
#   
# #  Mtry <- tuneRF(Training[,grep("WAP", colnames(Training), value = TRUE)], Training$BUILDINGID, ntreeTry=100, stepFactor=2, improve=0.05, trace=TRUE, plot=T)
#   build_rf <- randomForest(y = Training$BUILDINGID, x = Training[grep("WAP", names(Training), value = T)], importance=T, method="rf")
#   build_pred_rf <- predict(build_rf, Testing[,grep("WAP", names(Testing), value = T)])
#   build_metrics_rf <- postResample(Testing$BUILDINGID, build_pred_rf)
#   
#   Predictions[["RF"]] <- build_pred_rf
#   Metrics[["RF"]] <- build_metrics_rf
# 
#   build_svm <- svm(y = Training$BUILDINGID, x=Training[grep("WAP", names(Training), value = T)])
#   build_pred_svm <- predict(build_svm, Testing[,grep("WAP", names(Testing), value = T)])
#   build_metrics_svm <- postResample(Testing$BUILDINGID, build_pred_svm)
#    
#   Predictions[["SVM"]] <- build_pred_svm
#   Metrics[["SVM"]] <- build_metrics_svm
#    
#   build_knn <- knn3(Training$BUILDINGID ~ ., data = Training)
#   build_pred_knn <- predict(build_knn, Testing, type = "class")
#   build_metrics_knn <- postResample(Testing$BUILDINGID, build_pred_knn)
#    
#   Predictions[["KNN"]] <- build_pred_knn
#   Metrics[["KNN"]] <- build_metrics_knn
#   
#   Final[["Predictions"]] <- Predictions
#   Final[["Metrics"]] <- Metrics
#   
#   Final
#   
# }
# 
# Prediction_B <- building.prediction(BUILDING$train, BUILDING$test) 
# 
# # Plot metrics
# Models <- c("RF", "SVM", "KNN")
# for (j in Models) {
#   print(paste("        -", j, "-     "))
#   print(Prediction_B$Metrics[[j]])
# }
# 
# # Apply RF Kappa 0.9957
# Testing$BUILD_PRED <- Prediction_B$Predictions$RF




#### 02. 2. Floors ####

# Apply BUILDING prediction to the data Accuracy 1 Kappa 1
Testing$BUILD_PRED <- pred_rf_model_building$predict

# Keeping interesting features
features <- c("WAP", "BUILDINGID")
BUILDING <- c()
feature.selection <- function(training, testing, features){
  
  data <- list()
  Keep <- grep(paste(features,collapse="|"), colnames(training), value = TRUE)
  
  data[["train"]] <- training[Keep]
  data[["test"]] <- testing[Keep]
  
  data
}

# Split Data between buildings for predict floor by building
b <- c(0, 1, 2)
Data_split <- list()
for (i in b) {
  Data_split[["train"]][[paste("B", i)]] <- Training %>% filter(BUILDINGID == i)
  Data_split[["test"]][[paste("B", i)]] <- Testing %>% filter(BUILD_PRED == i)
} # Data_split

# CLEAN DATA (removing "no wap" columns, vaianze = 0 and mean = -105, for training and testing datasets)
Clean.training <- function(data) {
  no_WAPS <- grep("WAP", colnames(data), value = TRUE, invert = TRUE)
  f <- data[,apply(data[grep("WAP", colnames(data), value = TRUE)], 2, function(x) {var(x) != 0})]
  f <- f[,grep("WAP", colnames(f), value = TRUE)]
  data <- cbind(f, data[,no_WAPS])
  data <- data[apply(data[grep("WAP", colnames(data), value = TRUE)], 1, function(x){mean(x) != -105}),]
  data
}
Clean.testing <- function(training, testing){
  WAPS <- grep("WAP", colnames(training), value = TRUE)
  no_WAPS <- grep("WAP", colnames(testing), value = TRUE, invert = TRUE)
  toMatch <- c(WAPS, no_WAPS)
  testing <- testing[,grep(paste(toMatch, collapse="|"), names(testing), value=TRUE)]
  testing
}

# and by building
Names <- names(Data_split$train)
Data_clean <- list()
list <- list()

for (i in Names) {
  train <- Clean.training(Data_split$train[[i]])
  test <- Clean.testing(train, Data_split$test[[i]])
  list[["train"]] <- train
  list[["test"]] <- test
  Data_clean[[i]] <- list
} # Data_clean


# Select important features for floor prediction
features <- c("WAP", "FLOOR")
FLOOR <- c()
x <- c()
for (i in Names) {
  x <- feature.selection(Data_clean[[i]][["train"]], Data_clean[[i]][["test"]], features)
  FLOOR[[i]] <- x
}


#### 02. 2. 1. Building 0 ####

# train.h2o <- as.h2o(Data_clean[["B 0"]][["train"]])
test.h2o <- as.h2o(Data_clean[["B 0"]][["test"]])
# names(train.h2o)
# train.h2o[,164] <- as.factor(train.h2o[,164]) # dependent variable as factor (FLOOR)
# y.dep <- 164
# x.indep <- c(1:161)

# #random forest
# rforest.model_floor_b0 <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = train.h2o, ntrees = 5000, max_depth = 10, seed = 123)
# h2o.performance(rforest.model_floor_b0)
# h2o.varimp(rforest.model_floor_b0)

# Data_clean[["B 0"]][["test"]]$FLOOR <- as.factor(Data_clean[["B 0"]][["test"]]$FLOOR)
# system.time(predict.rforest_floor_b0 <- as.data.frame(h2o.predict(rforest.model_floor_b0, test.h2o)))
# predict.rforest_floor_b0
# postResample(predict.rforest_floor_b0, Data_clean[["B 0"]][["test"]]$FLOOR)

# Accuracy     Kappa 
# 0.9626866 0.9475510

# confusion matrix
# table(predict.rforest_floor_b0$predict, Data_clean[["B 0"]][["test"]]$FLOOR)
# cfm_rf_model_floor_b0_h2o <- table(predict.rforest_floor_b0$predict, Data_clean[["B 0"]][["test"]]$FLOOR)


#running gradient boosted trees
# system.time(gbm_model_floor_b0 <- 
#               h2o.gbm(y=y.dep, x=x.indep, training_frame = train.h2o, ntrees = 1000, max_depth = 4, learn_rate = 0.01, seed = 1122))

# LOAD THE MODEL
gbm_model_floor_b0 <- h2o.loadModel(path=paste0(getwd(),"/","models", "/", "gbm_model_floor_b0"))
gbm_model_floor_b0

h2o.performance(gbm_model_floor_b0)
h2o.varimp(gbm_model_floor_b0)

# applying the model to testing set
Data_clean[["B 0"]][["test"]]$FLOOR <- as.factor(Data_clean[["B 0"]][["test"]]$FLOOR)
system.time(pred_gbm_floor_b0 <- as.data.frame(h2o.predict(gbm_model_floor_b0, test.h2o)))
pred_gbm_floor_b0
postResample(pred_gbm_floor_b0, Data_clean[["B 0"]][["test"]]$FLOOR)

# Accuracy     Kappa 
# 0.9664179 0.9525992

# confusion matrix
table(pred_gbm_floor_b0$predict, Data_clean[["B 0"]][["test"]]$FLOOR)
cfm_rf_model_floor_b0_h2o <- table(pred_gbm_floor_b0$predict, Data_clean[["B 0"]][["test"]]$FLOOR)


# save the best model (GBT)
# h2o.saveModel(object=gbm_model_floor_b0, path=paste0(getwd(),"/", "models"), force=TRUE)
#(this will save it as the default name, rename it manually on the folder to loaded after)


#### 02. 2. 2. Building 1 ####

# train.h2o <- as.h2o(Data_clean[["B 1"]][["train"]])
# test.h2o <- as.h2o(Data_clean[["B 1"]][["test"]])
# names(train.h2o)
# train.h2o[,182] <- as.factor(train.h2o[,182]) # dependent variable as factor (FLOOR)
# y.dep <- 182
# x.indep <- c(1:179)

# running random forest
# rforest.model_floor_b1 <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = train.h2o, ntrees = 5000, max_depth = 10, seed = 123)

# LOAD THE MODEL
rforest.model_floor_b1 <- h2o.loadModel(path=paste0(getwd(),"/","models", "/", "rforest.model_floor_b1"))
rforest.model_floor_b1

h2o.performance(rforest.model_floor_b1)
h2o.varimp(rforest.model_floor_b1)

#applying results to the test set
Data_clean[["B 1"]][["test"]]$FLOOR <- as.factor(Data_clean[["B 1"]][["test"]]$FLOOR)
system.time(predict.rforest_floor_b1 <- as.data.frame(h2o.predict(rforest.model_floor_b1, test.h2o)))
predict.rforest_floor_b1
postResample(predict.rforest_floor_b1, Data_clean[["B 1"]][["test"]]$FLOOR)

# Accuracy     Kappa 
# 0.8338983 0.7589990

# confusion matrix
table(predict.rforest_floor_b1$predict, Data_clean[["B 1"]][["test"]]$FLOOR)
cfm_rf_model_floor_b1_h2o <- table(predict.rforest_floor_b1$predict, Data_clean[["B 1"]][["test"]]$FLOOR)


#gradient boosted trees
# system.time(gbm_model_floor_b1 <- 
#               h2o.gbm(y=y.dep, x=x.indep, training_frame = train.h2o, ntrees = 1000, max_depth = 4, learn_rate = 0.01, seed = 1122))

# h2o.performance(gbm_model_floor_b1)
# h2o.varimp(gbm_model_floor_b1)

# applying results to the test set
# Data_clean[["B 1"]][["test"]]$FLOOR <- as.factor(Data_clean[["B 1"]][["test"]]$FLOOR)
# system.time(pred_gbm_floor_b1 <- as.data.frame(h2o.predict(gbm_model_floor_b1, test.h2o)))
# pred_gbm_floor_b1
# postResample(pred_gbm_floor_b1, Data_clean[["B 1"]][["test"]]$FLOOR)

# Accuracy     Kappa 
# 0.7694915 0.6669157

# save the best model (RF)
# h2o.saveModel(object=rforest.model_floor_b1, path=paste0(getwd(),"/", "models"), force=TRUE)
#(this will save it as the default name, rename it manually on the folder to loaded after)


#### 02. 2. 3. Building 2 ####

# train.h2o <- as.h2o(Data_clean[["B 2"]][["train"]])
# test.h2o <- as.h2o(Data_clean[["B 2"]][["test"]])
# names(train.h2o)
# train.h2o[,111] <- as.factor(train.h2o[,111]) # dependent variable as factor (FLOOR)
# y.dep <- 111
# x.indep <- c(1:108)

# #running random forest
# rf_model_floor_b2 <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = train.h2o, ntrees = 5000, max_depth = 10, seed = 123)
# h2o.performance(rf_model_floor_b2)
# h2o.varimp(rf_model_floor_b2)

# #applying to the test set
# Data_clean[["B 2"]][["test"]]$FLOOR <- as.factor(Data_clean[["B 2"]][["test"]]$FLOOR)
# system.time(pred_rf_floor_b2 <- as.data.frame(h2o.predict(rf_model_floor_b2, test.h2o)))
# pred_rf_floor_b2
# postResample(pred_rf_floor_b2, Data_clean[["B 2"]][["test"]]$FLOOR)

# #Accuracy     Kappa 
# #0.7929825 0.7183889 

# #gradient boosted trees
# system.time(gbm_model_floor_b2 <- 
#               h2o.gbm(y=y.dep, x=x.indep, training_frame = train.h2o, ntrees = 1000, max_depth = 4, learn_rate = 0.01, seed = 1122))

# # LOAD THE MODEL
# gbm_model_floor_b2 <- h2o.loadModel(path=paste0(getwd(),"/","models", "/", "gbm_model_floor_b2"))
# rforest.model_floor_b1

# h2o.performance(gbm_model_floor_b2)
# h2o.varimp(gbm_model_floor_b2)

# # applying results to the test set
# Data_clean[["B 2"]][["test"]]$FLOOR <- as.factor(Data_clean[["B 2"]][["test"]]$FLOOR)
# system.time(pred_gbm_floor_b2 <- as.data.frame(h2o.predict(gbm_model_floor_b2, test.h2o)))
# pred_gbm_floor_b2
# postResample(pred_gbm_floor_b2, Data_clean[["B 2"]][["test"]]$FLOOR)

# #Accuracy     Kappa 
# #0.8421053 0.7858431

# # confusion matrix
# table(pred_gbm_floor_b2$predict, Data_clean[["B 2"]][["test"]]$FLOOR)
# cfm_gbm_model_floor_b2_h2o <- table(pred_gbm_floor_b2$predict, Data_clean[["B 2"]][["test"]]$FLOOR)

# #save the best model (RF)
# h2o.saveModel(object=gbm_model_floor_b2, path=paste0(getwd(),"/", "models"), force=TRUE)
#(this will save it as the default name, rename it manually on the folder to loaded after)

FLOOR$`B 2`$train
FLOOR$`B 2`$test

train_floor_b2 <- as.data.frame(FLOOR$`B 2`$train)
train_floor_b2$FLOOR <- as.factor(train_floor_b2$FLOOR)
test_floor_b2 <- as.data.frame(FLOOR$`B 2`$test)
test_floor_b2$FLOOR <- as.factor(test_floor_b2$FLOOR)


floor_b2_knn <- knn3(train_floor_b2$FLOOR ~ ., data = train_floor_b2)

floor_pred_knn <- predict(floor_b2_knn, test_floor_b2[,grep("WAP", colnames(test_floor_b2), value = TRUE)], type = "class")
floor_pred_knn
floor_metrics_knn <- postResample(test_floor_b2$FLOOR, floor_pred_knn)
floor_metrics_knn

# # Predict floor function - building 2
# floor.prediction <- function(training, testing) {
#   
#   prediction <- list()
#   metrics <- list()
#   models <- list()
#   final <- c()
#   
#   training$FLOOR <- as.factor(training$FLOOR)
  
  # Mtry <- tuneRF(training[,grep("WAP", colnames(training), value = TRUE)], training$FLOOR, ntreeTry=100, stepFactor=2, improve=0.05, trace=TRUE, plot=T)
  # floor_rf <- randomForest(y = training$FLOOR, x = training[,grep("WAP", names(training), value = T)], importance=T, method="rf")
  # floor_pred_rf <- predict(floor_rf, testing)
  # floor_metrics_rf <- postResample(testing$FLOOR, floor_pred_rf)
  # 
  # prediction[["RF"]] <- floor_pred_rf
  # metrics[["RF"]] <- floor_metrics_rf
#  models[["RF"]] <- floor_rf
  
  # floor_svm <- svm(y = training$FLOOR, x=training[grep("WAP", names(training), value = T)])
  # floor_pred_svm <- predict(floor_svm, testing[grep("WAP", names(testing), value = T)])
  # floor_metrics_svm <- postResample(testing$FLOOR, floor_pred_svm)
  # 
  # prediction[["SVM"]] <- floor_pred_svm
  # metrics[["SVM"]] <- floor_metrics_svm
#  models[["SVM"]] <- floor_svm
  
  # floor_knn <- knn3(training$FLOOR ~ ., data = training)
  # floor_pred_knn <- predict(floor_knn, testing[grep("WAP", names(testing), value = T)], type = "class")
  # floor_metrics_knn <- postResample(testing$FLOOR, floor_pred_knn)
  
  # prediction[["KNN"]] <- floor_pred_knn
  # metrics[["KNN"]] <- floor_metrics_knn
#  models[["KNN"]] <- floor_knn
  
  # final[["Predictions"]] <- prediction
  # final[["Metrics"]] <- metrics
#  final[["Models"]] <- models
  
  # final
  
# }

# Prediction_F <- list()
# v <- list()
# for (i in Names) {
#   v <- floor.prediction(FLOOR[[i]][["train"]], FLOOR[[i]][["test"]])
#   Prediction_F[[i]] <- v
# }

# PLOT METRICS
# Models <- c("RF", "SVM", "KNN")
# for (i in Names) {
#   print(paste("===== PREDICTION", i, "====="))
#   for (j in Models) {
#     print(paste("        -", j, "-     "))
#     print(Prediction_F[[i]][["Metrics"]][[j]])
#   }
# }

# Adding floor predictions
Data_clean$`B 0`$test$FLOOR_PRED <- Prediction_F$`B 0`$Predictions$KNN # KAPPA KNN 0.963
Data_clean$`B 1`$test$FLOOR_PRED <- Prediction_F$`B 1`$Predictions$SVM # KAPPA SVM 0.723
Data_clean$`B 2`$test$FLOOR_PRED <- Prediction_F$`B 2`$Predictions$KNN # KAPPA KNN 0.83


#### 02. 3. Latitude ####

#Names <- names(Buildings_data)
Features <- c("WAP", "LATITUDE")
LATTITUDE <- list()
v <- list()
for (i in Names) {
  v <- feature.selection(Data_clean[[i]][["train"]], Data_clean[[i]][["test"]], Features)
  LATTITUDE[[i]] <- v
}

latt.prediction <- function(training, testing) {
  
  prediction <- list()
  metrics <- list()
  models <- list()
  Final <- c()
#  Mtry <- tuneRF(training[grep("WAP", names(training), value = T)], training$LATITUDE, ntreeTry=100, stepFactor=2, improve=0.05, trace=TRUE, plot=T)  
   # lat_rf <- randomForest(y = training$LATITUDE, x = training[grep("WAP", names(training), value = T)], importance=T, method="rf")
   # 
   # lat_pred_rf <- predict(lat_rf, testing)
   # lat_metrics_rf <- postResample(testing$LATITUDE, lat_pred_rf)
   # 
   # prediction[["RF"]] <- lat_pred_rf
   # metrics[["RF"]] <- lat_metrics_rf
   # models[["RF"]] <- lat_rf
   # 
   # lat_svm <- svm(y = training$LATITUDE, x=training[grep("WAP", names(training), value = T)])
   # lat_pred_svm <- predict(lat_svm, testing[,grep("WAP", names(testing), value = T)])
   # lat_metrics_svm <- postResample(testing$LATITUDE, lat_pred_svm) 
   # 
   # prediction[["SVM"]] <- lat_pred_svm
   # metrics[["SVM"]] <- lat_metrics_svm
   # models[["SVM"]] <- lat_svm
  
  lat_knn <- knnreg(training$LATITUDE ~ ., data = training)
  lat_pred_knn <- predict(lat_knn, testing)
  lat_metrics_knn <- postResample(testing$LATITUDE, lat_pred_knn)
  
  prediction[["KNN"]] <- lat_pred_knn
  metrics[["KNN"]] <- lat_metrics_knn
#  models[["KNN"]] <- lat_knn
  
  Final[["Predictions"]] <- prediction
  Final[["Metrics"]] <- metrics
#  Final[["Models"]] <- models
  
  Final
  
}

Prediction_Latt <- list()
v <- list()
for (i in Names) {
  v <- latt.prediction(LATTITUDE[[i]][["train"]], LATTITUDE[[i]][["test"]])
  Prediction_Latt[[i]] <- v
}

# CHECK METRICS
Models <- c("RF", "SVM", "KNN")
for (i in Names) {
  print(paste("===== PREDICTION", i, "====="))
  for (j in Models) {
    print(paste("        -", j, "-     "))
    print(Prediction_Latt[[i]][["Metrics"]][[j]])
  }
}

# ADD LATTITUDE PREDICTION
Data_clean$`B 0`$test$LATT_PRED <- Prediction_Latt$`B 0`$Predictions$KNN # KNN 3.36
Data_clean$`B 1`$test$LATT_PRED <- Prediction_Latt$`B 1`$Predictions$KNN # KNN 6.67
Data_clean$`B 2`$test$LATT_PRED <- Prediction_Latt$`B 2`$Predictions$KNN # KNN 11.41


#### 02. 4. Longitude ####

Features <- c("WAP", "LONG")
LONGITUDE <- list()
v <- list()
for (i in Names) {
  v <- feature.selection(Data_clean[[i]][["train"]], Data_clean[[i]][["test"]], Features)
  LONGITUDE[[i]] <- v
}

long.prediction <- function(training, testing) {
  
  prediction <- list()
  metrics <- list()
#  models <- list()
  Final <- c()
   
#    long_rf <- randomForest(y = training$LONGITUDE, x = training[grep("WAP", names(training), value = T)], importance=T, method="rf")
#    long_pred_rf <- predict(long_rf, testing)
#    long_metrics_rf <- postResample(testing$LONGITUDE, long_pred_rf)
#    
#    prediction[["RF"]] <- long_pred_rf
#    metrics[["RF"]] <- long_metrics_rf
# #  models[["RF"]] <- long_rf
#   
#    long_svm <- svm(y = training$LONGITUDE, x=training[grep("WAP", names(training), value = T)])
#    long_pred_svm <- predict(long_svm, testing[,grep("WAP", names(testing), value = T)])
#    long_metrics_svm <- postResample(testing$LONGITUDE, long_pred_svm) 
#    
#    prediction[["SVM"]] <- long_pred_svm
#    metrics[["SVM"]] <- long_metrics_svm
# #  models[["SVM"]] <- long_svm
#    
  long_knn <- knnreg(training$LONGITUDE ~ ., data = training)
  long_pred_knn <- predict(long_knn, testing)
  long_metrics_knn <- postResample(testing$LONGITUDE, long_pred_knn)

  prediction[["KNN"]] <- long_pred_knn
  metrics[["KNN"]] <- long_metrics_knn
#  models[["KNN"]] <- long_knn

  Final[["Predictions"]] <- prediction
  Final[["Metrics"]] <- metrics
#  Final[["Models"]] <- models
  
  Final
  
}

Prediction_Long <- list()
v <- list()
for (i in Names) {
  v <- long.prediction(LONGITUDE[[i]][["train"]], LONGITUDE[[i]][["test"]])
  Prediction_Long[[i]] <- v
}

# PLOT METRICS
Models <- c("RF", "SVM", "KNN")
for (i in Names) {
  print(paste("===== PREDICTION", i, "====="))
  for (j in Models) {
    print(paste("        -", j, "-     "))
    print(Prediction_Long[[i]][["Metrics"]][[j]])
  }
}

# ADD LONGITUDE PREDICTION
Data_clean$`B 0`$test$LONG_PRED <- Prediction_Long$`B 0`$Predictions$KNN # KNN 3.85
Data_clean$`B 1`$test$LONG_PRED <- Prediction_Long$`B 1`$Predictions$KNN # KNN 6.51
Data_clean$`B 2`$test$LONG_PRED <- Prediction_Long$`B 2`$Predictions$KNN # KNN 18.81

# MERGE FINAL DATASET
Final_no_wapps <- rbind(Data_clean$`B 0`$test[grep("WAP", names(Data_clean$`B 0`$test), value = T, invert = TRUE)], Data_clean$`B 1`$test[,grep("WAP", names(Data_clean$`B 1`$test), value = T, invert = TRUE)], Data_clean$`B 2`$test[,grep("WAP", names(Data_clean$`B 2`$test), value = T, invert = TRUE)])

FINAL <- cbind(Testing_wapps, Final_no_wapps)

# Save in csv
write.csv(FINAL, file = "Data/Prediction.csv")


#### ENDOFCODE - Testing zone ####
