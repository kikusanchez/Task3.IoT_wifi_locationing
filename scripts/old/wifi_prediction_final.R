#####
library(h2o)
#library(readr)
library(caret)


test.h2o <- as.h2o(Testing)
train.h2o <- as.h2o(Training)

train.h2o[,"BUILDINGID"] <- as.factor(train.h2o[,"BUILDINGID"])
y.dep <- "BUILDINGID"
x.indep <- WAPS
rforest.model <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = train.h2o, ntrees = 5000, mtries = 41, max_depth = 10, seed = 123)

#h2o.performance(rforest.model)
#h2o.varimp(rforest.model)Testing$BUILDINGID <- as.factor(Testing$BUILDINGID)
predict.rforest_building <- as.data.frame(h2o.predict(rforest.model, test.h2o))
postResample(predict.rforest_building, Testing$BUILDINGID)

predict.rforest <- as.data.frame(h2o.predict(rforest.model, test.h2o))
predict.rforest
postResample(rforest.model, Testing$BUILDINGID)

cfm <- confusionMatrix(table(predict.rforest, Testing$BUILDINGID))
cfm_table <- cfm$table
predict.rforest$predict
#####

library(dplyr)
library(ggplot2)
library(caret)
library(randomForest)
library(e1071)
library(reshape2)
library(h2o)
library(class)

# IMPORT DATA

Training <- read.csv(file="Data/trainingData.csv", header = TRUE)
Testing <- read.csv(file="Data/validationData.csv", header = TRUE)


    ##################
    #### TRAINING ####
    ##################

# Extrat WAPS columns of Data
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
Training <- cbind(as.data.frame(apply(Training[WAPS], 2, function(x){ifelse(x <= 0 & x > -30 | x == 100 | x < -96, -105, x)})), Training[no_WAPS])

Building2 <- Training[Training$BUILDINGID == 2,]
Training<-Training[!Training$BUILDINGID == 2,]
Building2 <- cbind(as.data.frame(apply(Building2[WAPS], 2, function(x){ifelse(x < -80, -105, x)})), Building2[no_WAPS])
Training <- rbind(Training, Building2)

# New feature for best value
Training$MAX_W <- colnames(Training)[apply(Training[,grep("WAP", colnames(Training))],1,which.max)]

# Remove waps which appear in the three buildings
Training$WAP248 <- NULL

    #################
    #### TESTING ####
    #################

# Replace outliers valyes by -105
Testing <- cbind(as.data.frame(apply(Testing[,grep("WAP", names(Testing), value = TRUE)], 2, function(x){ifelse(x <= 0 & x > -30 | x == 100 | x < -96, -105, x)})), Testing[,grep("WAP", names(Testing), value = TRUE, invert = TRUE)])

# Extract features names of training
Names <- colnames(Training)

# Keep only thoose feature in testing
Testing <- Testing[,grep(paste(Names, collapse="|"), names(Testing), value=TRUE)]

    ##################
    #### BUILDING ####
    ##################

# Retain interesting features
features <- c("WAP", "BUILDINGID")
BUILDING <- c()
feature.selection <- function(training, testing, features){
  
  data <- list()
  Keep <- grep(paste(features,collapse="|"), colnames(training), value = TRUE)
  
  data[["train"]] <- training[Keep]
  data[["test"]] <- testing[Keep]
  
  data
}

# Remove Phone 17
#Training <- Training[!(Training$USERID == 17 & Training$BUILDINGID == 2),]

BUILDING <- feature.selection(Training, Testing, features)

# Building prediction
building.prediction <- function(Training, Testing) {
  
  Training$BUILDINGID <- as.factor(Training$BUILDINGID)

  Final <- list()
  Predictions <- list()
  Metrics <- list()
  
#  Mtry <- tuneRF(Training[,grep("WAP", colnames(Training), value = TRUE)], Training$BUILDINGID, ntreeTry=100, stepFactor=2, improve=0.05, trace=TRUE, plot=T)
  build_rf <- randomForest(y = Training$BUILDINGID, x = Training[grep("WAP", names(Training), value = T)], importance=T, method="rf")
  build_pred_rf <- predict(build_rf, Testing[,grep("WAP", names(Testing), value = T)])
  build_metrics_rf <- postResample(Testing$BUILDINGID, build_pred_rf)
  
  Predictions[["RF"]] <- build_pred_rf
  Metrics[["RF"]] <- build_metrics_rf

  build_svm <- svm(y = Training$BUILDINGID, x=Training[grep("WAP", names(Training), value = T)])
  build_pred_svm <- predict(build_svm, Testing[,grep("WAP", names(Testing), value = T)])
  build_metrics_svm <- postResample(Testing$BUILDINGID, build_pred_svm)
   
  Predictions[["SVM"]] <- build_pred_svm
  Metrics[["SVM"]] <- build_metrics_svm
   
  build_knn <- knn3(Training$BUILDINGID ~ ., data = Training)
  build_pred_knn <- predict(build_knn, Testing, type = "class")
  build_metrics_knn <- postResample(Testing$BUILDINGID, build_pred_knn)
   
  Predictions[["KNN"]] <- build_pred_knn
  Metrics[["KNN"]] <- build_metrics_knn
  
  Final[["Predictions"]] <- Predictions
  Final[["Metrics"]] <- Metrics
  
  Final
  
}

Prediction_B <- building.prediction(BUILDING$train, BUILDING$test) 

# Plot metrics
Models <- c("RF", "SVM", "KNN")
for (j in Models) {
  print(paste("        -", j, "-     "))
  print(Prediction_B$Metrics[[j]])
}

# Apply RF Kappa 0.9957
Testing$BUILD_PRED <- Prediction_B$Predictions$RF

    ###############
    #### FLOOR ####
    ###############

# Split Data between buildings
b <- c(0, 1, 2)
Data_split <- list()
for (i in b) {
  Data_split[["train"]][[paste("B", i)]] <- Training %>% filter(BUILDINGID == i)
  Data_split[["test"]][[paste("B", i)]] <- Testing %>% filter(BUILD_PRED == i)
} # Data_split

# CLEAN DATA
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

# SELECT FEATURES
features <- c("WAP", "FLOOR")
FLOOR <- c()
x <- c()
for (i in Names) {
  x <- feature.selection(Data_clean[[i]][["train"]], Data_clean[[i]][["test"]], features)
  FLOOR[[i]] <- x
}

# Predict floor function
floor.prediction <- function(training, testing) {
  
  prediction <- list()
  metrics <- list()
  models <- list()
  final <- c()
  
  training$FLOOR <- as.factor(training$FLOOR)
  
  # Mtry <- tuneRF(training[,grep("WAP", colnames(training), value = TRUE)], training$FLOOR, ntreeTry=100, stepFactor=2, improve=0.05, trace=TRUE, plot=T)
  floor_rf <- randomForest(y = training$FLOOR, x = training[,grep("WAP", names(training), value = T)], importance=T, method="rf")
  floor_pred_rf <- predict(floor_rf, testing)
  floor_metrics_rf <- postResample(testing$FLOOR, floor_pred_rf)
  
  prediction[["RF"]] <- floor_pred_rf
  metrics[["RF"]] <- floor_metrics_rf
#  models[["RF"]] <- floor_rf
  
  floor_svm <- svm(y = training$FLOOR, x=training[grep("WAP", names(training), value = T)])
  floor_pred_svm <- predict(floor_svm, testing[grep("WAP", names(testing), value = T)])
  floor_metrics_svm <- postResample(testing$FLOOR, floor_pred_svm)
  
  prediction[["SVM"]] <- floor_pred_svm
  metrics[["SVM"]] <- floor_metrics_svm
#  models[["SVM"]] <- floor_svm
  
  floor_knn <- knn3(training$FLOOR ~ ., data = training)
  floor_pred_knn <- predict(floor_knn, testing[grep("WAP", names(testing), value = T)], type = "class")
  floor_metrics_knn <- postResample(testing$FLOOR, floor_pred_knn)
  
  prediction[["KNN"]] <- floor_pred_knn
  metrics[["KNN"]] <- floor_metrics_knn
#  models[["KNN"]] <- floor_knn
  
  final[["Predictions"]] <- prediction
  final[["Metrics"]] <- metrics
#  final[["Models"]] <- models
  
  final
  
}
Prediction_F <- list()
v <- list()
for (i in Names) {
  v <- floor.prediction(FLOOR[[i]][["train"]], FLOOR[[i]][["test"]])
  Prediction_F[[i]] <- v
}

# PLOT METRICS
Models <- c("RF", "SVM", "KNN")
for (i in Names) {
  print(paste("===== PREDICTION", i, "====="))
  for (j in Models) {
    print(paste("        -", j, "-     "))
    print(Prediction_F[[i]][["Metrics"]][[j]])
  }
}

# ADD FLOOR PREDICTION
Data_clean$`B 0`$test$FLOOR_PRED <- Prediction_F$`B 0`$Predictions$KNN # KAPPA KNN 0.963
Data_clean$`B 1`$test$FLOOR_PRED <- Prediction_F$`B 1`$Predictions$SVM # KAPPA SVM 0.723
Data_clean$`B 2`$test$FLOOR_PRED <- Prediction_F$`B 2`$Predictions$KNN # KAPPA KNN 0.939

    ###################
    #### LATTITUDE ####
    ###################

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
   lat_rf <- randomForest(y = training$LATITUDE, x = training[grep("WAP", names(training), value = T)], importance=T, method="rf")

   lat_pred_rf <- predict(lat_rf, testing)
   lat_metrics_rf <- postResample(testing$LATITUDE, lat_pred_rf)
   
   prediction[["RF"]] <- lat_pred_rf
   metrics[["RF"]] <- lat_metrics_rf
   models[["RF"]] <- lat_rf
   
   lat_svm <- svm(y = training$LATITUDE, x=training[grep("WAP", names(training), value = T)])
   lat_pred_svm <- predict(lat_svm, testing[,grep("WAP", names(testing), value = T)])
   lat_metrics_svm <- postResample(testing$LATITUDE, lat_pred_svm) 
   
   prediction[["SVM"]] <- lat_pred_svm
   metrics[["SVM"]] <- lat_metrics_svm
   models[["SVM"]] <- lat_svm
  
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
Data_clean$`B 0`$test$LATT_PRED <- Prediction_Latt$`B 0`$Predictions$KNN # KNN 3.32
Data_clean$`B 1`$test$LATT_PRED <- Prediction_Latt$`B 1`$Predictions$KNN # KNN 7.19
Data_clean$`B 2`$test$LATT_PRED <- Prediction_Latt$`B 2`$Predictions$KNN # KNN 6.74

    ###################
    #### LONGITUDE ####
    ###################

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
   
   long_rf <- randomForest(y = training$LONGITUDE, x = training[grep("WAP", names(training), value = T)], importance=T, method="rf")
   long_pred_rf <- predict(long_rf, testing)
   long_metrics_rf <- postResample(testing$LONGITUDE, long_pred_rf)
   
   prediction[["RF"]] <- long_pred_rf
   metrics[["RF"]] <- long_metrics_rf
#  models[["RF"]] <- long_rf
  
   long_svm <- svm(y = training$LONGITUDE, x=training[grep("WAP", names(training), value = T)])
   long_pred_svm <- predict(long_svm, testing[,grep("WAP", names(testing), value = T)])
   long_metrics_svm <- postResample(testing$LONGITUDE, long_pred_svm) 
   
   prediction[["SVM"]] <- long_pred_svm
   metrics[["SVM"]] <- long_metrics_svm
#  models[["SVM"]] <- long_svm
   
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
Data_clean$`B 0`$test$LONG_PRED <- Prediction_Long$`B 0`$Predictions$KNN # KNN 3.74
Data_clean$`B 1`$test$LONG_PRED <- Prediction_Long$`B 1`$Predictions$KNN # KNN 6.64
Data_clean$`B 2`$test$LONG_PRED <- Prediction_Long$`B 2`$Predictions$KNN # KNN 8.46

# MERGE FINAL DATASET
Final_no_wapps <- rbind(Data_clean$`B 0`$test[grep("WAP", names(Data_clean$`B 0`$test), value = T, invert = TRUE)], Data_clean$`B 1`$test[,grep("WAP", names(Data_clean$`B 1`$test), value = T, invert = TRUE)], Data_clean$`B 2`$test[,grep("WAP", names(Data_clean$`B 2`$test), value = T, invert = TRUE)])

FINAL <- cbind(Testing_wapps, Final_no_wapps)

# Save in csv
write.csv(FINAL, file = "Data/Prediction.csv")

#######################################
#### ENDOFCODE ENDOFCODE ENDOFCODE ####
#######################################