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

# IMPORT DATA

Training <- read.csv(file="./datasets/trainingData.csv", header = TRUE)
Testing <- read.csv(file="./datasets/testData.csv", header = TRUE)


#### PRE-PROCESS ####
#### Training set ####

# Extract WAPS columns of Data
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


#### Testing set ####

# Replace outliers valyes by -105
Testing <- cbind(as.data.frame(apply(Testing[,grep("WAP", names(Testing), value = TRUE)], 2, function(x){ifelse(x <= 0 & x > -30 | x == 100 | x < -96, -105, x)})), Testing[,grep("WAP", names(Testing), value = TRUE, invert = TRUE)])

# Extract features names of training
Names <- colnames(Training)

# Keep only thoose feature in testing
Testing <- Testing[,grep(paste(Names, collapse="|"), names(Testing), value=TRUE)]


#### Building ####

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


#### MODELLING ####
#### Buildings prediction ####

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
#   Models <- list()
#     
# Mtry <- tuneRF(Training[,grep("WAP", colnames(Training), value = TRUE)], Training$BUILDINGID, ntreeTry=100, stepFactor=2, improve=0.05, trace=TRUE, plot=T)
# build_rf <- randomForest(y = Training$BUILDINGID, x = Training[grep("WAP", names(Training), value = T)], importance=T, method="rf")
# build_pred_rf <- predict(build_rf, Testing[,grep("WAP", names(Testing), value = T)])
# build_metrics_rf <- postResample(Testing$BUILDINGID, build_pred_rf)
# 
# Predictions[["RF"]] <- build_pred_rf
# Metrics[["RF"]] <- build_metrics_rf
# Models[["RF"]] <- build_rf
# 
# build_svm <- svm(y = Training$BUILDINGID, x=Training[grep("WAP", names(Training), value = T)])
# build_pred_svm <- predict(build_svm, Testing[,grep("WAP", names(Testing), value = T)])
# build_metrics_svm <- postResample(Testing$BUILDINGID, build_pred_svm)
#  
# Predictions[["SVM"]] <- build_pred_svm
# Metrics[["SVM"]] <- build_metrics_svm
#  
# build_knn <- knn3(Training$BUILDINGID ~ ., data = Training)
# build_pred_knn <- predict(build_knn, Testing, type = "class")
# build_metrics_knn <- postResample(Testing$BUILDINGID, build_pred_knn)
#  
# Predictions[["KNN"]] <- build_pred_knn
# Metrics[["KNN"]] <- build_metrics_knn
#   
#   Final[["Predictions"]] <- Predictions
#   Final[["Metrics"]] <- Metrics
#   Final[["Models"]] <- Models
#   
#   Final
#   
# }
# 
# Prediction_B <- building.prediction(BUILDING$train, BUILDING$test) 
# 
# 
# # Plot metrics
# Models <- c("RF", "SVM", "KNN")
# for (j in Models) {
#   print(paste("        -", j, "-     "))
#   print(Prediction_B$Metrics[[j]])
# }


# Best model metrics (Random Forest):
# Accuracy     Kappa 
# 0.9963996   0.9943112
#
# Confusion matrix:
# 0    1    2       class.error
# 0 5245    0    0  0.0000000000
# 1    0 4901    3  0.0006117455
# 2    0    1 9077  0.0001101564


# # Saving the best model (Random Forest)
# saveRDS(Prediction_B$Models$RF, "./models/final_model_building_rf.rds")

# Loading the final model for buildings prediction
final_model_building_rf <- readRDS("./models/final_model_building_rf.rds")

# making the prediction
build_pred_rf <- predict(final_model_building_rf, Testing[,grep("WAP", names(Testing), value = T)])

# applying the results to testing dataset
build_metrics_rf <- postResample(Testing$BUILDINGID, build_pred_rf)
build_metrics_rf

# Accuracy     Kappa 
# 0.9963996   0.9943112 

# Confusion matrix
table(build_pred_rf, Testing$BUILDINGID)

#     0   1   2
# 0 535   0   0
# 1   0 306   2
# 2   1   1 266


# Apply prediction to the validation set
Testing$BUILD_PRED <- build_pred_rf


#### Floors prediction ####

# Split the data by buildings
b <- c(0, 1, 2)
Data_split <- list()
for (i in b) {
  Data_split[["train"]][[paste("B", i)]] <- Training %>% filter(BUILDINGID == i)
  Data_split[["test"]][[paste("B", i)]] <- Testing %>% filter(BUILD_PRED == i)
} # Data_split


# Cleaning the data
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


# Selecting features for floors predictions
features <- c("WAP", "FLOOR")
FLOOR <- c()
x <- c()
for (i in Names) {
  x <- feature.selection(Data_clean[[i]][["train"]], Data_clean[[i]][["test"]], features)
  FLOOR[[i]] <- x
}


# Predict floor function
# floor.prediction <- function(training, testing) {
#   
#   prediction <- list()
#   metrics <- list()
#   models <- list()
#   final <- c()
#   
#   training$FLOOR <- as.factor(training$FLOOR)
#   
#   # Mtry <- tuneRF(training[,grep("WAP", colnames(training), value = TRUE)], training$FLOOR, ntreeTry=100, stepFactor=2, improve=0.05, trace=TRUE, plot=T)
#   floor_rf <- randomForest(y = training$FLOOR, x = training[,grep("WAP", names(training), value = T)], importance=T, method="rf")
#   floor_pred_rf <- predict(floor_rf, testing)
#   floor_metrics_rf <- postResample(testing$FLOOR, floor_pred_rf)
#   
#   prediction[["RF"]] <- floor_pred_rf
#   metrics[["RF"]] <- floor_metrics_rf
#   models[["RF"]] <- floor_rf
#   
#   floor_svm <- svm(y = training$FLOOR, x=training[grep("WAP", names(training), value = T)])
#   floor_pred_svm <- predict(floor_svm, testing[grep("WAP", names(testing), value = T)])
#   floor_metrics_svm <- postResample(testing$FLOOR, floor_pred_svm)
#   
#   prediction[["SVM"]] <- floor_pred_svm
#   metrics[["SVM"]] <- floor_metrics_svm
#   models[["SVM"]] <- floor_svm
#   
#   floor_knn <- knn3(training$FLOOR ~ ., data = training)
#   floor_pred_knn <- predict(floor_knn, testing[grep("WAP", names(testing), value = T)], type = "class")
#   floor_metrics_knn <- postResample(testing$FLOOR, floor_pred_knn)
#   
#   prediction[["KNN"]] <- floor_pred_knn
#   metrics[["KNN"]] <- floor_metrics_knn
#   models[["KNN"]] <- floor_knn
#   
#   final[["Predictions"]] <- prediction
#   final[["Metrics"]] <- metrics
#   final[["Models"]] <- models
#   
#   final
#   
# }
# 
# Prediction_F <- list()
# 
# v <- list()
# for (i in Names) {
#   v <- floor.prediction(FLOOR[[i]][["train"]], FLOOR[[i]][["test"]])
#   Prediction_F[[i]] <- v
# }
# 
# 
# # PLOT METRICS
# Models <- c("RF", "SVM", "KNN")
# for (i in Names) {
#   print(paste("===== PREDICTION", i, "====="))
#   for (j in Models) {
#     print(paste("        -", j, "-     "))
#     print(Prediction_F[[i]][["Metrics"]][[j]])
#   }
# }


# SAVING/LOADING BEST FLOOR MODELS FOR EACH BUILDING

# Building 0 (KNN model)
# saveRDS(Prediction_F$`B 0`$Models$KNN, "./models/final_model_floor_b0_knn.rds")

# Loading the final model for floor prediction in building 0
final_model_floor_b0_knn <- readRDS("./models/final_model_floor_b0_knn.rds")

# making the prediction
floor_b0_pred_knn <- predict(final_model_floor_b0_knn, FLOOR[["B 0"]][["test"]][,grep("WAP", names(FLOOR[["B 0"]][["test"]]), value = T)], type = "class")

# applying the results to testing dataset
floor_b0_metrics_knn <- postResample(FLOOR[["B 0"]][["test"]]$FLOOR, floor_b0_pred_knn)
floor_b0_metrics_knn

# Accuracy     Kappa 
# 0.9738318   0.9630155

# Confusion matrix
table(floor_b0_pred_knn, FLOOR[["B 0"]][["test"]]$FLOOR)

#     0   1   2   3
# 0  76   2   1   0
# 1   2 204   6   1
# 2   0   1 158   1
# 3   0   0   0  83


# Building 1 (SVM model)
# saveRDS(Prediction_F$`B 1`$Models$SVM, "./models/final_model_floor_b1_svm.rds")

# Loading the final model for floor prediction in building 1
final_model_floor_b1_svm <- readRDS("./models/final_model_floor_b1_svm.rds")

# making the prediction
floor_b1_pred_svm <- predict(final_model_floor_b1_svm, FLOOR[["B 1"]][["test"]][,grep("WAP", names(FLOOR[["B 1"]][["test"]]), value = T)], type = "class")

# applying the results to testing dataset
floor_b1_metrics_svm <- postResample(FLOOR[["B 1"]][["test"]]$FLOOR, floor_b1_pred_svm)
floor_b1_metrics_svm

# Accuracy     Kappa 
# 0.8045603   0.7193057

# Confusion matrix
table(floor_b1_pred_svm, FLOOR[["B 1"]][["test"]]$FLOOR)

#    0  1  2  3  4
# 0 24 10  1  0  0
# 1  4 98  0  0  0
# 2  2 35 83  5  1
# 3  0  0  3 42  0


# Building 2 (KNN model)
# saveRDS(Prediction_F$`B 2`$Models$KNN, "./models/final_model_floor_b2_knn.rds")

# Loading the final model for floor prediction in building 2
final_model_floor_b2_knn <- readRDS("./models/final_model_floor_b2_knn.rds")

# making the prediction
floor_b2_pred_knn <- predict(final_model_floor_b2_knn, FLOOR[["B 2"]][["test"]][,grep("WAP", names(FLOOR[["B 2"]][["test"]]), value = T)], type = "class")

# applying the results to testing dataset
floor_b2_metrics_knn <- postResample(FLOOR[["B 2"]][["test"]]$FLOOR, floor_b2_pred_knn)
floor_b2_metrics_knn

# Accuracy     Kappa 
# 0.9552239   0.9391024 

# Confusion matrix
table(floor_b2_pred_knn, FLOOR[["B 2"]][["test"]]$FLOOR)

#     0   1   2   3   4
# 0  22   1   0   0   0
# 1   2 107   0   0   1
# 2   0   4  54   0   0
# 3   0   0   0  39   3
# 4   0   0   0   1  34



# ADDING FLOOR PREDICTIONS TO THE DATA
Data_clean$`B 0`$test$FLOOR_PRED <- floor_b0_pred_knn # KNN: Accuracy 0.974, Kappa 0.963
Data_clean$`B 1`$test$FLOOR_PRED <- floor_b1_pred_svm # SVM: Accuracy 0.847, Kappa 0.723
Data_clean$`B 2`$test$FLOOR_PRED <- floor_b2_pred_knn # KNN: Accuracy 0.955, Kappa 0.939



#### Latitude predictions ####

#Names <- names(Buildings_data)
Features <- c("WAP", "LATITUDE")
LATTITUDE <- list()
v <- list()
for (i in Names) {
  v <- feature.selection(Data_clean[[i]][["train"]], Data_clean[[i]][["test"]], Features)
  LATTITUDE[[i]] <- v
}

# latt.prediction <- function(training, testing) {
#   
#   prediction <- list()
#   metrics <- list()
#   models <- list()
#   Final <- c()
# #  Mtry <- tuneRF(training[grep("WAP", names(training), value = T)], training$LATITUDE, ntreeTry=100, stepFactor=2, improve=0.05, trace=TRUE, plot=T)  
#    lat_rf <- randomForest(y = training$LATITUDE, x = training[grep("WAP", names(training), value = T)], importance=T, method="rf")
# 
#    lat_pred_rf <- predict(lat_rf, testing)
#    lat_metrics_rf <- postResample(testing$LATITUDE, lat_pred_rf)
#    
#    prediction[["RF"]] <- lat_pred_rf
#    metrics[["RF"]] <- lat_metrics_rf
#    models[["RF"]] <- lat_rf
#    
#    lat_svm <- svm(y = training$LATITUDE, x=training[grep("WAP", names(training), value = T)])
#    lat_pred_svm <- predict(lat_svm, testing[,grep("WAP", names(testing), value = T)])
#    lat_metrics_svm <- postResample(testing$LATITUDE, lat_pred_svm) 
#    
#    prediction[["SVM"]] <- lat_pred_svm
#    metrics[["SVM"]] <- lat_metrics_svm
#    models[["SVM"]] <- lat_svm
#   
#   lat_knn <- knnreg(training$LATITUDE ~ ., data = training)
#   lat_pred_knn <- predict(lat_knn, testing)
#   lat_metrics_knn <- postResample(testing$LATITUDE, lat_pred_knn)
#   
#   prediction[["KNN"]] <- lat_pred_knn
#   metrics[["KNN"]] <- lat_metrics_knn
#   models[["KNN"]] <- lat_knn
#   
#   Final[["Predictions"]] <- prediction
#   Final[["Metrics"]] <- metrics
#   Final[["Models"]] <- models
#   
#   Final
#   
# }
# 
# Prediction_Latt <- list()
# v <- list()
# for (i in Names) {
#   v <- latt.prediction(LATTITUDE[[i]][["train"]], LATTITUDE[[i]][["test"]])
#   Prediction_Latt[[i]] <- v
# }
# 
# 
# # CHECK METRICS
# Models <- c("RF", "SVM", "KNN")
# for (i in Names) {
#   print(paste("===== PREDICTION", i, "====="))
#   for (j in Models) {
#     print(paste("        -", j, "-     "))
#     print(Prediction_Latt[[i]][["Metrics"]][[j]])
#   }
# }



# SAVING/LOADING BEST LATITUDE MODELS FOR EACH BUILDING

# Building 0 (KNN model)
# saveRDS(Prediction_Latt$`B 0`$Models$KNN, "./models/final_model_latitude_b0_knn.rds")

# Loading the final model for latitude prediction in building 0
final_model_latitude_b0_knn <- readRDS("./models/final_model_latitude_b0_knn.rds")

# making the prediction
latitude_b0_pred_knn <- predict(final_model_latitude_b0_knn, LATTITUDE[["B 0"]][["test"]][,grep("WAP", names(LATTITUDE[["B 0"]][["test"]]), value = T)])

# applying the results to testing dataset
latitude_b0_metrics_knn <- postResample(LATTITUDE[["B 0"]][["test"]]$LATITUDE, latitude_b0_pred_knn)
latitude_b0_metrics_knn

# RMSE     Rsquared      MAE 
# 5.049183 0.974993 3.323119 


# Building 1 (KNN model)
# saveRDS(Prediction_Latt$`B 1`$Models$KNN, "./models/final_model_latitude_b1_knn.rds")

# Loading the final model for latitude prediction in building 1
final_model_latitude_b1_knn <- readRDS("./models/final_model_latitude_b1_knn.rds")

# making the prediction
latitude_b1_pred_knn <- predict(final_model_latitude_b1_knn, LATTITUDE[["B 1"]][["test"]][,grep("WAP", names(LATTITUDE[["B 1"]][["test"]]), value = T)])

# applying the results to testing dataset
latitude_b1_metrics_knn <- postResample(LATTITUDE[["B 1"]][["test"]]$LATITUDE, latitude_b1_pred_knn)
latitude_b1_metrics_knn

# RMSE       Rsquared       MAE 
# 11.973274  0.886492  7.323468 


# Building 2 (KNN model)
# saveRDS(Prediction_Latt$`B 2`$Models$KNN, "./models/final_model_latitude_b2_knn.rds")

# Loading the final model for latitude prediction in building 1
final_model_latitude_b2_knn <- readRDS("./models/final_model_latitude_b2_knn.rds")

# making the prediction
latitude_b2_pred_knn <- predict(final_model_latitude_b2_knn, LATTITUDE[["B 2"]][["test"]][,grep("WAP", names(LATTITUDE[["B 2"]][["test"]]), value = T)])

# applying the results to testing dataset
latitude_b2_metrics_knn <- postResample(LATTITUDE[["B 2"]][["test"]]$LATITUDE, latitude_b2_pred_knn)
latitude_b2_metrics_knn

# RMSE        Rsquared        MAE 
# 14.2454200  0.7892703  6.7346407


# ADD LATITUDE PREDICTION
Data_clean$`B 0`$test$LATT_PRED <- latitude_b0_pred_knn # KNN: MAE 3.32
Data_clean$`B 1`$test$LATT_PRED <- latitude_b1_pred_knn # KNN: MAE 7.32
Data_clean$`B 2`$test$LATT_PRED <- latitude_b2_pred_knn # KNN: MAE 6.73



#### Longitude predictions ####

Features <- c("WAP", "LONG")
LONGITUDE <- list()
v <- list()
for (i in Names) {
  v <- feature.selection(Data_clean[[i]][["train"]], Data_clean[[i]][["test"]], Features)
  LONGITUDE[[i]] <- v
}


# long.prediction <- function(training, testing) {
#   
#   prediction <- list()
#   metrics <- list()
#   models <- list()
#   Final <- c()
#    
#    long_rf <- randomForest(y = training$LONGITUDE, x = training[grep("WAP", names(training), value = T)], importance=T, method="rf")
#    long_pred_rf <- predict(long_rf, testing)
#    long_metrics_rf <- postResample(testing$LONGITUDE, long_pred_rf)
#    
#    prediction[["RF"]] <- long_pred_rf
#    metrics[["RF"]] <- long_metrics_rf
#    models[["RF"]] <- long_rf
#   
#    long_svm <- svm(y = training$LONGITUDE, x=training[grep("WAP", names(training), value = T)])
#    long_pred_svm <- predict(long_svm, testing[,grep("WAP", names(testing), value = T)])
#    long_metrics_svm <- postResample(testing$LONGITUDE, long_pred_svm) 
#    
#    prediction[["SVM"]] <- long_pred_svm
#    metrics[["SVM"]] <- long_metrics_svm
#    models[["SVM"]] <- long_svm
#    
#   long_knn <- knnreg(training$LONGITUDE ~ ., data = training)
#   long_pred_knn <- predict(long_knn, testing)
#   long_metrics_knn <- postResample(testing$LONGITUDE, long_pred_knn)
# 
#   prediction[["KNN"]] <- long_pred_knn
#   metrics[["KNN"]] <- long_metrics_knn
#   models[["KNN"]] <- long_knn
# 
#   Final[["Predictions"]] <- prediction
#   Final[["Metrics"]] <- metrics
#   Final[["Models"]] <- models
#   
#   Final
#   
# }
# 
# Prediction_Long <- list()
# v <- list()
# for (i in Names) {
#   v <- long.prediction(LONGITUDE[[i]][["train"]], LONGITUDE[[i]][["test"]])
#   Prediction_Long[[i]] <- v
# }
# 
# 
# # CHECK METRICS
# Models <- c("RF", "SVM", "KNN")
# for (i in Names) {
#   print(paste("===== PREDICTION", i, "====="))
#   for (j in Models) {
#     print(paste("        -", j, "-     "))
#     print(Prediction_Long[[i]][["Metrics"]][[j]])
#   }
# }


# SAVING/LOADING BEST LONGITUDE MODELS FOR EACH BUILDING

# Building 0 (KNN model)
# saveRDS(Prediction_Long$`B 0`$Models$KNN, "./models/final_model_longitude_b0_knn.rds")

# Loading the final model for longitude prediction in building 0
final_model_longitude_b0_knn <- readRDS("./models/final_model_longitude_b0_knn.rds")

# making the prediction
longitude_b0_pred_knn <- predict(final_model_longitude_b0_knn, LONGITUDE[["B 0"]][["test"]][,grep("WAP", names(LONGITUDE[["B 0"]][["test"]]), value = T)])

# applying the results to testing dataset
longitude_b0_metrics_knn <- postResample(LONGITUDE[["B 0"]][["test"]]$LONGITUDE, longitude_b0_pred_knn)
longitude_b0_metrics_knn

# RMSE      Rsquared        MAE 
# 5.9228686 0.9508384 3.7474438


# Building 1 (KNN model)
# saveRDS(Prediction_Long$`B 1`$Models$KNN, "./models/final_model_longitude_b1_knn.rds")

# Loading the final model for longitude prediction in building 1
final_model_longitude_b1_knn <- readRDS("./models/final_model_longitude_b1_knn.rds")

# making the prediction
longitude_b1_pred_knn <- predict(final_model_longitude_b1_knn, LONGITUDE[["B 1"]][["test"]][,grep("WAP", names(LONGITUDE[["B 1"]][["test"]]), value = T)])

# applying the results to testing dataset
longitude_b1_metrics_knn <- postResample(LONGITUDE[["B 1"]][["test"]]$LONGITUDE, longitude_b1_pred_knn)
longitude_b1_metrics_knn

# RMSE        Rsquared         MAE 
# 10.6492860  0.9484416  6.6960814


# Building 1 (KNN model)
# saveRDS(Prediction_Long$`B 2`$Models$KNN, "./models/final_model_longitude_b2_knn.rds")

# Loading the final model for longitude prediction in building 2
final_model_longitude_b2_knn <- readRDS("./models/final_model_longitude_b2_knn.rds")

# making the prediction
longitude_b2_pred_knn <- predict(final_model_longitude_b2_knn, LONGITUDE[["B 2"]][["test"]][,grep("WAP", names(LONGITUDE[["B 2"]][["test"]]), value = T)])

# applying the results to testing dataset
longitude_b2_metrics_knn <- postResample(LONGITUDE[["B 2"]][["test"]]$LONGITUDE, longitude_b2_pred_knn)
longitude_b2_metrics_knn

# RMSE        Rsquared         MAE 
# 20.7502450  0.6653355  8.4865987 


# ADD LONGITUDE PREDICTION
Data_clean$`B 0`$test$LONG_PRED <- longitude_b0_pred_knn # KNN: MAE 3.74
Data_clean$`B 1`$test$LONG_PRED <- longitude_b1_pred_knn # KNN: MAE 6.69
Data_clean$`B 2`$test$LONG_PRED <- longitude_b2_pred_knn # KNN: MAE 8.48

#### FINAL RESULTS ####

# MERGING FINAL DATASETS (Data_clean $ Testing)
Final_no_wapps <- rbind(Data_clean$`B 0`$test[grep("WAP", names(Data_clean$`B 0`$test), value = T, invert = TRUE)], Data_clean$`B 1`$test[,grep("WAP", names(Data_clean$`B 1`$test), value = T, invert = TRUE)], Data_clean$`B 2`$test[,grep("WAP", names(Data_clean$`B 2`$test), value = T, invert = TRUE)])

Testing_wapps <- Testing[grep("WAP", names(Testing), value = T)]

FINAL <- cbind(Testing_wapps, Final_no_wapps)

# keeping only the predictions columns to make the challenge submission properly
FINAL <- FINAL[grep("PRED", names(FINAL), value = T)]

# removing the building prediction
FINAL$BUILD_PRED <- NULL

# rearrange columns (floor_pred to the end) to make the challenge submission properly
FINAL <- FINAL%>%select(-FLOOR_PRED,everything())

# renaming features to make the challenge submission properly
FINAL <- FINAL %>% rename(LATITUDE = LATT_PRED) %>%
  rename(LONGITUDE = LONG_PRED) %>%
  rename(FLOOR = FLOOR_PRED)

# Saving it in a csv to make the challenge submission properly
# write.csv(FINAL, file = "./datasets/final_all_predictions_challenge.csv")

