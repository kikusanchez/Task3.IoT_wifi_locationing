#Load Libraries: p_load can install, load,  and update packages
if(require("pacman")=="FALSE"){
  install.packages("pacman")
}


pacman::p_load(rstudioapi,dplyr,tidyr,ggplot2,caret,randomForest,e1071,reshape2,h2o,plotly)

library(ggpubr)
theme_pubr()



# Setwd (1º current wd where is the script, then we move back to the
# general folder)
current_path = getActiveDocumentContext()$path
setwd(dirname(current_path))
setwd("..")
rm(current_path)


#### PREDICT FLOOR ####
Testing <- read.csv(file = "../datasets/02.testing_building_pred_ok.csv")
Training <- read.csv(file = "../datasets/02.training_building_pred_ok.csv")


Data_train <- Training
Data_test <- Testing

# Split Data between buildings and store it in list
b <- c(0, 1, 2)
Training <- list()
Testing <- list()

for (i in b) {
  training_b <- Data_train %>% filter(BUILDINGID == i)
  Training[[paste("B", i)]] <- training_b

  testing_b <- Data_test %>% filter(BUILDINGID == i)
#  testing_b <- Data_test %>% filter(BUILD_PRED == i)
  Testing[[paste("B", i)]] <- testing_b
  
  }

# run function to clean data
Clean.training.building <- function(data) {
  no_WAPS <- grep("WAP", colnames(data), value = TRUE, invert = TRUE)
  f <- data[,apply(data[grep("WAP", colnames(data), value = TRUE)], 2, function(x) {var(x) != 0})]
  f <- f[,grep("WAP", colnames(f), value = TRUE)]
  data <- cbind(f, data[,no_WAPS])
  data <- data[apply(data[grep("WAP", colnames(data), value = TRUE)], 1, function(x){mean(x) != -105}),]
  data
}
Clean.testing.building <- function(training, testing){
  WAPS <- grep("WAP", colnames(training), value = TRUE)
  no_WAPS <- grep("WAP", colnames(training), value = TRUE, invert = TRUE)
  toMatch <- c(WAPS, no_WAPS)
  testing <- testing[,grep(paste(toMatch, collapse="|"), names(testing), value=TRUE)]
  testing
}

# Clean Data
Names <- names(Training)
Buildings_data <- list()
list <- list()

for (i in Names) {
  train <- Clean.training.building(Training[[i]])
  test <- Clean.testing.building(train, Testing[[i]])
  list[["train"]] <- train
  list[["test"]] <- test
  Buildings_data[[i]] <- list
}

# select usefull feature
features <- c("WAP", "FLOOR")

# remove other feature
floor.columns <- function(training, testing, features){
  
  data <- list()
  Keep <- grep(paste(features,collapse="|"), colnames(training), value = TRUE)
  
  data[["train"]] <- training[,Keep]
  data[["test"]] <- testing[,Keep]
  
  data
}

Buildings <- list()
x <- list()

for (i in Names) {
  x <- floor.columns(Buildings_data[[i]][["train"]], Buildings_data[[i]][["test"]], features)
  Buildings[[i]] <- x
}


# Buildings_data[["B 1"]][["train"]]$FLOOR <- as.factor(Buildings_data[["B 1"]][["train"]]$FLOOR)
# 
# knnVCN(Buildings_data[["B 1"]][["train"]][,grep("WAP", names(Buildings_data[["B 1"]][["train"]]), value = T)], Buildings_data[["B 1"]][["train"]]$FLOOR, Buildings_data[["B 1"]][["test"]], K = 3, method = "manhattan")
# 
# train_manhattan <- dist(Buildings_data[["B 1"]][["train"]], method = "manhattan")
# test_manhattan <- dist(Buildings_data[["B 1"]][["test"]], method = "manhattan")
# 
# train_manhattan$FLOOR <- as.factor(train_manhattan$FLOOR)
# 
# floor_knn <- knn3(train_manhattan$FLOOR ~ ., data = train_manhattan)
# floor_pred_knn <- predict(floor_knn, testing_manhattan, type = "class")
# floor_metrics_knn <- postResample(testing_manhattan$FLOOR, floor_pred_knn)


# Predict floor function
floor.prediction <- function(training, testing) {
  
  prediction <- list()
  metrics <- list()
  models <- list()
  final <- c()
  
  training$FLOOR <- as.factor(training$FLOOR)
  
  # Mtry <- tuneRF(training[grep("WAP", names(training), value = T)], training$BUILDINGID, ntreeTry=100, stepFactor=2, improve=0.05, trace=TRUE, plot=T)
  floor_rf <- randomForest(y = training$FLOOR, x = training[grep("WAP", names(training), value = T)], importance=T, method="rf")
  floor_pred_rf <- predict(floor_rf, testing)
  floor_metrics_rf <- postResample(testing$FLOOR, floor_pred_rf)
  
  prediction[["RF"]] <- floor_pred_rf
  metrics[["RF"]] <- floor_metrics_rf
  models[["RF"]] <- floor_rf
  
  floor_svm <- svm(y = training$FLOOR, x=training[grep("WAP", names(training), value = T)])
  floor_pred_svm <- predict(floor_svm, testing[,grep("WAP", names(testing), value = T)])
  floor_metrics_svm <- postResample(testing$FLOOR, floor_pred_svm)
  
  prediction[["SVM"]] <- floor_pred_svm
  metrics[["SVM"]] <- floor_metrics_svm
  models[["SVM"]] <- floor_svm
    
  floor_knn <- knn3(training$FLOOR ~ ., data = training)
  floor_pred_knn <- predict(floor_knn, testing, type = "class")
  floor_metrics_knn <- postResample(testing$FLOOR, floor_pred_knn)
  
  prediction[["KNN"]] <- floor_pred_knn
  metrics[["KNN"]] <- floor_metrics_knn
  models[["KNN"]] <- floor_knn
  
  final[["Predictions"]] <- prediction
  final[["Metrics"]] <- metrics
  final[["Models"]] <- models
    
  final
  
}

Prediction <- list()
v <- list()


for (i in Names) {
  v <- floor.prediction(Buildings[[i]][["train"]], Buildings[[i]][["test"]])
  Prediction[[i]] <- v
}




B0 <- cbind(Testing[["B 0"]], Prediction[["B 0"]][["Predictions"]][["RF"]]) # KAPPA RF 0.968
B0 <- rename(B0, FLOOR_PRED = `Prediction[["B 0"]][["Predictions"]][["RF"]]`)

B1 <- cbind(Testing[["B 1"]], Prediction[["B 1"]][["Predictions"]][["SVM"]]) # KAPPA SVM 0.715
B1 <- rename(B1, FLOOR_PRED = `Prediction[["B 1"]][["Predictions"]][["SVM"]]`)

B2 <- cbind(Testing[["B 2"]], Prediction[["B 2"]][["Predictions"]][["KNN"]]) # KAPPA KNN 0.832
B2 <- rename(B2, FLOOR_PRED = `Prediction[["B 2"]][["Predictions"]][["KNN"]]`)

Testing <- rbind(B0, B1, B2)

write.csv(Testing, file = "Testing_Floor.csv")

Training <- Data_train

# Clean environment
rm(list=setdiff(ls(), c("Training", "Testing", "floor.columns")))
