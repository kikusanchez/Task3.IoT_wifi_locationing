Testing <- read.csv(file = "../datasets/validationData.csv.csv")
Training <- read.csv(file = "../datasets/trainingData.csv")

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

grep(paste(features,collapse="|"), colnames(Buildings_data[["B 0"]][["train"]]), value = TRUE)
###################
#### LATTITUDE ####
###################
Names <- names(Buildings_data)
features <- c("WAP", "LATITUDE")
Final_latt <- list()
v <- list()
for (i in Names) {
  v <- floor.columns(Buildings_data[[i]][["train"]], Buildings_data[[i]][["test"]], features)
  Final_latt[[i]] <- v
}

install.packages("knnGarden")
library(knnGarden)





Lat.prediction <- function(training, testing) {
  
  prediction <- list()
  metrics <- list()
  models <- list()
  Final <- c()
  Mtry <- tuneRF(training[grep("WAP", names(training), value = T)], training$LATITUDE, ntreeTry=100, stepFactor=2, improve=0.05, trace=TRUE, plot=T)  
  lat_rf <- randomForest(y = training$LATITUDE, x = training[grep("WAP", names(training), value = T)], mtry=Mtry, importance=T, method="rf")
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
  models[["KNN"]] <- lat_knn
  
  Final[["Prediction"]] <- prediction
  Final[["Metrics"]] <- metrics
  Final[["Models"]] <- models
  
  Final
  
}

Pred_latt <- list()
v <- list()
for (i in Names) {
  v <- Lat.prediction(Final_latt[[i]][["train"]], Final_latt[[i]][["test"]])
  Pred_latt[[i]] <- v
}

LA0 <- cbind(Testing[["B 0"]], Pred_latt[["B 0"]][["Prediction"]][["KNN"]]) # MAE KNN 3.45
LA0 <- rename(LA0, LATT_PRED = `Pred_latt[["B 0"]][["Prediction"]][["KNN"]]`)

LA1 <- cbind(Testing[["B 1"]], Pred_latt[["B 1"]][["Prediction"]][["KNN"]]) # MAE KNN 7.22
LA1 <- rename(LA1, LATT_PRED = `Pred_latt[["B 1"]][["Prediction"]][["KNN"]]`)

LA2 <- cbind(Testing[["B 2"]], Pred_latt[["B 2"]][["Prediction"]][["KNN"]]) # MAE KNN 7.87
LA2 <- rename(LA2, LATT_PRED = `Pred_latt[["B 2"]][["Prediction"]][["KNN"]]`)

test1 <- rbind(LA0, LA1, LA2)

###################
#### LONGITUDE ####
###################

Features <- c("WAP", "LONG")
Final_long <- list()
v <- list()
for (i in Names) {
  v <- floor.columns(Buildings_data[[i]][["train"]], Buildings_data[[i]][["test"]], Features)
  Final_long[[i]] <- v
}

Long.prediction <- function(training, testing) {
  
  prediction <- list()
  metrics <- list()
  models <- list()
  Final <- c()
  
  long_rf <- randomForest(y = training$LONGITUDE, x = training[grep("WAP", names(training), value = T)], importance=T, method="rf")
  long_pred_rf <- predict(long_rf, testing)
  long_metrics_rf <- postResample(testing$LONGITUDE, long_pred_rf)
  
  prediction[["RF"]] <- long_pred_rf
  metrics[["RF"]] <- long_metrics_rf
  models[["RF"]] <- long_rf
  
  long_svm <- svm(y = training$LONGITUDE, x=training[grep("WAP", names(training), value = T)])
  long_pred_svm <- predict(long_svm, testing[,grep("WAP", names(testing), value = T)])
  long_metrics_svm <- postResample(testing$LONGITUDE, long_pred_svm) 
  
  prediction[["SVM"]] <- long_pred_svm
  metrics[["SVM"]] <- long_metrics_svm
  models[["SVM"]] <- long_svm
    
  long_knn <- knnreg(training$LONGITUDE ~ ., data = training)
  long_pred_knn <- predict(long_knn, testing)
  long_metrics_knn <- postResample(testing$LONGITUDE, long_pred_knn)
  
  prediction[["KNN"]] <- long_pred_knn
  metrics[["KNN"]] <- long_metrics_knn
  models[["KNN"]] <- long_knn
  
  Final[["Prediction"]] <- prediction
  Final[["Metrics"]] <- metrics
  Final[["Models"]] <- models

  Final
  
}

Pred_long <- list()
v <- list()
for (i in Names) {
  v <- Long.prediction(Final_long[[i]][["train"]], Final_long[[i]][["test"]])
  Pred_long[[i]] <- v
}

LO0 <- cbind(Testing[["B 0"]], Pred_long[["B 0"]][["Prediction"]][["KNN"]]) # MAE KNN 3.91
LO0 <- rename(LO0, LON_PRED = `Pred_long[["B 0"]][["Prediction"]][["KNN"]]`)

LO1 <- cbind(Testing[["B 1"]], Pred_long[["B 1"]][["Prediction"]][["RF"]]) # MAE RF 6.77
LO1 <- rename(LO1, LON_PRED = `Pred_long[["B 1"]][["Prediction"]][["RF"]]`)

LO2 <- cbind(Testing[["B 2"]], Pred_long[["B 2"]][["Prediction"]][["KNN"]]) # MAE KNN 8.82
LO2 <- rename(LO2, LON_PRED = `Pred_long[["B 2"]][["Prediction"]][["KNN"]]`)

Testing <- rbind(LO0, LO1, LO2)

