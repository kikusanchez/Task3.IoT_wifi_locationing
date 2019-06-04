library(rstudioapi)
library(dplyr)
library(ggplot2)
library(caret)
library(randomForest)
library(e1071)
library(reshape2)
library(h2o)
library(class)

# Setwd (1º current wd where is the script, then we move back to the
# general folder)
current_path = getActiveDocumentContext()$path
setwd(dirname(current_path))
setwd("..")
rm(current_path)


##################
#### MODELS ####
##################


#making a sample of our pre process data
sample200_train <- Training %>% group_by(BUILDINGID, FLOOR) %>% sample_n(200)
sample200_test <- Testing %>% group_by(BUILDINGID, FLOOR) %>% sample_n(200)

# converting dependent variable as factor because we will run a rf classification model
sample200_train$BUILDINGID <- as.factor(sample200_train$BUILDINGID)
is.factor(sample200_train$BUILDINGID)
Testing$BUILDINGID <- as.factor(Testing$BUILDINGID)
is.factor(sample200_test$BUILDINGID)

# Train a random forest using caret package
bestmtry_rf<-tuneRF(sample200_train[grep("WAP", names(sample200_train), value = T)], sample200_train$BUILDINGID, ntreeTry=100,stepFactor=2,improve=0.05,trace=TRUE, plot=T)
system.time(rf_caret<-train(y=sample200_train$BUILDINGID, x=sample200_train[grep("WAP", names(sample200_train), value = T)], data = sample200_train, method="rf", ntree=100, tuneGrid=expand.grid(.mtry=11)))
rf_caret_pred <- predict(rf_caret, Testing)
rf_caret_metrics <- postResample(Testing$BUILDINGID, rf_caret_pred)
rf_caret_metrics
cfm_rf_caret <- confusionMatrix(table(rf_caret_pred, Testing$BUILDINGID))
cfm_rf_caret$table
saveRDS(rf_caret, file="../models/RF_caret_model.rds") # caret model (n=200 samples) of training and testing pre-processed data (2 min)
rf_caret <- readRDS("../models/RF_caret_model.rds")

#random forest package
bestmtry_rf<-tuneRF(sample10_train[grep("WAP", names(sample10_train), value = T)], sample10_train$BUILDINGID, ntreeTry=100,stepFactor=2,improve=0.05,trace=TRUE, plot=T)
build_rf <- randomForest(y = sample10_train$BUILDINGID, x = sample10_train[grep("WAP", names(sample10_train), value = T)], importance=T, method="rf", ntree = 5, mtry = 44)
build_pred_rf <- predict(build_rf, sample10_test[,grep("WAP", names(sample10_test), value = T)])
build_metrics_rf <- postResample(sample10_test$BUILDINGID, build_pred_rf)
build_metrics_rf
cfm <- confusionMatrix(table(build_pred_rf, sample10_test$BUILDINGID))
cfm_table <- cfm$table

# global variance of the columns (waps)
global_var <- apply(Training[grep("WAP", names(sample10_train), value = T)], 2, function(x) var(x))  
str(global_var)
plot(global_var)

global_var
ggplot(glob_var,
       aes(glob_var$global_var))+
         geom_abline()

plot(global_var, type = "l")       
# global variance of the rows (checkpoints)
global_var2 <- apply(Training[grep("WAP", names(sample10_train), value = T)], 1, function(x) var(x))
str(global_var2)
hist(global_var2, breaks = 100)


rf_sergi <- readRDS("../Task3.IoT_wifi_locationing/models/Sergi_RF_Model.rds")
build_pred_rf <- predict(rf_sergi,Testing)
build_metrics_rf <- postResample(Testing$BUILDINGID, build_pred_rf)
build_metrics_rf
cfm_caret <- confusionMatrix(table(build_pred_rf, Testing$BUILDINGID))
cfm_table <- cfm_caret$table
cfm_table


# load final csv
final <- read.csv("../datasets/Pre-processed datasets/Prediction.csv")

final2 <- final %>% filter(FLOOR != FLOOR_PRED)
final5 <- final %>% filter(abs(LATITUDE - LATT_PRED) > 18)
final6 <- final %>% filter(abs(LONGITUDE - LONG_PRED) > 20)
final6 <- final6 %>% group_by(PHONEID) %>% summarise(n())
final3 <- final %>% group_by(PHONEID) %>% summarise(n())
final4 <- final2 %>% group_by(PHONEID) %>% summarise(n())
final5 <- final5 %>% group_by(PHONEID) %>% summarise(n())


ggplot(final2,
       aes())+
  geom_bar(final2$PHONEID)


ggplot(final3, aes(final3$PHONEID, final3$`n()`)) +
  geom_line() +
  geom_line(data = final4, aes(final4$PHONEID, final4$`n()`), color ="red") +
  geom_line(data = final5, aes(final5$PHONEID, final5$`n()`), color ="blue") +
  geom_line(data = final6, aes(final6$PHONEID, final6$`n()`), color ="green")
