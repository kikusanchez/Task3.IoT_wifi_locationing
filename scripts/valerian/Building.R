#### BUILDING ####

# loading pre-processed training and testing
# Training <- read.csv("../datasets/01.training_preprocessed.csv")
# Testing <- read.csv("../datasets/01.testing_preprocessed.csv")

# Keeping interesting features for the prediction
Features <- c("WAP", "BUILDINGID")
Keep <- grep(paste(Features,collapse="|"), colnames(Training), value = TRUE)
Remove <- grep(paste(Features,collapse="|"), colnames(Training), value = TRUE, invert = TRUE)

# Clean environment
rm(list=setdiff(ls(), c("Training", "Testing")))

#Saving training and testing ready for building prediction
write.csv(Training, file = "../datasets/02.training_ready_build_pred.csv")
write.csv(Testing, file = "../datasets/02.testing_ready_build_pred.csv")


### MODEL FOR BUILDING PREDICTION WITH H2O PACKAGE - GO TO h2o script ###



# Cleaning the data
# Training_keep <- Training[,Keep]
# Testing_keep <- Training[,Keep]


# Function for Building prediction
# building.prediction <- function(Training, Testing) {
#   
#   Training$BUILDINGID <- as.factor(Training$BUILDINGID)
#   Results <- list()
#   
#   build_rf <- randomForest(y = Training$BUILDINGID, x = Training[grep("WAP", names(Training), value = T)], importance=T, method="rf", mtry = 6)
#   build_pred_rf <- predict(build_rf, Testing[,grep("WAP", names(Testing), value = T)])
#   build_metrics_rf <- postResample(Testing$BUILDINGID, build_pred_rf)
#   
#   Results[["RF"]] <- build_pred_rf
#   print(build_metrics_rf) # Accuracy 0.9981998 Kappa 0.9971548
#   
#   build_svm <- svm(y = Training$BUILDINGID, x=Training[grep("WAP", names(Training), value = T)])
#   build_pred_svm <- predict(build_svm, Testing[,grep("WAP", names(Testing), value = T)])
#   build_metrics_svm <- postResample(Testing$BUILDINGID, build_pred_svm)
#   
#   Results[["SVM"]] <- build_pred_svm
#   print(build_metrics_svm) # Accuracy 
#   
#   build_knn <- knn3(Training$BUILDINGID ~ ., data = Training)
#   build_pred_knn <- predict(build_knn, Testing, type = "class")
#   build_metrics_knn <- postResample(Testing$BUILDINGID, build_pred_knn)
#   
#   Results[["KNN"]] <- build_pred_knn
#   print(build_metrics_knn) # Accuracy 0.993994 Kappa 0.9900539
#   
#   Results
#   
# }

# Train different models
# Prediction <- building.prediction(Training_keep, Testing_keep) 

# Apply RF Kappa 0.9985778
# Testing <- cbind(Testing, rforest.model)

# Rename prediction feature
# Testing <- rename(Testing, BUILD_PRED = 'rforest.model') #For renaming dataframe column

# Save results
# write.csv(Testing, file = "../datasets/Testing_Building.csv")

# Clean environment
# rm(list=setdiff(ls(), c("Training", "Testing")))