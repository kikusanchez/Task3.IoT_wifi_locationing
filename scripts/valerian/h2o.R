# running and initializing h2o package
library(h2o)
h2o.init(nthreads = -1)


# BUILDING PREDICTION

# Loading training and testing ready for building prediction
# Training <- read.csv("../datasets/01.training_preprocessed.csv", na = c("N/A"))
# Testing <- read.csv("../datasets/01.testing_preprocessed.csv", na = c("N/A"))

# converting datasets into h2o environments and checking the names of the features
train.h2o <- as.h2o(Training)
test.h2o <- as.h2o(Testing)
names(train.h2o)
names(test.h2o)

# dependent variable as factor (BUILDINGID)
train.h2o[,405] <- as.factor(train.h2o[,405])

# setting dependent variable and independent ones
y.dep <- 405
x.indep <- c(1:401)

# setting the model
rforest.model <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = train.h2o, ntrees = 5000, mtries = 41, max_depth = 10, seed = 123)

# results of my model in training dataset
h2o.performance(rforest.model)
h2o.varimp(rforest.model)

# aplying the model to the validation set
Testing$BUILDINGID <- as.factor(Testing$BUILDINGID)
system.time(predict.rforest <- as.data.frame(h2o.predict(rforest.model, test.h2o)))
predict.rforest
postResample(predict.rforest, Testing$BUILDINGID) # Accuracy 1 | Kappa 1

# confusion matrix
table(predict.rforest$predict, Testing$BUILDINGID)
cfm_rf_building_h2o <- table(predict.rforest$predict, Testing$BUILDINGID)

# save a h2o model
h2o.saveModel(object=rforest.model, path=paste0(getwd(),"/", "models"), force=TRUE)
#(this will save it as the default name, rename it manually on the folder to loaded after)

# LOAD THE MODEL
h2o_rf_model_building <- h2o.loadModel(path=paste0(getwd(),"/","models", "/", "h2o_rf_model_building"))
h2o_rf_model_building







# apply the predictions to the validation data
Testing <- cbind(Testing, predict.rforest$predict)

# waterfall approach.- rename BUILDINGID as ORIG_BUILDING and the prediction as BUILDINGID
# (if the accuracy and kappa are 1 it doesn't matter but if not, is the correct practice)
Testing <- Testing %>% rename(ORIG_BUILDING = BUILDINGID)

Testing <- rename(Testing, BUILDINGID = 'predict.rforest$predict') #For renaming prediction column

# Save results
write.csv(Testing, file = "../datasets/02.testing_building_pred_ok.csv", row.names=FALSE)
write.csv(Training, file = "../datasets/02.training_building_pred_ok.csv", row.names=FALSE)

# Clean all the environment but Training and Testing
rm(list=setdiff(ls(), c("Training", "Testing")))



### GO TO "Floors" SCRIPT TO CONTINUE PRE-PROCESSING FOR PREDICT FLOORS





# FLOOR - ALL BUILDINGS
# names(train.h2o)
# train.h2o[,404] <- as.factor(train.h2o[,404]) # dependent variable as factor
# y.dep <- 404
# x.indep <- c(1:401)
# rforest.model_floor <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = train.h2o, ntrees = 5000, mtries = 41, max_depth = 10, seed = 123)
# h2o.performance(rforest.model_floor)
# h2o.varimp(rforest.model_floor)
# 
# Testing$FLOOR <- as.factor(Testing$FLOOR)
# system.time(predict.rforest_floor <- as.data.frame(h2o.predict(rforest.model_floor, test.h2o)))
# predict.rforest_floor
# postResample(predict.rforest_floor, Testing$FLOOR)
# 
# 
# cfm <- confusionMatrix(table(predict.rforest, Testing$BUILDINGID))
# cfm_table <- cfm$table
# predict.rforest$predict




# FLOOR - BUILDING1
train.h2o <- as.h2o(Data_clean[["B 1"]][["train"]])
test.h2o <- as.h2o(Data_clean[["B 1"]][["test"]])
names(train.h2o)
train.h2o[,182] <- as.factor(train.h2o[,182]) # dependent variable as factor (FLOOR)
y.dep <- 182
x.indep <- c(1:179)
rforest.model_floor_b1 <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = train.h2o, ntrees = 5000, max_depth = 10, seed = 123)
h2o.performance(rforest.model_floor_b1)
h2o.varimp(rforest.model_floor_b1)

Data_clean[["B 1"]][["test"]]$FLOOR <- as.factor(Data_clean[["B 1"]][["test"]]$FLOOR)
system.time(predict.rforest_floor_b1 <- as.data.frame(h2o.predict(rforest.model_floor_b1, test.h2o)))
predict.rforest_floor_b1
postResample(predict.rforest_floor_b1, Data_clean[["B 1"]][["test"]]$FLOOR)




