#### 00. INCLUDES ---------------------------------------------------------------------
#Load Libraries: p_load can install, load,  and update packages
if(require("pacman")=="FALSE"){
  install.packages("pacman")
}


pacman::p_load(readr,h2o, rstudioapi, caret)

library(h2o)
h2o.init(nthreads = -1)


# Setwd (1º current wd where is the script, then we move back to the
# general folder)
current_path = getActiveDocumentContext()$path
setwd(dirname(current_path))
setwd("..")
rm(current_path)


#DATA SETS
validation <- read.csv("./datasets/trainingData.csv")

train <- read_csv("./datasets/validationData.csv", na = c("N/A"))


#data sets to h2o cluster
train.h2o <- as.h2o(train)

test.h2o <- as.h2o(validation)


#dependent variable (Lat)
y.dep <- 522

#independent variables (WAPS)
x.indep <- c(1:520)

regression.model <- h2o.glm( y = y.dep, x = x.indep, training_frame = train.h2o, family = "gaussian")

h2o.performance(regression.model)


#make predictions
predict.reg <- as.data.frame(h2o.predict(regression.model, test.h2o))

postResample(predict.reg, validation$LATITUDE)

#Random Forest
system.time(rforest.model <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = train.h2o, 
                                    ntrees = 1000, mtries = 3, max_depth = 4, seed = 1122))

h2o.performance(rforest.model)

h2o.varimp(rforest.model)

#making predictions on unseen data (testing)
system.time(predict.rforest <- as.data.frame(h2o.predict(rforest.model, test.h2o)))

postResample(predict.rforest, validation$LATITUDE)


#GBM
system.time(gbm.model <- h2o.gbm(y=y.dep, x=x.indep, training_frame = train.h2o, 
                       ntrees = 1000, max_depth = 4, learn_rate = 0.01, seed = 1122))

h2o.performance (gbm.model)

predict.gbm <- as.data.frame(h2o.predict(gbm.model, test.h2o))

postResample(predict.gbm, validation$LATITUDE)












