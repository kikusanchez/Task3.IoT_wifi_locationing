#### 00. INCLUDES ---------------------------------------------------------------------
#Load Libraries: p_load can install, load,  and update packages
if(require("pacman")=="FALSE"){
  install.packages("pacman")
}


pacman::p_load(rstudioapi,dplyr,tidyr,ggplot2,caret,randomForest,e1071,reshape2,h2o)


# Setting my Plotly API
# Sys.setenv("plotly_username"="kikusanchez")
# Sys.setenv("plotly_api_key"="Uky3F2ELrykJSTQHGBiP")

# Setwd (1º current wd where is the script, then we move back to the
# general folder)
current_path = getActiveDocumentContext()$path
setwd(dirname(current_path))
setwd("..")
rm(current_path)


#DATA SETS

#load data frame
training <- read.csv("./datasets/trainingData.csv")

testing <- read.csv("./datasets/validationData.csv")


#### 01. PRE-PROCESS ------------------------------------------------------------------------

# Extract WAPS columns of Data
WAPS <- grep("WAP", colnames(training), value = TRUE)
no_WAPS <- grep("WAP", colnames(training), value = TRUE, invert = TRUE)

# Replace x <= 0 & x > -30 & x == 100 by -105
x <- as.data.frame(apply(training[,WAPS], 2, function(x){ifelse(x <= 0 & x > -30 | x == 100 | x < -96, -105, x)}))
training <- cbind(x, training[,no_WAPS])

# Remove Feature with var = 0
f <- training[,apply(training[grep("WAP", colnames(training), value = TRUE)], 2, function(x) {var(x) != 0})]
f <- f[,-c(454:460)]
training <- cbind(f, training[,no_WAPS])

# Remove phone 19 (bad phone)
training <- training[!(training$PHONEID==19),]

# Remove row mean = 100
training <- training[apply(training[grep("WAP", colnames(training), value = TRUE)], 1, function(x){mean(x) < 100}),]

# Remove dupplicate rows
training <- distinct(training)

# New feature for best value
# training$Max_Wapp <- colnames(training)[apply(training[,grep("WAP", colnames(training))],1,which.max)]


# Dependent values as factors
training$BUILDINGID <- as.factor(training$BUILDINGID)
training$FLOOR <- as.factor(training$FLOOR)

# Import Testing Set
testing <- read.csv(file="./datasets/validationData.csv", header = TRUE)

# Replace x <= 0 & x > -30 (phone 19) | x == 100 by -105
x <- as.data.frame(apply(testing[,grep("WAP", names(testing), value = TRUE)], 2, function(x){ifelse(x <= 0 & x > -30 | x == 100 | x < -96, -105, x)}))
testing <- cbind(x, testing[,no_WAPS])

Names <- colnames(training[,grep("WAP", names(training), value = TRUE)])
toMatch <- c(Names, no_WAPS)
testing <- testing[,grep(paste(toMatch, collapse="|"), names(testing), value=TRUE)]


# Clean environment (all except training and testing)
rm(list=setdiff(ls(), c("training", "testing")))



#### TRAIN CLASSIFICATION FOR BUILDING ####

#save(rf_reg, file="Building.rda")
#load(file="Building.rda")

# # training_B RF for Building (with a sample of 10 for each floor and building)
# training_B <- training %>% group_by(FLOOR, BUILDINGID) %>% sample_n(10)
# build_rf_sample <- randomForest(y = training_B$BUILDINGID, x = training_B[grep("WAP", names(training_B), value = T)], importance=T, method="rf")
# build_pred_rf_sample <- predict(build_rf, testing)
# rf_metrics_sample <- postResample(testing$BUILDINGID, build_pred_rf)
# rf_metrics_sample # Accuracy 0.96, Kappa 0.94

h2o.init(nthreads = -1)

#random forest with h2o package
#data to h2o cluster
train.h2o <- as.h2o(training)

test.h2o <- as.h2o(testing)


#dependent variable (building)
y.dep <- 457

#independent variables (WAPS)
x.indep <- c(1:453)

system.time(rforest.model <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = train.h2o, 
                                              ntrees = 1000, mtries = 22, max_depth = 4, seed = 1122))

h2o.performance(rforest.model)

h2o.varimp(rforest.model)

#making predictions on unseen data (testing)
system.time(predict.rforest <- as.data.frame(h2o.predict(rforest.model, test.h2o)))

postResample(predict.rforest, testing$BUILDINGID)
predict.rforest



# training RF for Building (with all pre-rpocessed data)
build_rf <- randomForest(y = training$BUILDINGID, x = training[grep("WAP", names(training), value = T)], importance=T, method="rf")
build_pred_rf <- predict(build_rf, testing)
build_metrics_rf <- postResample(testing$BUILDINGID, build_pred_rf)
build_metrics_rf # Accuracy 0.998, Kappa 0.997

# confusion matrix to check the errors
confusionMatrix(table(build_pred_rf, testing$BUILDINGID))

# saving the model
save(build_rf, file="../Task3.IoT_wifi_locationing/models/build_rf.rda")
load(file="../Task3.IoT_wifi_locationing/models/build_rf.rda")



#### 1st WATERFALL MODEL - TRAIN REGRESSION FOR LONG/LATT ####

#create a column is testing set called BUILDINGID with my prediction and renaming the other BUILDINGID column as ORIGINALBUILDINGID
# first change the name of the original building column to ORIGINALBUILDINGID
testing <- rename(testing, ORIGINALBUILDINGID = BUILDINGID) #For renaming dataframe column

# then create a column called as the original one and put inside the results of random forest prediction for building
testing$BUILDINGID <- build_pred_rf

# Split the testing data set between buildings
b <- c(0, 1, 2)
list <- list()
for (i in b) {
  df <- testing %>% filter(build_pred_rf == i)
  list[[length(list)+1]] <- df
}

b_0 <- list[[1]]
b_1 <- list[[2]]
b_2 <- list[[3]]

# Split the training data set between buildings
training_0 <- training %>% filter(training$BUILDINGID == 0) # %>% sample_n(500)
training_1 <- training %>% filter(training$BUILDINGID == 1) # %>% sample_n(500)
training_2 <- training %>% filter(training$BUILDINGID == 2) # %>% sample_n(500)

# RF for LONGITUDE in building 0
long_b0_rf <- randomForest(y = training_0$LONGITUDE, x = training_0[grep("WAP", names(training_0), value = T)], importance=T, method="rf")
long_pred_b0_rf <- predict(long_b0_rf, b_0)
long_metrics_b0_rf <- postResample(b_0$LONGITUDE, long_pred_b0_rf)
long_metrics_b0_rf # RMSE 6.96, Rsquared 0.93, MAE 4.62

save(long_b0_rf, file="../Task3.IoT_wifi_locationing/models/long_b0_rf.rda")


# RF for LATITUDE in building 0
lat_b0_rf <- randomForest(y = training_0$LATITUDE, x = training_0[grep("WAP", names(training_0), value = T)], importance=T, method="rf")
lat_pred_b0_rf <- predict(lat_b0_rf, b_0)
lat_metrics_b0_rf <- postResample(b_0$LATITUDE, lat_pred_b0_rf)
lat_metrics_b0_rf # RMSE 5.98 , Rsquared 0.97, MAE 3.95 

save(lat_b0_rf, file="../Task3.IoT_wifi_locationing/models/lat_b0_rf.rda")

# Plot results
ggplot(b_0) +
  geom_point(aes(x=long_pred_b0_rf, y=lat_pred_b0_rf), color = "red") +
  geom_point(aes(x=LONGITUDE, y=LATITUDE), color = "blue")

# Check for floors
rf_reg <- randomForest(y = training_2$FLOOR, x = training_2[grep("WAP", names(training_2), value = T)], importance=T, method="rf")
pred_long <- predict(rf_reg, B_2)
rf.metric_long <- postResample(B_2$FLOOR, pred_long)
rf.metric_long

confusionMatrix(table(pred_long, B_2$FLOOR))




install.packages("h2o", type="source", repos="http://h2o-release.s3.amazonaws.com/h2o/master/3908/R")
library('h2o')
remoteH2O <- h2o.init(ip='10.226.6.95', startH2O=FALSE) #  Connection successful!

