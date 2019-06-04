#### 00. INCLUDES ---------------------------------------------------------------------
#Load Libraries: p_load can install, load,  and update packages
if(require("pacman")=="FALSE"){
  install.packages("pacman")
}


pacman::p_load(rstudioapi,dplyr,tidyr,ggplot2,caret,randomForest,e1071,reshape2,h2o,plotly)

library(ggpubr)
theme_pubr()

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


#DATA SETS

#load data frame
training <- read.csv("./datasets/trainingData.csv")

testing <- read.csv("./datasets/validationData.csv")

#### workshop 22/05/2019 ----
sample <- training %>% group_by(BUILDINGID, FLOOR) %>% sample_n(10)
testing_sample <- testing %>% group_by(BUILDINGID, FLOOR) %>% sample_n(10)

sample$BUILDINGID <- as.factor(sample$BUILDINGID)

rf_reg_sample <- randomForest(y=sample$BUILDINGID, x=sample[,grep("WAP", colnames(sample))], importance=T, method="rf")
rf_reg_sample$confusion

pred_rf_sample <- predict(rf_reg_sample, testing_sample)

rf.metrics_sample <- postResample(testing_sample$BUILDINGID, pred_rf_sample)
rf.metrics_sample # Accuracy 1; Kappa 1

bestmtry_rf<-tuneRF(sample[WAPS], sample$LONGITUDE, ntreeTry=100,stepFactor=2,improve=0.05,trace=TRUE, plot=T)

# bestmtry_rf[which(bestmtry_rf[,"OOBError"]==min(bestmtry_rf[,"OOBError"]))]
rf_reg_sample <- randomForest(y=sample$BUILDINGID, x=sample[,grep("WAP", colnames(sample))], importance=T, method="rf", mtry = bestmtry_rf)




# knn

preprocessParams <- preProcess(sample[WAPS], method = c("center", "scale"))

stand_waps <- predict(preprocessParams, sample[WAPS])

stand_dataset<-cbind(stand_waps, BUILDINGID=sample$BUILDINGID,LONGITUDE=sample$LONGITUDE)

stand_dataset$BUILDINGID <- as.factor(stand_dataset$BUILDINGID)
system.time(knn_clasif <- knn3(BUILDINGID ~ as.matrix(stand_dataset[WAPS]), data = stand_dataset))

system.time(knn_clasif_caret <- train(y=stand_dataset$BUILDINGID, x=stand_dataset[WAPS], data = stand_dataset, method="knn"))


#e1071

# Load the packages
library(e1071)
library(caret)


# Train two classification svm (with svm and train)
system.time(svm_clasif <- svm(y = stand_dataset$BUILDINGID, x=stand_dataset[WAPS]))

system.time(svm_clasif_caret<-train(y=stand_dataset$BUILDINGID, x=stand_dataset[WAPS], data = stand_dataset, method="svmLinear"))

# Train two regression svm (with svm and train)
system.time(svm_reg <- svm(y = stand_dataset$LONGITUDE, x=stand_dataset[WAPS]))

system.time(svm_reg_caret<-train(y=stand_dataset$LONGITUDE, x=as.matrix(stand_dataset[WAPS], data = stand_dataset, method="svmLinear")))


#H2O
library(h2o)
h2o.init(nthreads= -1)






#### PRE-PROCESS ----------------------------------------------------------------------------------
#### Understanding the distribution of the data in Training set-------------------------------------------------
# plot the data
plot(training$LONGITUDE, training$LATITUDE)
ggplot(training,
       aes(x=LONGITUDE, y=LATITUDE))+
  geom_point(color="blue", fill="USERID")

# interactive general plot
plot_ly(data = training, x = ~LONGITUDE, y = ~LATITUDE)

# Separate by buildings
building0 <- training %>%
  filter(BUILDINGID == 0)


building1 <- training %>%
  filter(BUILDINGID == 1)

building2 <- training %>%
  filter(BUILDINGID == 2)


# Then each building by floors
# Building 0
building0_floor0 <-  building0 %>%
  filter(FLOOR == 0)

building0_floor1 <-  building0 %>%
  filter(FLOOR == 1)

building0_floor2 <-  building0 %>%
  filter(FLOOR == 2)

building0_floor3 <-  building0 %>%
  filter(FLOOR == 3)

# Building 1
building1_floor0 <-  building1 %>%
  filter(FLOOR == 0)

building1_floor1 <-  building1 %>%
  filter(FLOOR == 1)

building1_floor2 <-  building1 %>%
  filter(FLOOR == 2)

building1_floor3 <-  building1 %>%
  filter(FLOOR == 3)


# Building 2
building2_floor0 <-  building2 %>%
  filter(FLOOR == 0)

building2_floor1 <-  building2 %>%
  filter(FLOOR == 1)

building2_floor2 <-  building2 %>%
  filter(FLOOR == 2)

building2_floor3 <-  building2 %>%
  filter(FLOOR == 3)

building2_floor4 <-  building2 %>%
  filter(FLOOR == 4)

# Plotting the distribution of the results building by building and floor by floor
plot_all <- ggplot(training) + geom_point(aes(x=LONGITUDE, y=LATITUDE, color=as.factor(USERID)))+ ggtitle("ALL BUILDINGS AND FLOORS")
plot_b0f0 <- ggplot(building0_floor0) + geom_point(aes(x=LONGITUDE, y=LATITUDE, color=as.factor(USERID))) + ggtitle("BUILDING 0, FLOOR 0")
plot_b0f1 <- ggplot(building0_floor1) + geom_point(aes(x=LONGITUDE, y=LATITUDE, color=as.factor(USERID))) + ggtitle("BUILDING 0, FLOOR 1")
plot_b0f2 <- ggplot(building0_floor2) + geom_point(aes(x=LONGITUDE, y=LATITUDE, color=as.factor(USERID))) + ggtitle("BUILDING 0, FLOOR 2")
plot_b0f3 <- ggplot(building0_floor3) + geom_point(aes(x=LONGITUDE, y=LATITUDE, color=as.factor(USERID))) + ggtitle("BUILDING 0, FLOOR 3")
plot_b1f0 <- ggplot(building1_floor0) + geom_point(aes(x=LONGITUDE, y=LATITUDE, color=as.factor(USERID))) + ggtitle("BUILDING 1, FLOOR 0")
plot_b1f1 <- ggplot(building1_floor1) + geom_point(aes(x=LONGITUDE, y=LATITUDE, color=as.factor(USERID))) + ggtitle("BUILDING 1, FLOOR 1")
plot_b1f2 <- ggplot(building1_floor2) + geom_point(aes(x=LONGITUDE, y=LATITUDE, color=as.factor(USERID))) + ggtitle("BUILDING 1, FLOOR 2")
plot_b1f3 <- ggplot(building1_floor3) + geom_point(aes(x=LONGITUDE, y=LATITUDE, color=as.factor(USERID))) + ggtitle("BUILDING 1, FLOOR 3")
plot_b2f0 <- ggplot(building2_floor0) + geom_point(aes(x=LONGITUDE, y=LATITUDE, color=as.factor(USERID))) + ggtitle("BUILDING 2, FLOOR 0")
plot_b2f1 <- ggplot(building2_floor1) + geom_point(aes(x=LONGITUDE, y=LATITUDE, color=as.factor(USERID))) + ggtitle("BUILDING 2, FLOOR 1")
plot_b2f2 <- ggplot(building2_floor2) + geom_point(aes(x=LONGITUDE, y=LATITUDE, color=as.factor(USERID))) + ggtitle("BUILDING 2, FLOOR 2")
plot_b2f3 <- ggplot(building2_floor3) + geom_point(aes(x=LONGITUDE, y=LATITUDE, color=as.factor(USERID))) + ggtitle("BUILDING 2, FLOOR 3")
plot_b2f4 <- ggplot(building2_floor4) + geom_point(aes(x=LONGITUDE, y=LATITUDE, color=as.factor(USERID))) + ggtitle("BUILDING 2, FLOOR 4")

# Combining same building & floor plots into the same plot
ggarrange(plot_b0f0, plot_b0f1, plot_b0f2, plot_b0f3,
          labels = c("Floor 0", "Floor 1", "Floor 2", "Floor 3"),
          ncol = 2, nrow = 2) +
  ggtitle("BUILDING 0")

ggarrange(plot_b1f0, plot_b1f1, plot_b1f2, plot_b1f3,
          labels = c("Floor 0", "Floor 1", "Floor 2", "Floor 3"),
          ncol = 2, nrow = 2) +
  ggtitle("BUILDING 1")


ggarrange(plot_b2f0, plot_b2f1, plot_b2f2, plot_b2f3, plot_b2f4,
          labels = c("Floor 0", "Floor 1", "Floor 2", "Floor 3"),
          ncol = 2, nrow = 3) +
  ggtitle("BUILDING 2")


#### Understanding the distribution of the data in Testing set-------------------------------------------------
# plot the data
plot(testing$LONGITUDE, testing$LATITUDE)

# interactive general plot
plot_ly(data = testing, x = ~LONGITUDE, y = ~LATITUDE)

# Separate by buildings
building0_test <- testing %>%
  filter(BUILDINGID == 0)


building1_test <- testing %>%
  filter(BUILDINGID == 1)

building2_test <- testing %>%
  filter(BUILDINGID == 2)


# Then each building by floors
# Building 0
building0_floor0_test <-  building0_test %>%
  filter(FLOOR == 0)

building0_floor1_test <-  building0_test %>%
  filter(FLOOR == 1)

building0_floor2_test <-  building0_test %>%
  filter(FLOOR == 2)

building0_floor3_test <-  building0_test %>%
  filter(FLOOR == 3)

# Building 1
building1_floor0_test <-  building1_test %>%
  filter(FLOOR == 0)

building1_floor1_test <-  building1_test %>%
  filter(FLOOR == 1)

building1_floor2_test <-  building1_test %>%
  filter(FLOOR == 2)

building1_floor3_test <-  building1_test %>%
  filter(FLOOR == 3)


# Building 2
building2_floor0_test <-  building2_test %>%
  filter(FLOOR == 0)

building2_floor1_test <-  building2_test %>%
  filter(FLOOR == 1)

building2_floor2_test <-  building2_test %>%
  filter(FLOOR == 2)

building2_floor3_test <-  building2_test %>%
  filter(FLOOR == 3)

building2_floor4_test <-  building2_test %>%
  filter(FLOOR == 4)

# Plotting the distribution of the results building by building and floor by floor
plot_all_test <- ggplot(testing) + geom_point(aes(x=LONGITUDE, y=LATITUDE))+ ggtitle("ALL BUILDINGS AND FLOORS")
plot_b0f0_test <- ggplot(building0_floor0_test) + geom_point(aes(x=LONGITUDE, y=LATITUDE)) + ggtitle("BUILDING 0, FLOOR 0")
plot_b0f1_test <- ggplot(building0_floor1_test) + geom_point(aes(x=LONGITUDE, y=LATITUDE)) + ggtitle("BUILDING 0, FLOOR 1")
plot_b0f2_test <- ggplot(building0_floor2_test) + geom_point(aes(x=LONGITUDE, y=LATITUDE)) + ggtitle("BUILDING 0, FLOOR 2")
plot_b0f3_test <- ggplot(building0_floor3_test) + geom_point(aes(x=LONGITUDE, y=LATITUDE)) + ggtitle("BUILDING 0, FLOOR 3")
plot_b1f0_test <- ggplot(building1_floor0_test) + geom_point(aes(x=LONGITUDE, y=LATITUDE)) + ggtitle("BUILDING 1, FLOOR 0")
plot_b1f1_test <- ggplot(building1_floor1_test) + geom_point(aes(x=LONGITUDE, y=LATITUDE)) + ggtitle("BUILDING 1, FLOOR 1")
plot_b1f2_test <- ggplot(building1_floor2_test) + geom_point(aes(x=LONGITUDE, y=LATITUDE)) + ggtitle("BUILDING 1, FLOOR 2")
plot_b1f3_test <- ggplot(building1_floor3_test) + geom_point(aes(x=LONGITUDE, y=LATITUDE)) + ggtitle("BUILDING 1, FLOOR 3")
plot_b2f0_test <- ggplot(building2_floor0_test) + geom_point(aes(x=LONGITUDE, y=LATITUDE)) + ggtitle("BUILDING 2, FLOOR 0")
plot_b2f1_test <- ggplot(building2_floor1_test) + geom_point(aes(x=LONGITUDE, y=LATITUDE)) + ggtitle("BUILDING 2, FLOOR 1")
plot_b2f2_test <- ggplot(building2_floor2_test) + geom_point(aes(x=LONGITUDE, y=LATITUDE)) + ggtitle("BUILDING 2, FLOOR 2")
plot_b2f3_test <- ggplot(building2_floor3_test) + geom_point(aes(x=LONGITUDE, y=LATITUDE)) + ggtitle("BUILDING 2, FLOOR 3")
plot_b2f4_test <- ggplot(building2_floor4_test) + geom_point(aes(x=LONGITUDE, y=LATITUDE)) + ggtitle("BUILDING 2, FLOOR 4")

# Combining same building & floor plots into the same plot
ggarrange(plot_b0f0_test, plot_b0f1_test, plot_b0f2_test, plot_b0f3_test,
          labels = c("Floor 0", "Floor 1", "Floor 2", "Floor 3"),
          ncol = 2, nrow = 2) +
  ggtitle("BUILDING 0")


ggarrange(plot_b1f0_test, plot_b1f1_test, plot_b1f2_test, plot_b1f3_test,
          labels = c("Floor 0", "Floor 1", "Floor 2", "Floor 3"),
          ncol = 2, nrow = 2) +
  ggtitle("BUILDING 1")


ggarrange(plot_b2f0_test, plot_b2f1_test, plot_b2f2_test, plot_b2f3_test, plot_b2f4_test,
          labels = c("Floor 0", "Floor 1", "Floor 2", "Floor 3"),
          ncol = 2, nrow = 3) +
  ggtitle("BUILDING 2")


#### plotting errors
prediction <- read.csv("../datasets/Pre-processed datasets/Prediction.csv")

# building 0 floor 0
pred_b0_f0 <- prediction %>% filter(BUILDINGID==0, FLOOR==0)
B0F0good <- pred_b0_f0 %>% filter(pred_b0_f0$FLOOR == pred_b0_f0$FLOOR_PRED)
B0F0errors <- pred_b0_f0 %>% filter(pred_b0_f0$FLOOR != pred_b0_f0$FLOOR_PRED)

errors_b0_f0 <- ggplot(B0F0good, aes(x=B0F0good$LONGITUDE, y=B0F0good$LATITUDE)) + 
  geom_point(size = 3) +
  geom_point(data = B0F0errors, aes(x=B0F0errors$LONGITUDE, y=B0F0errors$LATITUDE), color = "red", size = 3)


# building 0 floor 1  
pred_b0_f1 <- prediction %>% filter(BUILDINGID==0, FLOOR==1)
B0F1good <- pred_b0_f1 %>% filter(pred_b0_f1$FLOOR == pred_b0_f1$FLOOR_PRED)
B0F1errors <- pred_b0_f1 %>% filter(pred_b0_f1$FLOOR != pred_b0_f1$FLOOR_PRED)

errors_b0_f1 <- ggplot(B0F1good, aes(x=B0F1good$LONGITUDE, y=B0F1good$LATITUDE)) + 
  geom_point(size = 3) +
  geom_point(data = B0F1errors, aes(x=B0F1errors$LONGITUDE, y=B0F1errors$LATITUDE), color = "red", size = 3)


# building 0 floor 2
pred_b0_f2 <- prediction %>% filter(BUILDINGID==0, FLOOR==2)
B0F2good <- pred_b0_f2 %>% filter(pred_b0_f2$FLOOR == pred_b0_f2$FLOOR_PRED)
B0F2errors <- pred_b0_f2 %>% filter(pred_b0_f2$FLOOR != pred_b0_f2$FLOOR_PRED)

errors_b0_f2 <- ggplot(B0F2good, aes(x=B0F2good$LONGITUDE, y=B0F2good$LATITUDE)) + 
  geom_point(size = 3) +
  geom_point(data = B0F2errors, aes(x=B0F2errors$LONGITUDE, y=B0F2errors$LATITUDE), color = "red", size = 3)


# building 1 floor 0
pred_b1_f0 <- prediction %>% filter(BUILDINGID==1, FLOOR==0)
B1F0good <- pred_b1_f0 %>% filter(pred_b1_f0$FLOOR == pred_b1_f0$FLOOR_PRED)
B0F0errors <- pred_b1_f0 %>% filter(pred_b1_f0$FLOOR != pred_b1_f0$FLOOR_PRED)

errors_b1_f0 <- ggplot(B1F0good, aes(x=B1F0good$LONGITUDE, y=B1F0good$LATITUDE)) + 
  geom_point(size = 3) +
  geom_point(data = B1F0errors, aes(x=B1F0errors$LONGITUDE, y=B1F0errors$LATITUDE), color = "red", size = 3)














#plotForUSer<-function(builNum) {
  builId<- paste("B.", builNum, sep = "" )
  plotTitle<- paste("Building", builNum, ": Users in floors", sep=" ")
  
  message(paste("building Id:", builId))
  message(paste("Plot title:", plotTitle))
  
  as.factor()
  training%>%filter(BUILDINGID==builId)%>%
    ggplot(Training.data)+
    geom_point(aes(x=LONGITUDE, y= LATITUDE, color=as.factor(USERID)))+
    theme_linedraw(base_size = 11, base_family = "") + 
    theme(plot.title = element_text(hjust = 0.5, face="bold"))
}

plotForUSer("1")


install.packages("gmap")
library(ggmap)
bbox <- make_bbox(training$LONGITUDE, training$LATITUDE, f=0.01)
bbox
map <- get_map(bbox)


map <- get_map(location = c(mean(coordinates[1:13,3]), mean(coordinates[1:13,2])), zoom = 13, maptype = "satellite", source = "google")






#### Cleaning the data -----------------------------------------------------------------------




# change timestamp to real dateTime
training$TIMESTAMP <- as_datetime(training$TIMESTAMP,origin = "1970-01-01", tz="UTC")


# Convert longitud and latitude values to absolute values into Training and validation data: 
# training$LATITUDE<- training$LATITUDE -min(training$LATITUDE)
# training$LATITUDE<-round(training$LATITUDE, digits = 2)
# training$LONGITUDE<-training$LONGITUDE -min(training$LONGITUDE)
# training$LONGITUDE<-round(training$LONGITUDE, digits = 2)
# head(training[,515:529])


# coordinates ranges for each building
building2 <- training %>% filter(BUILDINGID==2)
building2 <- building2[,521:522]
plot_ly(data = building2, x = ~LONGITUDE, y = ~LATITUDE)


#### METHOD 2 - keeping only useful variables (WAPS and BUILDINGID for predict the BUILDING) ----

#only waps
training_keep <- training[, grep("WAP", colnames(training), value = TRUE)]

#creating the vector of BUILDINGID
BUILDINGID <- training[, grep("BUIL", colnames(training), value = TRUE)]


# combine both
training_keep <- cbind(training_keep, BUILDINGID)


# applying the same PRE-PROCESS like in method 1 to this new data...

# duplicated rows means bad userchecks (i.e. duplicates) so we can remove them
training_keep <- distinct(training_keep)

# REMOVING WAPS AND USERCHECKS THAT ARE ALWAYS WITH A UNIQUE SIGNAL OF 100 (USELESS)  
# creating a vector only with the WAPS columns (returns TRUES and FALSES)
WAPS <- grep("WAP", colnames(training_keep), value = TRUE)

# saving into the only those TRUE values (length unique is =!1, it means not only the same value in all columns)
# (it means 'not useless waps', it means 'useful waps')
WAPS <- WAPS[apply(training_keep[WAPS], 2, function(x) length(unique(x))!=1)]

# creating a vector only with the no WAPS columns (for combining it after)
no_WAPS <- grep("WAP", colnames(training_keep), value = TRUE, invert = TRUE)

# putting into the original dataset a combination with USEFUL WAPS columns and the 9 last columns
training_keep <- training_keep[,c(WAPS,no_WAPS)]

#same for the rows (checking if length unique is =!1, it means not only the same value in all rows)
WAPS_2 <- training_keep[apply(training_keep[WAPS], 1, function(x) length(unique(x))!=1),]

# saving it into the original dataset
training_keep <- WAPS_2


# TRANSFORM ALL WAP VALUES INTO -105 (FOR BETTER PLOTTING AND ANALYSIS)
# creating a df transforming all 100 values (no signal) and values between 0 & -30 (unreal values) into -105
x <- as.data.frame(apply(training_keep[,WAPS], 2, function(x){ifelse(x <= 0 & x > -30 | x == 100, -105, x)}))

# putting the results into the original df
training_keep <- cbind(x, BUILDINGID)

# REMOVING LOW VARIANCE
v <- training_keep[,apply(training_keep[,WAPS], 2, function(x) {var(x) > 0})]





#updating BUILDINGID vector without low variance
BUILDINGID <- v[, grep("BUIL", colnames(training), value = TRUE)]

# combining columns
training_keep <- cbind(v, BUILDINGID)


waps_signal <- apply(training_keep[,1:465], 2, sum)

training_keep$waps_signal <- waps_signal

waps_signal

ggplot(training_keep,
       aes(x=grep("WAP", colnames(training_keep), value = TRUE), y=waps_signal))+
  geom_bar()

grep("WAP", colnames(training_keep), value = TRUE)



plotly()




#### METHOD 1 - with all the data ----
# duplicated rows means bad userchecks (i.e. duplicates) so we can remove them
training <- distinct(training)

# REMOVING WAPS AND USERCHECKS THAT ARE ALWAYS WITH A UNIQUE SIGNAL OF 100 (USELESS)  
# creating a vector only with the WAPS columns (returns TRUES and FALSES)
WAPS <- grep("WAP", colnames(training), value = TRUE)

# saving into the only those TRUE values (length unique is =!1, it means not only the same value in all columns)
# (it means 'not useless waps', it means 'useful waps')
WAPS <- WAPS[apply(training[WAPS], 2, function(x) length(unique(x))!=1)]

# creating a vector only with the no WAPS columns (for combining it after)
no_WAPS <- grep("WAP", colnames(training), value = TRUE, invert = TRUE)

# putting into the original dataset a combination with USEFUL WAPS columns and the 9 last columns
training <- training[,c(WAPS,no_WAPS)]

#same for the rows (checking if length unique is =!1, it means not only the same value in all rows)
WAPS_2 <- training[apply(training[WAPS], 1, function(x) length(unique(x))!=1),]

# saving it into the original dataset
training <- WAPS_2

remove(WAPS_2)


# TRANSFORM ALL WAP VALUES INTO -105 (FOR BETTER PLOTTING AND ANALYSIS)
# creating a df transforming all 100 values (no signal) and values between 0 & -30 (unreal values) into -105
x <- as.data.frame(apply(training[,WAPS], 2, function(x){ifelse(x <= 0 & x > -30 | x == 100, -105, x)}))

# putting the results into the original df
training <- cbind(x, training[,no_WAPS])



# GROUPING ALL THE DATA INTO THE 933 CHECKPOINTS
# creating a feature with the location (pasting long & lat)
#training$location <- paste(training$LATITUDE, training$LONGITUDE)

# splitting the data into different floors
# floor0_points <- training %>% filter(training$FLOOR == 0)
# floor1_points <- training %>% filter(training$FLOOR == 1)
# floor2_points <- training %>% filter(training$FLOOR == 2)
# floor3_points <- training %>% filter(training$FLOOR == 3)
# floor4_points <- training %>% filter(training$FLOOR == 4)

# creating a vector to later create a df with waps, buildings and floors
#toMatch <- c("WAP", "BUIL", "FL")

# creating a df with the WAP mean of each checkpoint for each floor
# floor0_points <- aggregate(floor0_points[,grep(paste(toMatch, collapse="|"), names(floor0_points), value=TRUE)], list(Name = floor0_points$location), mean)
# floor1_points <- aggregate(floor1_points[,grep(paste(toMatch, collapse="|"), names(floor1_points), value = TRUE)], list(Name = floor1_points$location), mean)
# floor2_points <- aggregate(floor2_points[,grep(paste(toMatch, collapse="|"), names(floor2_points), value = TRUE)], list(Name = floor2_points$location), mean)
# floor3_points <- aggregate(floor3_points[,grep(paste(toMatch, collapse="|"), names(floor3_points), value = TRUE)], list(Name = floor3_points$location), mean)
# floor4_points <- aggregate(floor4_points[,grep(paste(toMatch, collapse="|"), names(floor4_points), value = TRUE)], list(Name = floor4_points$location), mean)

# combining all floors into a unique df
#Avg_data <- rbind(floor0_points, floor1_points, floor2_points, floor3_points, floor4_points)


# REMOVING LOW VARIANCE
v <- training[,apply(training[,WAPS], 2, function(x) {var(x) > 0})]
v <- v[,-c(466:474)]

training <- cbind(v, training[,no_WAPS])

remove(v, x)

# removing the 13 waps that give signal in the 3 buildings at the same time

waps_b0 <- training %>% filter(BUILDINGID==0)
waps_b0 <- grep("WAP", colnames(waps_b0), value = TRUE)

waps_b1 <- training %>% filter(BUILDINGID==1)
waps_b1 <- grep("WAP", colnames(waps_b1), value = TRUE)

waps_b2 <- training %>% filter(BUILDINGID==2)
waps_b2 <- grep("WAP", colnames(waps_b2), value = TRUE)

waps_all <- c(waps_b0, waps_b1, waps_b2)
waps_all <- unique(waps_all)






write.csv2(training, "../Task3.IoT_wifi_locationing/datasets/Pre-processed datasets/training_cleaned.csv")

# HIGHEST WAPs IN EACH ROW
# creating a column with the name of the highest wap in each row
training$max_wap <- colnames(training)[apply(training[,grep("WAP", colnames(training))], 1, which.max)]
training$max_wap

# creating a column with the value of the highest wap in each row
training$max_wap_value <- apply(training[,grep("WAP", colnames(training))], 1, max)
training$max_wap_value


#training_maxwaps<- training[,c(297:307)]


# creating the feature gps coords
# training$gps_coords <- paste(training$LONGITUDE, training$LATITUDE, sep = ",")
# training_maxwaps<- training[,c(297:308)]

write.csv2(training_maxwaps, "../Task3.IoT_wifi_locationing/datasets/Pre-processed datasets/max_waps_2.csv")

#### MODELING -----------------------------------------------------------------------------------

#training <- training
training <- training
testing <- read.csv("../Task3.IoT_wifi_locationing/datasets/validationData.csv")



# PREPARING testing DATASET TO BECOME EQUAL THAN LIKE training DATASET
WAPS <- grep("WAP", colnames(training), value = TRUE)

# TRANSFORM ALL WAP VALUES INTO -105 (FOR BETTER PLOTTING AND ANALYSIS)
# creating a df transforming all 100 values (no signal) and values between 0 & -30 (unreal values) into -105
x <- as.data.frame(apply(testing[,WAPS], 2, function(x){ifelse(x >= -30 | x < 96, -105, x)}))
# putting the results into the original df
testing <- cbind(x, testing[,no_WAPS])


# Getting the same columns like
# putting into the original dataset a combination with USEFUL WAPS columns and the 9 last columns
testing <- training[,c(WAPS,no_WAPS)]


# testing$max_wap <- colnames(testing)[apply(testing[,grep("WAP", colnames(testing))], 1, which.max)]
# testing$max_wap_value <- apply(testing[,grep("WAP", colnames(testing))], 1, max)


as.h2o(training$BUILDINGID)

training_h2o <- as.h2o(training)

# RF
# variables as factors
training$BUILDINGID <- as.factor(training$BUILDINGID)
training$max_wap <- as.factor(training$max_wap)
testing$BUILDINGID <- as.factor(testing$BUILDINGID)
testing$max_wap <- as.factor(testing$max_wap)

install.packages("h2o")
library(h2o)
h2o.init()
train.h2o <- as.h2o(training)
rf_reg <- h2o.randomForest(y=training$BUILDINGID, x=training[,grep("WAP", colnames(training))], train.h2o)




#bestmtry_rf <- tuneRF(training[WAPs], training$BUILDINGID, ntreeTry=100,stepFactor=2,improve=0.05,trace=TRUE, plot=T)



# with caret package
#10 folds repeat 3 times
control <- trainControl(method='repeatedcv', number=10, repeats=3)
rf_reg <- caret::train(BUILDINGID~ max_wap, data=training, method='rf')



#bestmtry_rf <- tuneRF(training[WAPs], training$BUILDINGID, ntreeTry=100,stepFactor=2,improve=0.05,trace=TRUE, plot=T)
rf_reg <- randomForest(y=training$BUILDINGID, x=training[,grep("WAP", colnames(training))], importance=T, method="rf")
rf_reg$confusion

pred_rf <- predict(rf_reg, testing)

rf.metrics <- postResample(testing$BUILDINGID, pred_rf)
rf.metrics # Accuracy 1; Kappa 1

confusionMatrix(table(pred_rf, testing$BUILDINGID))

# save the model to disk
saveRDS(rf_reg, "../Task3.IoT_wifi_locationing/models/rf_buildings.rds")


# KNN
knn_clasif <- knn3(BUILDINGID ~ ., data = training) # Classification
knn_clasif
#knn_reg <- knnreg(LONGITUDE ~ as.matrix(training[WAPs]), data = training)) # Regression

system.time(knn_clasif <- knn3(BUILDINGID ~ as.matrix(training[WAPS]), data = training))

system.time(knn_clasif_caret<-train(y=training$BUILDINGID, x=training[WAPS], data = training, method="knn"))





# SVM
svm_clasif <- svm(y = training$BUILDINGID, x=training$max_wap) # Classification
svm_clasif
#svm_reg <- svm(y = training$LONGITUDE, x=training[WAPs])) # Regression






#### testing ZONE - RESTRICTED AREA --------------------------------------------------------------


# transform all values equal to 100, of entire dataset, into -105 values
#training[training==100] <- -105

# transform, only the values equal to 100 that are inside WAPS columns, into -105 values
#training_waps<-as.data.frame(apply(training[,grep("WAP", colnames(training))], 2, function(x) ifelse (x==100, -105, x)))

# replacing values into the original dataset between columns 1 and 465 (waps columns)
#training[,1:465] <- training_waps

#remove(training_waps)


# looking for bad phones
# sample100 <- sample_n(training, 100)
# sample100[,521:527]<- NULL
# sample100[,522]<- NULL
# sample100 <- sample100 %>% group_by(PHONEID)%>%
#   summarise(mean = sum(apply(sample100, 1, mean)))


# looking for WAP values between 0 and -30
#only_waps <- training[,grep("WAP", colnames(training))]
#from0to_30_waps <- as.data.frame(apply(training[,grep("WAP", colnames(training))], 1, function(r) any(r %in% c(0, -30))))

#sum(from0to_30_waps)


# only_waps$unusual <- from0to_30
# only_waps <- cbind(training[,466:475], only_waps)

# only_unusual_waps <- only_waps %>% filter(only_waps$unusual == TRUE)


# x <- training %>% filter(training$FLOOR == 5) 
# 
# length(unique(x$location)) # 197 - 216 - 222 - 231 - 67 = 933 crosses
# length(unique(training$location))
# length(unique(training$LATITUDE))
# 
# 
# 
# apply(floor0_points[, c(1:465)], 1, mean)








test <- sample_n(training, 100) 
test2 <- test%>%rowwise()%>%mutate(mean=mean(.))
















# keeping only waps in each building
Building0_waps <- Building0 %>% select(starts_with("WAP"))
Building1_waps <- Building1 %>% select(starts_with("WAP"))
Building2_waps <- Building2 %>% select(starts_with("WAP"))

# removing rows (observations) with no signal FUNCTION (100*520)
sum(Building0_waps[1,]) == 52000 # -> TRUE


no_signal <- function(data) {
  
l <- c()

  for (i in 1:nrow(data)) {
    
    if (sum(data[i,]) == 52000) {
      
      l <- c(l, i)   
      
    }
    
    else{
      
    }
    
    l
  }
  
  data <- data[-c(l),]
  
}

# removing no signal rows using the function no_signal
Building0_waps_clean <- no_signal(Building0_waps)
Building1_waps_clean <- no_signal(Building1_waps)
Building2_waps_clean <- no_signal(Building2_waps)




# creating 2 linear regression to split the waps into different buildings
summary(training$LATITUDE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 4864746 4864821 4864852 4864871 4864930 4865017 

summary(training$LONGITUDE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -7691   -7595   -7423   -7464   -7359   -7301 


plot(training$LONGITUDE,training$LATITUDE)

abline(a=0, b=1, h=4864746, v=-7701)

# a, b : single values specifying the intercept and the slope of the line
# h : the y-value(s) for horizontal line(s)
# v : the x-value(s) for vertical line(s)

line1 <- ggplot(training,
       aes(LONGITUDE, LATITUDE))+
  geom_abline(intercept =0 , slope = 1.5)+ xlim(4864746,7701)+ylim(4864746,7701)
  

line1 <- abline(intercept =0 , slope = 1.5)+ xlim(4864746,7701)+ylim(4864746,7701)







# creating a column with the mean of the WAPS signals
training %>% mutate(signal_mean = apply(training[WAPS_ok], 1, mean)) 








#training[,apply(training[WAPS], 2, function(x) length(unique(x))!=1)]

# method 2: works but removes building id and floor columns
training <- training[apply(training[,grep("WAP", names(training), value = TRUE)], 1, function(x){mean(x) < 100}),
             apply(training[,grep("WAP", names(training), value = TRUE)], 2, function(x){mean(x) != 100})]

# method 3: works but removes building id and floor columns
training <- training[apply(training[,c(1:520)], 1, function(x){mean(x) < 100}),
                             apply(training[,-c(521:529)], 2, function(x){mean(x) != 100})]




# VAL's CODE --
library(dplyr)
library(ggplot2)
library(caret)
#library(class)
#library(DMwR)
library(randomForest)
library(e1071)
library(reshape2)

#### Build training Data Set ####

# Import Data
training <- read.csv(file="../Task3.IoT_wifi_locationing/datasets/training.csv", header = TRUE)

# Extract WAPS columns of Data
WAPS <- grep("WAP", colnames(training), value = TRUE)
no_WAPS <- grep("WAP", colnames(training), value = TRUE, invert = TRUE)

# Remove row mean = 100
training <- training[apply(training[WAPS], 1, function(x){mean(x) < 100}),]

# Remove duplicate rows
training <- distinct(training)

# Replace x <= 0 & x > -30 (phone 19) | x == 100 by -105
x <- as.data.frame(apply(training[,WAPS], 2, function(x){ifelse(x <= 0 & x > -30 | x == 100, -105, x)}))
training <- cbind(x, training[,no_WAPS])

# Remove low variance
v <- training[,apply(training[,WAPS], 2, function(x) {var(x) > 0.5})]
v <- v[,-c(209:313)]
training <- cbind(v, training[,no_WAPS])


#### Method 1: Average way ####
training$location <- paste(training$LATITUDE, training$LONGITUDE)


t_0 <- training %>% filter(training$FLOOR == 0)
t_1 <- training %>% filter(training$FLOOR == 1)
t_2 <- training %>% filter(training$FLOOR == 2)
t_3 <- training %>% filter(training$FLOOR == 3)
t_4 <- training %>% filter(training$FLOOR == 4)


toMatch <- c("WAP", "BUIL", "FLOOR")


t_0 <- aggregate(t_0[,grep(paste(toMatch, collapse="|"), names(t_0), value=TRUE)], list(Name = t_0$location), mean)
t_1 <- aggregate(t_1[,grep(paste(toMatch, collapse="|"), names(t_1), value = TRUE)], list(Name = t_1$location), mean)
t_2 <- aggregate(t_2[,grep(paste(toMatch, collapse="|"), names(t_2), value = TRUE)], list(Name = t_2$location), mean)
t_3 <- aggregate(t_3[,grep(paste(toMatch, collapse="|"), names(t_3), value = TRUE)], list(Name = t_3$location), mean)
t_4 <- aggregate(t_4[,grep(paste(toMatch, collapse="|"), names(t_4), value = TRUE)], list(Name = t_4$location), mean)


training <- rbind(t_0, t_1, t_2, t_3, t_4)

rm(list=setdiff(ls(), c("training", "no_WAPS")))
#### Method 2: Sample way ####
toMatch <- c("WAP", "BUIL", "FLOOR")
training <- training[,grep(paste(toMatch, collapse="|"), names(training), value=TRUE)]
training <- training %>% group_by(FLOOR, BUILDINGID) %>% sample_n(10)

rm(list=setdiff(ls(), c("training", "no_WAPS")))


#### Build testing Set ####

testing <- read.csv(file="../Task3.IoT_wifi_locationing/datasets/validationData.csv", header = TRUE)

# Replace x <= 0 & x > -30 (phone 19) | x == 100 by -105
x <- as.data.frame(apply(testing[,grep("WAP", names(testing), value = TRUE)], 2, function(x){ifelse(x <= 0 & x > -30 | x == 100, -105, x)}))
testing <- cbind(x, testing[,no_WAPS])


Names <- colnames(training[,grep("WAP", colnames(training), value = TRUE)])
toMatch <- c(Names, "BUIL", "FLOOR")
testing <- testing[,grep(paste(toMatch, collapse="|"), colnames(testing), value=TRUE)]


rm(list=setdiff(ls(), c("training", "testing")))


#### TRAIN CLASSIFICATION FOR BUILDING ####

training$BUILDINGID <- as.factor(training$BUILDINGID)


# training RF for Building
rf_reg <- randomForest(y=training$BUILDINGID, x=training[,grep("WAP", names(training), value = T)], importance=T, method="rf")
pred_rf <- predict(rf_reg, testing)
rf.metric <- postResample(testing$BUILDINGID, pred_rf)
rf.metric # Avg way: Acc = 0.97 ; Kappa = 0.95 | Sample way: Acc = 0.97 ; Kappa = 0.95

# training SVM for Buildings
svm_clasif <- svm(y = training$BUILDINGID, x=training[grep("WAP", names(training), value = T)])
pred_svm <- predict(svm_clasif, testing[,grep("WAP", names(testing), value = T)])
svm.metric <- postResample(testing$BUILDINGID, pred_svm)
svm.metric # Avg way: Acc = 0.97 ; Kappa = 0.95 | Sample way: Error


# training KNN for Buildings
knn_clasif <- knn3(BUILDINGID ~ ., data = training)
pred_knn <- predict(knn_clasif, newdata = testing, type = "class")
pred.metric <- postResample(testing$BUILDINGID, pred_knn)
pred.metric # Avg way: Error | Sample way: Error


#### TRAIN CLASSIFICATION FOR FLOOR ####
training$FLOOR <- as.factor(training$FLOOR)

# adding the best building prediction to our test df
test <- cbind(testing , pred_svm)

# Split Data between buildings
b <- c(0, 1, 2)
list <- list()

for (i in b) {
  df <- test %>% filter(pred_svm == i)
  list[[length(list)+1]] <- df
}


B_0 <- list[[1]]
B_1 <- list[[2]]
B_2 <- list[[3]]


B_0$FLOOR <- as.factor(B_0$FLOOR)
B_1$FLOOR <- as.factor(B_1$FLOOR)
B_2$FLOOR <- as.factor(B_2$FLOOR)

# training SVM for Floors in building 0
svm_clasif <- svm(y = training$FLOOR, x=training[grep("WAP", names(training), value = T)])
pred_svm <- predict(svm_clasif, B_0[,grep("WAP", names(B_0), value = T)])
svm.metric <- postResample(B_0$FLOOR, pred_svm)
svm.metric # Avg way: Acc = 0.87 ; Kappa = 0.81




  
# training SVM for Floors in building 1
svm_clasif <- svm(y = training$FLOOR, x=training[grep("WAP", names(training), value = T)])
pred_svm <- predict(svm_clasif, B_1[,grep("WAP", names(B_1), value = T)])
svm.metric <- postResample(B_1$FLOOR, pred_svm)
svm.metric # Avg way: Acc = 0.81 ; Kappa = 0.72

# training SVM for Floors in building 2
svm_clasif <- svm(y = training$FLOOR, x=training[grep("WAP", names(training), value = T)])
pred_svm <- predict(svm_clasif, B_2[,grep("WAP", names(B_2), value = T)])
svm.metric <- postResample(B_2$FLOOR, pred_svm)
svm.metric # Avg way: Acc = 0.92 ; Kappa = 0.89






test <- as.data.frame(t(apply(df, 1, function(x) ifelse(x>= min(head(as.numeric(sort(x, decresing = TRUE)), 3)), -110, x))))








#### RSSSI TO METERS 
# Meters  RSSI
# 0.25    -41
# 0.5     -43
# 1       -49
# 2       -65
# 3       -58
# 4       -57
# 5       -67
# 6       -67
# 7       -77
# 8       -70
# 9       -69
# 10      -75
# 12      -72
# 14      -72
# 16      -78
# 18      -83
# 20      -81
# 25      -81
# 30      -75
# 40      -83
# 
# Android device:
# version: 4.4.2
# build_number: LPV79
# model: Nexus 5
# manufacturer: LGE
# 
# Beacon Info:
#   RadBeacon Tag
# Advertisements per second: 10
# Transmit Power: Max
# 
# IPhone 5s Average RSSI @1m: -51
# 
# Distance formula coefficients calculated for these values:
#   Intercept: 0.1820634  
# Multiplier: 0.8229884
# Power: 6.6525179


