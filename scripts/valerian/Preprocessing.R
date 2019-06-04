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


# Import Data
Data_clean <- read.csv(file="../datasets/trainingData.csv", header = TRUE)

# Extract WAPS columns of Data
WAPS <- grep("WAP", colnames(Data_clean), value = TRUE)
no_WAPS <- grep("WAP", colnames(Data_clean), value = TRUE, invert = TRUE)

# Replace x <= 0 & x > -30 & x == 100 by -105
x <- as.data.frame(apply(Data_clean[,WAPS], 2, function(x){ifelse(x <= 0 & x > -30 | x == 100 | x < -90, -105, x)})) #  | x < -90
Data_clean <- cbind(x, Data_clean[,no_WAPS])

# Remove phone 19
Data_clean <- Data_clean[!(Data_clean$PHONEID==19),]

# Remove Feature with var = 0
# nzv <- nearZeroVar(Data_clean, saveMetrics = TRUE)
# nzv <- nzv[,grep("WAP", colnames(nzv), value = TRUE)]
f <- Data_clean[,apply(Data_clean[WAPS], 2, function(x) {var(x) != 0})]
f <- f[,grep("WAP", colnames(f), value = TRUE)]
Data_clean <- cbind(f, Data_clean[,no_WAPS])

# Remove row mean = 100
Data_clean <- Data_clean[apply(Data_clean[grep("WAP", colnames(Data_clean), value = TRUE)], 1, function(x){mean(x) < 100}),]

# Remove dupplicate rows
Data_clean <- distinct(Data_clean)

# New feature for best value
# Data_clean$Max_Wapp <- colnames(Data_clean)[apply(Data_clean[,grep("WAP", colnames(Data_clean))],1,which.max)]

# Check which waps appear in the three buildings

 # c0 <- Data_clean %>% filter(Data_clean$BUILDINGID == 0)
 # c0 <- c0[,apply(c0[grep("WAP", colnames(Data_clean), value = TRUE)], 2, function(x) var(x) != 0)]
 # names0 <- colnames(c0)
 # 
 # c1 <- Data_clean %>% filter(Data_clean$BUILDINGID == 1)
 # c1 <- c1[,apply(c1[grep("WAP", colnames(Data_clean), value = TRUE)], 2, function(x) var(x) != 0)]
 # names1 <- colnames(c1)
 # 
 # c2 <- Data_clean %>% filter(Data_clean$BUILDINGID == 2)
 # c2 <- c2[,apply(c2[grep("WAP", colnames(Data_clean), value = TRUE)], 2, function(x) var(x) != 0)]
 # names2 <- colnames(c2)
 # 
 # Dupplicat_names <- intersect(intersect(names0,names1),names2)
 # test <- unique(names0,names1,names2)
# 
# var(c0$WAP248) # 61
# var(c1$WAP248) # 83
# var(c2$WAP248) # 103

# removing waps that get signal into the 3 buildings at the same time
Data_clean$WAP248 <- NULL
Data_clean$WAP362 <- NULL
Data_clean$WAP413 <- NULL

# Change Name
Training <- Data_clean


# SAME PRE-PROCESS FOR VALIDATION DATA

# Import Testing Set
Testing_data <- read.csv(file="../datasets/validationData.csv", header = TRUE)

# Replace x <= 0 & x > -30 (phone 19 / user 6) | x == 100 by -105
x <- as.data.frame(apply(Testing_data[,grep("WAP", names(Testing_data), value = TRUE)], 2, function(x){ifelse(x <= 0 & x > -30 | x == 100 | x < -90 , -105, x)})) #  | x < -90
Testing_data <- cbind(x, Testing_data[,no_WAPS])

# Testing_data$MAX_WAPP <- colnames(Testing_data)[apply(Testing_data[,grep("WAP", colnames(Testing_data))],1,which.max)]
Names <- colnames(Training[,grep("WAP", names(Training), value = TRUE)])
no_WAPS <- grep("WAP", colnames(Training), value = TRUE, invert = TRUE)
toMatch <- c(Names, no_WAPS)
Testing <- Testing_data[,grep(paste(toMatch, collapse="|"), names(Testing_data), value=TRUE)]


# Clean environment
rm(list=setdiff(ls(), c("Training", "Testing", "WAPS")))

#Saving pre-processed training and testing
# write.csv(Training, file = "../datasets/01.training_preprocessed.csv", row.names=FALSE)
# write.csv(Testing, file = "../datasets/01.testing_preprocessed.csv", row.names=FALSE)


### GO TO "h2o" SCRIPT TO RUN BUILDING PREDICTION ###
