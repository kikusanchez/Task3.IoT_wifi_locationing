#### 00. INCLUDES ---------------------------------------------------------------------
#Load Libraries: p_load can install, load,  and update packages
if(require("pacman")=="FALSE"){
  install.packages("pacman")
}


pacman::p_load(rstudioapi,dplyr,tidyr,ggplot2,caret,randomForest,e1071,reshape2,h2o,plotly)



# Setwd (1º current wd where is the script, then we move back to the
# general folder)
current_path = getActiveDocumentContext()$path
setwd(dirname(current_path))
setwd("..")
rm(current_path)


# DATA SETS
Training <- read.csv("./datasets/trainingData.csv")
Data_clean <- Training

# Data_clean <- cbind(test, Data_clean[,no_WAPS])

Data_clean$location <- paste(Data_clean$LATITUDE, Data_clean$LONGITUDE)

t_0 <- Data_clean %>% filter(Data_clean$FLOOR == 0)
t_1 <- Data_clean %>% filter(Data_clean$FLOOR == 1)
t_2 <- Data_clean %>% filter(Data_clean$FLOOR == 2)
t_3 <- Data_clean %>% filter(Data_clean$FLOOR == 3)
t_4 <- Data_clean %>% filter(Data_clean$FLOOR == 4)

toMatch <- c("WAP", "BUIL", "FLOOR", "LONG", "LAT", "MAX")

t_0 <- aggregate(t_0[,grep(paste(toMatch, collapse="|"), names(t_0), value=TRUE)], list(Name = t_0$location), median)
t_1 <- aggregate(t_1[,grep(paste(toMatch, collapse="|"), names(t_1), value = TRUE)], list(Name = t_1$location), median)
t_2 <- aggregate(t_2[,grep(paste(toMatch, collapse="|"), names(t_2), value = TRUE)], list(Name = t_2$location), median)
t_3 <- aggregate(t_3[,grep(paste(toMatch, collapse="|"), names(t_3), value = TRUE)], list(Name = t_3$location), median)
t_4 <- aggregate(t_4[,grep(paste(toMatch, collapse="|"), names(t_4), value = TRUE)], list(Name = t_4$location), median)

Crosses <- rbind(t_0, t_1, t_2, t_3, t_4)







x <- as.vector(Training %>% group_by(LATITUDE) %>% summarise(n()))$LATITUDE
y <- as.vector(Training %>% group_by(LONGITUDE) %>% summarise(n()))$LONGITUDE


x[findInterval(prediction$LATT_PRED, x)]






