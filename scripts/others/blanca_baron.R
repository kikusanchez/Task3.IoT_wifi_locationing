pacman::p_load(readr, caret, dplyr, scatterplot3d, ggplot2, tidyr, doSNOW, parallel, ranger, e1071)

setwd("../Task3.IoT_wifi_locationing/")
validationData <- read.csv("../Task3.IoT_wifi_locationing/datasets/validationData.csv", stringsAsFactors = FALSE)
Training.data<-read.csv("../Task3.IoT_wifi_locationing/datasets/trainingData.csv", stringsAsFactors = FALSE ) 

head(Training.data[,515:529])

head(validationData[,515:529])

# Convert longitud and latitude values to absolute values into Training and validation data: 
Training.data$LATITUDE<- Training.data$LATITUDE -min(Training.data$LATITUDE)
Training.data$LATITUDE<-round(Training.data$LATITUDE, digits = 1)
Training.data$LONGITUDE<-Training.data$LONGITUDE -min(Training.data$LONGITUDE)
Training.data$LONGITUDE<-round(Training.data$LONGITUDE, digits = 1)

plotForUSer<-function(builNum) {
  builId<- paste("B.", builNum, sep = "" )
  plotTitle<- paste("Building", builNum, ": Users in floors", sep=" ")
  
  message(paste("building Id:", builId))
  message(paste("Plot title:", plotTitle))
  
  
  Training.data%>%filter(BUILDINGID==builId)%>%
    ggplot()+
    geom_point(aes(x=LONGITUDE, y= LATITUDE, color=USERID)) + 
    facet_grid(. ~ FLOOR) + 
    labs(title=plotTitle) + 
    theme_linedraw(base_size = 11, base_family = "") + 
    theme(plot.title = element_text(hjust = 0.5, face="bold"))
}

plotForUSer("1")
