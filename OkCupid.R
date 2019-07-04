#Set Working Directory
getwd()
setwd("/Users/daniellebeddoe/Desktop/UofHart/Data Visualization/RProject")

#InstallPackages

library(data.table)
library(stringr)
library(ggplot2)
library(dplyr)
library(mice)
library(tidyr)

#Datacleaning OK Cupid----First Import and remplace blanks with "NA"
Data<-read.csv('profiles.csv', na.strings =c(""))
#remove all essays but essay 5 "The six things I could never do without"
Data$essay0<-NULL
Data$essay1<-NULL
Data$essay2<-NULL
Data$essay3<-NULL
Data$essay4<-NULL
Data$essay6<-NULL
Data$essay7<-NULL
Data$essay8<-NULL
Data$essay9<-NULL
#remove variable "last online" information is NA to analysis
Data$last_online<-NULL
#review data structure
head(Data)
str(Data)
summary(Data)

#Make a Backup of Data before moving onto additional scrubbing
Data_Backup<-Data

#remove rows with more than 10 NA using manual function 'delete.na'

delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}

Data<-delete.na(Data, 10)

#Check missing data for age, gender, and education variables
Data[is.na(Data$age)]
#0 missing for age, do not need to run MICE
Data[is.na(Data$sex),]
#0 missing for age
Data[is.na(Data$education),]
Data[!is.na(Data$education),]
#remove rows with missing education data for analysis
Education<-Data[!is.na(Data$education),]



