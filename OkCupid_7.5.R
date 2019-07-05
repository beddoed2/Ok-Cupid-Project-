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
library(mice)

#Datacleaning OK Cupid----First Import and remplace blanks with "NA" and "-1" in Income category with "NA"
Data<-read.csv('profiles.csv', na.strings =c("","-1"))

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



#remove rows with more than 7 (50% of optional responses) missing values using manual function 'delete.na'. 
delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]}

#59K before 
Data<-delete.na(Data, 7)
#56K Observations remaining 

#determine the percentage of missing values for each category 

NA_Percentage<-sort(sapply(Data, function(x) { 
  sum(is.na(x))/56236*100 }), decreasing=TRUE) 

NA_Percentage<-as.data.frame(NA_Percentage)

#variables with missing values greater than 30% of observations are removed from analysis. 
#This includes "income", "offspring", and "Diet
#one can assume data is MNAR (therefore MICE would not be approriate) 
#The variables should not be used in the analysis due to the % of missing observations.
#Income and Children are sensitive topics that people are reluctant to discuss publically. Therefore, questions are MNAR

Data$income<-NULL
Data$offspring<-NULL
Data$diet<-NULL

#18 variables remain

#cleaning data / formatting 
#replace "&rsquo;" in sign" category with "'"

Data$sign<-str_replace_all(Data$sign,"&rsquo;","'")

#replace "<<br/>" in essay 5 with " " 
Data$essay5<-str_replace_all(Data$essay5,"<br />"," ")

#review data structure
str(Data)

#update height to numeric category..Factor Variable Trap, must convert to character first 
Data$height<-as.numeric(as.character(Data$height))
#update sign to factor category
Data$sign<-as.factor(Data$sign)
#Make a Backup of Data before moving on with analysis 
Data_Backup<-Data

#view height and age summarys to indentify outliers
summary(Data$height)
summary(Data$age)
#identify outliers for height and age 
sum(Data$height<48)
sum(Data$height>84)
sum(Data$age>65)
#outliers will remain for analysis

#check summary of other variables 
summary(Data$drinks)
summary(Data$drugs)
summary(Data$education)
summary(Data$smokes)
summary(Data$body_type)
summary(Data$sign)
summary(Data$ethnicity)

#drinks, smoking, body type, and education are MCAR. Can perform MICE
#Drugs and Signs leaving as is.NA is approrpiate response for those variables

#create dataframe to compute missing values for drinks, smoking, body type, and education

imp_data<-cbind.data.frame(Data$drinks,Data$education,Data$smokes,Data$body_type)
str(imp_data)

#run MICE for missing data for Drinks, Drugs, Body Type and Smoking

imp_1<-mice(imp_data)
complete_imp<-mice::complete(imp_1,1)

#check for NA values on imp data
sum(is.na(imp_data))
sum(is.na(complete_imp))
#NA reduced from 13234 to 3260.
#put imp data into main dataset

Data$drinks<-complete_imp$`Data$drinks`
Data$education<-complete_imp$`Data$education`
Data$smokes<-complete_imp$`Data$smokes`
Data$body_type<-complete_imp$`Data$body_type`


#review new NA percentages after MICE
NA_Percentage_Imp_Complete<-sort(sapply(Data, function(x) { 
  sum(is.na(x))/56236*100 }), decreasing=TRUE)
#update as dataframe
NA_Percentage_Imp_Complete<-as.data.frame(NA_Percentage_Imp_Complete)
#MICE reduced Missing Data % for drinks,education,smokes, and body type to under 2% for each variable.
#Data is ready for analysis 

