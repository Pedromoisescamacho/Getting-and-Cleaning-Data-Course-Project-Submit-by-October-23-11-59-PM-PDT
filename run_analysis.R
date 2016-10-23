#please download the data to the directory of your preference
#the following code will prompt you to select a directory, please select the directory where you have the data
rm(list=ls())
setwd(choose.dir())
#the packages reshage2 and dplyr are going to be used. if you don't have it you need to download it
library(reshape2)
library(dplyr)

###################first step. select the needed variables from "X_train" and "X_test"

#there are 6 tables in total to merge. 2 of them have a lot of variables that we don't need so first we are going to 
#select only the variables (columns) that we need and get rid of the other ones. With this will will have a much 
#managable DB. 
#the file "features.txt" has the labels for the variables on two files "X_train" and "X_test". For this two files
#we only need to select the variables of standard deviation and mean (POINT ONE OF THE ASSIGNMENT. 
#the following code does that

features        <- as.character(read.table("./features.txt")[,2]) #converted to char in order to seach though it correctly
var_index       <- grep("mean\\(\\)|std\\(\\)", features) #geting the index of the desiered variables
labels          <- features[var_index] 
labels          <- gsub("-", " ", labels)
labels          <- gsub("\\(\\)", "", labels)

#now we can load 6 tables only taking the variables we are going to be using

x_test          <- read.table("./test/X_test.txt")[,var_index] #dim = 2947 x 66. ONLY TAKING MEAN AND STD
x_train         <- read.table("./train/X_train.txt")[,var_index] #dim = 7352 x 66 ONLY TAKING MEAN AND STD
y_test          <- read.table("./test/Y_test.txt") #activity
y_train         <- read.table("./train/Y_train.txt") #activity
subject_test    <- read.table("./test/subject_test.txt")
subject_train   <- read.table("./train/subject_train.txt")

#POINT ONE OF THE ASSIGNMENT. now that we have the loaded variables we need to merge them.
# bind by columns the test tables
test            <- cbind(subject_test, y_test, x_test) 
# bind by columns the train tables
train           <- cbind(subject_train,y_train,x_train)
#now we can create only one data set by binding test and train objects by rows
dataset         <- rbind(train,test)

#we add names to the variables in order to have a data set more readable.  POINT 4 OF THE ASSIGNMENT
labels          <- features[var_index]  
labels          <- gsub("-", " ", labels) #take away the "-" 
labels          <- gsub("\\(\\)", "", labels) #take away the "()"
colnames(dataset) <- c("subject", "activity", labels)

#since activity and subjects are factor variables it will be better if they are converted to it.
activity_labels <-  read.table("./activity_labels.txt")[,2] #loading the table with the factors
dataset$activity<-  factor(dataset$activity, labels = activity_labels) #creating the factors, POINT 3 OF THE ASSIGNMENT
dataset$subject <-  as.factor(dataset$subject)
sum(is.na(dataset)) #we test to see whether or not it has values as NA, during the testing it had 0

#we group de dataset by subject and activity. then we create a table with the means of all the other variables
grouped_DS      <- melt(dataset) #will use subject and activity as id variables
tidy            <- dcast(grouped_DS, subject + activity ~ variable ,mean) 
tidy            <- arrange(tidy) #sorting the data by subject and then activity
write.table(tidy, "tidy.txt", row.names = FALSE)

#the dimension of tidy is 180 x 68. 