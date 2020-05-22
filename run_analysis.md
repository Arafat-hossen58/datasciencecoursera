.libPaths("G:/package")

library(dplyr)
#import file with tidy format
setwd("D:/course/data science/getdata_projectfiles_UCI HAR Dataset")
var.name<- read.table("UCI HAR Dataset/features.txt", col.names = c("n","var_name"))
var.lab<- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("value", "activity"))
subject_test<-read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test<-read.table("UCI HAR Dataset/test/X_test.txt",col.names=var.name$var_name)
y_test<-read.table("UCI HAR Dataset/test/y_test.txt", col.names ="label")
subject_train<- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train<-read.table("UCI HAR Dataset/train/X_train.txt", col.names =var.name$var_name)
y_train<-read.table("UCI HAR Dataset/train/y_train.txt", col.names = "label")
#bind the dataset with rbind and cbind
#merge the training and test data set
aa <- rbind(x_train, x_test)
bb <- rbind(y_train, y_test)
Subject <- rbind(subject_train, subject_test)
Merged_Data <- cbind(Subject, Y, X)
head(Merged_Data)
#extract the measurements with dplyr
names(Merged_Data)
TidyData <- Merged_Data %>% select(subject,label,contains("mean"),contains("std"))
##labeling the variable

TidyData$label<- var.lab[TidyData$label,2]
names(TidyData)
#appropriately labels the data set

names(TidyData)<-gsub("Acc","Accelerometer",names(TidyData))
names(TidyData)<-gsub("Gyro","Gyroscope",names(TidyData))
names(TidyData)<-gsub("BodyBody","Body",names(TidyData))
##mean the tidy data set
names(TidyData)[2]<-"Activity"
data<-TidyData%>%group_by(Activity,subject)%>%summarise_all(funs(mean))
data