run_analysis.R

Sun Apr 26 18:23:37 2015
## Merges the training and the test sets
## Extracts only the measurements on the mean and standard deviation 
## Creates a new tidy data set with the average of each variable for each activity and each subject.

run_analysis<-function () {
  
  library(plyr)
  library(dplyr)
  library(reshape2)
  
  #read activity test & train, subject test & train data, features test & train data
  
  
  ActivityTest  <- read.table("Y_test.txt",header = FALSE)
  ActivityTrain <- read.table("Y_train.txt",header = FALSE)
  SubjectTrain <- read.table("subject_train.txt",header = FALSE)
  SubjectTest  <- read.table("subject_test.txt",header = FALSE)
  FeaturesTest  <- read.table("X_test.txt" ,header = FALSE)
  FeaturesTrain <- read.table("X_train.txt",header = FALSE)

  #merging training and test sets into one data frame by using rbind function

  Subject <- rbind(SubjectTrain, SubjectTest)
  Activity<- rbind(ActivityTrain, ActivityTest)
  Features<- rbind(FeaturesTrain, FeaturesTest)

  # assigning names to variables

  names(Subject)<-c("subject")
  names(Activity)<- c("activity")
  FeaturesNames <- read.table("features.txt",head=FALSE)
  names(Features)<- FeaturesNames$V2

  # merging data frames by using cbing

  CombinedData <- cbind(Subject, Activity)
  Data <- cbind(Features, CombinedData)

  # extracting mean & standard measurements

  dataFeaturesNames<-FeaturesNames$V2[grep("mean\\(\\)|std\\(\\)", FeaturesNames$V2)]
  selectNames<-c(as.character(dataFeaturesNames), "subject", "activity" )
  Data<-subset(Data,select=selectNames)

  # renaming activity labels


  Data$activity<-as.character(Data$activity)
  Data$activity[Data$activity == 1] <- "WALKING"
  Data$activity[Data$activity == 2] <- "WALKING_UPSTAIRS"
  Data$activity[Data$activity == 3] <- "WALKING_DOWNSTAIRS"
  Data$activity[Data$activity == 4] <- "SITTING"
  Data$activity[Data$activity == 5] <- "STANDING"
  Data$activity[Data$activity == 6] <- "LAYING"
  Data$activity<-as.factor(Data$activity)
  
  
  #relabel data names

  names(Data)<-gsub("^t", "time", names(Data))
  names(Data)<-gsub("^f", "frequency", names(Data))
  names(Data)<-gsub("Acc", "Accelerometer", names(Data))
  names(Data)<-gsub("Gyro", "Gyroscope", names(Data))
  names(Data)<-gsub("Mag", "Magnitude", names(Data))
  names(Data)<-gsub("BodyBody", "Body", names(Data))
                    
  #create tidy dataset and save it
  
  Data2<-aggregate(. ~subject + activity, Data, mean)
  Data2<-Data2[order(Data2$subject,Data2$activity),]
  write.table(Data2, file = "tidydata.txt",row.name=FALSE)
  
                    
}                  
