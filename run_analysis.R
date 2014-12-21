run_analysis<-function( )  
{
  library(dplyr)
  ## Load the package needed
  
  if(!file.exists("./UCI HAR Dataset"))
  {
    print("Where is you'r data~~~")
  }
  
  activity_labels<-read.table("./UCI HAR Dataset/activity_labels.txt")
  features<-read.table("./UCI HAR Dataset/features.txt")
  subject_test<-read.table("./UCI HAR Dataset/test/subject_test.txt")
  X_test<-read.table("./UCI HAR Dataset/test/X_test.txt")
  y_test<-read.table("./UCI HAR Dataset/test/y_test.txt")
  subject_train<-read.table("./UCI HAR Dataset/train/subject_train.txt")
  X_train<-read.table("./UCI HAR Dataset/train/X_train.txt")
  y_train<-read.table("./UCI HAR Dataset/train/y_train.txt")
  ## Load the raw data
  
  test<-cbind(subject_test,y_test,X_test)
  train<-cbind(subject_train,y_train,X_train)
  data<-rbind(test,train)
  ## Merge the test datasets and the train datasets into one dataframe 
  
  display<-agrep("mean",features$V2)
  display<-c(display,agrep("std",features$V2))
  display<-sort(unique(display))
  ## Find the features needed
  
  data<-data[,c(1,2,display+2)]
  ## Exact the features needed
  
  var<-as.character(features[display,2])
  ## Find the colnames
  
  data<-merge(data,activity_labels,by.x="V1.1",by.y="V1",all=T)
  names(data)<-c("labels","subject",var,"activity")
  data<-data[,-1]
  ## Replace the lebals by the activites' names
  
  newdata<-group_by(data,subject,activity)
  newdata<-summarise_each(newdata,funs(mean),-(1:2))
  ## Independent tidy data set with the average of each variable for each activity and each subject.
  
  newdata
  ## Return the tidy dataset
}
