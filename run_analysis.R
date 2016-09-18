##Coursera - Cleaning Data - Course Project
##Link for data: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
##root-data directory: C:\\Data\\R\\Repo\\cleaningdata\\Cleaningdataproject\\data 
##test data directory: C:\\Data\\R\\Repo\\cleaningdata\\Cleaningdataproject\\data\\test
##training data directory: C:\\Data\\R\\Repo\\cleaningdata\\Cleaningdataproject\\data\\train
library(reshape2)
library(dplyr)
library(tidyr)
rm(list = ls())
wd<-"C:\\Data\\R\\Repo\\cleaningdata\\Cleaningdataproject\\data\\"
setwd(wd)
features<-read.table("features.txt", stringsAsFactors=FALSE)
activitylabels<-read.table("activity_labels.txt", stringsAsFactors=FALSE)

##Importing Test datasets
xtest<-read.table(".\\test\\X_test.txt")
ytest<-read.table(".\\test\\y_test.txt")
colnames(ytest)[1]<-"act"
subjecttest<-read.table(".\\test\\subject_test.txt")
colnames(subjecttest)[1]<-"sub"
##merging it all together first for test subjects
test<-cbind(xtest, ytest, subjecttest) ##column 562 is activity id and column 563 is subject id

##Importing traing datasets
xtrain<-read.table(".\\train\\X_train.txt")
ytrain<-read.table(".\\train\\y_train.txt")
colnames(ytrain)[1]<-"act"
subjecttrain<-read.table(".\\train\\subject_train.txt")
colnames(subjecttrain)[1]<-"sub"
##merging it all together first for test subjects
train <-cbind(xtrain, ytrain,subjecttrain) ##column 562 is activity id and column 563 is subject id

##2.Creating a combined data set by merging the two datasets
  all<-rbind(test,train)
  names(all)[1:561]<-features$V2
  
##3. Now keeping only the variables that are means and standard deviations
  ##cleaning up variable names
  names(all)<-tolower(gsub("-|)|\\(", "", names(all))) 
  rvart<-all[grepl("(std|std[x|y|z]|mean[x|y|z]|mean|act|sub)$" , names(all))]
  ##some additional cleaning required
  rvar<-rvart[grepl("^[^(angle)]" , names(rvart))]
  rvar<-cbind(rvar, rvart$act)
  names(rvar)[68] <-"act"
  dim(rvar)
  ##only 66 relevant variables and 2 more columns for the subject and activity
  rm(rvart) ##removing the useless dataset
##4. Uses descriptive activity names to name the activities in the data set
  rvar$act <- factor(rvar$act, levels = activitylabels$V1, labels = activitylabels$V2)

##5. Creating a second tidy data set with the average of each variable for each activity and each subject.
##First creating a function called mtab to generate summary tables  
  mtab<-function(var, type=string){ ##takes name of the variable to summarize as a input
    a<-rvar %>%                     ##using rvar
    melt(id=c("sub", "act"), measure.vars=var)  %>% ##melting the data with sub and act as identifiers
    dcast( sub~act, mean) %>% ##using dcast to generate the summary table but activity gets divided into columns
    gather(act, max, -sub)  %>% ##using gather to put act in rows
    arrange(sub, act) ##sorting according to subject and activity
    
    names(a)[3]<-paste0(var,"avg") ##renaming the varirable to input variable + avg
    print(a)
  }
##using lapply to run this over all the variables in rvar; lapply gives a list so using as.data.frame to convert into a dataset
data<-as.data.frame(lapply(names(select(rvar, -sub, -act)), mtab))   
##data frame has multiple duplicate values of sub and act, removing those
tidydata<-data[,grep("[^0-9]$", names(data))]
##removing unnecassry files
rm(data)

##generating self-explanatory labels for activity 
tidydata$act<-gsub("WALKING$", "1.Walking", tidydata$act)
tidydata$act<-gsub("WALKING_UPSTAIRS$", "2.Walking_Upstairs",tidydata$act)
tidydata$act<-gsub("WALKING_DOWNSTAIRS$", "3.Walking_downstairs",tidydata$act)
tidydata$act<-gsub("SITTING$", "4.Sitting",tidydata$act)
tidydata$act<-gsub("STANDING$", "5.Standing",tidydata$act)
tidydata$act<-gsub("LAYING$", "6.Laying",tidydata$act)
##converting into factor
tidydata$act<-factor(tidydata$act)
tidydata<-arrange(tidydata,sub, act)
##tidydata is the final output 
write.table(tidydata, file="tidydata.txt", row.name=FALSE)
