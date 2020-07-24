#Reading the features
setwd("~/UCI HAR Dataset")

features<-read.table("features.txt")

#Removing the first column of features and converting it to vector
features<-features[,2]

#Reading training data
setwd("~/UCI HAR Dataset/train")
dt_train<-read.table("X_train.txt", col.names = features)

setwd("~/UCI HAR Dataset/test")
dt_test<-read.table("X_test.txt",col.names = features)

#Reading y_test file
test_activity<-read.table("y_test.txt")

#Reading Activity Labels
setwd("~/UCI HAR Dataset")
activity_labels<-read.table("activity_labels.txt")

#Replacing numbers with corresponding activity labels
test_activity<-apply(test_activity,1,function(x){
  switch(x,"WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")
})

setwd("~/UCI HAR Dataset/train")
#Reading the y_train file
train_activity<-read.table("y_train.txt")

#Replacing numbers with corresponding activity labels
train_activity<-apply(train_activity,1,function(x){
       switch(x,"WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")})

#Ceating an activity array with train_activity and test_activity
activity<-c(train_activity,test_activity)

#Merging test and train data set
dt<-rbind(dt_train,dt_test)

#Adding activity column to our merged data set
dt$activityname<-activity

#Extracting only mean and standard deviation from the table 
meanlogi<-sapply(names(dt), function(x)grepl("mean",x,ignore.case = TRUE))
stdlogi<-sapply(names(dt), function(x)grepl("std",x,ignore.case = TRUE))

for (i in 1:562) {
  logi[i]<-meanlogi[i]||stdlogi[i]
}

#Creating average of each activity 
 logi[562]<-TRUE
 tidydata<-dt[logi]
 
 #Grouping data as per Activity
 sp<-split(tidydata,factor(tidydata[,87]))

 #Calculating the Average  wrt activity
 avgact<-data.frame()
 
 for (i in seq_along(sp)) {
  avgact[i,1]<-names(sp)[[i]]
   for (j in 1:86) {
     
     avgact[i,j+1]<-colMeans(sp[[i]][j])
   }
   
 }
 colnames(avgact)<-c('Activity',names(tidydata)[1:86])
 #Saving the avgact in .txt format
 write.table(avgact,"final.txt",row.names = FALSE)
