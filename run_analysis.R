library(plyr)
library(reshape2)

#read features.txt 
features<-read.table("features.txt",col.names=c("Feature","Feature_Label"))

# identify indecies only on the measurements on the mean and standard deviation for each measurement.

idx<-grep("-mean\\(\\)|-std\\(\\)", features[, 2])

# read activity_labels.txt
labels <- read.table("activity_labels.txt",col.names=c("Activity","Activity_Label"))

#read traning data sets
x_train<-read.table("train/X_train.txt",col.names=features[[2]])
x_train<-x_train[,idx]
y_train<-read.table("train/y_train.txt",col.names="Activity")
s_train<-read.table("train/subject_train.txt",col.names="Subject")

#read testing data sets
x_test<-read.table("test/X_test.txt",col.names=features[[2]])
x_test<-x_test[,idx]
y_test<-read.table("test/y_test.txt",col.names="Activity")
s_test<-read.table("test/subject_test.txt",col.names="Subject")

# row bind traning and testing data sets
x<-rbind(x_test,x_train)
y<-rbind(y_test,y_train)
s<-rbind(s_test,s_train)

#column bind to create one single dataset
xys<-cbind(s,y,x)


# merge labels and xys data set by "Activity"
xys_1<-merge(labels,xys,by.x="Activity",by.y="Activity")

# remove numeric activity column
xys_1<-xys_1[,-1]

#created melted dataset by specifying the identity variables
melted <- melt(xys_1, id.vars=c("Subject", "Activity_Label"))

#create melted_mean dataset which has the mean of all values at Subject/Activity_Level/varaiable level
melted_mean<-ddply(melted, c("Subject", "Activity_Label", "variable"), summarise,
                   mean = mean(value))

#reshape the above dataset 
final <- dcast(melted_mean, Subject + Activity_Label ~ variable,mean)

#write the final tidy dataset to "Final.txt" file
write.table(final,"final.txt",row.name=FALSE )