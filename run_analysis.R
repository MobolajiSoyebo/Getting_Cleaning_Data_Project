
## Getting and Cleaning Data Course Project

## Mobolaji Soyebo


rm(list=ls()


## Section 1 - Merge the training and the test sets to create one data set.


# Working directory to UCI HAR Dataset - unzipped

setwd('/Desktop/Data Science Course/UCI HAR Dataset/');


# Read from files

features     = read.table('./features.txt',header=FALSE); #imports features.txt

activityType = read.table('./activity_labels.txt',header=FALSE); #imports activity_labels.txt

subjectTrain = read.table('./train/subject_train.txt',header=FALSE); #imports subject_train.txt

xTrain       = read.table('./train/x_train.txt',header=FALSE); #imports x_train.txt

yTrain       = read.table('./train/y_train.txt',header=FALSE); #imports y_train.txt



# Assigin column names to data imported above

colnames(activityType)  = c('activityId','activityType');

colnames(subjectTrain)  = "subjectId";

colnames(xTrain)        = features[,2]; 

colnames(yTrain)        = "activityId";



# Create final training set data by merging yTrain, subjectTrain, and xTrain

trainingData = cbind(yTrain,subjectTrain,xTrain);


# Read  the test data

subjectTest = read.table('./test/subject_test.txt',header=FALSE); #imports subject_test.txt

xTest       = read.table('./test/x_test.txt',header=FALSE); #imports x_test.txt

yTest       = read.table('./test/y_test.txt',header=FALSE); #imports y_test.txt



# Assign column names to test data imp

colnames(subjectTest) = "subjectId";

colnames(xTest)       = features[,2]; 

colnames(yTest)       = "activityId";



# Create final test data by merging the xTest, yTest and subjectTest data

testData = cbind(yTest,subjectTest,xTest);



# Combine training and test data to create a final data set

finalData = rbind(trainingData,testData);



# Create a vector for the column names from the finalData

colNames  = colnames(finalData); 



# Section 2 - Extract the measurements on the mean and standard deviation for each measurement


# Create a logicalVector that contains TRUE values for the ID, mean() and stddev() columns and FALSE for others

logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

finalData = finalData[logicalVector==TRUE];



# Section 3 -  Use descriptive activity names to name the activities in the data set


finalData = merge(finalData,activityType,by='activityId',all.x=TRUE);



# Update the colNames vector to include the new column names after merge

colNames  = colnames(finalData); 



# Section 4 - Label the data set with descriptive activity names.


# Clean up the variable names

for (i in 1:length(colNames)) 

{

  colNames[i] = gsub("\\()","",colNames[i])

  colNames[i] = gsub("-std$","StdDev",colNames[i])

  colNames[i] = gsub("-mean","Mean",colNames[i])

  colNames[i] = gsub("^(t)","time",colNames[i])

  colNames[i] = gsub("^(f)","freq",colNames[i])

  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])

  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])

  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])

  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])

  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])

  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])

  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])

};



# Reassigning the new descriptive column names to the finalData set

colnames(finalData) = colNames;



# Section 5 - Create second independent tidy data set with the average of each variable for each activity and each subject.


finalDataNoActivityType  = finalData[,names(finalData) != 'activityType'];



# Summarise the finalDataNoActivityType table to include the mean of each variable for each activity and each subject

tidyData = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);


# Merge the tidyData with activityType to include descriptive acitvity names

tidyData = merge(tidyData,activityType,by='activityId',all.x=TRUE);


# Export the tidyData set 

write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');