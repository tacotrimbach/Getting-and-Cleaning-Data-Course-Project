# COurse Project 3 - Getting & Cleaning Data

# You should create one R script called run_analysis.R that does the following:

# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Load packages needed
library("reshape2")  

setwd("~/R Course Coursera/Course 3 - Getting & Cleaning Data/Week 4")

#1. Merge the training and the test sets to create one data set.

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

# Download data.zip from the web
download.file(fileUrl, "Dataset.zip")
unzip("Dataset.zip")

setwd("~/R Course Coursera/Course 3 - Getting & Cleaning Data/Week 4/UCI HAR Dataset")

#Reading general data
features        <- read.table("./features.txt",header=FALSE)
activityLabel   <- read.table("./activity_labels.txt",header=FALSE)

#Assign column names
colnames(activityLabel) <- c("ActivityID", "ActivityType")

#Reading training data
subjectTrain    <- read.table("./train/subject_train.txt", header=FALSE)
xTrain          <- read.table("./train/X_train.txt", header=FALSE)
yTrain          <- read.table("./train/y_train.txt", header=FALSE)

#Assign column names
colnames(xTrain)        <- features[,2]
colnames(yTrain)        <- "ActivityID"
colnames(subjectTrain)  <- "SubjectID"

#Merging training data
TrainData       <- cbind(yTrain,subjectTrain,xTrain)

#Reading the test Data
subjectTest     <- read.table("./test/subject_test.txt", header=FALSE)
xTest           <- read.table("./test/X_test.txt", header=FALSE)
yTest           <- read.table("./test/y_test.txt", header=FALSE)

#Assign column names
colnames(xTest)        <- features[,2]
colnames(yTest)        <- "ActivityID"
colnames(subjectTest)  <- "SubjectID"

#Merging training data
TestData        <- cbind(yTrain,subjectTrain,xTrain)

#Merging test and training data
CombinedData    <- rbind(TrainData, TestData)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
MeanStdData <- grepl("mean|std|ActivityID|SubjectID", colnames(CombinedData))
MeanStdData <- CombinedData[,MeanStdData]

# 3. Uses descriptive activity names to name the activities in the data set
actlabels               <- activityLabel[,2]
activity                <- factor(MeanStdData$ActivityID)
levels(activity)        <- actlabels
MeanStdData$ActivityID  <- activity

# 4. Appropriately labels the data set with descriptive variable names.

# Remove parentheses
names(MeanStdData) <- gsub("[()]", "", names(MeanStdData))

# Make correct names
names(MeanStdData) <- make.names(names(MeanStdData))

# Better descriptive names
names(MeanStdData) <- gsub("std", "Std", names(MeanStdData))
names(MeanStdData) <- gsub("mean", "Mean", names(MeanStdData))
names(MeanStdData) <- gsub("Freq", "Frequency", names(MeanStdData))
names(MeanStdData) <- gsub("^f", "Frequency", names(MeanStdData))
names(MeanStdData) <- gsub("^t", "Time", names(MeanStdData))
names(MeanStdData) <- gsub("BodyBody", "Body", names(MeanStdData))
names(MeanStdData) <- gsub("Acc", "Acceleration", names(MeanStdData))
names(MeanStdData) <- gsub("Gyro", "Gyroscope", names(MeanStdData))
names(MeanStdData) <- gsub("Mag", "Magnitude", names(MeanStdData))

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
MeltData <- melt(MeanStdData,(id.vars=c("SubjectID","ActivityID")))
FinalAvgData <- dcast(MeltData, SubjectID + ActivityID ~ variable, mean)
names(FinalAvgData)[-c(1:2)] <- paste("[mean]" , names(FinalAvgData)[-c(1:2)] )

#Download dataset as textfile 
write.table(FinalAvgData, "tidy_data.txt", sep = ",", row.names = FALSE)