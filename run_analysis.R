library(dplyr)

#####Download and get data#####

fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
datasetZip <- "UCI HAR Dataset.zip"
if (!file.exists(dataset)){
        download.file(fileURL, datasetZip, method = "curl")
}
dataset <- "UCI HAR Dataset"
if (!file.exists(dataset)){
        unzip(datasetZip)
}

####Read data#####

####Read Train Data####

TrainSubject <- read.table(file.path(dataset, "train", "subject_train.txt"))
TrainSet <- read.table(file.path(dataset, "train", "X_train.txt"))
TrainLabels <- read.table(file.path(dataset, "train", "y_train.txt"))

#####Read Test Data####

TestSubject <- read.table(file.path(dataset, "test", "subject_test.txt"))
TestSet <- read.table(file.path(dataset, "test", "X_test.txt"))
TestLabels <- read.table(file.path(dataset, "test", "y_test.txt"))

####Read Features & Activity Labels####

Features <- read.table(file.path(dataset, "features.txt"), as.is = TRUE)
ActivityLabels <- read.table(file.path(dataset, "activity_labels.txt"))
colnames(ActivityLabels) = c("activityID" ,"activityLabel")

####1. Merge the training and the test sets to create one data set####

trainData <- cbind(TrainSubject, TrainSet, TrainLabels)

testData <- cbind(TestSubject, TestSet, TestLabels)

allData <- rbind(trainData, testData)

colnames(allData) <- c("Subject", Features[, 2], "Activity")

####2. Extract only the measurements on the mean and standard deviation for each measurement####

columns_M_STD <- grep("Subject|Activity|mean|std", colnames(allData))

allData <- allData[, columns_M_STD]

####3. Use descriptive activity names to name the activities in the data set####

allData$Activity <- factor(allData$Activity, levels = ActivityLabels[, 1], labels = ActivityLabels[, 2])

####4. Appropriately label the data set with descriptive variable names####

allDataColumns <- colnames(allData)

allDataColumns <- gsub("[\\(\\)-]", "", allDataColumns)

allDataColumns <- gsub("^f", "frequencyDomain", allDataColumns)
allDataColumns <- gsub("^t", "timeDomain", allDataColumns)
allDataColumns <- gsub("Acc", "Accelerometer", allDataColumns)
allDataColumns <- gsub("Gyro", "Gyroscope", allDataColumns)
allDataColumns <- gsub("Mag", "Magnitude", allDataColumns)
allDataColumns <- gsub("Freq", "Frequency", allDataColumns)
allDataColumns <- gsub("mean", "Mean", allDataColumns)
allDataColumns <- gsub("std", "StD", allDataColumns)

allDataColumns <- gsub("BodyBody", "Body", allDataColumns)

colnames(allData) <- allDataColumns

####5. From the data set in 4, create a second, independent tidy data set with 
####the average of each variable for each activity and each subject####

allDataAverage <- allData %>% 
        group_by(Subject, Activity) %>%
        summarise_each(funs(mean))
write.table(allDataAverage, "tidyDataSet.txt", row.names = FALSE, quote = FALSE)