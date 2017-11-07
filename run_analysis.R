library(dplyr)
library(reshape2)

#####Download and get data#####

fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
datasetZip <- "UCI HAR Dataset.zip"
if (!file.exists(dataset)){
        download.file(fileURL, datasetZip, method = "curl")
}
dataset <- "UCI HAR Dataset"
if (!file.exists(dataset)){
        unzip(dataset)
}

####Read data#####

####Read Train Data####

TrainSubject <- read.table(file.path(dataset, "train", "subject_train.txt"))
TrainSet <- read.table(file.path(dataset, "train", "X_train.txt"))
TrainLabels <- read.table(file.path(dataset, "train", "Y_train.txt"))

#####Read Test Data####

TestSubject <- read.table(file.path(dataset, "test", "subject_test.txt"))
TestSet <- read.table(file.path(dataset, "test", "X_test.txt"))
TestLabels <- read.table(file.path(dataset, "test", "Y_test.txt"))

####Read Features & Activity Labels####

Features <- read.table(file.path(dataset, "features.txt"))
ActivityLabels <- read.table(file.path(dataset, "activity_labels.txt", colnames = c("activityID" ,"activityLabel")))

####1. Merge the training and the test sets to create one data set####

trainData <- cbind(TrainSubject, TrainSet, TrainLabels)

testData <- cbind(TestSubject, TestSet, TestLabels)

allData <- rbind(trainData, testData)

colnames(allData) <- c("Subject", features[, 2], "Labels")

####2. Extract only the measurements on the mean and standard deviation for each measurement####

columns_M_STD <- grepl("Subject|Labels|mean|std", colnames(allData))

allData <- allData[, columns_M_STD]

####3. Use descriptive activity names to name the activities in the data set####

allData$Labels <- factor(allData$Labels, levels = Labels[, 1], labels = Labels[, 2])

####4. Appropriately label the data set with descriptive variable names####

allDataColumns <- colnames(allData)

allData <- gsub("[\\(\\)-]", "", allDataColumns)

colnames(allData) <- allDataColumns

####5. From the data set in 4, create a second, independent tidy data set with 
####the average of each variable for each activity and each subject####

allDataAverage <- allData %>%
        group_by(Subject, Labels) %>%
        summarise_each(funs(mean))

write.table(allDataAverage, "tidyDataSet.txt", row.names = FALSE, quote = FALSE)