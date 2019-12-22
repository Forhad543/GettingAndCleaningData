# Getting And Cleaning Data
# Author: forhad

# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement.
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names.
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


# Load Packages
library(data.table)
library(reshape2)

# Get the data
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

download.file(url, file.path(getwd(), 'dfiles.zip'))
unzip('dfiles.zip')

# Load data

activity_labels <- fread(file.path(getwd(), "UCI HAR Dataset/activity_labels.txt"), 
                         col.names = c("classLabels", "activityName"))

features <- fread(file.path(getwd(), "UCI HAR Dataset/features.txt")
                  , col.names = c("index", "featureNames"))

features_Wanted <- grep("(mean|std)\\(\\)", features[, featureNames])
measurements <- features[features_Wanted, featureNames]
measurements <- gsub('[()]', '', measurements)

# Load train datasets
train <- fread(file.path(getwd(), "UCI HAR Dataset/train/X_train.txt"))[, features_Wanted, with = FALSE]
data.table::setnames(train, colnames(train), measurements)
train_Activities <- fread(file.path(getwd(), "UCI HAR Dataset/train/Y_train.txt")
                         , col.names = c("Activity"))
train_Subjects <- fread(file.path(getwd(), "UCI HAR Dataset/train/subject_train.txt")
                       , col.names = c("SubjectNum"))
train <- cbind(train_Subjects, train_Activities, train)


# Load test datasets
test <- fread(file.path(getwd(), "UCI HAR Dataset/test/X_test.txt"))[, features_Wanted, with = FALSE]
data.table::setnames(test, colnames(test), measurements)
test_Activities <- fread(file.path(getwd(), "UCI HAR Dataset/test/Y_test.txt")
                        , col.names = c("Activity"))
test_Subjects <- fread(file.path(getwd(), "UCI HAR Dataset/test/subject_test.txt")
                      , col.names = c("SubjectNum"))
test <- cbind(test_Subjects, test_Activities, test)


# merge datasets
data_combined <- rbind(train, test)

# Convert classLabels to activityName basically. More explicit. 
data_combined[["Activity"]] <- factor(data_combined[, Activity]
                                 , levels = activity_labels[["classLabels"]]
                                 , labels = activity_labels[["activityName"]])

data_combined[["SubjectNum"]] <- as.factor(data_combined[, SubjectNum])
data_combined <- reshape2::melt(data = data_combined, id = c("SubjectNum", "Activity"))
data_combined <- reshape2::dcast(data = data_combined, SubjectNum + Activity ~ variable, fun.aggregate = mean)

data.table::fwrite(x = data_combined, file = "tidyData.txt", quote = FALSE)
