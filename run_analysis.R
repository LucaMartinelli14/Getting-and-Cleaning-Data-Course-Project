library(dplyr)

# *** SECTION: DOWLOAD file *** #

# download zip file 
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
fileZip <- "UCI HAR Dataset.zip"

if (!file.exists(fileZip)) {
  download.file(fileUrl, fileZip, mode = "wb")
}

# unzip zip file
dataMainPath <- "UCI HAR Dataset"
if (!file.exists(dataMainPath)) {
  unzip(fileZip)
}


# *** SECTION: READ file *** #

trainingSubjects <- read.table(file.path(dataMainPath, "train", "subject_train.txt"))
trainingValues <- read.table(file.path(dataMainPath, "train", "X_train.txt"))
trainingActivity <- read.table(file.path(dataMainPath, "train", "y_train.txt"))

testSubjects <- read.table(file.path(dataMainPath, "test", "subject_test.txt"))
testValues <- read.table(file.path(dataMainPath, "test", "X_test.txt"))
testActivity <- read.table(file.path(dataMainPath, "test", "y_test.txt"))

features <- read.table(file.path(dataMainPath, "features.txt"), as.is = TRUE)
activities <- read.table(file.path(dataMainPath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")


##############################################################################
# Step 1 - Merge the training and the test sets to create one data set
##############################################################################

# *** SECTION: DATA SET with training and test *** #

humanActivity <- rbind(
  cbind(trainingSubjects, trainingValues, trainingActivity),
  cbind(testSubjects, testValues, testActivity)
)

rm(trainingSubjects, trainingValues, trainingActivity, 
   testSubjects, testValues, testActivity)

colnames(humanActivity) <- c("subject", features[, 2], "activity")


### SECTION: getting mean and std dev ###

columnsToKeep <- grepl("subject|activity|mean|std", colnames(humanActivity))
humanActivity <- humanActivity[, columnsToKeep]
humanActivity$activity <- factor(humanActivity$activity, 
levels = activities[, 1], labels = activities[, 2])


humanActivityCols <- colnames(humanActivity)
humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)
humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
humanActivityCols <- gsub("std", "StandardDeviation", humanActivityCols)
humanActivityCols <- gsub("BodyBody", "Body", humanActivityCols)
colnames(humanActivity) <- humanActivityCols


### NEW DATA SET ###

humanActivityMeans <- humanActivity %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))

# output to file "new_tidy_data.txt"
write.table(humanActivityMeans, "new_tidy_data.txt", row.names = FALSE, 
quote = FALSE)
