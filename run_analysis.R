################################################
# This R script does the following:
# 1. Downloads data collected from the accelerometers from the Samsung Galaxy S smartphone, related to:
#    experiments carried out with a group of 30 volunteers within an age bracket of 19-48 years, where
#    each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING,
#    LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and 
#    gyroscope, it was captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate 
#    of 50Hz. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers 
#    was selected for generating the training data and 30% the test data. 
#
# 2. Merges the training and the test sets to create one data set.
#
# 3. Extracts only the measurements on the mean and standard deviation for each measurement.
#
# 4. Uses descriptive activity names to name the activities in the data set
#
# 5. Appropriately labels the data set with descriptive variable names.
#
# 6. From the data set in step 5, creates a second, independent tidy data set with the average of each 
#    variable for each activity and each subject.
################################################

library(dplyr)

# 1. Downloads and unzip data collected
if (!file.exists("Dataset.zip")) {
  download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", destfile = "Dataset.zip")
}  

if (!file.exists("UCI HAR Dataset")) { 
  unzip("Dataset.zip")
}


# 3. Extracts only the measurements on the mean and standard deviation for each measurement.
# Read training data
training_set <- read.table("UCI HAR Dataset\\train\\X_train.txt")
training_labels <- read.table("UCI HAR Dataset\\train\\y_train.txt")
training_subject <- read.table("UCI HAR Dataset\\train\\subject_train.txt")

# Read test data
test_set <- read.table("UCI HAR Dataset\\test\\X_test.txt")
test_labels <- read.table("UCI HAR Dataset\\test\\y_test.txt")
test_subject <- read.table("UCI HAR Dataset\\test\\subject_test.txt")
  
# Read activity labels, that links the class labels with their activity name
activity_labels = read.table("UCI HAR Dataset\\activity_labels.txt")
colnames(activity_labels) <- c("activityId", "activityLabel")

# Read feature list and get only mean and std dev data
features_list <- read.table("UCI HAR Dataset\\features.txt")  
features_list[,2] <- as.character(features_list[,2])
features_mean_stddev <-  grep(".*mean.*|.*std.*", features_list[,2])
features_mean_stddev.names <- features_list[features_mean_stddev,2]
features_mean_stddev.names <- gsub('-mean', 'Mean', features_mean_stddev.names)  
features_mean_stddev.names <- gsub('-std', 'Std', features_mean_stddev.names)
features_mean_stddev.names <- gsub('[-()]', '', features_mean_stddev.names)

training_set <- cbind(training_subject, training_labels, training_set[features_mean_stddev])
test_set <- cbind(test_subject, test_labels, test_set[features_mean_stddev])

#### 2. Merges the training and the test sets to create one data set
merged_training_test_set <- rbind(training_set, test_set)

# 4. Uses descriptive activity names to name the activities in the data set
# add labels into merged data
colnames(merged_training_test_set) <- c("subject", "activity", features_mean_stddev.names)

# descriptive activity names
merged_training_test_set$activity <- factor(merged_training_test_set$activity, levels = activity_labels[, 1], labels = activity_labels[, 2])

# 5. Appropriately labels the data set with descriptive variable names.
# get column names
merged_training_test_set_columns <- colnames(merged_training_test_set)

# remove special characters
merged_training_test_set_columns <- gsub("[\\(\\)-]", "", merged_training_test_set_columns)

# change names to descriptive name
merged_training_test_set_columns <- gsub("^f", "frequency", merged_training_test_set_columns)
merged_training_test_set_columns <- gsub("^t", "time", merged_training_test_set_columns)
merged_training_test_set_columns <- gsub("Acc", "Accelerometer", merged_training_test_set_columns)
merged_training_test_set_columns <- gsub("Gyro", "Gyroscope", merged_training_test_set_columns)
merged_training_test_set_columns <- gsub("Mag", "Magnitude", merged_training_test_set_columns)
merged_training_test_set_columns <- gsub("Freq", "Frequency", merged_training_test_set_columns)
merged_training_test_set_columns <- gsub("mean", "Mean", merged_training_test_set_columns)
merged_training_test_set_columns <- gsub("Std", "StandardDeviation", merged_training_test_set_columns)

# remove some extra Body strings from some column names
merged_training_test_set_columns <- gsub("BodyBody", "Body", merged_training_test_set_columns)

# replace columns with more descriptive column names
colnames(merged_training_test_set) <- merged_training_test_set_columns

# 6. From the data set in step 5, creates a second, independent tidy data set with the average of each 
#    variable for each activity and each subject.
#    Group by subject and activity and summarise using mean
tidy_data_set <- merged_training_test_set %>% 
  group_by(subject, activity) %>%
  summarise_all(funs(mean))

# output to file "tidy_data_set.txt"
write.table(tidy_data_set, "tidy_data_set.txt", row.names = FALSE, quote = FALSE)
