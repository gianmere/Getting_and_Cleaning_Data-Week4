library(data.table)
library(dplyr)

#Download the data, if it's the first time the script runs
if (!file.exists("getdata%2Fprojectfiles%2FUCI HAR Dataset.zip")){
    print("Download the file")
    dataset_url <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(dataset_url, destfile = "getdata%2Fprojectfiles%2FUCI HAR Dataset.zip", method = "curl")
}

unzip("getdata%2Fprojectfiles%2FUCI HAR Dataset.zip")

###Load the features names
dt_labels <- read.table("UCI HAR Dataset/features.txt",header = F, col.names = c("id", "feature"))
labels <- dt_labels$feature


### Load test data
dt_subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", header = F,col.names = c("subject"))

dt_test_labels <- read.table("UCI HAR Dataset/test/y_test.txt",header = F, col.names = c("id_activity"))

dt_test_sets <- read.table("UCI HAR Dataset/test/X_test.txt")
###### Name the column for convenience
names(dt_test_sets) <- labels

dt_test <- cbind(dt_subject_test, dt_test_labels)
dt_test <- cbind(dt_test, dt_test_sets)

### Load train data
dt_subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", header = F, col.names = c("subject"))

dt_train_labels <- read.table("UCI HAR Dataset/train/y_train.txt", header = F, col.names = c("id_activity"))

dt_train_sets <- read.table("UCI HAR Dataset/train/X_train.txt")
###### Name the column for convenience
names(dt_train_sets) <- labels

dt_train <- cbind(dt_subject_train, dt_train_labels)
dt_train <- cbind(dt_train, dt_train_sets)

#1. Merges the training and the test sets to create one data set
dt_all <- rbind(dt_test, dt_train)

#2. Extracts only the measurements on the mean and standard deviation for each measurement.
index_names <- grep("mean\\(\\)|std\\(\\)", names(dt_all))
selected_column = c(1,2,index_names)
dt_all <- dt_all[,selected_column]


#3.Uses descriptive activity names to name the activities in the data set
### Load the activity label
dt_activity_label <- read.table("UCI HAR Dataset/activity_labels.txt", header = F, col.names = c("id","activity"))
dt_with_Activity_label <- merge(dt_all, dt_activity_label, by.x = "id_activity", by.y = "id")

dt_all <- select(dt_with_Activity_label, -id_activity)

#4. Appropriately labels the data set with descriptive variable names.
names(dt_all)<-gsub("Acc", "Accelerometer", names(dt_all))
names(dt_all)<-gsub("Gyro", "Gyroscope", names(dt_all))
names(dt_all)<-gsub("Mag", "Magnitude", names(dt_all))
names(dt_all)<-gsub("^t", "Time", names(dt_all))
names(dt_all)<-gsub("^f", "Frequency", names(dt_all))
names(dt_all)<-gsub("-mean()", "Mean", names(dt_all))
names(dt_all)<-gsub("-std()", "STD", names(dt_all))


#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject

summarize <- dt_all %>% group_by(subject, activity) %>% summarise_all(funs(mean))
write.table(summarize, file = "./tidy_data.txt", row.name=FALSE)
