## Read the data files (all the files )
x_train <- read.table("UCI HAR Dataset/train/X_train.txt")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt")

## 1. Merges the training and the test sets to create one data set.
Data <- rbind(x_train, x_test)

remove(x_train)
remove(x_test)

subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
Subject <- rbind(subject_train, subject_test)

remove(subject_train)
remove(subject_test)

y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
Label <- rbind(y_train, y_test)

remove(y_train)
remove(y_test)


## 2. Extracts only the measurements on the mean and standard deviation for each measurement.

features <- read.table("UCI HAR Dataset/features.txt")
filtered_cols <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
Data_Col_Filtered <- Data[, filtered_cols]

## 3 Uses descriptive activity names to name the activities in the data set

activities <- read.table("UCI HAR Dataset/activity_labels.txt")
Label[,1] = activities[Label[,1], 2]


## Appropriately labels the data set with descriptive activity names.
names(Subject) <- "subject"
names(Label) <- "activity"
names(Data_Col_Filtered) <- features[filtered_cols, 2]

tidy_data <- cbind(Subject, Label, Data_Col_Filtered)
write.table(tidy_data, "merged_clean_data.txt", row.name = FALSE)


# 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.

Subjects = unique(Subject)[,1]
Subjects_Count = length(Subjects)
Activities_Count = length(activities[,1])
data_column_count = dim(tidy_data)[2]
final_output = tidy_data[1:(Subjects_Count * Activities_Count),]

row = 1
for (s in 1:Subjects_Count) {
  for (a in 1:Activities_Count) {
    final_output[row, 1] = Subjects[s]
    final_output[row, 2] = activities[a, 2]
    tmp <-
      tidy_data[tidy_data$subject == s &
                tidy_data$activity == activities[a, 2],]
    final_output[row, 3:data_column_count] <- colMeans(tmp[, 3:data_column_count])
    row = row + 1
  }
}
write.table(final_output, "final_output_avg.txt", row.name = FALSE)
remove(list = ls())
