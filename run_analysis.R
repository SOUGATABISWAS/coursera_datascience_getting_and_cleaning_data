# Step1. Merges the training and the test sets to create one data set.
# setwd("H:/sougata courses 2014/online courses/coursera/john hopkins university/data science/III getting & cleaning data/gcd_Rdirectory")
x_train <- read.table("X_train.txt")
dim(x_train) # 7352*561
head(x_train)
y_train <- read.table("y_train.txt")
table(y_train)
subject_train <- read.table("subject_train.txt")
x_test <- read.table("X_test.txt")
dim(x_test) # 2947*561
y_test <- read.table("y_test.txt") 
table(y_test) 
subject_test <- read.table("subject_test.txt")
x_Data <- rbind(x_train, x_test)
dim(x_Data) # 10299*561
y_Data <- rbind(y_train, y_test)
dim(y_Data) # 10299*1
joinSubject <- rbind(subject_train, subject_test)
dim(joinSubject) # 10299*1

# Step2. Extracts only the measurements on the mean and standard 
# deviation for each measurement. 
features <- read.table("features.txt")
dim(features)  # 561*2
meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
length(meanStdIndices) # 66
x_Data <- x_Data[, meanStdIndices]
dim(x_Data) # 10299*66
names(x_Data) <- gsub("\\(\\)", "", features[meanStdIndices, 2]) # remove "()"
names(x_Data) <- gsub("mean", "Mean", names(x_Data)) # capitalize M
names(x_Data) <- gsub("std", "Std", names(x_Data)) # capitalize S
names(x_Data) <- gsub("-", "", names(x_Data)) # remove "-" in column names 

# Step3. Uses descriptive activity names to name the activities in the data set
activity <- read.table("activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activity_y <- activity[y_Data[, 1], 2]
y_Data[, 1] <- activity_y
names(activity_y) <- "activity"

# Step4. Appropriately labels the data set with descriptive activity names. 
names(joinSubject) <- "subject"
cleanedData <- cbind(joinSubject, y_Data, x_Data)
dim(cleanedData) # 10299*68
write.table(cleanedData, "merged_data.txt") # write out the 1st dataset

# Step5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
subject_length <- length(table(joinSubject)) # 30
activity_length <- dim(activity)[1] # 6
column_length <- dim(cleanedData)[2]
end_result <- matrix(NA, nrow=subject_length*activity_length, ncol=column_length) 
result <- as.data.frame(end_result)
colnames(result) <- colnames(cleanedData)
row <- 1
for(i in 1:subject_length) {
    for(j in 1:activity_length) {
        end_result[row, 1] <- sort(unique(joinSubject)[, 1])[i]
        end_result[row, 2] <- activity[j, 2]
        bool1 <- i == cleanedData$subject
        bool2 <- activity[j, 2] == cleanedData$activity
        result[row, 3:column_length] <- colMeans(cleanedData[bool1&bool2, 3:column_length])
        row <- row + 1
    }
}
head(end_result)
write.table(end_result, "data_with_means.txt") # write out the 2nd dataset

# data <- read.table("./data_with_means.txt")
# data[1:12, 1:3]
