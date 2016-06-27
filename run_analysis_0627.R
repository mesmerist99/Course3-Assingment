### course3 project assignment 

## Assignment :
# create one R script called run_analysis.R that does the following
#1. Merges the training and the test sets to create one data set.
#2. Extracts only the measurements on the mean and standard deviation
#   for each measurement.
#3. Uses descriptive activity names to name the activities in the data set
#4. Appropriately labels the data set with descriptive variable names.
#5. From the data set in step 4, creates a second, independent tidy data set 
#   with the average of each variable for each activity and each subject.

## STEP1. Merges the training and the test sets to create one data set.
# set working directory
setwd("C:/Users/SY.YI/Desktop/R")

# load training dataset
train.x <- read.table("./UCI HAR Dataset/train/X_train.txt")
train.label <- read.table("./UCI HAR Dataset/train/y_train.txt")
train.subject <- read.table("./UCI HAR Dataset/train/subject_train.txt")

# load feature labels dataset
labels <- read.table("./UCI HAR Dataset/features.txt")

# give column names
names(train.x) <- labels$V2
names(train.label) <- c("activity")
names(train.subject) <- c("subject")
train <- cbind(train.x, train.label, train.subject)

# load test dataset
test.x <- read.table("./UCI HAR Dataset/test/X_test.txt")
test.label <- read.table("./UCI HAR Dataset/test/y_test.txt")
test.subject <- read.table("./UCI HAR Dataset/test/subject_test.txt")

# give column names
names(test.x) <- labels$V2
names(test.label) <- c("activity")
names(test.subject) <- c("subject")
test <- cbind(test.x, test.label, test.subject)

# merge the two datasets
MergedDataset <- rbind(train, test)


## STEP2. Extracts only the measurements on the mean and standard deviation
##        for each measurement.

# find meand and std measurement names 
Means <- names(MergedDataset)[grepl("(.*)+mean()+(.*)", names(MergedDataset))]
Stds <- names(MergedDataset)[grepl("(.*)+std()+(.*)", names(MergedDataset))]

# Extracts the measurements on the mean and std for each measurement
MeanData <- subset(MergedDataset, select = Means)
StdData <- subset(MergedDataset, select = Stds)

MeanStdData <- cbind(MeanData, StdData) 

actsub <- cbind(MergedDataset$activity, MergedDataset$subject)
colnames(actsub) <- c("activity","subject")
MeanStdData <- cbind(MeanStdData, actsub) 

names(MeanStdData)

## STEP3. Uses descriptive activity names to name the activities in the data set

# check the activity labels
act.label <- read.table("./UCI HAR Dataset/activity_labels.txt")

#V1                 V2
#1  1            WALKING
#2  2   WALKING_UPSTAIRS
#3  3 WALKING_DOWNSTAIRS
#4  4            SITTING
#5  5           STANDING
#6  6             LAYING

# change the activity variables
MeanStdData$activity[MeanStdData$activity == "1"] <- "WALKING"
MeanStdData$activity[MeanStdData$activity == "2"] <- "WALKING_UPSTAIRS"
MeanStdData$activity[MeanStdData$activity == "3"] <- "WALKING_DOWNSTAIRS"
MeanStdData$activity[MeanStdData$activity == "4"] <- "SITTING"
MeanStdData$activity[MeanStdData$activity == "5"] <- "STANDING"
MeanStdData$activity[MeanStdData$activity == "6"] <- "LAYING"


## STEP4. Appropriately labels the data set with descriptive variable names.

names(MeanStdData)  <- gsub("^t", "Time", names(MeanStdData))
names(MeanStdData)  <- gsub("^f", "Frequency", names(MeanStdData))

## STEP5. From the data set in step4, creates a second, independent tidy dataset 
##        with the average of each variable for each activity and each subject.

library(reshape)

# grouped by activity and subject
TidyData <- melt(data = MeanStdData, id.vars = c("activity","subject"),
                  measure.vars = names(MeanStdData)[1:79])

# summarized in average
TidyData <- cast(data = TidyData3, activity + subject ~ variable, fun = mean)

# create a file
write.table(TidyData, file = "Tidy.txt", row.names = FALSE)

# end

names(TidyData)



