##assigning the file name of the data to be downloaded to a variable for ease of use.
myfile <- "getdata%2Fprojectfiles%2FUCI HAR Dataset.zip"

##creating the directory module3 (where myfile will be saved).
if(!file.exists("module3")){
  dir.create("module3")
}

##downloading myfile.
myfileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(myfileURL, myfile)

##unzips myfile.
mydata <- "UCI HAR Dataset"
if(!file.exists(mydata)){
  unzip(myfile)
}

##reads data from train and test folders.
trainSubj <- read.table(file.path(mydata, "train", "subject_train.txt"))
trainVals <- read.table(file.path(mydata, "train", "X_train.txt"))
trainAct <- read.table(file.path(mydata, "train", "y_train.txt"))
testSubj <- read.table(file.path(mydata, "test", "subject_test.txt"))
testVals <- read.table(file.path(mydata, "test", "X_test.txt"))
testAct <- read.table(file.path(mydata, "test", "y_test.txt"))

##reads the features.
feature <- read.table(file.path(mydata, "features.txt"), as.is = TRUE)

##reads the activity labels.
activity <- read.table(file.path(mydata, "activity_labels.txt"))
colnames(activity) <- c("ActivityNumber", "ActivityType")

##Merge the training and the test sets to create one data set.
train_test <- rbind(cbind(trainSubj, trainVals, trainAct), cbind(testSubj, testVals, testAct))
colnames(train_test) <- c("subject", feature[, 2], "activity")

##Extract only the measurements on the mean and standard deviation for each measurement.
Mean_StDev <- grepl("subject|activity|mean|std", colnames(train_test))
train_test <- train_test[, Mean_StDev]

##Uses descriptive activity names to name the activities in the data set.
train_test$activity <- factor(train_test$activity, levels = activity[, 1], labels = activity[, 2])

##Appropriately labels the data set with descriptive variable names.
train_testCol <- colnames(train_test)
train_testCol <- gsub("^t", "Time", train_testCol)
train_testCol <- gsub("^f", "Frequency", train_testCol)
train_testCol <- gsub("-mean\\(\\)", "Mean", train_testCol)
train_testCol <- gsub("-std\\(\\)", "StdDev", train_testCol)
train_testCol <- gsub("-", "", train_testCol)
train_testCol <- gsub("BodyBody", "Body", train_testCol)

##From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
train_test.Melt <- melt(train_test, id = c("subject", "activity"))
train_test.Mean <- dcast(train_test.Melt, subject + activity ~ variable, mean)

##Creation of independent tidy data set.
write.table(train_test.Mean, "tidy.txt", row.names = FALSE, quote = FALSE)

