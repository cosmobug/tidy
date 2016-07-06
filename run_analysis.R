proj.init <- function() {
  library(reshape2)
  
  filename <- "dataset.zip"
  
  ## Download and unzip the dataset:
  if (!file.exists(filename)) {
    fileURL <-
      "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileURL, filename)
  }
  if (!file.exists("UCI HAR Dataset")) {
    unzip(filename)
  }
}


proj.data <- function() {
  # Load activity labels + mdata
  actLabels <- read.table("UCI HAR Dataset/activity_labels.txt")
  actLabels[, 2] <- as.character(actLabels[, 2])
  
  mdata <- read.table("UCI HAR Dataset/features.txt")
  mdata[, 2] <- as.character(mdata[, 2])
  
  # Extract only mean and standard deviation
  mdata2 <- grep(".*mean.*|.*std.*", mdata[, 2])
  mdata2.names <- mdata[mdata2, 2]
  mdata2.names = gsub('-mean', 'Mean', mdata2.names)
  mdata2.names = gsub('-std', 'Std', mdata2.names)
  mdata2.names <-
    gsub('[-()]', '', mdata2.names)
  
  
  # Load the train datasets
  train <-
    read.table("UCI HAR Dataset/train/X_train.txt")[mdata2]
  trainActivities <- read.table("UCI HAR Dataset/train/Y_train.txt")
  trainSubjects <-
    read.table("UCI HAR Dataset/train/subject_train.txt")
  train <- cbind(trainSubjects, trainActivities, train)
  
  # Load the test datasets
  testFldr <- "UCI HAR Dataset/test/"
  test <-
    read.table(paste0(testFldr, "X_test.txt"))[mdata2]
  testActivities <- read.table(paste0(testFldr, "Y_test.txt"))
  testSubjects <- read.table(paste0(testFldr, "subject_test.txt"))
  test <- cbind(testSubjects, testActivities, test)
  
  # Merge datasets
  allData <- rbind(train, test)
  
  # Add labels
  colnames(allData) <-
    c("subject", "activity", mdata2.names)
  
  # turn activities & subjects into factors
  allData$activity <-
    factor(allData$activity, levels = actLabels[, 1], labels = actLabels[, 2])
  allData$subject <- as.factor(allData$subject)
  
  allData.melted <- melt(allData, id = c("subject", "activity"))
  allData.mean <-
    dcast(allData.melted, subject + activity ~ variable, mean)
  
  write.table(allData.mean,
              "tidy.txt",
              row.names = FALSE,
              quote = FALSE)
}