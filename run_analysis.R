proj.init <- function() {
  if (!register(reshape2)) {
    library(reshape2)
  }
  filename <- "dataset.zip"
  
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
  # Make sure reshape2 is loaded
  if (!require(reshape2)) {
    library(reshape2)
  }

  # Load activity labels
  actLabels <- read.table("UCI HAR Dataset/activity_labels.txt")
  actLabels[, 2] <- as.character(actLabels[, 2])
  
  # Load measurements
  mdata <- read.table("UCI HAR Dataset/features.txt")
  mdata[, 2] <- as.character(mdata[, 2])
  
  # Extract only mean and standard deviation
  mdata2 <- grep(".*mean.*|.*std.*", mdata[, 2])
  mdata2.names <- mdata[mdata2, 2]
  mdata2.names = gsub('-mean', 'Mean', mdata2.names)
  mdata2.names = gsub('-std', 'Std', mdata2.names)
  mdata2.names <-
    gsub('[-()]', '', mdata2.names)
  
  
  # Load train datasets
  tFldr <- "UCI HAR Dataset/train/"
  train <-
    read.table(paste0(tFldr, "X_train.txt"))[mdata2]
  trainActivities <- read.table(paste0(tFldr, "Y_train.txt"))
  trainSubjects <-
    read.table(paste0(tFldr, "subject_train.txt"))
  train <- cbind(trainSubjects, trainActivities, train)
  
  # Load test datasets
  testFldr <- "UCI HAR Dataset/test/"
  test <-
    read.table(paste0(testFldr, "X_test.txt"))[mdata2]
  testActivities <- read.table(paste0(testFldr, "Y_test.txt"))
  testSubjects <- read.table(paste0(testFldr, "subject_test.txt"))
  test <- cbind(testSubjects, testActivities, test)
  
  # Merge test and train datasets
  allData <- rbind(train, test)
  
  # Add subject and activity labels
  deflab <- c("subject", "activity")
  colnames(allData) <-
    c(deflab, mdata2.names)
  
  
  # Convert activities & subjects into factors
  allData$activity <-
    factor(allData$activity, levels = actLabels[, 1], labels = actLabels[, 2])
  allData$subject <- as.factor(allData$subject)
  
  allData.melted <- melt(allData, id = deflab)
  allData.mean <-
    dcast(allData.melted, subject + activity ~ variable, mean)
  
  # Record the final results
  write.table(allData.mean,
              "tidy.txt",
              row.names = FALSE,
              quote = FALSE)
}