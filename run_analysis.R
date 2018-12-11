library(reshape2)

GenerateTidyDateaSet <- function(dataPath = "UCI HAR Dataset/") {
  ## 2. Process subjects
  subject_train <- read.table( paste(dataPath, "train/subject_train.txt", sep="") );
  subject_test <- read.table( paste(dataPath, "test/subject_test.txt", sep="") );
  # Merge datasets
  subject <- rbind(subject_train, subject_test)
  # set colname to "subject"
  colnames(subject) <- "subject"
  
  
  ## 3. Process activity
  y_train <- read.table( paste(dataPath, "train/y_train.txt", sep="") );
  y_test <- read.table( paste(dataPath, "test/y_test.txt", sep="") );
  # Merge datasets
  y <- rbind(y_train, y_test)
  # Get activity labels
  activity_labels <- read.table( paste(dataPath, "activity_labels.txt", sep="") );
  # Decode activity labels
  activity <- merge(y, activity_labels, by=1)[,2]
  
  
  ## 4. Process features
  X_test <- read.table( paste(dataPath, "test/X_test.txt", sep="") );
  X_train <- read.table( paste(dataPath, "train/X_train.txt", sep="") );
  # Merge datasets
  X <- rbind(X_train, X_test)
  # Get features column names
  features <- read.table( paste(dataPath, "features.txt",sep="") );
  # set colnames by faeatures
  colnames(X) <- features[, 2]
  
  
  ## 5. Combine datasets
  data <- cbind(subject, activity, X)
  
  
  ## 6. Extracts only the measurements on the mean and standard deviation for each measurement
  # Contains: -mean()-X; -mean()-Y; -mean()-Z; -mean(); -std()-X; -std()-Y; -std()-Z; -std()
  search <- grep("-mean\\(\\)|-std\\(\\)", colnames(data))
  extracted_data <- data[,c(1,2,search)]
  
  ## 7. Compute the means, grouped by activity/subject
  melted = melt(extracted_data, id.var = c("activity", "subject"))
  # Applying mean function to the melted dataset
  result = dcast(melted, activity + subject ~ variable, mean)
  
  
  ## 8. Writing the result to the file
  write.table(result, file="EachVariable_EachActivity_EachSubject_Average.txt", sep=";", row.names=FALSE)
}



