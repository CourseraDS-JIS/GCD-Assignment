library(dplyr)

if (!file.exists("./test/subject_test.txt") | !file.exists("./test/X_test.txt") | !file.exists("./test/Y_test.txt")) {
  print("Test resource missing")
} else {
  if (!file.exists("./train/subject_train.txt") | !file.exists("./train/X_train.txt") | !file.exists("./train/Y_train.txt")) {
      print("Train resource missing")
  } else {
      sub <- read.table("./test/subject_test.txt")
      x <- read.table("./test/X_test.txt")
      y <- read.table("./test/Y_test.txt")
      test <- cbind(sub,x,y, groupId = as.factor("TEST"))

      sub <- read.table("./train/subject_train.txt")
      x <- read.table("./train/X_train.txt")
      y <- read.table("./train/Y_train.txt")
      train <- cbind(sub,x,y, groupId = as.factor("TRAIN"))

      data <- rbind(test,train)

      n <- read.table("features.txt")
      n <- as.character(n[,2])
      n <- c("subject",n, "activity","groupID")
      n <-  make.names(names=n, unique=TRUE, allow_ = TRUE)
      colnames(data) <- n

      data$activities <- factor(data$activity, labels=c("WALKING", "WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING"))

      # refer to the documentation, make names makes mean() into mean..
      # I chose to not include means that had frequencies etc.
      data.analysis.mstd <- select(data, contains("mean..", ignore.case = TRUE),contains("std..", ignore.case = TRUE))
      data.analysis.mstd <- data.analysis.mstd[,order(names(data.analysis.mstd))]
      

      new.data <- cbind(subject =data$subject, activity=data$activities, groupID=data$groupID, data.analysis.mstd)


      new.data <- arrange(new.data, subject, activity)
      names(new.data) <- gsub("\\.\\.", "", names(new.data))
      names(new.data) <- gsub("BodyBody", "Body", names(new.data))
      names(new.data)[4] <- c("angle.tBodyAccJerkMeangravity.Mean")
      new.data <- select(new.data, -angle.tBodyAccJerkMeangravity.Mean)

    
      data_d <- new.data %>% group_by(subject, activity) %>% summarise_each(funs(mean))
      write.table(data_d, "HARdata.csv", sep=",", row.names=FALSE)
    }
}
## Data Citation: [1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012