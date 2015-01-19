run_analysis <- function() 
{
    # Read all the data for next action.
    x_train         <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
    y_train         <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
    x_test          <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
    y_test          <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
    subject_train   <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
    subject_test    <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
    activity_lables <- read.table("./data/UCI HAR Dataset/activity_labels.txt")
    features        <- read.table("./data/UCI HAR Dataset/features.txt")


    # Step 1. Merges the training and the test sets to create one data set.
    x_all   <- rbind(x_train, x_test)


    # Step 2. Extracts only the measurements on the mean and standard deviation for each measurement.
    colnames(x_all) <- c(as.character(features[,2]))
    Mean            <- grep( "mean()", colnames(x_all), fixed = TRUE )
    SD              <- grep( "std()", colnames(x_all), fixed = TRUE )
    MeanSD          <- x_all[,c(Mean,SD)]


    # Step 3. Uses descriptive activity names to name the activities in the data set.
    y_all                       <- rbind(y_train, y_test)
    activity_all                <- cbind(y_all, MeanSD)
    colnames(activity_all)[1]   <- "Activity"


    # Step 4. Appropriately labels the data set with descriptive activity names.
    activity_lables[,2]         <- as.character(activity_lables[,2])

    for( i in 1:length(activity_all[,1]) )
    {
        activity_all[i,1]   <- activity_lables[ activity_all[i, 1], 2 ]
    }



    # Step 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
    subject_all             <- rbind( subject_train, subject_test)
    all_data                <- cbind( subject_all, activity_all )
    colnames(all_data)[1]   <- "Subject"
    Tidy                    <- aggregate( all_data[,3] ~ Subject + Activity, data = all_data, FUN = "mean" ) # separate all_data[,3], Subject and Activity from data frame "all_data".

    for(i in 4:ncol(all_data))
    {
      Tidy[,i] <- aggregate( all_data[,i] ~ Subject + Activity, data = all_data, FUN= "mean" )[,3]
    }

    colnames(Tidy)[3:ncol(Tidy)] <- colnames(MeanSD)
    write.table(Tidy, file = "finalData.txt")
    final <- read.table("./finalData.txt")
    return(final)

}

