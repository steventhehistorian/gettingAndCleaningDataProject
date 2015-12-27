## This script is for the course project for week 3 of the Coursera Getting and Cleaning Data course, Dec 2015.
# By Steven Reddick, Dec 2015.
library(dplyr)
library(reshape2)
library(plyr)

        

        # Reads in data assuming the dl'd file is unzipped in the user's working directory.
        testNames <- read.table("./UCI HAR Dataset/features.txt", quote="\"", comment.char="", stringsAsFactors=FALSE, row.names = NULL)
        trainNames <- read.table("./UCI HAR Dataset/features.txt", quote="\"", comment.char="", stringsAsFactors=FALSE, row.names = NULL)
        testSet <- read.table("./UCI HAR Dataset/test/X_test.txt", quote="\"", comment.char="", stringsAsFactors=FALSE, row.names = NULL)
        names(testSet) <- testNames[,2]
        trainSet <- read.table("./UCI HAR Dataset/train/X_train.txt", quote="\"", comment.char="", stringsAsFactors=FALSE, row.names = NULL)
        names(trainSet) <- trainNames[,2]
        #row.names(trainSet) <- NULL
        #row.names(testSet) <- NULL
        # Adds names to the variables from the downloaded file. 
        # commented out since I rearranged this to be earlier in the code #names(testSet) <- testNames[,2]
        # commented out since I rearranged this to be earlier in the code #names(trainSet) <- trainNames[,2]
        
        # rbind the datasets together into a full set.
        fullSet <- rbind(testSet,trainSet)
        
        # Load subject and activity data and combine with the full set.
        testSubjectNumber <- read.table("./UCI HAR Dataset/test/subject_test.txt", quote="\"", comment.char="", row.names = NULL)
        trainSubjectNumber <- read.table("./UCI HAR Dataset/train/subject_train.txt", quote="\"", comment.char="", row.names = NULL)
        
        testActivityType <- read.table("./UCI HAR Dataset/test/y_test.txt", quote="\"", comment.char="", row.names = NULL)
        trainActivityType <- read.table("./UCI HAR Dataset/train/y_train.txt", quote="\"", comment.char="", row.names = NULL)
        
        testSubjectNumber <- unlist(testSubjectNumber, use.names = FALSE)
        trainSubjectNumber <- unlist(trainSubjectNumber, use.names = FALSE)
        subjectNumber <- c(testSubjectNumber,trainSubjectNumber)
        fullSet$subjectNumber <- subjectNumber
        
        testActivityType <- unlist(testActivityType, use.names = FALSE)
        trainActivityType <- unlist(trainActivityType, use.names = FALSE)
        activityType <- c(testActivityType,trainActivityType)
        fullSet$activityType <- activityType
        

                # Indices of the columns containing std or mean.
                meanCols <- grep(c("mean()"), names(fullSet), value=FALSE)
                stdCols <- grep(c("std()"), names(fullSet), value=FALSE)
                subjectCols <- grep(c("subjectNumber"), names(fullSet), value=FALSE)
                activityCols <- grep(c("activityType"), names(fullSet), value=FALSE)
                # converts the indices from list form to vector form
                meanCols <- unlist(meanCols, use.names = FALSE)
                stdCols <- unlist(stdCols, use.names = FALSE)
                subjectCols <- unlist(subjectCols, use.names = FALSE)
                activityCols <- unlist(activityCols, use.names = FALSE)
                # creates a single vector of "good" columns to keep and then sorts numerically.
                keepCols <- c(meanCols, stdCols, subjectCols, activityCols) 
                keepCols <- sort(keepCols)
                
                # The set of all means and std.dev's.
                interestedSet <- fullSet[,keepCols]
                
                # Create more informative variable names.
                newNames <- sub("tB","time_B", names(interestedSet))
                newNames <- sub("tG","time_G", newNames)
                newNames <- gsub("f","frequency_", newNames)
                newNames <- gsub(c("[:():]"),"", newNames)
                newNames <- gsub("Acc","Accelerate", newNames)

                # Set updated names to the data frame.
                names(interestedSet) <- newNames
                
                # Creating a tidyset, step 1
                by_subject <- group_by(interestedSet, subjectNumber) 
                by_activity <- group_by(by_subject, activityType)
                by_activity$subjectNumber <- as.factor(by_activity$subjectNumber)
                by_activity$activityType <- as.factor(by_activity$activityType)
                
                #summarizedData <- by_activity %>% group_by(c(activityType, subjectNumber)) %>% summarize_each(funs(mean))
                
                # Creating a tidyset, step 2
                summarizedData <- melt(by_activity, id=c("subjectNumber","activityType"))
                summarizedData <- aggregate(by_activity, list(subjectNumber,activityType), mean)
                
                # Rename columns to make more readable.  Change to factor and make levels more friendly.
                summarizedData <- rename(summarizedData, replace = c("Group.1"="Subject", "Group.2" = "Activity"))
                summarizedData <- summarizedData[,-c(82,83)] 
                summarizedData$Activity <- as.factor(summarizedData$Activity)
                levels(summarizedData$Activity) <- c("Walking", "Walking_Upstairs","Walking_Downstairs","Sitting", "Standing" , "Laying")

                
                # Writes a .txt file with the summarized tidy data.
                write.table(summarizedData, file = "./summarizedData.txt", row.name=FALSE) 
        

