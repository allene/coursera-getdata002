#Set a working directory
#setwd("C:\\Users\\ballene\\Documents\\Coursera\\DataScience")
#Download data for the assignment: 
#download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip","UCI HAR Dataset.zip")
#unzip the file
#unzip("UCI HAR Dataset.zip")
setwd("C:\\Users\\ballene\\Documents\\Coursera\\DataScience\\UCI HAR Dataset")
featuresData <- read.table("features.txt", colClasses=c("NULL", "character"))[[1]]
boolFeaturesDataStd <- grepl("std()", featuresData, fixed=T)
boolFeaturesDataMean <- grepl("mean()", featuresData, fixed=T)


#rename feature labels 
featuresData [boolFeaturesDataStd] <- sapply(featuresData [boolFeaturesDataStd], 
                                  function (s) paste0("StandardDeviation", 
								  gsub("std()","", s, fixed=T)), USE.NAMES=F)
featuresData [boolFeaturesDataMean] <- sapply(featuresData [boolFeaturesDataMean], 
                                   function (s) paste0("Mean", 
								   gsub("mean()","", s, fixed=T)), USE.NAMES=F)

featuresData <- sapply(featuresData, function (s) gsub ("-","", s, fixed=T), USE.NAMES=F)
#initialise
featuresClasses <- rep ("NULL", length(featuresData))
featuresClasses [boolFeaturesDataStd | boolFeaturesDataMean] <- "numeric"

#load activity labels and rename them
activitiesData <- unlist( read.table("activity_labels.txt", 
					colClasses=c("NULL", "character"))[[1]])
activitiesData <- sapply( activitiesData, 
                  function (s) gsub("\\b(\\w)","\\U\\1", 
				  tolower(gsub("_", " ", s)), perl=T), USE.NAMES=F)
activitiesData <- as.factor(activitiesData )

#Rubber meets the road -- merge data
trainData <- cbind(activityData = activitiesData[read.table("train/y_train.txt")[[1]]],
                    subject = read.table("train/subject_train.txt")[[1]],
                    read.table("train/X_train.txt", colClasses=featuresClasses, 
					col.names=featuresData))

testData <- cbind(activityData = activitiesData[read.table("test/y_test.txt")[[1]]],
                  subject = read.table("test/subject_test.txt")[[1]],
                  read.table("test/X_test.txt", colClasses=featuresClasses, 
				  col.names=featuresData))

mergedData <- rbind(trainData, testData)

library(plyr)
averageData <- ddply(mergedData, .(subject,activityData), numcolwise(mean))

# transform result data 
library(reshape2)
averageData <- melt(averageData, id.vars = c("subject", "activityData"),  variable.name = "feature",   value.name = "mean")

#Store the data
write.table(averageData, file="average.data.txt", sep="\t", row.names=F)
