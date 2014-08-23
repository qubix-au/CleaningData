# 1.Merges the training and the test sets to create one data set
# I know the files exist here -> R\UCI HAR Dataset
# After reading the README File turns out that the following files will be specifically important;
# - 'features.txt': List of all features.
#- 'activity_labels.txt': Links the class labels with their activity name.
#- 'train/X_train.txt': Training set.
#- 'train/y_train.txt': Training labels.
#- 'test/X_test.txt': Test set.
#- 'test/y_test.txt': Test labels.

# Turns out you need to have a library loaded for this to work.
# Thanks everyone in the forums ;-)
require(reshape2)
# Ok, so how do I merge data. I needed to Skip ahead to Week 3.

#Lets load the features into a table of columns;
features <- read.table("./UCI HAR Dataset/features.txt")
#Lets load teh activity labels as a class;
activityLbls <- read.table("./UCI HAR Dataset/activity_labels.txt")
#And now for the actual data
xTest <- read.table("./UCI HAR Dataset/test/X_test.txt")
yTest <- read.table("./UCI HAR Dataset/test/y_test.txt")
subjectTest <- read.table("./UCI HAR Dataset/test/subject_test.txt")
#Don't forget the training data
yTrain <- read.table("./UCI HAR Dataset/train/y_train.txt")
xTrain <- read.table("./UCI HAR Dataset/train/x_train.txt")
subjectTrain <- read.table("./UCI HAR Dataset/train/subject_train.txt")

#Now to merge the data, Mooohaaahahahahahaha!
yTrainLbls <- merge(yTrain,activityLbls,by="V1")
yTestLbls <- merge(yTest,activityLbls,by="V1")

#I'm assuming that column bind with a row bind is what is expected??
trainData <- cbind(subjectTrain,yTrainLbls,xTrain)
testData <- cbind(subjectTest,yTestLbls,xTest)

#Compile our result set so that we can start doing some extractions.
#This is Part 1.
resultSet <- rbind(trainData,testData)

#Part 2 is all about extracting the standard deviation.
#Note: we only need Mean and Std so Grep our way to a reduced list.
meanStdSubSet <- subset(features,  grepl("(mean\\(\\)|std\\(\\))", features$V2) )
#Add some column headers, Subject, ID and Activity (Part 3 and 4)
colnames(resultSet) <- c("Subject", "Id", "Activity", as.vector(features[,2]))

meanSet <- grep("mean()", colnames(resultSet), fixed=TRUE)
stdSet <- grep("std()", colnames(resultSet), fixed=TRUE)
# Put them together and sort them.
# Must admit bit surprised that they both have 33 entries? Must be right.
meanStdVector <- c(meanSet, stdSet)
meanStdVector <- sort(meanStdVector)
#Finally extract the retrieved data
extractedData <- resultSet[,c(1,2,3,meanStdVector)]

#Part 5 Create a second independent data set.
meltedData <- melt(extractedData, id=c("Subject", "Id", "Activity"))
#meltedData is pretty big. More than half a million rows?
tidyData <- dcast(meltedData, formula = Subject + Id + Activity ~ variable, mean)
#Tidy data looks great, just means and std labels.

#And finally write out the result, rock and roll.
write.table(tidyData, file='./UCI HAR Dataset/tidyDataResult.txt', sep="\t", row.names=FALSE)
