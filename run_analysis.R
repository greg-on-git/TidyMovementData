# This script will perform the following steps:
# 1.Merges the training and the test sets to create one data set.
# 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3.Uses descriptive activity names to name the activities in the data set
# 4.Appropriately labels the data set with descriptive variable names. 
# 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


# 1. Merge the data
# Read the training data from the current working directory
features <- read.table("features.txt", header=FALSE);
activitylabels <- read.table("activity_labels.txt", header=FALSE);
subjecttrain <- read.table("./train/subject_train.txt", header=FALSE);
xtrain <- read.table("./train/x_train.txt", header=FALSE); 
ytrain <- read.table("./train/y_train.txt", header=FALSE); 

subjecttest <- read.table("./test/subject_test.txt", header=FALSE);
xtest <- read.table("./test/x_test.txt", header=FALSE); 
ytest <- read.table("./test/y_test.txt", header=FALSE); 

# Assign appropriate column names to the data  
colnames(activitylabels) <- c("activityId", "activityType");
colnames(subjecttrain) <- "subjectId";
colnames(xtrain) <- features[,2]; 
colnames(ytrain) <- "activityId";

colnames(subjecttest) <- "subjectId";
colnames(xtest) <- features[,2]; 
colnames(ytest) <- "activityId";

# Merge the training data into a single data set
trainingdata <- cbind(ytrain, subjecttrain, xtrain);

# Merge the test data into a single data set
testdata <- cbind(ytest, subjecttest, xtest);

# Combine training and test data to create a final data set
finaldata <- rbind(trainingdata, testdata);

# Create a vector of column names from finaldata.
# This will make later calculations easier.
fdcolumns  <- colnames(finaldata); 

# 2.Extracts only the measurements on the mean and standard deviation for each measurement. 

# Create a logical vector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
logicalcolumns <- (grepl("activity..", fdcolumns) 
	| grepl("subject..", fdcolumns) 
	| grepl("-mean..", fdcolumns) 
	& !grepl("-meanFreq..", fdcolumns) 
	& !grepl("mean..-", fdcolumns) 
	| grepl("-std..", fdcolumns) 
	& !grepl("-std()..-", fdcolumns));

# Subset finaldata, keeping only the columns extracted above
finaldata <- finaldata[logicalcolumns==TRUE];

# 3.Use descriptive activity names to name the activities in the data set.

# Merge finaldata with the activitylabels to bring in activity names
finaldata <- merge(finaldata, activitylabels, by="activityId", all.x=TRUE);

# Update the final data column names to include the new names after merging
fdcolumns  <- colnames(finaldata); 

# 4. Appropriately label the data set with descriptive activity names. 

# Loop through and clean up each variable name. 
for (i in 1:length(fdcolumns)) 
{
  fdcolumns[i] = gsub("\\()","",fdcolumns[i])
  fdcolumns[i] = gsub("-std$","StdDev",fdcolumns[i])
  fdcolumns[i] = gsub("-mean","Mean",fdcolumns[i])
  fdcolumns[i] = gsub("^(t)","time",fdcolumns[i])
  fdcolumns[i] = gsub("^(f)","freq",fdcolumns[i])
  fdcolumns[i] = gsub("([Gg]ravity)","Gravity",fdcolumns[i])
  fdcolumns[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",fdcolumns[i])
  fdcolumns[i] = gsub("[Gg]yro","Gyro",fdcolumns[i])
  fdcolumns[i] = gsub("AccMag","AccMagnitude",fdcolumns[i])
  fdcolumns[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",fdcolumns[i])
  fdcolumns[i] = gsub("JerkMag","JerkMagnitude",fdcolumns[i])
  fdcolumns[i] = gsub("GyroMag","GyroMagnitude",fdcolumns[i])
};

# Reset finaldata's column names with the more descriptive column names 
colnames(finaldata) <- fdcolumns;

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Create a new table without the activityType column
finaldataNoActivityType <- finaldata[,names(finaldata) != 'activityType'];

# Get the mean of each variable for each activity and each subject
tidydata = aggregate(
	finaldataNoActivityType[,names(finaldataNoActivityType) != c('activityId','subjectId')], 
	by=list(activityId=finaldataNoActivityType$activityId, subjectId = finaldataNoActivityType$subjectId), 
	mean);

# Merge the tidydata with activitylabels to include descriptive names
tidydata <- merge(tidydata, activitylabels, by='activityId', all.x=TRUE);

# Save the tidy data to the file system
write.table(tidydata, "./tidyData.txt", row.names=TRUE, sep="\t");	
