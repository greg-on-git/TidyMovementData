## The Data
The tidyData.txt data set contains information from sensors in smartphones while the owner carried out various activities.  This data helps us to understand real-life data for purposes of reporting fitness information to smartphone users via various apps.

The original data can be found at [The UCI Machine Learning Repository](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)

## Transformation

The data in the original experiments has been modified to include both the test data and the training data into one set.  It now only contains the mean and the standard deviations, instead of all data.  The column names for the data have been changed to be easier to understand.  

Out of this, the tidyData.txt file has been made, which contains only the average of each variable for each activity and each subject.

## Columns in tidyData

The identifier columns are used to indicate which activity was performed and which subject performed it.

* "activityId"
* "subjectId"
 
The measurement columns show what was measured

* "timeBodyAccMagnitudeMean"
* "timeBodyAccMagnitudeStdDev"
* "timeGravityAccMagnitudeMean"
* "timeGravityAccMagnitudeStdDev"
* "timeBodyAccJerkMagnitudeMean"
* "timeBodyAccJerkMagnitudeStdDev"
* "timeBodyGyroMagnitudeMean"
* "timeBodyGyroMagnitudeStdDev"
* "timeBodyGyroJerkMagnitudeMean"
* "timeBodyGyroJerkMagnitudeStdDev"
* "freqBodyAccMagnitudeMean"
* "freqBodyAccMagnitudeStdDev"
* "freqBodyAccJerkMagnitudeMean"
* "freqBodyAccJerkMagnitudeStdDev"
* "freqBodyGyroMagnitudeMean"
* "freqBodyGyroMagnitudeStdDev"
* "freqBodyGyroJerkMagnitudeMean"
* "freqBodyGyroJerkMagnitudeStdDev"

The activityType column was added to easily see which activity was performed

* "activityType"

Possible values of this column include

* WALKING (value 1): subject was walking during the test
* WALKING_UPSTAIRS (value 2): subject was walking up a staircase during the test
* WALKING_DOWNSTAIRS (value 3): subject was walking down a staircase during the test
* SITTING (value 4): subject was sitting during the test
* STANDING (value 5): subject was standing during the test
* LAYING (value 6): subject was laying down during the test
