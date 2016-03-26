## The Data
The data set contains information from sensors in smartphones while the owner carried out various activities.  This data helps us to understand real-life data for purposes of reporting fitness information to smartphone users via various apps.

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

## Transformation

The data in the original experiments has been modified to include both the test data and the training data into one set.  It now only contains the mean and the standard deviations, instead of all data.  The column names for the data have been changed to be easier to understand.  

Out of this, the tidyData.txt file has been made, which contains only the average of each variable for each activity and each subject.

## Links

See the Readme file for more information on where the data originated.
