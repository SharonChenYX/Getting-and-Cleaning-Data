data source:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
disclosure about the data
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Original data from online (link and data description provided above) includes 10299 observations, among which 7352 are from training data and 2947 are from test data.
Both data from training and test include 2+561 variables:
==================================================================
-1. Subject index
-2. Activity index
-3. 561 variables of features
Additional data for subject and activity labels are also provided. 

The purposes of my codes are:
==================================================================
-Merges the training and the test sets to create one data set.
-Extracts only the measurements on the mean and standard deviation for each measurement.
-Uses descriptive activity names to name the activities in the data set
-Appropriately labels the data set with descriptive variable names---namaed tidy1.
-From tidy1, creates a second, independent tidy data set (tidy2) with the average of each variable for each activity and each subject.
Therefore, running my codes, you should get two data set: tidy1 and tidy2.

My codes accomplish these purposes by:
==================================================================
-1.locate data files, and get working directory (with getwd() function)
-2.get activity labels(processed to lower case), feature names(only mean and std), training related data and test related data.
-3.merge above into training data and test data (by activity index), then merge training data and test data into a compelete set of data. Sorted by ascending order of subject, tidy1 is born.
-4.create tidy2 data set with the average of each variable for each activity and each subject
