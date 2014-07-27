##Getting and Cleaning Data==================================

### Bohdan Horak

### Description 
Includes addition inforamation about the variable and some the analysis methodology.

Full Description of the data used can be found at 
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

This data can be downloaded at 
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

### Data Set Information 

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

For each record it is provided:
======================================

- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

## Code Overview

### Getting this things ready
    -check for location of file
        -if it does not exist download it
        -if it exists but is still zipped then unzip it
    -clear the workspace to preserve memory
    -set working directory to the downloaded and unzipped folder
### Section 1:
    -read in the necessary data from
        -features.txt
        -activity_labels.txt
        -subject_train.txt
        -x_train.txt
        -y_train.txt
        -subject_test.txt
        -x_test.txt
        -y_test.txt
        
    -assign column names them into one file named "finished_data"
### Section 2:
    -extract the mean and standard deviation measurement
    -along with the "Activity-ID" and "Subject-ID"
    -this is done by a pattern searching function grepl and getting a vector of booleans
    -with this vector it easy to subset the finished_data table to include only the necessary variables
### Section 3:
    -create a column that corresponds to the activity_id but uses descriptive terms
   
      Activity_ID  Activity_Measured
          1            WALKING
          2            WALKING_UPSTAIRS
          3            WALKING_DOWNSTAIRS
          4            SITTING
          5            STANDING
          6            LAYING
    -using the merge function this is easily accomplished
### Section 4:
    -label the data with names that make more sense but are that also do not take up to much space 
    -I expanded some of the words like "b" became "body" and "f" became "freq"
    -I think I struck a good middle ground between readability and space - the new and old names are as follows
    -here are the old and new names 
      columns_names_old                     column_names_new
1                  Activity_ID                          Activity_ID
2                   Subject_ID                           Subject_ID
3            tBodyAcc-mean()-X               Time_Body_Accel_Mean-X
4            tBodyAcc-mean()-Y               Time_Body_Accel_Mean-Y
5            tBodyAcc-mean()-Z               Time_Body_Accel_Mean-Z
6             tBodyAcc-std()-X              Time_Body_Accel_StDev-X
7             tBodyAcc-std()-Y              Time_Body_Accel_StDev-Y
8             tBodyAcc-std()-Z              Time_Body_Accel_StDev-Z
9         tGravityAcc-mean()-X                  Time_Gravity_Mean-X
10        tGravityAcc-mean()-Y                  Time_Gravity_Mean-Y
11        tGravityAcc-mean()-Z                  Time_Gravity_Mean-Z
12         tGravityAcc-std()-X                 Time_Gravity_StDev-X
13         tGravityAcc-std()-Y                 Time_Gravity_StDev-Y
14         tGravityAcc-std()-Z                 Time_Gravity_StDev-Z
15       tBodyAccJerk-mean()-X          Time_Body_Accel_Jerk_Mean-X
16       tBodyAccJerk-mean()-Y          Time_Body_Accel_Jerk_Mean-Y
17       tBodyAccJerk-mean()-Z          Time_Body_Accel_Jerk_Mean-Z
18        tBodyAccJerk-std()-X         Time_Body_Accel_Jerk_StDev-X
19        tBodyAccJerk-std()-Y         Time_Body_Accel_Jerk_StDev-Y
20        tBodyAccJerk-std()-Z         Time_Body_Accel_Jerk_StDev-Z
21          tBodyGyro-mean()-X                Time_Body_Gyro_Mean-X
22          tBodyGyro-mean()-Y                Time_Body_Gyro_Mean-Y
23          tBodyGyro-mean()-Z                Time_Body_Gyro_Mean-Z
24           tBodyGyro-std()-X               Time_Body_Gyro_StDev-X
25           tBodyGyro-std()-Y               Time_Body_Gyro_StDev-Y
26           tBodyGyro-std()-Z               Time_Body_Gyro_StDev-Z
27      tBodyGyroJerk-mean()-X           Time_Body_Gyro_Jerk_Mean-X
28      tBodyGyroJerk-mean()-Y           Time_Body_Gyro_Jerk_Mean-Y
29      tBodyGyroJerk-mean()-Z           Time_Body_Gyro_Jerk_Mean-Z
30       tBodyGyroJerk-std()-X          Time_Body_Gyro_Jerk_StDev-X
31       tBodyGyroJerk-std()-Y          Time_Body_Gyro_Jerk_StDev-Y
32       tBodyGyroJerk-std()-Z          Time_Body_Gyro_Jerk_StDev-Z
33          tBodyAccMag-mean()       Time_Body_Accel_Magnitude_Mean
34           tBodyAccMag-std()      Time_Body_Accel_Magnitude_StDev
35       tGravityAccMag-mean()          Time_Gravity_Magnitude_Mean
36        tGravityAccMag-std()         Time_Gravity_Magnitude_StDev
37      tBodyAccJerkMag-mean()  Time_Body_Accel_Jerk_Magnitude_Mean
38       tBodyAccJerkMag-std() Time_Body_Accel_Jerk_Magnitude_StDev
39         tBodyGyroMag-mean()        Time_Body_Gyro_Magnitude_Mean
40          tBodyGyroMag-std()       Time_Body_Gyro_Magnitude_StDev
41     tBodyGyroJerkMag-mean()   Time_Body_Gyro_Jerk_Magnitude_Mean
42      tBodyGyroJerkMag-std()  Time_Body_Gyro_Jerk_Magnitude_StDev
43           fBodyAcc-mean()-X               Freq_Body_Accel_Mean-X
44           fBodyAcc-mean()-Y               Freq_Body_Accel_Mean-Y
45           fBodyAcc-mean()-Z               Freq_Body_Accel_Mean-Z
46            fBodyAcc-std()-X              Freq_Body_Accel_StDev-X
47            fBodyAcc-std()-Y              Freq_Body_Accel_StDev-Y
48            fBodyAcc-std()-Z              Freq_Body_Accel_StDev-Z
49       fBodyAccJerk-mean()-X          Freq_Body_Accel_Jerk_Mean-X
50       fBodyAccJerk-mean()-Y          Freq_Body_Accel_Jerk_Mean-Y
51       fBodyAccJerk-mean()-Z          Freq_Body_Accel_Jerk_Mean-Z
52        fBodyAccJerk-std()-X         Freq_Body_Accel_Jerk_StDev-X
53        fBodyAccJerk-std()-Y         Freq_Body_Accel_Jerk_StDev-Y
54        fBodyAccJerk-std()-Z         Freq_Body_Accel_Jerk_StDev-Z
55          fBodyGyro-mean()-X                Freq_Body_Gyro_Mean-X
56          fBodyGyro-mean()-Y                Freq_Body_Gyro_Mean-Y
57          fBodyGyro-mean()-Z                Freq_Body_Gyro_Mean-Z
58           fBodyGyro-std()-X               Freq_Body_Gyro_StDev-X
59           fBodyGyro-std()-Y               Freq_Body_Gyro_StDev-Y
60           fBodyGyro-std()-Z               Freq_Body_Gyro_StDev-Z
61          fBodyAccMag-mean()       Freq_Body_Accel_Magnitude_Mean
62           fBodyAccMag-std()      Freq_Body_Accel_Magnitude_StDev
63  fBodyBodyAccJerkMag-mean()  Freq_Body_Accel_Jerk_Magnitude_Mean
64   fBodyBodyAccJerkMag-std() Freq_Body_Accel_Jerk_Magnitude_StDev
65     fBodyBodyGyroMag-mean()        Freq_Body_Gyro_Magnitude_Mean
66      fBodyBodyGyroMag-std()       Freq_Body_Gyro_Magnitude_StDev
67 fBodyBodyGyroJerkMag-mean()   Freq_Body_Gyro_Jerk_Magnitude_Mean
68  fBodyBodyGyroJerkMag-std()  Freq_Body_Gyro_Jerk_Magnitude_StDev
69           Activity_Measured                    Activity_Measured

### Section 5
    -need to create a second, independent tidy data set with the average of 
    each variable for each activity and each subject. 
    -I used an aggregate function to calculate the mean accross both the Activity and the Subject
    -I then used a write.csv function to output my tidy_data table into a csv file
        
