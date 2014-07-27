## Coursera Getting and Cleaning Data Project - 7/27/2014
## Bohdan Horak

## run_analysis.R code - Overview - according to the assignment
## Section 1: Merges the training and the test sets to create one data set.
## Section 2: Extracts only the measurements on the mean and standard deviation for each measurement. 
## Section 3: Uses descriptive activity names to name the activities in the data set.
## Section 4: Appropriately labels the data set with descriptive variable names.
## Section 5: Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 



##Code

## Getting this things ready--------------------------------------------------------------------------


##checks if the file exists, if it doesn't the code will download the file into the current directory 
## unzips it

if (!file.exists("UCI HAR Dataset_1")) {
    if (!file.exists("data.zip")) {
        fileUrl <-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(fileUrl, destfile="./data.zip")
    }
    unzip("data.zip")
}

##  clears the workspace
rm(list=ls())

## set the working directory to the unziped file
setwd("~/GitHub/Getting and Cleaning Data/Getting-and-Cleaning-Data-Coursera/UCI HAR Dataset")


## Section 1 -----------------------------------------------------------------------------------------
## read data from the train subfolder

features <- read.table("./features.txt")
activity_type <- read.table("./activity_labels.txt")
subject_train <- read.table("./train/subject_train.txt")
X_train <-read.table("./train/X_train.txt")
Y_train <-read.table("./train/Y_train.txt")

## assign column names to the data

colnames(activity_type) <- c("Activity_ID","Activity_Measured")
colnames(subject_train) <-"Subject_ID"
colnames(X_train) <-features[,2]
colnames(Y_train)<-"Activity_ID"

## combine the train data set - column wise

train_data <-cbind(Y_train, subject_train, X_train);


## read data from the test subfolder

subject_test<-read.table("./test/subject_test.txt")
X_test<-read.table("./test/X_test.txt")
Y_test<-read.table("./test/Y_test.txt")

## assign column names to the data

colnames(subject_test)<-"Subject_ID"
colnames(X_test)<-features[,2]
colnames(Y_test)<-"Activity_ID"

## combine the test data set - columns wise

test_data<-cbind(Y_test,subject_test,X_test)

##combine the test_data set and train_data sets - row wise


finished_data<-rbind(train_data,test_data)

## Section 2----------------------------------------------------------------------------------------------

## Extract the variable names from the finished data set to be used to identify needed variables
col_names<-colnames(finished_data)

## need only the measurements on the mean and standard deviation columns as well as the "Activity_ID"
## and Subject_ID columns
## used a pattern recognition function to save time
## first four conditions look for the four variable types mention above
## the fifth excludes the frequncy means and stdevs

selection_vector<-((grepl("Activity..",col_names)|grepl("Subject..",col_names)|
                        grepl("-mean()",col_names)|grepl("-std()",col_names)) & !grepl("Freq()",col_names))

## subsets the finished_data set so it only includes the needed columns

finished_data<-finished_data[selection_vector]

##Section 3 -----------------------------------------------------------------------------------

## merges back in the activity_measured vector which is the same as the activity_id vector 
## except it showcases the the number 1 through 6 mean

finished_data<-merge(finished_data,activity_type, by="Activity_ID",all.x=T)

## Section 4 ----------------------------------------------------------------------------------

## updates the column names of the "finished_data" set to allow for easy renaming
col_names<-colnames(finished_data)

## renames some of the veriables and makes them easier to interpret
## refer to the codebook for a more extensive explanation
## used a series of gsub functions and inside a for loop
## to make the process more efficient

for (i in 1:length(col_names)) {
    col_names[i]=gsub("^(t)","Time_",col_names[i])
    col_names[i]=gsub("^(f)","Freq_",col_names[i])
    col_names[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body_",col_names[i])
    col_names[i]=gsub("GravityAcc","Gravity_",col_names[i])
    col_names[i]=gsub("Acc","Accel_",col_names[i])
    col_names[i]=gsub("Gyro","Gyro_",col_names[i])
    col_names[i]=gsub("Jerk","Jerk_",col_names[i])
    col_names[i]=gsub("Mag","Magnitude_",col_names[i])
    col_names[i]=gsub("-mean()","Mean",col_names[i])
    col_names[i]=gsub("-std()", "StDev",col_names[i])
    col_names[i]=gsub("\\()","",col_names[i])   
}

## Apply the column names vactor to the data set

colnames(finished_data)=col_names

## Section 5--------------------------------------------------------------------------------------------------

##excludes the non numeric column of "Activity_Measured" in order to help with finding the means

data_sans_type=finished_data[,1:68]

## finding the means 
## uses the aggraget function to find the means for each activity and each subject
tidy_data=aggregate(data_sans_type[,3:68], 
                    by=list(Activity_ID=data_sans_type$Activity_ID,Subject_ID=data_sans_type$Subject_ID),mean)

## merge back in the non-numerics "Activity_Measured" column
    
tidy_data=merge(tidy_data,activity_type,by="Activity_ID",all.x=T)

## Writes the finished data set "tidy_data" into the imaginatively titled
## file "tidy_data.csv"

write.csv(tidy_data,"./tidy_data.csv",row.names=F)

## THE END--------------------------------------------------------------------------------------------------------


