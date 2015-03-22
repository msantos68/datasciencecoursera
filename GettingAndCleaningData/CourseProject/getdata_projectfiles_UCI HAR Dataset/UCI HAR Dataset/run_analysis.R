## run_analysis.R
# Michelle Santos 3/20/2015
# Purpose:  For Coursera Getting and Cleaning Data Course Project
#   This program will run through the following steps:
#  1. Read in the Training and Test data on the accelerometers from Samsung Galaxy S Smartphone
#  2. Merge the Training and Test data together
#  3. Extracts the measurements on MEAN and STDEV for each measurement
#  4. Create a tidy data set ensuring the names of activities are descriptive and that labels are used.

####################################################################################
#FIRST ENSURE YOU DOWNLOADED THE DATA AND UNZIPPED THE FILE
#YOUR WORKING DIRECTORY SHOULD BE POINTED TO THE UCI HAR Dataset folder which will have subfolders
#Called test and train
####################################################################################

#The program is set up into a number of parts:
##  1. Read in the Activity_Label and Features files - create a vector of Feature_Names
##  2. Read and Process the Test data first - create column names, select only necessary columns
##     and merge on the Activity and Subject Info
##  3. Repeat Step 2 with Train data
##  4. Row Bind the Train and Test data together
##  5. Create all necessary stats as requested in the project

#Adding librarys incase they are not already loaded
library(dplyr)
library(tidyr)
library(reshape2)

#Step 1 - Read in Activity Label and Features
##MAKE SURE YOUR WORKING DIRECTORY IS SET TO WHERE DATASETS ARE
activity_label <- read.table("activity_labels.txt", col.names = c("Activity_Num", "Activity_Label"))
features <- read.table("features.txt", col.names =  c("Col_Num","Measurement_Name"))
feature_names <- as.vector(features$Measurement_Name) #Turn into vector so we can use for column names
#Replacing all the ... and upcasing the mean and std variables
feature_names <- gsub("mean","MEAN",gsub("std","STD",gsub("\\.\\.\\.",".",gsub("\\.\\.",".",make.names(feature_names))),ignore.case = TRUE), ignore.case = TRUE)

#Step 2 - Read and Process Test Data
##MAKE SURE YOUR TEST DATA IS WITHIN A SUBDIRECTORY CALLED test
subject_test <- read.table("test/subject_test.txt", col.names = "SubjectID")

#With Y_test we need to merge on the Activity Label to it
y_test <- read.table("test/Y_test.txt", col.names = "Activity_Num")
y_test_labels <- merge(y_test, activity_label, by = "Activity_Num")
y_test_labels <- select(y_test_labels, Activity_Label)

#Reading in x_test data - here we will have to set the column names using feature_names, select only those
#that have mean or std in the name and cbind with y_test_labels and subject_test
x_test <- read.table("test/X_test.txt", col.names = feature_names)
x_test <- x_test %>% select(contains("mean"), contains("std")) %>% mutate(file = "test")
x_test_merge <- cbind(subject_test, y_test_labels, x_test)

#Step 3 - Repeat Step 2 with the Train data
subject_train <- read.table("train/subject_train.txt", col.names = "SubjectID")

#With Y_train we need to merge on the Activity Label to it
y_train <- read.table("train/Y_train.txt", col.names = "Activity_Num")
y_train_labels <- merge(y_train, activity_label, by = "Activity_Num")
y_train_labels <- select(y_train_labels, Activity_Label) #We only want the label on the final data

#Reading in x_train data - here we will have to set the column names using feature_names, select
#the columns that have mean or std in the anem only and cbind with subject_train and y_train_labels
x_train <- read.table("train/x_train.txt", col.names = feature_names)
x_train <- x_train %>% select(contains("mean"), contains("std")) %>% mutate(file = "train")
x_train_merge <- cbind(subject_train, y_train_labels, x_train)

#Step 4 - Combine the x_test_merge and x_train_merge data
master_data <- rbind(x_test_merge, x_train_merge)

#Step 5 - Create a tidy dataset with the average values of all the measurement variables by
#SubjectID and Activity_Label

#First create a vector of the variable names
varname <- as.vector(names(master_data))
varname <- varname[c(3:88)]

#Now use Melt to create a tall skinny dataset
master_melt <- melt(master_data, id = c("SubjectID", "Activity_Label"), measure.vars = varname)

#Using dcast to calculate the average value by Subject ID and Activity Label
tidy_avg <- dcast(master_melt, SubjectID + Activity_Label ~ variable, mean)
write.table(tidy_avg, file = "Final_Tidy_Data.txt", row.names = FALSE)
