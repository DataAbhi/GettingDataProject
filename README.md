# Cleaningdataproject
Getting and Cleaning Data Project 

Introduction
=========================================

This project is about a dataset of an experiment where researchers carried out experiments with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, they captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 


The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'codebook.txt' for more details. 

As per the project instructions, the raw data was extracted for the training and test subjects and the following analysises was carried out: 

1. Merges the training and the test sets to create one data set.

2. Extracts only the measurements on the mean and standard deviation for each measurement.

3. Uses descriptive activity names to name the activities in the data set

4. Appropriately labels the data set with descriptive variable names.

5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


The repo includes the following files:
=========================================

- 'README.txt'

- 'codebook.txt': Shows information about the variablesnames, units etc. 

- tidydata.txt: The final data set 

- 'project_script1.R' with the script that runs it


Explaination of 'run_analysis.R':
=========================================

One script does the analysis for the project. The data has already been downloaded into the working directory. The script has five sections

Section 1: Reads the raw files from the working directory, and stores into dataframes

Section 2: Combines the dataframe into one

Section 3: Removes all the variables that are not means and standard deviations; also gives the variable names 

Section 4: Uses descriptive activity names to name the activities in the data set

Section 5: Creating a second tidy data set with the average of each variable for each activity and each subject.

