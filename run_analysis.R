#/******************************************************************************
# SCRIPT NAME: run_analysis.R
# PROJECT: Coursera "Gettinga and Cleaning Data" final project
# DESCRIPTION: Load and clean "wearable tech" sensor data taken from a Samsung Galaxy S smartphone.
# PROGRAMMER: Vergil Weatherford
# DATE: 2017-05-02
# GIT: https://github.com/vergilcw/coursera_wearable
# INPUT PATH: input/
# OUTPUT PATH: output/
# SECTIONS:
#   Read in the Raw Data
#   1 - Merge the training and the test sets to create one data set.
#   2 - Extract only the measurements on the mean and sd for each measurement.
#   3 - Use descriptive activity names to name the activities in the data set.
#   4 - Label the data set with descriptive variable names.
#   5 - Create a second, independent tidy data set with the average of each variable for each activity and each subject.
#******************************************************************************/

library(dplyr)
library(stringr)

# Read raw data -----------------------------------------------------------


#read test data
#subject codes for test data:
test_subj <- readLines('input/UCI HAR Dataset/test/subject_test.txt')
#activity codes for test data:
test_x <- read.table('input/UCI HAR Dataset/test/X_test.txt',
                     stringsAsFactors = FALSE)
test_y <- readLines('input/UCI HAR Dataset/test/y_test.txt')
#bring subject and feature codes into test data
test_all <- test_x %>%
  bind_cols(data_frame(subj_code = test_subj,
                       activity_code = test_y,
                       set = 'training'))

#read training data
#subject codes for training data:
train_subj <- readLines('input/UCI HAR Dataset/train/subject_train.txt')
#activity codes for training data:
train_x <- read.table('input/UCI HAR Dataset/train/X_train.txt',
                     stringsAsFactors = FALSE)
train_y <- readLines('input/UCI HAR Dataset/train/y_train.txt')

#bring subject and feature codes into training data
train_all <- train_x %>%
  bind_cols(data_frame(subj_code = train_subj,
                       activity_code = train_y,
                       set = 'training'))


# 1 - Merge the training and the test sets --------------------------------
all_data <- bind_rows(test_all, train_all)

# 2 - Extract only the measurements on the mean and sd --------------------

#read feature labels
features <- readLines('input/UCI HAR Dataset/features.txt')

#keep just the "mean" or "std" containing columns (aftern inspecting features)
keep_cols <- which(grepl('mean\\(|std\\(', features))
all_data <- all_data %>%
  select(keep_cols, activity_code, subj_code, set)

# 3 - Use descriptive activity names to name the activities ---------------

#read activity labels
activity_labels <- readLines('input/UCI HAR Dataset/activity_labels.txt')

#make the activity descriptions into a data frame for easy merging
activity_descrip <- data_frame(
  activity_code = str_extract(activity_labels, '^\\d+'),
  activity_descrip = str_replace_all(activity_labels,'\\d| ', ''))

#merge in activity descriptions
all_data <- all_data %>%
  left_join(activity_descrip, by = 'activity_code')


# 4 - Label the data set with descriptive variable names ------------------


#clean up features into valid R names
col_names <- features %>%
  str_replace_all('^\\d+ |\\(|\\)', '') %>%
  str_replace_all('-|,', '_')

#assign those names to the variables
names(all_data)[1:66] <- col_names[keep_cols]


# 5 - Create a data set with averages by activity and subject -------------

all_data_avg <- all_data %>%
  select(-activity_code, -set) %>%
  group_by(activity_descrip, subj_code) %>%
  summarize_each(funs(mean))

# Export tidy data --------------------------------------------------------

write.table(all_data, 'output/accel_data.txt', row.names = F)
write.table(all_data_avg, 'output/accel_data_avg.txt', row.names = F)


