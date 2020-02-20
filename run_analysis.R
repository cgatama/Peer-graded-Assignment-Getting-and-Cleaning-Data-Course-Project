# Downloads and unzips the 'UCI HAR Dataset
if (!dir.exists("UCI HAR Dataset")) {
    data_url = paste0("https://d396qusza40orc.cloudfront.net/",
                    "getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
                    ")
  download.file(data_url, "data.zip")
  
  # log date and the data url
  log_url <- paste0("Url:", data_url)
  log_date <- paste0("Date: " , date())
  
  log_con <- file("log.txt")
  writeLines(c(log_url, log_date), log_con)
  close(log_con)
  
  
  # Extracts files from zipped file 'data.zip'
  
  unzip("data.zip")
  message("data files extracted successfully, in the directory \n",
          "./Peer-graded-Assignment-Getting-and-Cleaning-Data-Course-Project/UCI HAR Dataset.")
  
} else {
  message("    ...data files are already available, in the working directory.")
}









##
####
######
########
##Analysis

# Loads required libraries
install.packages("dplyr")
library(dplyr)

#Read data
## Create the list with the instructions needed by 'read.table()'
read.table_instructions <- list(
  # Create 'file' object listwith values
  # thatindicates the path of each file.
  file = list(
    activity_labels = "UCI HAR Dataset/activity_labels.txt",
    features = "UCI HAR Dataset/features.txt",
    subject_train = "UCI HAR Dataset/train/subject_train.txt",
    y_train = "UCI HAR Dataset/train/y_train.txt",
    X_train = "UCI HAR Dataset/train/X_train.txt",
    subject_test = "UCI HAR Dataset/test/subject_test.txt",
    y_test = "UCI HAR Dataset/test/y_test.txt",
    X_test = "UCI HAR Dataset/test/X_test.txt"
  ),
  # Object list with name 'colClasses'
  # that contains the values for 'colClasses' argument
  # that indicates the classes of all variables in each file.
  # It is supplied to correctly identify column classes,
  # and as side effect also improves reading speed.
  colClasses = list(
    activity_labels = c("integer", "character"),
    features = c("integer", "character"),
    subject_train = "integer",
    y_train = "integer",
    X_train = rep("numeric", 561),
    subject_test = "integer",
    y_test = "integer",
    X_test = rep("numeric", 561)
  ),
  # Object list with name 'nrows'
  # that contains the values for 'nrows' argument
  # that indicates the number of rows to read in each file.
  nrows = list(
    activity_labels = 6,
    features = 561,
    subject_train = 7352,
    y_train = 7352,
    X_train = 7352,
    subject_test = 2947,
    y_test = 2947,
    X_test = 2947
  )
)

## Load data with 'Map()'.
## For each file the correct arguments are supplied to function 'read.table()',
## as well as some extra, common arguments for all files.
data_files <- with(read.table_instructions,
                   Map(read.table,
                       file = file, colClasses = colClasses, nrows = nrows,
                       quote = "", comment.char = "",
                       stringsAsFactors = FALSE))

message(" data files  successfully loaded into R, \n",
        "       in the list with name 'data_files'.")




##
####
######
########
# 1: Merges the training and the test sets to create one data set.

## Merges the train and test sets
merged_data <- with(data_files,
                    rbind(cbind(subject_train, y_train, X_train),
                          cbind(subject_test,  y_test,  X_test)))




##
####
######
########
# 2: Extracts only the measurements on the mean and standard deviation
#         for each measurement.
#
target_features_indexes <- grep("mean\\(\\)|std\\(\\)",
                                data_files$features[[2]])



target_variables_indexes <- c(1, 2, target_features_indexes + 2)

## Extracts the target variables to create the target data frame
target_data <- merged_data[ , target_variables_indexes]


##
####
######
########
# 3: Uses descriptive activity names to name the activities in the data set


## Replace activity values with a factor based on levels and labels
## contained in the activity_labels data file.
target_data[[2]] <- factor(target_data[[2]],
                           levels = data_files$activity_labels[[1]],
                           labels = data_files$activity_labels[[2]])


##
####
######
########
# 4: Appropriately labels the data set with descriptive variable names.



## Extract the target variables names
descriptive_variable_names <- data_files$features[[2]][target_features_indexes]

## Correct a typo
descriptive_variable_names <- gsub(pattern = "BodyBody", replacement = "Body",
                                   descriptive_variable_names)

## Create a tidy data set with appropriate labels for the variable names
tidy_data <- target_data
names(tidy_data) <- c("subject", "activity", descriptive_variable_names)





##
####
######
########
# 5: created a second, a tidy data set with the average of each variable
#         for each activity and each subject.

## Create a dataset with the mean of each column for 'subject' and 'activity'
tidy_data_summary <- tidy_data %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean)) %>%
  ungroup()



## Replace the variable names of 'tidy_data_summary' with new descriptive ones.
## Just the prefix "Avrg-" will be added in all variable names,
## except the first two, 'subject' and 'activity'.
new_names_for_summary <- c(names(tidy_data_summary[c(1,2)]),
                           paste0("Avrg-", names(tidy_data_summary[-c(1, 2)])))
names(tidy_data_summary) <- new_names_for_summary

## Save the data frame created as a text file in working directory
write.table(tidy_data_summary, "tidy_data_summary.txt", row.names = FALSE)

message("Success! Bingo!")