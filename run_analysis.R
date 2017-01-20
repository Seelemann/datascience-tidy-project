library(plyr)
library(dplyr)

###########################################################
## This function takes the following input:
##
## x_file : data set
## y_file : data labels, ie. activity code for each row
## subject_file : subject id for each row
## feature_names : variable names
## dataset : train or test data
##
## and creates a dataset that starts with x_file and 
## inserts columns for subject and activity at the
## front.  It also inserts a column that indicates whether
## this is training or test data.  Adding a column which indicates
## source when combining tables is described as a best practise in
## section 3.5 of Hadley Wickham’s Tidy Data paper.
##
## It also cleans the variable names, by making
## them lower case, removing '-'s and '()'s.
############################################################
read_input <- function(x_file, y_file, subject_file, feature_names, dataset = "test") {
            # Read in raw data
            x_data <- read.table(x_file)
           
             # Read in activity code for each variable in raw data
             y_data <- read.table(y_file)
             
             # Read in subject id for each row in raw data
             subject_id <- read.table(subject_file)
             
             # Assign variables names from feature_names list
             names(x_data) <- feature_names[,2]
             
             # Insert the activity code vector as the first column
             x_data <- cbind(y_data[,1], x_data)
             
             # Name the activity code column
             names(x_data)[1] <- "activity"
             
             # Insert the subject id vector as the first column
             x_data <- cbind(subject_id[,1], x_data)
             
             # Name the subject id code column
             names(x_data)[1] <- "subject"
             
             ## Add a new column called 'dataset' which is set to
             ## the value specified in dataset
             x_data[,'dataset'] <- dataset
             
             x_data
}

# Read in the feature names for the data
feature_names <- read.table("UCI HAR Dataset/features.txt")

# Load test data
print("Loading test data")
x_test <- read_input("UCI HAR Dataset/test/X_test.txt",
                     "UCI HAR Dataset/test/Y_test.txt",
                     "UCI HAR Dataset/test/subject_test.txt",
                     feature_names)

# Load training data
print("Loading training data")
x_train <- read_input("UCI HAR Dataset/train/X_train.txt",
                      "UCI HAR Dataset/train/Y_train.txt",
                      "UCI HAR Dataset/train/subject_train.txt",
                      feature_names,
                      "train")

# Merge the test and training data
print("Merging training and test datasets")
experiment_data <- rbind(x_test, x_train)

# Get the column positions for mean and std.  These are the columsn
# that contain strings "std()" or "mean()".  Also keep the subject and activity columns.
print("Tidy names and subset the data to mean and std")
stdmeanrows <- grep("subject|activity|dataset|(std|mean)[(][)]",names(experiment_data))

# Subset the dataframe to only the columns with mean and std as well as subject and activity.
experiment_data_subset <- experiment_data[stdmeanrows]

# Convert the subject column from numeric to factor
experiment_data_subset$subject <- as.factor(experiment_data_subset$subject)

# Convert the activity column from numeric to factor
experiment_data_subset$activity <- as.factor(experiment_data_subset$activity)


# Rename the levels from numbers to meaningful activity names
experiment_data_subset$activity <- 
            revalue(experiment_data_subset$activity, 
                    c("1" = "walking", 
                      "2" = "walking_upstairs",
                      "3" = "walking_downstairs",
                      "4" = "sitting",
                      "5" = "standing",
                      "6" = "laying"))

# Tidy the variable names by removing the ()'s and -'s and changing to lower case.
names(experiment_data_subset) <- tolower(gsub("[(][)]|-","",names(experiment_data_subset)))

# group the data by subject and activity
group_by_sa <- group_by(experiment_data_subset, subject, activity)

# create a new dataset which applies the mean function to every mean and std variable, 
# grouped by subject and activity (exclude dataset column from calculation)
print("Generating the tidy dataset")
tidy_dataset <- summarize_each(group_by_sa, funs(mean(., na.rm=TRUE)), -dataset)

## write the final_dataset
write.table(tidy_dataset, "tidy_dataset.txt", row.name=FALSE)
