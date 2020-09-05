library(dplyr)

#Obtaining features of the files
file.list <- unzip("getdata_projectfiles_UCI HAR Dataset.zip", list=T)
features <- read.table(unz("getdata_projectfiles_UCI HAR Dataset.zip",file.list[2,"Name"] ),
                       header = F, col.names = c("N", "Var"), stringsAsFactors = F)
features$Var <- sub(pattern = "()", "", features$Var)
features$Names <- stringr::str_replace_all(features$Var, pattern = "[()]", replacement = "")

# Reading Train and Test datasets
train <- read.table(unz("getdata_projectfiles_UCI HAR Dataset.zip",file.list[31,"Name"] ),
                   header = F, col.names = features$Names,stringsAsFactors = F)
trainSub <- read.table(unz("getdata_projectfiles_UCI HAR Dataset.zip",file.list[30,"Name"] ),
                       header = F, stringsAsFactors = F)
train$ID <- trainSub$V1
train$modality <- "train"

test <- read.table(unz("getdata_projectfiles_UCI HAR Dataset.zip",file.list[17,"Name"] ),
                   header = F, col.names = features$Names,stringsAsFactors = F)
testSub <- read.table(unz("getdata_projectfiles_UCI HAR Dataset.zip",file.list[16,"Name"] ),
                      header = F, stringsAsFactors = F)
test$ID <- testSub$V1
test$modality <- "test"

# 1. Merges the training and the test sets to create one data set 
rundata <- rbind(test, train)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
rundata <- rundata %>% 
    select(c("ID", "modality", grep("[Mm]ean|std", names(train), value = T)) )

# 3. Uses descriptive activity names to name the activities in the data set &
# 4. Appropriately labels the data set with descriptive variable names.
names(rundata) <- gsub("^t","Time", names(rundata))
names(rundata) <- gsub("^f","Frequency", names(rundata))

# 5.From the data set in step 4, creates 
#a second, independent tidy data set with the average 
#of each variable for each activity and each subject.

rundataG <- rundata %>% group_by(ID, modality)
rundatatidy <-  rundataG %>% 
    summarise_at(vars(TimeBodyAcc.mean.X:angleZ.gravityMean),mean, na.rm = TRUE)

write.table(x = rundatatidy, "rundatatidy.txt", row.name=FALSE)
