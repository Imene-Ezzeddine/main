##reading text files
library(data.table)
features <- read.table("C:/Users/dell/Desktop/tyding data/UCI HAR Dataset/features.txt"
                       , header = FALSE, sep = "")[,2]
activities <- read.table("C:/Users/dell/Desktop/tyding data/UCI HAR Dataset/activity_labels.txt"
                         , header = FALSE, sep = "")[,2]
subject_test <- read.table("C:/Users/dell/Desktop/tyding data/UCI HAR Dataset/test/subject_test.txt",
                           header = FALSE, sep = "")
feature_test <- read.table("C:/Users/dell/Desktop/tyding data/UCI HAR Dataset/test/X_test.txt",
                           header = FALSE, sep = "")
activity_test <- read.table("C:/Users/dell/Desktop/tyding data/UCI HAR Dataset/test/y_test.txt",
                           header = FALSE, sep = "")
subject_train <- read.table("C:/Users/dell/Desktop/tyding data/UCI HAR Dataset/train/subject_train.txt",
                           header = FALSE, sep = "")
feature_train <- read.table("C:/Users/dell/Desktop/tyding data/UCI HAR Dataset/train/X_train.txt",
                           header = FALSE, sep = "")
activity_train <- read.table("C:/Users/dell/Desktop/tyding data/UCI HAR Dataset/train/y_train.txt",
                            header = FALSE, sep = "")
##binding files into a subject/feature/activity table frame
library(dplyr)
subject <- bind_rows(subject_test, subject_train, id= NULL)
activity <- bind_rows( activity_test, activity_train)
feature <- bind_rows( feature_test, feature_train, id = NULL)
##naming columns
colnames(subject) <- "subjct"
colnames(activity)<- "activities"
colnames(feature)<-features
##binding files into a single data frame
df <- bind_cols(subject, activity, feature)
##Extracts only the measurements on the mean and standard deviation for each measurement
df_mean <- df[,grepl("mean", names(df))]
df_std <- df[, grepl("std", names(df))]
df_sub_act <- bind_cols(df$subjct, df$activities)
colnames(df_sub_act) <- c("subject", "activities")
df_mean_std <- bind_cols(df_sub_act, df_mean, df_std)
#Uses descriptive activity names to name the activities in the data set
for (i in 1:length(df_mean_std$activities)){
  if (df_mean_std$activities[i] == "1"){
    df_mean_std$activities[i]=activities[1]
  }
  if (df_mean_std$activities[i] == "2"){
    df_mean_std$activities[i]=activities[2]
  } 
  else if (df_mean_std$activities[i] == "3"){
    df_mean_std$activities[i]=activities[3]
  }
  else if (df_mean_std$activities[i] == "4"){
    df_mean_std$activities[i]=activities[4]
  } 
  else if (df_mean_std$activities[i] == "5"){
    df_mean_std$activities[i]=activities[5]
  }
  else {
    df_mean_std$activities[i]=activities[6]
  } 
}
#Appropriately labels the data set with descriptive variable names
names(df_mean_std) <- sub(pattern = "^t", "Time of ", names(df_mean_std))
names(df_mean_std) <-sub(pattern = "^f", "Frequency of ", names(df_mean_std))
names(df_mean_std) <-sub(pattern = "Acc-", " Accelerometer ", names(df_mean_std))
names(df_mean_std) <-sub(pattern = "BodyAcc", " Body Accelerometer ", names(df_mean_std))
names(df_mean_std) <-sub(pattern = "AccMagnitude", " Accelerometer Magnitude ", names(df_mean_std))
names(df_mean_std) <-sub(pattern = "Accelerometer", " Accelerometer ", names(df_mean_std))
names(df_mean_std) <-sub(pattern = "Gyroscope", " Gyroscope ", names(df_mean_std))
names(df_mean_std) <-sub(pattern = "Gyro-", " Gyroscope ", names(df_mean_std))
names(df_mean_std) <-sub(pattern = "Gyro", " Gyroscope ", names(df_mean_std))
names(df_mean_std) <-sub(pattern = "Jerk-", " Jerk Signal ", names(df_mean_std))
names(df_mean_std) <-sub(pattern = "Jerk", " Jerk signal ", names(df_mean_std))
names(df_mean_std) <-sub(pattern = "std()", " Standard ", names(df_mean_std))
names(df_mean_std) <-sub(pattern = "mean()", " Mean ", names(df_mean_std))
names(df_mean_std) <-sub(pattern = "mean", " Mean ", names(df_mean_std))
names(df_mean_std) <-sub(pattern = "Mag-",  "Magnitude ", names(df_mean_std))
names(df_mean_std) <-sub(pattern = "Frequency", " Frequency ", names(df_mean_std))
names(df_mean_std) <-sub(pattern = "Freq", " Frequency ", names(df_mean_std))
names(df_mean_std) <-sub(pattern = "Freq()", " Frequency ", names(df_mean_std))
names(df_mean_std) <-sub(pattern = "-X", " on the X axis ", names(df_mean_std))
names(df_mean_std) <-sub(pattern = "-Y", " on the Y axis ", names(df_mean_std))
names(df_mean_std) <-sub(pattern = "-Z", " on the Z axis ", names(df_mean_std))
##From the data set in step 4, creates a second, independent tidy data set with
##the average of each variable for each activity and each subject.
dfmelt <- melt(df_mean_std, id=c("subject", "activities"))
dftidy <- dcast(dtmelt, subject + activities ~ variable, mean)
##convert to text file
write.table(dftidy,"C:/Users/dell/Desktop/tyding data/UCI HAR Dataset/final.txt",row.name=FALSE)