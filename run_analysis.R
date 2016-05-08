####Lazarus105
####  Look at the folders in the directories from the Zip
####  Identify Which data sets from Test and Train is usable
####  In the Test Folder the file X_Test is Usable
####  In the Train Folder the file X_Train is usable
####  Notice that the data sets do not have headers
####  Find the header information in the 'features' file and that it is a text file

####  Import the Headers as a table
Activitylabels <- read.table("./activity_labels.txt", header = FALSE)

####  Only keep relevant information 
Activitylabels[,2] <- as.character(Activitylabels[,2])
####  Remove the '_' from the Headers
gsub("_","",labels)

####  Make the labels lowercase
labels <- tolower(labels)


####  Import the Features
features <- read.table("./features.txt")
####  Clean
features <- features[,2]
features = gsub('-mean', 'Mean', features)
features = gsub('-std', 'Std', features)
features <- gsub('[-()]', '', features)

####look at the datasets, see that htere is 2 spaces at the front that will impact the import, fix it

#### Import the Test Data Set and labels
test<- read.table("./test/X_test.txt") 
testActivities <- read.table("./test/y_test.txt")
names(testActivities) <- "Activities"
testSubjects <- read.table("./test/subject_test.txt")
names(testSubjects) <- "Subjects"
#### add a column to identify that this data was test
colnames(test) <- features
test <- cbind(testSubjects, testActivities, test)
#### add the data labels
test$Source <- rep("test",nrow(test)) 

#### Import the Train Data Set and labels

train <- read.table("./train/X_train.txt") 
trainActivities <- read.table("./train/y_train.txt")
names(trainActivities) <- "Activities"
trainSubjects <- read.table("./train/subject_train.txt")
names(trainSubjects) <- "Subjects"
colnames(train) <- features
#### add a column to identify that this data was train
train$Source <- rep("train",nrow(train)) 
train <- cbind(trainSubjects, trainActivities, train)

####  combine the train and test datasets
Combined <- rbind(test,train)

#### Identify all the columns to keep in the tidy data set inc the MEAN and STD
subset_combined <- Combined[,grep(".*Activities.*|.*Subjects.*| .*Mean.*|.*Std.*|.*Source.*", colnames(Combined))]

####  Clean up the and Activities subjects by allocating the verbal description of the activities and replacing the numbers
subset_combined$Activities <- factor(subset_combined$Activities, levels = Activitylabels[,1], labels = Activitylabels[,2])
subset_combined$Subjects <- as.factor(subset_combined$Subjects)

### Creating the independent tidy data set with the average of each variable for each actibity and each subject
tidyData.melted <- melt(tidyData, id = c("Subjects", "Activities"))
tidyData.mean <- dcast(tidyData.melted, Subjects + Activities ~ variable, mean)
write.table(tidyData.mean, "tidy_mean.txt", row.names = FALSE, quote = FALSE)
