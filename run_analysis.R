library(dplyr) #load needed pacakage

###########################################################################################################################
#1 get working directory/where the data files are
dir1 <- paste(getwd(), "/UCI HAR Dataset", sep="")
dir2 <- paste(getwd(), "/UCI HAR Dataset/train", sep="")
dir3 <- paste(getwd(), "/UCI HAR Dataset/test", sep="")

###########################################################################################################################
#2 get and extract variables
#get activity labels
activitydata <- read.table(
  paste(dir1,"/activity_labels.txt",sep=""),
  header=FALSE,
  col.names = c("activityindex","activity"))
activitydata$activity <- tolower(activitydata$activity) #transform names to lower cases

#get feature names
featuredata <- read.table(
  paste(dir1,"/features.txt",sep=""),
  header=FALSE,
  col.names = c("featureindex","feature"))
#extract mean/std feature variables 
allfeatures <- as.character(featuredata$feature) #so that we can assign it to coloum names later
feature <- grep("mean\\(\\)|std\\(\\)",
                      allfeatures,
                      value=TRUE)
###########################################################################################################################
#3 get and merge data (training and test : subject, activity, data from 561 features)
#trainning
#subject
trainsub <- read.table(
  paste(dir2,"/subject_train.txt",sep=""),
  header=FALSE,
  col.names = c("subject"))
#activity
trainlabel <- read.table(
  paste(dir2,"/y_train.txt",sep=""),
  header=FALSE,
  col.names = c("activityindex")
  )
#data from 561 features
trainset <- read.table(
  paste(dir2,"/X_train.txt",sep=""),
  header=FALSE,
  col.names = allfeatures
  )

#test
#subject
testsub <- read.table(
  paste(dir3,"/subject_test.txt",sep=""),
  header=FALSE,
  col.names = c("subject"))
#activity
testlabel <- read.table(
  paste(dir3,"/y_test.txt",sep=""),
  header=FALSE,
  col.names = c("activityindex")
)
#data from 561 features
testset <- read.table(
  paste(dir3,"/X_test.txt",sep=""),
  header=FALSE,
  col.names = allfeatures
)

#merge
#merge into traindata
traindata <- cbind(trainsub,trainlabel,trainset)
#merge into testdata
testdata <- cbind(testsub,testlabel,testset)
#merge into alldata
alldata <- rbind(traindata,testdata)

###########################################################################################################################
#4 tidy it up
#(1) give data activity name and drop index
alldata <- select(
  merge(x=activitydata,y=alldata,
        by.x="activityindex",by.y="activityindex"),
  -activityindex
)
#(2) clean up feature names
names(alldata) <- gsub("^t", "time", names(alldata))
names(alldata) <- gsub("Acc", "Accelerometer", names(alldata))
names(alldata) <- gsub("Mag", "Magnitude", names(alldata))
names(alldata) <- gsub("Gyro", "Gyroscope", names(alldata))
names(alldata) <- gsub("^f", "frequency", names(alldata))
names(alldata) <- gsub("BodyBody", "Body", names(alldata))
names(alldata) <- gsub("-", "", names(alldata))
names(alldata) <- gsub("\\(|\\)", "", names(alldata))
names(alldata) <- gsub("mean", "Mean", names(alldata))
names(alldata) <- gsub("std", "StdDev", names(alldata))
#(3) tidy it up by subject and activity
tidy1 <- aggregate(. ~subject + activity, alldata, mean)
#(4) sort by subject to make it pretty
tidy1 <- tidy1[order(tidy1$subject),]
print(tidy1)

############################################################################################################################

#5
#create tidy2 data set with the average of each variable for each activity and each subject.
#feature name is unique, but subject and activity are not
sub <- unique(tidy1$subject)
Nsub <- length(sub)
Nvar <- ncol(tidy1)-2

tidy2_subject <- c()
tidy2_activity <- c()
tidy2_mean <- data.frame()

for (ss in 1:Nsub){
  sshere <- sub[ss]
  #for this subject, he/she has these activity(s)
  act <- as.character(unique(
    tidy1[which(tidy1$subject==sshere),]$activity))
  Nact <- length(act)
  
  for (aa in 1:Nact){
    aahere <-act[aa]
    #extract data into a matrix
    ddhere <- 
      tidy1[which(tidy1$subject==sshere & tidy1$activity==aahere),][,3:ncol(tidy1)]
      
    #for this activity, calculate the mean of each variable
    meanhere <- lapply(
      ddhere,mean)
    
    #include into tidy2
    tidy2_subject <- c(tidy2_subject,sshere)
    tidy2_activity <- c(tidy2_activity,aahere)
    tidy2_mean <- rbind(tidy2_mean,meanhere)
  }
}
tidy2 <- data.frame(subject=tidy2_subject,activity=tidy2_activity,feature=tidy2_mean)
print(tidy2)
#############################################################################################################################

write.table(tidy2, paste(dir1, "/tidy2.txt", sep=""), col.names=FALSE, row.names=FALSE)
