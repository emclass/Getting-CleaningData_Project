# Erik McCullen
# Course project for 'Getting and Cleaning Data'


#create function to change the activity labels using
#the activity labels file

gsub2 <- function(pattern, replacement, x) {
        for(i in 1:length(pattern))
                x <- gsub(pattern[i], replacement[i], x)
        x
}

#First load in all the data, and the features
test<-read.csv("./test/X_test.txt",sep="",header=FALSE)
testLab<-read.csv("./test/Y_test.txt",sep="",header=FALSE)
train<-read.csv("./train/X_train.txt",sep="",header=FALSE)
trainLab<-read.csv("./train/y_train.txt",sep="",header=FALSE)
features<-read.csv("features.txt",sep="",header=FALSE)
activities<-read.csv("activity_labels.txt",sep="",header=FALSE)
subj_test<-read.csv("./test/subject_test.txt",sep="",header=FALSE)
subj_train<-read.csv("./train/subject_train.txt",sep="",header=FALSE)
#convert to data frames and add the correct column names
#from the features.txt file
testDat<-data.frame(test)
colnames(testDat)<-features$V2
trainDat<-data.frame(train)
colnames(trainDat)<-features$V2

#combine the whole test and train data sets & subjects
allData<-rbind(testDat,trainDat)
subjects<-rbind(subj_test,subj_train)
names(subjects)<-c("subjects")
#assignment asks for only those measurements of mean
#or standard deviation. So, extract only those feature
#names with "mean" or "std" in name
featuresD<-grepl("mean|std",features$V2)

#now subset data to only those of mean or std
Data<-allData[,featuresD]

#remove data no longer needed
rm(allData,features,test,train,testDat,trainDat,subj_test,subj_train)

#combine the test and train labels
allLab<-rbind(testLab,trainLab)
#rename the columns
names(allLab)<-c("activity")

#add the activity column as a first column
#to the whole 'Data' data frame.

# the reshape package is required, so load the library
library(reshape)

#first convert the activity type to the activity name
#using the activities_labels.txt file
#then change the first column of the data to the actual
#activity name

Labels<-as.data.frame(gsub2(activities$V1,activities$V2,allLab$activity))
names(Labels)<-c("activity")
#Data$activity<-labels$activity
Data<-cbind(Labels,subjects,Data)

rm(testLab,trainLab)

#aggregate data using the melt function from reshape package
Data<-melt(Data,id=c("activity","subjects"))
subjectMean<-cast(Data,subjects~variable,mean)
activityMean<-cast(Data,activity~variable,mean)

#combine the two means data and output with write.table

Final<-cbind(subjectMean,activityMean)
write.table(Final,file="NewTidyData.txt",sep=",")

