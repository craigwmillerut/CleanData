setwd("C:/Users/Craig/Documents/Coursera/CleanData/CoursePrject/UCI HAR Dataset")
features=scan("features.txt",what=character(),sep="\n")
olddir<-getwd()

# returns string w/o leading whitespace
trim.leading <- function (x)  sub("^\\s+", "", x)

# returns string w/o trailing whitespace
trim.trailing <- function (x) sub("\\s+$", "", x)
#Get the test data
setwd("test")
testdata<-scan("X_test.txt",sep="\n",what="character")
subjectest <- scan("subject_test.txt",sep="\n",what="character")
tests<-c()
for (i in 1:length(testdata)){
  bob <- as.numeric(strsplit(trim.leading(trim.trailing(testdata[i]))," ")[[1]])
  good <- !is.na(bob)
  tests <- rbind(tests,bob[good])
}
#Get the training data
head(tests)
setwd("../train")
traindata <- scan("X_train.txt",sep="\n",what="character")
subjectrain <- scan("subject_train.txt",sep="\n",what="character")
train=c()
for (i in 1:length(traindata)){
  bob <- as.numeric(strsplit(trim.leading(trim.trailing(traindata[i]))," ")[[1]])
  good <- !is.na(bob)
  train <- rbind(train,bob[good])
}
alldata <- rbind(tests,train)
allsubject <- c(subjectest,subjectrain)

goodmean <- grep("mean()",features,fixed=TRUE)
goodstd <- grep("std()",features,fixed=TRUE)
allgood <- sort(c(goodmean,goodstd))
colhead <- features[allgood]
bob <- unlist(strsplit(colhead," ",fixed=TRUE))
dim(bob)=c(2,length(bob)/2)
headings <- bob[2,]

#Put means and std in one data set and label them appropriately
#Step 4 data set
step4data <- alldata[,allgood]
colnames(step4data) <- headings
require(data.table)

#Get an average of each variable for each subject
avdata <- c()
rowsub <- c()
for (i in 1:ncol(step4data)){
  bob <- tapply(step4data[,i],allsubject,mean,simplify=TRUE)
  if (i==1){
    rowsub <- rownames(bob)
  }
  avdata <- cbind(avdata,bob)
}
bob<-cbind(as.numeric(allsubject),step4data)
colnames(bob)[1]<-"subject"
step4data <- bob
bob <- cbind(as.numeric(rowsub),avdata)
colnames(bob)<-c("subject",headings)
avdata <- bob[order(bob[,1]),]

step4data <- data.table(step4data)
avdata <- data.table(avdata)

setwd(olddir)
write.table(step4data,file="step4data.txt",sep=",",row.names=FALSE)
write.table(avdata,file="avdata.txt",sep=",",row.names=FALSE)

