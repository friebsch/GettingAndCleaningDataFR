#load libraries

library("data.table")
library("reshape2")
library("knitr")
library("markdown")

#set the actualPath Variable to work with

actualPath <- getwd()
actualPath

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
f <- "Dataset.zip"
if (!file.exists(actualPath)) {
    dir.create(actualPath)
}
#download.file(url, file.path(actualPath, f)) and set time. (in my case: "2014-11-22 17:56:59 CET")

#timeDownloaded <- Sys.time()
timeDownloaded

dataPath <- file.path(actualPath, "UCI HAR Dataset")
list.files(dataPath, recursive = TRUE)

#read files

subjectTrain <- fread(file.path(dataPath, "train", "subject_train.txt"))
subjectTest <- fread(file.path(dataPath, "test", "subject_test.txt"))
yTrain <- fread(file.path(dataPath, "train", "Y_train.txt"))
yTest <- fread(file.path(dataPath, "test", "Y_test.txt"))

fileToDataTable <- function(f) {
    df <- read.table(f)
    dt <- data.table(df)
}
xTrain <- fileToDataTable(file.path(pathIn, "train", "X_train.txt"))
xTest <- fileToDataTable(file.path(pathIn, "test", "X_test.txt"))


#merge Files

dtSubject <- rbind(subjectTrain, subjectTest)
setnames(dtSubject, "V1", "subject")
dtActivity <- rbind(yTrain, yTest)
setnames(dtActivity, "V1", "activityNum")
dt <- rbind(xTrain, xTest)


dtSubject <- cbind(dtSubject, dtActivity)
dt <- cbind(dtSubject, dt)

setkey(dt, subject, activityNum)

dtFeatures <- fread(file.path(dataPath, "features.txt"))
setnames(dtFeatures, names(dtFeatures), c("featureNum", "featureName"))

dtFeatures <- dtFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]

dtFeatures$featureCode <- dtFeatures[, paste0("V", featureNum)]

#subsetting

select <- c(key(dt), dtFeatures$featureCode)
dt <- dt[, select, with = FALSE]


#get activity names

activityNames <- fread(file.path(dataPath, "activity_labels.txt"))
setnames(activityNames, names(activityNames), c("activityNum", "activityName"))

#merge lables and data and set key

dt <- merge(dt, activityNames, by = "activityNum", all.x = TRUE)

setkey(dt, subject, activityNum, activityName)

dt <- data.table(melt(dt, key(dt), variable.name = "featureCode"))

dt <- merge(dt, dtFeatures[, list(featureNum, featureCode, featureName)], by = "featureCode", all.x = TRUE)

dt$activity <- factor(dt$activityName)
dt$feature <- factor(dt$featureName)

grepthis <- function(regex) {
    grepl(regex, dt$feature)
}
## Features with 2 categories
n <- 2
y <- matrix(seq(1, n), nrow = n)
x <- matrix(c(grepthis("^t"), grepthis("^f")), ncol = nrow(y))
dt$featDomain <- factor(x %*% y, labels = c("Time", "Freq"))
x <- matrix(c(grepthis("Acc"), grepthis("Gyro")), ncol = nrow(y))
dt$featInstrument <- factor(x %*% y, labels = c("Accelerometer", "Gyroscope"))
x <- matrix(c(grepthis("BodyAcc"), grepthis("GravityAcc")), ncol = nrow(y))
dt$featAcceleration <- factor(x %*% y, labels = c(NA, "Body", "Gravity"))
x <- matrix(c(grepthis("mean()"), grepthis("std()")), ncol = nrow(y))
dt$featVariable <- factor(x %*% y, labels = c("Mean", "SD"))
## Features with 1 category
dt$featJerk <- factor(grepthis("Jerk"), labels = c(NA, "Jerk"))
dt$featMagnitude <- factor(grepthis("Mag"), labels = c(NA, "Magnitude"))
## Features with 3 categories
n <- 3
y <- matrix(seq(1, n), nrow = n)
x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol = nrow(y))
dt$featAxis <- factor(x %*% y, labels = c(NA, "X", "Y", "Z"))

#  create the tidy data set

setkey(dt, subject, activity, featDomain, featAcceleration, featInstrument, 
       featJerk, featMagnitude, featVariable, featAxis)
dtTidy <- dt[, list(count = .N, average = mean(value)), by = key(dt)]

#write tidy data

f <- file.path(actualPath, "TidyData.txt")
write.table(dtTidy, f, quote=FALSE, sep="\t", row.names=FALSE)