# Looking at the information gain to choose all relevant variables
originalData <- read.csv("MS - Quarter II/Data Analytics/Breast Cancer METABRIC-Original.csv")
originalData$`Relapse Free Status` <- as.factor(originalData$`Relapse Free Status`)
# install.packages("FSelector")
library("FSelector")
infoGainOriginal <- information.gain(`Relapse Free Status`~., originalData)
infoGainOriginal

# Loading data set with only relevant variables and missing data removed
library(readr)
bcData <- read_csv("MS - Quarter II/Data Analytics/Breast Cancer METABRIC-Final(words).csv")
bcData$`Relapse Free Status` <- as.factor(bcData$`Relapse Free Status`)
str(bcData)

# Looking at the information gain for these variables
# install.packages("FSelector")
library("FSelector")
infoGain <- information.gain(`Relapse Free Status`~., bcData)
infoGain

# Create training and testing data sets
# install.packages("caret")
library("caret")
set.seed(3456)
trainIndex <- createDataPartition(bcData$`Relapse Free Status`, p = 0.7, list = FALSE, times = 1)
trainData <- bcData[trainIndex, ]
testData <- bcData[-trainIndex, ]

write.csv(trainData, 'bcDataTrainSet.csv')
write.csv(testData, "bcDataTestSet.csv")

# Removing the Patient ID
trainData <- trainData[ , -1]
testData <- testData[ , -1]

# Decision tree classification
library(rpart)
library(rpart.plot)

model <- rpart(`Relapse Free Status`~., data = trainData, method = 'class')
rpart.plot(model)

prediction <-predict(model, testData, type = 'class')
table_mat <- table(testData$`Relapse Free Status`, prediction)
table_mat

treeAccuracy <- ((table_mat[1,1] + table_mat[2,2])/nrow(testData))*100
treeAccuracy
# 96.30252% accuracy -- more false negatives (21) vs 1 false positive


# Naive Bayes
library(e1071)
NBmodel <- naiveBayes(`Relapse Free Status`~ ., data = trainData, na.action = na.pass)
predict(NBmodel, testData)

pred <- predict(NBmodel, testData)
table(testData$`Relapse Free Status`, pred)
NBaccuracy <- ((321+203)/595)*100
NBaccuracy
#88.06723% accurate -- 38 false negatives and 33 false positives

# Using Laplace Smoothing
NBmodelLS <- naiveBayes(`Relapse Free Status`~ ., data = trainData, laplace = 1, na.action = na.pass)
predLS <- predict(NBmodelLS, testData)
table(testData$`Relapse Free Status`, predLS)
# Accuracy was the same as without Laplace Smoothing


# K-Nearest Neighbor classification

# Using the data set with dummy variables for all of the categorical variables
bcDataN <- read_csv("MS - Quarter II/Data Analytics/Breast Cancer METABRIC-Final(numeric).csv")
bcDataN$`Pam50 + 2 subtype` <- as.factor(bcDataN$`Pam50 + 2 subtype`)                   
bcDataN$`Integrative Cluster` <- as.factor(bcDataN$`Integrative Cluster`)
bcDataN$`Overall Survival Status` <- as.factor(bcDataN$`Overall Survival Status`)
bcDataN$`Relapse Free Status` <- as.factor(bcDataN$`Relapse Free Status`)
bcDataN$`Patient's Vital Status` <- as.factor(bcDataN$`Patient's Vital Status`)
str(bcDataN)

# Create training and testing data sets
# install.packages("caret")
library("caret")
set.seed(3456)
trainDataN <- bcDataN[trainIndex, ]
testDataN <- bcDataN[-trainIndex, ]

write.csv(trainDataN, 'bcDataTrainSetN.csv')
write.csv(testDataN, "bcDataTestSetN.csv")

# Removing the Patient ID
trainDataN <- trainDataN[ , -1]
testDataN <- testDataN[ , -1]

# Remove Relapse Status from the data and remove na's
noNATrainDataN <- na.omit(trainDataN)
noNATestDataN <- na.omit(testDataN)
trainDataKnn <- noNATrainDataN[, -8]
testDataKnn <- noNATestDataN[, -8]

relapseTrainLabels <- noNATrainDataN[, 8]
relapseTestLabels <- noNATestDataN[, 8]

# k = 15
classPredictionK15 <- knn(train = trainDataKnn, test = testDataKnn, cl = relapseTrainLabels$`Relapse Free Status`, k=15)

CrossTable(x = relapseTestLabels$`Relapse Free Status`, y = classPredictionK15, prop.chisq=FALSE)
k15NNAccuracy <- ((223+127)/414)*100
k15NNAccuracy
# 84.54106% accuracy -- more false negatives (43) and false positives (21)

# k = 30
classPredictionK30 <- knn(train = trainDataKnn, test = testDataKnn, cl = relapseTrainLabels$`Relapse Free Status`, k=30)

CrossTable(x = relapseTestLabels$`Relapse Free Status`, y = classPredictionK30, prop.chisq=FALSE)
k30NNAccuracy <- ((218+118)/414)*100
k30NNAccuracy
# 81.15942% accuracy -- more false negatives (52) and false positives (26)

#k = 5
classPredictionK5 <- knn(train = trainDataKnn, test = testDataKnn, cl = relapseTrainLabels$`Relapse Free Status`, k=5)

CrossTable(x = relapseTestLabels$`Relapse Free Status`, y = classPredictionK5, prop.chisq=FALSE)
k5NNAccuracy <- ((227+130)/414)*100
k5NNAccuracy
# 86.23188% accuracy -- fewer false positives (17), lower number of false negatives (40)




