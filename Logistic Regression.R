# Importing Packages and data sets from the Package
install.packages("MASS")
install.packages("caTools")
library(MASS)
library(caTools)

# Data Exploration
data(Pima.te)
View(Pima.te)
summary(Pima.te)
describe(Pima.te)
head(Pima.te)
labels(Pima.te)
table(Pima.te)
describe(Pima.te$type)
describe(Pima.te$bmi)
describe(Pima.te$glu)
cor(Pima.te$type,Pima.te$age)
# Spliting the data in training and testing of data
split <- sample.split(Pima.te, SplitRatio = 0.80)
split

# Devide data into traning and testing
training <- subset(Pima.te,split == TRUE)
testing <- subset(Pima.te,split == FALSE)

# Plot the Regression line with given data set
model <- glm(type~.,training ,family = "binomial")
summary(model)


# Consider most significant variables from results when "bp" remove
model <- glm(type~.-bp,training ,family = "binomial")
summary(model)

# Consider most significant variables from results when "skin" remove
model <- glm(type~.-skin,training ,family = "binomial")
summary(model)


# Consider most significant variables from results when "skin" remove
model <- glm(type~.-skin-bp,training ,family = "binomial")
summary(model)


# Consider most significant variables from results when "bp,skin,age" remove
model <- glm(type~.-skin-bp-age,training ,family = "binomial")
summary(model)

# Consider most significant variables from results when "bp,skin,age" remove
model <- glm(type~.-skin-bp,training ,family = "binomial")
summary(model)

# Response Values of Confusion Matrix
res <- predict(model,training,type = "response")
(table(ActualValue = training$type,PredictValue = res>0.5))

# From Result try to get Accuracy, Precision, Recall, Sesitivity Accuracy(TP+FP)/(Total)
#Accuracy
(152+52)/(152+52+16+29)


# Precision : TP/(TP+FP)
52/(52+16)

# Recall : TP/(TP+FN)
52/(52+29)

install.packages('ROCR')

# Plot of ROC Curve with Training data 
library(ROCR)
ROCRPred = prediction(res,training$type)
ROCRref <- performance(ROCRPred,"tpr","fpr")
plot(ROCRref,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))
