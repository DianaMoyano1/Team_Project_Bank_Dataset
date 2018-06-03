
# Decision Tree 

# Importing the dataset

bank <- read.csv("bank.csv")

# what kind of data integer, character etc.

str(bank)

# Find the mean, median, min and max etc.

summary(bank)

bank$balance <- as.numeric(bank$balance)
bank$day <- as.numeric(bank$day)
bank$duration <- as.numeric(bank$duration)
bank$campaign <- as.numeric(bank$campaign)
bank$pdays <- as.numeric(bank$pdays)
bank$previous <- as.numeric(bank$previous)


# The new bank dataset after removing age, job, previous, default, pdays, balance
bank <- bank[c(3:4, 7:13, 16:17)]
# Splitting the dataset into training set and test set

# install.packages("caTools")
library(caTools)
set.seed(123)
split = sample.split(bank$y, SplitRatio = 2/3)

training_set = subset(bank, split == TRUE)
test_set = subset(bank, split == FALSE)


# Fitting classifier to the training set

# install.packages("rpart")
library(rpart)

classifier = rpart(y ~ ., data = training_set)
classifier
# Plot the data

plot(classifier, margin = 0.1)
text(classifier, use.n = TRUE, pretty = TRUE, cex = 0.8)

#install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(classifier,type = 4,extra=100, branch.lty=2,nn=TRUE,digits=4, fallen.leaves = FALSE)

# Predicting the test set results

y_pred = predict(classifier, newdata = test_set, type = "class")
y_pred
# Making confusion matrix
library("caret")
cm = table(test_set$y, y_pred)
cm
Accuracy = (cm[1,1]+cm[2,2]) / (cm[1,1]+cm[1,2]+cm[2,1]+cm[2,2])
Accuracy
#We got 89% accuracy

Precision <- cm[2,2] / (cm[2,2]+ cm[1,2])
Precision
# We got 57% precision
# Recall is the ratio of correctly predicted positive observations to the all observations in actual class
Recall <- cm[2,2] / (cm[2,2] + cm[2,1])
Recall
# We got 33% recall
# F1 Score = 2*(Recall * Precision) / (Recall + Precision)
F1 <- 2*(Recall * Precision) / (Recall + Precision)
F1
# 42% is the F1 score
