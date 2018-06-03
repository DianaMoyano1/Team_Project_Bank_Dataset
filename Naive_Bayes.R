# Naive Bayes 

# Importing the dataset

bank <- read.csv("bank.csv")

# change the catagorical data 

bank$y <- factor(bank$y,
                 levels = c("no", "yes"),
                 labels = c(0,1))

bank$marital <- factor(bank$marital,
                       levels = c("single", "married", "divorced"),
                       labels = c(1, 2, 3))

bank$contact <- factor(bank$contact,
                       levels = c("cellular", "telephone", "unknown"),
                       labels = c(1, 2, 3))

bank$education <- factor(bank$education,
                         levels = c("primary", "secondary", "tertiary", "unknown"),
                         labels = c(1, 2, 3, 4))

bank$default <- factor(bank$default,
                       levels = c("no", "yes"),
                       labels = c(0, 1))

bank$housing <- factor(bank$housing,
                       levels = c("no", "yes"),
                       labels = c(0, 1))

bank$loan <- factor(bank$loan,
                    levels = c("no", "yes"),
                    labels = c(0, 1))

bank$poutcome <- factor(bank$poutcome,
                        levels = c("failure", "success", "unknown", "other"),
                        labels = c(1, 2, 3,4))

bank$job <- factor(bank$job,
                   levels = c("admin.", "blue-collar", "entrepreneur", "housemaid", "management", "retired", "self-employed", "services", "student", "technician", "unemployed", "unknown"),
                   labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))

# The new bank dataset after removing age, job, previous, default, pdays, balance
bank <- bank[c(3:4, 7:13, 16:17)]
summary(bank)

bank$day <- as.numeric(bank$day)
bank$duration <- as.numeric(bank$duration)
bank$campaign <- as.numeric(bank$campaign)

# Feature Scaling

#bank$balance = scale(bank$balance)
bank$duration = scale(bank$duration)
#bank$pdays = scale(bank$pdays)

# Splitting the dataset into training set and test set

# install.packages("caTools")
library(caTools)
set.seed(123)
split = sample.split(bank$y, SplitRatio = 2/3)

training_set = subset(bank, split == TRUE)
test_set = subset(bank, split == FALSE)


# Fitting classifier to the training set

# install.packages("e1071")
library(e1071)
# install.packages("caret")
library(caret)
library(lattice)
library(ggplot2)

classifier = naiveBayes(y ~ ., data = training_set)
classifier

# Predicting the test set results

y_pred = predict(classifier, newdata = test_set)
y_pred

# Making confusion matrix

cm = table(test_set$y, y_pred)
cm

Accuracy <- (cm[1,1]+cm[2,2]) / (cm[1,1]+cm[1,2]+cm[2,1]+cm[2,2])
Accuracy

# we got 90% accuracy

Precision <- cm[2,2] / (cm[2,2]+ cm[1,2])
Precision
# We got 58% precision
# Recall is the ratio of correctly predicted positive observations to the all observations in actual class
Recall <- cm[2,2] / (cm[2,2] + cm[2,1])
Recall
# We got 42% recall
# F1 Score = 2*(Recall * Precision) / (Recall + Precision)
F1 <- 2*(Recall * Precision) / (Recall + Precision)
F1
# 49% is the F1 score



