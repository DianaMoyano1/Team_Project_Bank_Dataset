#Logistic Regression

# Importing the dataset

bank <- read.csv("bank.csv")

# what kind of data integer, character etc.

str(bank)

# Find the mean, median, min and max etc.

summary(bank)
# change the catagorical data 

bank$y <- factor(bank$y,
                 levels = c("no", "yes"),
                 labels = c(0,1))

bank$marital <- factor(bank$marital,
                       levels = c("single", "married", "divorced"),
                       labels = c(1, 2, 3))

bank$contact <- factor(bank$contact,
                       levels = c("telephone", "cellular", "unknown"),
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
                        labels = c(1,2,3,4))

bank$job <- factor(bank$job,
                   levels = c("admin.", "blue-collar", "entrepreneur", "housemaid", "management", "retired", "self-employed", "services", "student", "technician", "unemployed", "unknown"),
                   labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))

bank$month <- factor(bank$month,
                     levels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"),
                     labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))


bank$balance <- as.numeric(bank$balance)
bank$day <- as.numeric(bank$day)
bank$duration <- as.numeric(bank$duration)
bank$campaign <- as.numeric(bank$campaign)
bank$pdays <- as.numeric(bank$pdays)
bank$previous <- as.numeric(bank$previous)

# Checking correlation among independent numeric attributes

bank.cont <- data.frame(bank$age,bank$balance,bank$day,bank$duration,bank$campaign,bank$pdays, bank$previous)
str(bank.cont)
cor(bank.cont)

# Using stepwise logistic regression method to select the attributes 

step(glm(y ~ ., data = bank, family = "binomial"))

# So we are not connsider age, job, previous, default, pdays, balance


bank <- bank[c(3:4, 7:13, 16:17)]
summary(bank)

# Splitting the dataset into training set and test set

# install.packages("caTools")
library(caTools)
set.seed(123)
split = sample.split(bank$y, SplitRatio = 2/3)
training_set = subset(bank, split == TRUE)
test_set = subset(bank, split == FALSE)


# Fitting logistic regression to the training test

classifier = glm(y ~ ., 
                 family = binomial, data = training_set)
summary(classifier)

# Predicting the test results

prob_pred = predict(classifier, type = 'response', newdata = test_set)
y_pred = ifelse(prob_pred > 0.4, 1, 0)
y_pred

# Confusion matrix

cm = table(test_set$y, y_pred)
cm
# Accuracyis the ratio of correctly predicted observation to the total observations
Accuracy <- (cm[1,1]+cm[2,2]) / (cm[1,1]+cm[1,2]+cm[2,1]+cm[2,2])
Accuracy
# So we got 90% accuracy
# Precision is the ratio of correctly predicted positive observations to the total predicted positive observations.
Precision <- cm[2,2] / (cm[2,2]+ cm[1,2])
Precision
# We got 65% precision
# Recall is the ratio of correctly predicted positive observations to the all observations in actual class
Recall <- cm[2,2] / (cm[2,2] + cm[2,1])
Recall
# We got 34% recall
# F1 Score = 2*(Recall * Precision) / (Recall + Precision)
F1 <- 2*(Recall * Precision) / (Recall + Precision)
F1
# 45% is the F1 score
#ROCR Curve
library(ROCR)
ROCRpred = prediction(prob_pred, test_set$y)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize = TRUE, print.cutoffs.at = seq(0.1, by = 0.1))


