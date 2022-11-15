# -------------------------- A)Load the libraries -------------------------------

library("ggplot2")
library("corrgram")
library("car")
library("class")
library("lattice")
library("ROCR")
library("plotly")
library("tree")
library(devtools)
library(tidyverse)
library(downloader)
library(BiocGenerics)
library(corrplot)
library(PerformanceAnalytics)
library(psych)
library(GGally)
library(MASS)
library(ISLR)
library(class)
library(randomForest)
library(RSNNS)
library(dplyr)
library(MASS)
library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(caret)
library(dplyr)
library(ROCR)


################
# Read the Data#
################
# url Breast Cancer data from Wisconsin

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data"


#########################################################################

################################################
#     Multi Layered Neural Networks: MLP       #
################################################

library(monmlp)
library(ROCR)
library(neuralnet)

data <- read.csv(file = url, header = FALSE,
                col.names = c("id","CT", "UCSize", "UCShape", "MA", "SECS", "BN", "BC", "NN","M", "diagnosis") )

data$outcome[data$diagnosis==4] = 1
data$outcome[data$diagnosis==2] = 0
data$outcome = as.integer(data$outcome)
data$outcome <- as.factor(data$outcome)

head(data)

data2=data

head(data2)

data2 <- data %>% dplyr::select(-id, -BN, -diagnosis)
head(data2)



set.seed(1729)
sample_size = floor(0.5 * nrow(data2))
train_set = sample(seq_len(nrow(data2)), size = sample_size)
training = data2[train_set, ]
## The Validation Set Approach ##
testing = data2[-train_set, ]
y.test <- data2$outcome[-train_set]

x.train <- training%>% dplyr::select(-outcome)
x.test <- testing%>% dplyr::select(-outcome)
y.train <- data2$outcome[train_set] 
y.test <- data2$outcome[-train_set]



n <- names(training)
f <- as.formula(paste("outcome ~",
				paste(n[!n %in% "outcome"],
				collapse = " + ")))
nn <- neuralnet(f,
			data = training,
			hidden = c(4, 2),
			linear.output = T)

# Plotting the graph
plot(nn)


#######################################################
#After this, compile our function for Predicting
#benign (not dangerous to health) or malignant 
#using the neural network as follows:
#######################################################
# it's for when we look at 0 an1 like integer not factor
# Predicting the medv
pr.nn <- compute(nn, testing[, 1:8])
pr.nn_ <- pr.nn$net.result * (max(data2$outcome)
						- min(data2$outcome))
						+ min(data2$outcome)
test.r <- (testing$outcome) * (max(data2$outcome)
						- min(data2$outcome))
						+ min(data2$outcome)

MSE.nn <- sum((test.r - pr.nn_)^2) / nrow(testing)
MSE.nn

######################### MLP #################################
data <- read.csv(file = url, header = FALSE,
                col.names = c("id","CT", "UCSize", "UCShape", "MA", "SECS", "BN", "BC", "NN","M", "diagnosis") )

data$outcome[data$diagnosis==4] = 1
data$outcome[data$diagnosis==2] = 0
data$outcome = as.integer(data$outcome)
#data$outcome <- as.factor(data$outcome)

head(data)
# set the seed to make your partition reproductible


#data2 <- data %>% dplyr::select(-id, -BN)

data2$outcome[data2$diagnosis==4] = 1
data2$outcome[data2$diagnosis==2] = 0
data2$outcome = as.integer(data2$outcome)
data2 <- data %>% dplyr::select(-id, -BN, -diagnosis)
head(data2)

set.seed(1729)
sample_size = floor(0.5 * nrow(data2))
train_set = sample(seq_len(nrow(data2)), size = sample_size)
training = data2[train_set, ]
## The Validation Set Approach ##
testing = data2[-train_set, ]

x.train <- training%>% dplyr::select(-outcome)
x.test <- testing%>% dplyr::select(-outcome)
y.train <- data2$outcome[train_set] 
y.test <- data2$outcome[-train_set]


library(RSNNS)
#shuffle the vector
model_mlp <- mlp(x.train, y.train, learnFuncParams=c(0.1), 
              maxit=100, inputsTest=x.test, targetsTest=y.test)



summary(model_mlp)
#model_mlp
weightMatrix(model_mlp)
extractNetInfo(model_mlp)

par(mfrow=c(1,1))
# Plotting the final graph
plotIterativeError(model_mlp,
				col = 'green',
				main = 'Real:Green...Predicted:Red',
				pch = 18, cex = 0.7)

############################################################
########################
# Random Forest        #
########################

# For classification
# randomForest
library(tree)
library(randomForest)


data <- read.csv(file = url, header = FALSE,
                col.names = c("id","CT", "UCSize", "UCShape", "MA", "SECS", "BN", "BC", "NN","M", "diagnosis") )

data$outcome[data$diagnosis==4] = 1
data$outcome[data$diagnosis==2] = 0
data$outcome = as.integer(data$outcome)
data$outcome <- as.factor(data$outcome)

head(data)
data2$outcome[data2$diagnosis==4] = 1
data2$outcome[data2$diagnosis==2] = 0
data2 <- data %>% dplyr::select(-id, -BN, -diagnosis)
head(data2)

set.seed(1729)
sample_size = floor(0.5 * nrow(data2))
train_set = sample(seq_len(nrow(data2)), size = sample_size)
training = data2[train_set, ]
## The Validation Set Approach ##
testing = data2[-train_set, ]

x.train <- training
x.test <- testing
y.train <- data2$outcome[train_set] 
y.test <- data2$outcome[-train_set]


#model.rf <- randomForest(outcome ~ .,
#						data = training,
#						importance = TRUE,
#						proximity = TRUE)

print(model.rf)
# Plot the error vs
# The number of trees graph
plot(model.rf)


############################################################

# Random Forests #
##################
rf.model<- randomForest(outcome ~., training, mtry = 1, 
				importance = TRUE, ntree = 500)
rf.model
# The number of trees graph
plot(rf.model)
importance(rf.model)
varImpPlot(rf.model)


# Predictions on the test data 
pred = predict(rf.model, testing)
pred <- as.factor(pred)
cm = confusionMatrix(testing$outcome, pred)
print(cm)

yhat.rf <- predict(rf.model, testing)
#plot(yhat.rf, y.test)
abline(0, 1)

cbind(pred, y.test)[1:50,]
mean_rf = mean((pred - y.test)^2)
mean_rf

# variable importance plot
importance(rf.model)
varImpPlot(rf.model)
############################################################

#Optimzing ntree and mtry values
noTrees <- seq(from = 500, to = 2000, by = 100)
set.seed(1234)
rfs <- lapply(noTrees, FUN = function(x){
  randomForest(outcome ~ ., 
               data = data2, importance = TRUE, ntree = x)
})

#Get errors
ntrees.err <- as.data.frame(sapply(rfs, FUN = function(x){
  confusion.df <- as.data.frame(x$confusion)
  oob.error <- round((confusion.df[1,2] + confusion.df[2,1]) * 100 / sum(confusion.df[-3]), 2)
  return (data.frame(x$ntree, oob.error))
}))

ntrees.err
############################################################

#############################
# Support Vector Classifier #
#############################


data <- read.csv(file = url, header = FALSE,
                col.names = c("id","CT", "UCSize", "UCShape", "MA", "SECS", "BN", "BC", "NN","M", "diagnosis") )

data$outcome[data$diagnosis==4] = 1
data$outcome[data$diagnosis==2] = 0
data$outcome = as.integer(data$outcome)
data$outcome <- as.factor(data$outcome)

head(data)
data2$outcome[data2$diagnosis==4] = 1
data2$outcome[data2$diagnosis==2] = 0
data2 <- data %>% dplyr::select(-id, -BN, -diagnosis)
head(data2)

set.seed(1729)
sample_size = floor(0.5 * nrow(data2))
train_set = sample(seq_len(nrow(data2)), size = sample_size)
training = data2[train_set, ]
## The Validation Set Approach ##
testing = data2[-train_set, ]

x.train <- training%>% dplyr::select(1,2, 9)
x.test <- testing%>% dplyr::select(1,2,9)
y.train <- data2$outcome[train_set] 
y.test <- data2$outcome[-train_set]
head(x.train)


library(e1071) # For Support Vector Machine (SVM)
library(rgl) # For 3d plot
# Fit Support Vector Classifier using a linear kernel

# A cost argument allows us to specify the cost of a violation to the margin.
# When the cost argument is small, then the margins will be wide and many support vectors.
# When the cost argument is large, then the margins will be narrow and a few support vectors.

# cost = 10
svm.fit <- svm(outcome ~ ., data = x.train, kernel = "linear", cost = 0.001, scale = FALSE)
svm.fit
plot(svm.fit, x.train) # crosses are support vectors
svm.fit$index # identify support vectors

# cost = 0.1
svm.fit1 = svm(outcome ~ ., data = x.train, kernel = "linear", cost = 0.1, scale = FALSE)
svm.fit1 
plot(svm.fit1, x.train)
svm.fit1$index

# Use 10-fold CV to find the best cost
set.seed(1122)
tune.out <- tune(svm, outcome ~ ., data = x.train, kernel = "linear", ranges = list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out) 

# Plot the best result
plot(tune.out$best.model, x.train)





##########################
# Support Vector Machine #
##########################

# Fit SVM using a non-linear kernel such as a polynomial or a radial
# When kernel = "radial", specify gamma.


# cost = 1
svm.fit10 <- svm(outcome ~ ., data = x.train, kernel = "radial", gamma = 1, cost = 1)
plot(svm.fit1,x.train)
summary(svm.fit10)

# cost = 1000
svm.fit20 <- svm(outcome ~ ., data = x.train, kernel = "radial", gamma = 1, cost = 1000)
plot(svm.fit20, x.train)
summary(svm.fit20)

# Use 10-fold CV to find the best cost and gamma
set.seed(1122)
tune.out <- tune(svm, outcome ~ ., data = x.train, kernel = "radial", ranges = list(cost = c(0.1, 1, 10, 2, 100, 1000), gamma = c(0.5, 1, 2, 3, 4)))
summary(tune.out)

# Check optimal cost and gamma
tune.out$best.model

# Plot the best result
plot(tune.out$best.model, x.train)

# Compute the missclassification error rate using the test set
svm.pred <- predict(tune.out$best.model, newdata = sim[-train, ])
table(sim$y[-train], svm.pred) 
########################################################################

#####################
# Gradiant Boosting #
#####################
library(gbm)
library(MASS)
gbm.model = gbm(outcome ~ ., distribution = "gaussian", data = training, n.trees = 10000, interaction.depth = 4, shrinkage = 0.01)
summary(gbm.model)
plot(gbm.model) 

n.trees = seq(from=100 ,to=10000, by=100) #no of trees-a vector of 100 values 

#Generating a Prediction matrix for each Tree
predmatrix<-predict(gbm.model,training,n.trees = n.trees)
dim(predmatrix) #dimentions of the Prediction Matrix

#Calculating The Mean squared Test Error
test.error<-with(training,apply( (predmatrix-outcome)^2,2,mean))
head(test.error) #contains the Mean squared test error for each of the 100 trees averaged

#Plotting the test error vs number of trees

plot(n.trees , test.error , pch=19,col="blue",xlab="Number of Trees",ylab="Test Error", main = "Perfomance of Boosting on Test Set")

#adding the RandomForests Minimum Error line trained on same data and similar parameters
abline(h = min(test.error),col="red") #test.err is the test error of a Random forest fitted on same data
legend("topright",c("Minimum Test error Line for Random Forests"),col="red",lty=1,lwd=1)







######################################################
# Comparison
######################################################

# Make a table for test error rate 
comparison_table <- rbind(logit.test.error, lda.test.error, qda.test.error, 
					knn.k5.test.error, knn.k20.test.error, knn.k50.test.error, 
					knn.k100.test.error, MSE.nn)
colnames(comparison_table) <- c("test error rate")
comparison_table

