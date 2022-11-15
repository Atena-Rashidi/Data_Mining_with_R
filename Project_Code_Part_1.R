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


# Read the data
data <- read.csv(url)
head(data)

# Clean the Data

#Change the column headings
data <- read.csv(file = url, header = FALSE,
                col.names = c("id","CT", "UCSize", "UCShape", "MA", "SECS", "BN", "BC", "NN","M", "diagnosis") )

data$outcome[data$diagnosis==4] = 1
data$outcome[data$diagnosis==2] = 0
#data$outcome = as.integer(data$outcome)
data$outcome <- as.factor(data$outcome)

head(data)

# ----------- B)Adjust data set for analysis. Remove ID column and “?” values ------------

library(dplyr)
library(tidyverse)


# An error occurs because the select() function from the MASS package clashes 
# with the select() function from the dplyr package.
# The easiest way to fix this error is to explicitly tell R to use the select()function 
# from the dplyr package by using the following code:

data2 <- data %>% dplyr::select(-id, -BN, -diagnosis)

data2$outcome[data2$diagnosis==4] = 1
data2$outcome[data2$diagnosis==2] = 0
#data2$outcome = as.integer(data2$outcome)
data2$outcome <- as.factor(data2$outcome)

head(data2)

# -------------------------- Check Data Types ---------------------------------
 
glimpse(data2)

# Create a Correlation Chart for Means
chart.Correlation(data2[, c(1:8)], histogram=TRUE, col="grey10", pch=1, main="Cancer Means")

# Create Correlation Chart for SE
pairs.panels(data2[,c(1:8)], 
		 smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "pearson", # Correlation method (also "spearman" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals




######################################################
# the variables that have correlation coefficient > .7
######################################################

#numeric.data <- data2[sapply(data2, is.numeric)]
#descr.cor <- cor(numeric.data)
#highly.correlated <- findCorrelation(descr.cor, cutoff = 0.7, verbose = T, names = T)
#All correlations <= 0.7 
#highly.cor.col <- colnames(numeric.data[highly.correlated])
#data2 <- data2[, -which(colnames(data2) %in% highly.cor.col)]
#glimpse(data2)



###############################################################################
# ----- Split the dataset into the training sample and the testing sample -----
###############################################################################

sample_size = floor(0.5 * nrow(data2))

# set the seed to make your partition reproductible
set.seed(1729)
train_set = sample(seq_len(nrow(data2)), size = sample_size)

training = data2[train_set, ]

## The Validation Set Approach ##
testing = data2[-train_set, ]
y.test <- data2$outcome[-train_set]

head(training)
head(testing)

# Look at the Data Set, Training Data and Testing Data
ggplot(data2, aes(x = outcome)) +
  geom_bar(aes(fill = "blue")) +
  ggtitle("Distribution of diagnosis for the entire dataset") +
  theme(legend.position="none")


ggplot(training, aes(x = diagnosis)) + 
  geom_bar(aes(fill = 'blue')) + 
  ggtitle("Distribution of diagnosis for the training dataset") + 
  theme(legend.position="none")


ggplot(testing, aes(x = outcome)) + 
  geom_bar(aes(fill = 'blue')) + 
  ggtitle("Distribution of diagnosis for the testing dataset") + 
  theme(legend.position="none")


# Corrgram of the entire dataset
corrgram(data2, order=NULL, lower.panel=panel.shade, upper.panel=NULL, text.panel=panel.txt,
         main="Corrgram of the data")
 	   

# Corrgram of the training dataset
corrgram(training, order=NULL, lower.panel=panel.shade, upper.panel=NULL, text.panel=panel.txt,
         main="Corrgram of the training data")
    

# Corrgram of the testing dataset
corrgram(testing, order=NULL, lower.panel=panel.shade, upper.panel=NULL, text.panel=panel.txt,
         main="Corrgram of the testing data")
       



# ------------------ Coefficients and p-values ----------------
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.6, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.2, 0.1, txt2)
}
panel.cor

# Graph the linear correlation coef
pairs(training, upper.panel = panel.cor)

#############################################################
#															#
# 				MODEL FITTING								#
#															#
#############################################################

# Fit the model using glm Generalize Linear Model
# Model Fitting
# Start off with this (alpha = 0.05)

############################################
#	  GLM: Generalize Linear Model	 #
############################################	


##############################################################################
# Using features as predictors of diagnosis
##############################################################################
		 
model_algorithm = model = glm(outcome ~ CT + 
                                UCSize +
                                UCShape +
                                MA +
                                SECS + 
                                BC  +
                                NN  +
                                M ,
                               family=binomial(link='logit'), control = list(maxit = 50),data=training)

print(summary(model_algorithm))
coef(model_algorithm)
print(anova(model_algorithm, test="Chisq"))

##############################################################################
# Using Uniform Cell size and Uniform Cell Shape as predictors of diagnosis
##############################################################################

# Settled Uniform Cell Size and Uniform Cell Shape

model_algorithm_final = model = glm(outcome ~ UCSize + UCShape ,
                                    family=binomial(link='logit'), control = list(maxit = 50),data=training)

print(summary(model_algorithm_final))

model_algorithm_final = model = glm(outcome ~ UCSize + UCShape + MA ,
                                    family=binomial(link='logit'), control = list(maxit = 50),data=training)

print(summary(model_algorithm_final))

##################################################
# Apply the GLM algorithm to the training sample #
##################################################

prediction_training = predict(model_algorithm_final,training, type = "response")
prediction_training = ifelse(prediction_training > 0.5, 1, 0)
error = mean(prediction_training != training$outcome)
print(paste('Model Accuracy:',1-error))

#################################################
# Apply the GLM algorithm to the testing sample #
#################################################

prediction_testing = predict(model_algorithm_final,testing, type = "response")
prediction_testing = ifelse(prediction_testing > 0.5, 1, 0)
error = mean(prediction_testing != testing$outcome)
print(paste('Model Accuracy:',1-error))

#######################################
# Calcualte the ROC curve and the AUC #
#######################################

# -------------- Training -------------
# Apply the algorithm to the training sample
p = predict(model_algorithm_final, training, type="response")
pr = prediction(p, training$outcome)
prf = performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc = performance(pr, measure = "auc")
auc = auc@y.values[[1]]
print(paste("Model Accuracy:", auc))

# -------------- Testing -------------
# Apply the algorithm to the testing sample
prediction_testing = predict(model_algorithm_final,testing, type = "response")
prediction_testing = ifelse(prediction_testing > 0.5, 1, 0)
error = mean(prediction_testing != testing$outcome)
print(paste('Model Accuracy:',1-error))

# Get the ROC curve and the AUC
p = predict(model_algorithm_final, testing, type="response")
pr = prediction(p, testing$outcome)
prf = performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
# ---------------------------------------------------------------------------------------

#######################
# Logistic Regression #
#######################

model_algorithm_final = model = glm(outcome ~ CT + 
                                UCSize +
                                UCShape +
                                MA +
                                SECS + 
                                BC  +
                                NN  +
                                M ,
                                family=binomial(link='logit'), control = list(maxit = 50),data=training)

summary(model_algorithm_final)

logit.prob <- predict(model_algorithm_final, testing, type = "response")
logit.pred <- rep("Benign", dim(testing)[1])
# benign (not dangerous to health) or malignant 
logit.pred[logit.prob > 0.5] = "Malignant"
logit.table <- table(logit.pred, y.test)
logit.table
logit.test.error <- (logit.table[1,2] + logit.table[2,1])/sum(logit.table)
logit.test.error

# ######################################################################

#######
# LDA #
#######

lda.fit <- lda(outcome ~ CT + 
                         UCSize +
                         UCShape +
                         MA +
                         SECS + 
                         BC  +
                         NN  +
                         M , data = training)
lda.pred <- predict(lda.fit, testing)
lda.class <- lda.pred$class 
lda.table <- table(lda.class, y.test)
lda.table 
lda.test.error <- (lda.table[1,2] + lda.table[2,1])/sum(lda.table)
lda.test.error

########################################################################

#######
# QDA #
#######
qda.fit <- qda(outcome ~ CT + 
                         UCSize +
                         UCShape +
                         MA +
                         SECS + 
                         BC  +
                         NN  +
                         M , data = training)
qda.pred <- predict(qda.fit, testing)
qda.class <- qda.pred$class 
qda.table <- table(qda.class, y.test)
qda.table
qda.test.error <- (qda.table[1,2] + qda.table[2,1])/sum(qda.table)
qda.test.error

# #######################################################################

#############
# k-fold CV #
#############

# We will use Default data set

set.seed(1011)
# 5-fold CV
kfolds <- 5
folds <- rep_len(1:kfolds, dim(data2)[1])
folds <- sample(folds, dim(data2)[1])

lda.test.error.fold <- rep(0, kfolds)
qda.test.error.fold <- rep(0, kfolds)

for(k in 1:kfolds){
  fold <- which(folds == k)
  
  default_train <- data2[-fold, ]
  default_test <- data2[fold, ]
  
  lda.fit <- lda(outcome ~ ., data = default_train)
  lda.pred <- predict(lda.fit, default_test)
  lda.class <- lda.pred$class 
  lda.table <- table(lda.class, default_test$outcome)
  lda.test.error <- (lda.table[1,2] + lda.table[2,1])/sum(lda.table)
  lda.test.error.fold[k] <- lda.test.error
  
  qda.fit <- qda(outcome ~ ., data = default_train)
  qda.pred <- predict(qda.fit, default_test)
  qda.class <- qda.pred$class 
  qda.table <- table(qda.class, default_test$outcome)
  qda.test.error <- (qda.table[1,2] + qda.table[2,1])/sum(qda.table)
  qda.test.error.fold[k] <- qda.test.error
}

lda.error <- mean(lda.test.error.fold)
lda.error

qda.error <- mean(qda.test.error.fold)
qda.error

# Make a table for test error rate 
comparison_table <- rbind(lda.error, qda.error)
colnames(comparison_table) <- c("test error rate")
comparison_table

#########################################################################

# set the seed to make your partition reproductible
set.seed(1729)
train_set = sample(seq_len(nrow(data2)), size = sample_size)
training = data2[train_set, ]
## The Validation Set Approach ##
testing = data2[-train_set, ]
y.test <- data2$outcome[-train_set]

x.train <- training[, c(1:9)]
x.test <- testing[, c(1:9)]
y.train <- data2$outcome[train_set] 
y.test <- data2$outcome[-train_set]

#############
#    KNN	#
#############

# KNN with k = 5
knn.k5.pred <- knn(x.train, x.test, y.train, k = 5)
knn.k5.table <- table(knn.k5.pred, y.test)
knn.k5.table
knn.k5.test.error <- (knn.k5.table[1,2] + knn.k5.table[2,1])/sum(knn.k5.table)
knn.k5.test.error

# KNN with k = 20
knn.k20.pred <- knn(x.train, x.test, y.train, k = 20)
knn.k20.table <- table(knn.k20.pred, y.test)
knn.k20.table
knn.k20.test.error <- (knn.k20.table[1,2] + knn.k20.table[2,1])/sum(knn.k20.table)
knn.k20.test.error

# KNN with k = 50
knn.k50.pred <- knn(x.train, x.test, y.train, k = 50)
knn.k50.table <- table(knn.k50.pred, y.test)
knn.k50.table
knn.k50.test.error <- (knn.k50.table[1,2] + knn.k50.table[2,1])/sum(knn.k50.table)
knn.k50.test.error

# KNN with k = 100
knn.k100.pred <- knn(x.train, x.test, y.train, k = 100)
knn.k100.table <- table(knn.k100.pred, y.test)
knn.k100.table
knn.k100.test.error <- (knn.k100.table[1,2] + knn.k100.table[2,1])/sum(knn.k100.table)
knn.k100.test.error

# Make a table for test error rate 
comparison_table <- rbind(logit.test.error, lda.test.error, qda.test.error, knn.k5.test.error, knn.k20.test.error, knn.k50.test.error, knn.k100.test.error)
colnames(comparison_table) <- c("test error rate")
comparison_table

# ##############################################################################

#################
# Decision Tree #
#################

# Droping the outcome variable which was used for the logistic model



#training$diagnosis[training$diagnosis == 4] = 1
#training$diagnosis[training$diagnosis ==2] = 0

attach(training)
# Running our first tree 
model_tree = tree(outcome ~ UCSize + 
                    UCShape +
                    MA +
                    SECS +
                    BC  + 
                    NN  +
                    M,
                    data = training)

summary(model_tree)

# Now we want to plot our results
plot(model_tree, type = "uniform")

# Add some text to the plot
text(model_tree, pretty = 0, cex=0.8)

# Check the tree on the training data
# Distributional prediction

model_tree_pred_train = predict(model_tree, training) # gives the probability for each class
model_tree_pred_test = predict(model_tree, testing) # gives the probability for each class

#########################################################################

# set the seed to make your partition reproductible
set.seed(1729)
train_set = sample(seq_len(nrow(data2)), size = sample_size)
training = data2[train_set, ]
## The Validation Set Approach ##
testing = data2[-train_set, ]
y.test <- data2$outcome[-train_set]

x.train <- training[, c(1:9)]
x.test <- testing[, c(1:9)]
y.train <- data2$outcome[train_set] 
y.test <- data2$outcome[-train_set]
# Try to prune the tree to avoid over fitting
cv.tree(model_tree)

plot(cv.tree(model_tree)) # Seems like a tree of size 5 might be best

# Pruned model
model_tree_prune = prune.tree(model_tree, best = 5)
summary(model_tree_prune)

# Now we want to plot our results
plot(model_tree_prune, type = "uniform")

# Add some text to the plot
text(model_tree, pretty = 0, cex=0.8)

head(data2)
all_pca <- prcomp(data2[,1:8], cor=TRUE, scale = TRUE)
summary(all_pca)


library(factoextra)
fviz_eig(all_pca, addlabels=TRUE, ylim=c(0,60), geom = c("bar", "line"), barfill = "pink",  
         barcolor="blue",linecolor = "red", ncp=10)+
         labs(title = "Cancer All Variances - PCA",
          x = "Principal Components", y = "% of variances")


all_var <- get_pca_var(all_pca)
all_var

corrplot(all_var$cos2, is.corr=FALSE)
corrplot(all_var$contrib, is.corr=FALSE) 

library(gridExtra)
p1 <- fviz_contrib(all_pca, choice="var", axes=1, fill="pink", color="grey", top=10)
p2 <- fviz_contrib(all_pca, choice="var", axes=2, fill="skyblue", color="grey", top=10)
grid.arrange(p1,p2,ncol=2)  


set.seed(218)
res.all <- kmeans(all_var$coord, centers = 6, nstart = 25)
grp <- as.factor(res.all$cluster)

fviz_pca_var(all_pca, col.var = grp, 
             palette = "jco",
             legend.title = "Cluster") 


head(data2)
data2$outcome[data$diagnosis==4] = 'M'
data2$outcome[data$diagnosis==2] = 'B'

fviz_pca_biplot(all_pca, col.ind = data2$outcome, col="black",
                palette = "jco", geom = "point", repel=TRUE,
                legend.title="Diagnosis", addEllipses = TRUE)

