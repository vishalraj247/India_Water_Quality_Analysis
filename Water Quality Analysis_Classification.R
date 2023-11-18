#importing libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(lattice)
library(ROSE)
library(e1071)
library(corrplot)
library(caret)
library(randomForest)
library(xgboost)

## Data Gathering

#importing datasets into a data frame
wp <- read.csv("water_potability.csv")
wq <- read.csv("waterQuality1.csv")

#number of variable as columns and individual rows as values
dim(wp)
dim(wq)

#structure of the data frames
str(wp)
str(wq)

#counting unique values in the datasets
count_unique<-rapply(wp, function(x) length(unique(x)))
count_unique
count_unique1<-rapply(wq, function(x) length(unique(x)))
count_unique1

sum(is.na(wp))
sum(is.na(wq))

################

# % of NA in whole of 1st Dataset
sum(is.na(wp))/nrow(wp)
#Replacing NAs with median
wp <- wp %>%
  group_by(Potability) %>%
  mutate_at(vars(ph, Sulfate, Trihalomethanes), ~replace_na(., median(., na.rm = TRUE))) %>%
  summarise(ph=ph,Hardness=Hardness,Solids=Solids,Chloramines=Chloramines,Sulfate=Sulfate,
            Conductivity=Conductivity,Organic_carbon=Organic_carbon,Trihalomethanes=Trihalomethanes,
            Turbidity=Turbidity,.groups = 'drop')

head(wp)

## Data Cleaning and Processing
wp$Potability <- as.factor(wp$Potability)
wq$is_safe <- as.factor(wq$is_safe)

wq <- ovun.sample(is_safe~., data = wq, method = "over", N = 14200)$data
table(wq$is_safe)

theme_set(
  theme_classic() + 
    theme(legend.position = "top"))

# change fill and outline color manually 

colNames <- names(wp)[2:10]
for (i in colNames) {
  print(ggplot(wp, aes_string(x=i)) +
          geom_histogram(aes(color = Potability, fill = Potability), 
                         position = "identity", bins = 20, alpha = 0.4) +
          scale_color_manual(values = c("#00AFBB", "#E7B800")) +
          scale_fill_manual(values = c("#00AFBB", "#E7B800")))
}

########################################################
########################################################
################# MY PART-> ############################

#Correlation Plot
wp$Potability <- as.numeric(wp$Potability)
wp$Potability[wp$Potability=="1"] <- "0"
wp$Potability[wp$Potability=="2"] <- "1"
wp$Potability <- as.numeric(wp$Potability)

corr <- cor(wp[,1:10])
corr                                                                                #Checking colinearity
findCorrelation(corr, cutoff= 0.5, verbose = TRUE)                                  #Finding high correlation
corrplot(corr, method="circle")                                                     #Plotting Correlation
corrplot.mixed(corr)
wp$Potability <- as.factor(wp$Potability)

wq$is_safe <- as.numeric(wq$is_safe)
wq$is_safe[wq$is_safe=="1"] <- "0"
wq$is_safe[wq$is_safe=="2"] <- "1"
wq$is_safe <- as.numeric(wq$is_safe)

corr <- cor(wq[,1:21])
corr                                                                                #Checking colinearity
findCorrelation(corr, cutoff= 0.5, verbose = TRUE)                                  #Finding high correlation
corrplot(corr, method="circle")                                                     #Plotting Correlation
corrplot.mixed(corr)
wq$is_safe <- as.factor(wq$is_safe)


colNames1 <- names(wq)[1:20]
for (i in colNames1) {
print(ggplot(wq, aes_string(x=i)) +
  geom_histogram(aes(color = is_safe, fill = is_safe), 
                 position = "identity", bins = 20, alpha = 0.4) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")))
}

str(wp)
str(wq)

#Modelling on first dataset

wp <- ovun.sample(Potability~., data = wp, method = "over", N = 3995)$data
table(wp$Potability)

# 80% training and 20% test dataset
set.seed(24) 
train_index <- createDataPartition(y = wp$Potability, p = 0.80, list = FALSE)
train_water <- wp[train_index, ]
test_water <- wp[-train_index, ]


# Logistic regression
set.seed(24)
potability_lr <- train(Potability ~ ., method = "glm", data = train_water, 
                       family = binomial(link = "logit"),
                       trControl = trainControl(method = 'cv', number = 5))

# K-nearest neighbors
set.seed(24) 
potability_knn <- train(Potability ~ ., method = "knn", data = train_water,
                        trControl = trainControl(method = 'cv', number = 5, returnResamp = "all"))

# Random forest
set.seed(24)
potability_rf <- randomForest(Potability ~ .,
                              data=train_water, ntree= 1000)

# Extreme Gradient Boosting
set.seed(24)
potability_gbt <- train(Potability ~ ., method = "xgbTree", data = train_water, verbosity = 0,
                       trControl = trainControl("cv", number = 10))


potability_lr
potability_knn
potability_rf
potability_gbt

# Prediction on test-dataset
predicted_lr <- predict(potability_lr, test_water)
predicted_knn <- predict(potability_knn, test_water)
predicted_rf <- predict(potability_rf, test_water)
predicted_gbt <- predict(potability_gbt, test_water)

# Create Confusion Matrices
logistic_cfm <- confusionMatrix(predicted_lr, test_water$Potability, positive='1')
knn_cfm <- confusionMatrix(predicted_knn, test_water$Potability, positive='1')
rf_cfm <- confusionMatrix(predicted_rf, test_water$Potability, positive='1')
gbt_cfm <- confusionMatrix(predicted_gbt, test_water$Potability, positive='1')

logistic_cfm
knn_cfm
rf_cfm
gbt_cfm

accuracy_lr <- logistic_cfm$byClass['Balanced Accuracy']
accuracy_knn <- knn_cfm$byClass['Balanced Accuracy']
accuracy_rf <- rf_cfm$byClass['Balanced Accuracy']
accuracy_gbt <- gbt_cfm$byClass['Balanced Accuracy']

# plot of confusion matrices
plot_logistic <- as.data.frame(logistic_cfm$table)
plot_logistic$Prediction <- factor(plot_logistic$Prediction, 
                                   levels=rev(levels(plot_logistic$Prediction)))

plot_knn <- as.data.frame(knn_cfm$table)
plot_knn$Prediction <- factor(plot_knn$Prediction, levels=rev(levels(plot_knn$Prediction)))

plot_rf <- as.data.frame(rf_cfm$table)
plot_rf$Prediction <- factor(plot_rf$Prediction, levels=rev(levels(plot_rf$Prediction)))

plot_gbt <- as.data.frame(gbt_cfm$table)
plot_gbt$Prediction <- factor(plot_gbt$Prediction, levels=rev(levels(plot_gbt$Prediction)))


plot_conf_logistic <- ggplot(plot_logistic, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) + theme(legend.position = "None") +
  ggtitle("Logistic Regression. Accuracy: " ,(getElement(accuracy_lr, "Balanced Accuracy")*100))

plot_conf_knn <- ggplot(plot_knn, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) + theme(legend.position = "None") +
  ggtitle("K-Nearest Neighbor. Accuracy: " ,(getElement(accuracy_knn, "Balanced Accuracy")*100))

plot_conf_rf <- ggplot(plot_rf, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) + theme(legend.position = "None") +
  ggtitle("Random Forest. Accuracy: " ,(getElement(accuracy_rf, "Balanced Accuracy")*100))

plot_conf_gbt <- ggplot(plot_gbt, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) + theme(legend.position = "None") +
  ggtitle("Extreme Gradient Boosting. Accuracy: " ,(getElement(accuracy_gbt, "Balanced Accuracy")*100))


plot_conf_logistic
plot_conf_knn
plot_conf_rf
plot_conf_gbt

###############################
#Modelling on second dataset

# 80% training and 20% test dataset
set.seed(37) 
train_index1 <- createDataPartition(y = wq$is_safe, p = 0.80, list = FALSE)
train_water1 <- wq[train_index1, ]
test_water1 <- wq[-train_index1, ]


# Logistic regression
set.seed(37)
potability_lr1 <- train(is_safe ~ ., method = "glm", data = train_water1, 
                        family = binomial(link = "logit"),
                        trControl = trainControl(method = 'cv', number = 5))

# K-nearest neighbors
set.seed(37) 
potability_knn1 <- train(is_safe ~ ., method = "knn", data = train_water1,
                        trControl = trainControl(method = 'cv', number = 5, returnResamp = "all"))

# Random forest
set.seed(37)
potability_rf1 <- randomForest(is_safe ~ .,
                              data=train_water1, ntree= 1000)

# Extreme Gradient Boosting
set.seed(37)
potability_gbt1 <- train(is_safe ~ ., method = "xgbTree", data = train_water1, verbosity = 0, 
                       trControl = trainControl("cv", number = 10))


potability_rf1

# Prediction on test-dataset
predicted_lr1 <- predict(potability_lr1, test_water1)
predicted_knn1 <- predict(potability_knn1, test_water1)
predicted_rf1 <- predict(potability_rf1, test_water1)
predicted_gbt1 <- predict(potability_gbt1, test_water1)

# Create Confusion Matrices
logistic_cfm1 <- confusionMatrix(predicted_lr1, test_water1$is_safe, positive='1')
knn_cfm1 <- confusionMatrix(predicted_knn1, test_water1$is_safe, positive='1')
rf_cfm1 <- confusionMatrix(predicted_rf1, test_water1$is_safe, positive='1')
gbt_cfm1 <- confusionMatrix(predicted_gbt1, test_water1$is_safe, positive='1')

logistic_cfm1
knn_cfm1
rf_cfm1
gbt_cfm1

accuracy_lr1 <- logistic_cfm1$byClass['Balanced Accuracy']
accuracy_knn1 <- knn_cfm1$byClass['Balanced Accuracy']
accuracy_rf1 <- rf_cfm1$byClass['Balanced Accuracy']
accuracy_gbt1 <- gbt_cfm1$byClass['Balanced Accuracy']


# plot of confusion matrices
plot_logistic1 <- as.data.frame(logistic_cfm1$table)
plot_logistic1$Prediction <- factor(plot_logistic1$Prediction, 
                                    levels=rev(levels(plot_logistic1$Prediction)))

plot_knn1 <- as.data.frame(knn_cfm1$table)
plot_knn1$Prediction <- factor(plot_knn1$Prediction, levels=rev(levels(plot_knn1$Prediction)))

plot_rf1 <- as.data.frame(rf_cfm1$table)
plot_rf1$Prediction <- factor(plot_rf1$Prediction, levels=rev(levels(plot_rf1$Prediction)))

plot_gbt1 <- as.data.frame(gbt_cfm1$table)
plot_gbt1$Prediction <- factor(plot_gbt1$Prediction, levels=rev(levels(plot_gbt1$Prediction)))


plot_conf_logistic1 <- ggplot(plot_logistic1, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) + theme(legend.position = "None") +
  ggtitle("Logistic Regression. Accuracy: " ,(getElement(accuracy_lr1, "Balanced Accuracy")*100))

plot_conf_knn1 <- ggplot(plot_knn1, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) + theme(legend.position = "None") +
  ggtitle("K-Nearest Neighbor. Accuracy: ",(getElement(accuracy_knn1, "Balanced Accuracy")*100))

plot_conf_rf1 <- ggplot(plot_rf1, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) + theme(legend.position = "None") +
  ggtitle("Random Forest. Accuracy: ",(getElement(accuracy_rf1, "Balanced Accuracy")*100))

plot_conf_gbt1 <- ggplot(plot_gbt1, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) + theme(legend.position = "None") +
  ggtitle("Extreme Gradient Boosting. Accuracy: ",(getElement(accuracy_gbt1, "Balanced Accuracy")*100))


plot_conf_logistic1
plot_conf_knn1
plot_conf_rf1
plot_conf_gbt1