library(caret)


inputdata = read.csv('adult.csv')
str(inputdata)

table(inputdata$income)

inputdata$above50k = ifelse(inputdata$income == " <=50K", 0, 1) #dummy variable creation
table(inputdata$above50k)
inputdata$income = NULL
barplot(table(inputdata$above50k))

# using stratified sampling in training an test sets to keep the dependent variable in equal proportions
# install.packages('splitstackshape')

library(splitstackshape)
library(rpart)
library(rpart.plot)
set.seed(1000)

splitdata = stratified(inputdata, "above50k", 0.8, bothSets = TRUE)
trainingdata = as.data.frame(splitdata$SAMP1)
testdata = as.data.frame(splitdata$SAMP2)

Class_tree_adult = rpart(above50k ~ relationship + marital.status + age + capital.gain + occupation + education.num,
                         data = trainingdata)

rpart.plot(Class_tree_adult, digits = 3)
summary(Class_tree_adult)

predicted = predict(Class_tree_adult, testdata)
head(predicted, 10)  # represent probabilities of being 0 or 1 (if > 0.5 classification = 1)

# Accuracy, Sensitivity, Specificity 
# install.packages('caret')
# install.packages('e1071')
library(caret)

predicted_factor = ifelse(predicted >= 0.5, 1, 0)

confusionMatrix(as.factor(testdata$above50k), as.factor(predicted_factor), positive = '1')

#Random Forest
#install.packages('randomForest')
library(randomForest)

rf = randomForest(as.factor(above50k) ~ relationship + marital.status + age + capital.gain + occupation + education.num,
           data = trainingdata, ntree = 500, mtry = 4, nodesize = 5, importance = TRUE)

#variable importance plot
varImpPlot(rf, type = 1)

# confusion matrix

predicted_randomforest = predict(rf, testdata)
confusionMatrix(as.factor(predicted_randomforest), as.factor(testdata$above50k))


