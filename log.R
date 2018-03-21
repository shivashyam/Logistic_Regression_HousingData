# Logistic Regression

# Importing the dataset
setwd('C:/Users/magic/Desktop/Sem2/R/Asgn/proj/new_log/house-price-prediction-master')
dataset = read.csv('kc_house_data.csv')
dataset = dataset[, c("price", "yr_built", "waterfront")]
head(dataset)
# subset(dataset, select=c("price", "yr_built", "waterfront"))

# Encoding the target feature as factor
dataset$waterfront = factor(dataset$waterfront, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$waterfront, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[, 1:2] = scale(training_set[, 1:2])
test_set[, 1:2] = scale(test_set[, 1:2])

# Fitting Logistic Regression to the Training set
classifier = glm(formula = waterfront ~ .,
                 family = binomial,
                 data = training_set)
summary(classifier)

# Predicting the Test set results
prob_pred = predict(classifier, type = 'response', newdata = test_set[-3])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred > 0.5)
head(cm)
Accuracy = (cm[1,1]+cm[2,2])/(sum(cm))*100
Accuracy
# install.packages("iris")
# library(e1071)
# library(caret)
# 
# cm(iris$Species, predict(m1))
  

# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('price', 'yr_built')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Training set)',
     xlab = 'Price', ylab = 'Year_Built',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'skyblue', 'orange'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('price', 'yr_built')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Test set)',
     xlab = 'Price', ylab = 'Year_Built',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'lavender', 'grey'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

#Accuray we achieved
#Accuracy is sum of cm[1,1],cm[2.2]/ sum of the entire matrix 
Accuracy = (cm[1,1]+cm[2,2])/(sum(cm))*100
Accuracy