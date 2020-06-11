setwd("P:\\Spring 2020\\ML\\Classes\\Class 5")
admission = read.csv("admission.csv", header=T, sep=",")
View(admission)
library(ggplot2)
ggplot(data = admission, aes(x=gre, y=admit)) + geom_point()
ggplot(data = admission, aes(x=gre, y=admit)) + geom_jitter(width = 0.1, height = 0.1)
train = admission[1:320, 2:5]
test = admission[321:400, 2:5]
View(test)
View(train)
model = glm(formula = admit~., family = binomial(link = "logit"), data = train)
summary(model)
result = predict(model, newdata=test[,2:4], type="response")
result
result = ifelse(result>0.5,1,0)
result
accuracy = mean(result == test$admit)
accuracy

install.packages("caret")
library(caret)

num_iteration = 1000
acc_history = list(num_iterations)

## single full model training/testing
for (i in 1:num_iterations) {
  inTrain = createDataPartition(y=admission#admit, p=0.8, list=FALSE)
  X_train = admission[inTrain, ]
  X_test = admission[-inTrain, ]
  model = glm(formula = admit~gre+gpa+rank, family = binomial(link = "logit"), data = X_train)
  result = predict(model, newdata = X_test[,3:5], type = "response")
  result = ifelse(result>0.5,1,0)
  accuracy = mean(result == X_test$admit)
  acc_history_gre[[i]] = accuracy_gre
  model_1 = glm(formula
  }
