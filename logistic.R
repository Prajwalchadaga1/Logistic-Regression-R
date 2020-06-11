setwd("P:\\Spring 2020\\ML\\Classes\\Class 5")
admission <- read.csv("admission.csv", header=T, sep=",")
View(admission)

library(ggplot2)
ggplot(data = admission, aes(x=gre, y= admit)) + geom_point()
ggplot(data = admission, aes(x=gre, y= admit)) + 
  geom_jitter(width = 0.1, height = 0.1)
train <- admission[1:320, 2:5]
test <- admission[321:400, 2:5]
View(test)
View(train)
model <- glm(formula = admit~., family = binomial(link = "logit"), data= train)
summary(model)
result <- predict(model, newdata=test[,2:4], type="response")
result
result <- ifelse(result>0.5,1,0) 
result
accuracy <- mean(result == test$admit)
accuracy

library(caret)

num_iterations <- 100
acc_history <- list(num_iterations)

## single full model training/testing
for (i in 1:num_iterations) {
inTrain = createDataPartition(y=admission$admit, p=0.8, list=FALSE)
X_train = admission[inTrain, ]
X_test = admission[-inTrain, ]
model <- glm(formula = admit~gre+gpa+rank, family = binomial(link = "logit"), data= X_train)
result <- predict(model, newdata=X_test[,3:5], type="response")
result <- ifelse(result>0.5,1,0) 
accuracy <- mean(result == X_test$admit)
acc_history[[i]] <- accuracy
}

sum_acc = 0
for (i in 1:num_iterations) {
sum_acc = sum_acc + acc_history[[i]]
}
ave_acc = sum_acc/num_iterations
print(ave_acc)


#two models training/testing/comparison
num_iterations <- 100
acc_history <- list(num_iterations)
acc_history_gre <- list(num_iterations)

for (i in 1:num_iterations) {
  inTrain = createDataPartition(y=admission$admit, p=0.8, list=FALSE)
  X_train = admission[inTrain, ]
  X_test = admission[-inTrain, ]
  model <- glm(formula = admit~gre+gpa+rank, family = binomial(link = "logit"), data= X_train)
  result <- predict(model, newdata=X_test[,3:5], type="response")
  result <- ifelse(result>0.5,1,0) 
  accuracy <- mean(result == X_test$admit)
  acc_history[[i]] <- accuracy
  model_1 <- glm(formula = admit~gre, family = binomial(link = "logit"), data= X_train)
  result_gre <- predict(model_1, newdata=X_test[,3:5], type="response")
  result_gre <- ifelse(result_gre>0.5,1,0) 
  accuracy_gre <- mean(result_gre == X_test$admit)
  acc_history_gre[[i]] <- accuracy_gre
}

View(X_test)
##print accuracy result lists
#for (i in 1:num_iterations) {
#  print(acc_history[[i]])
#}
#for (i in 1:num_iterations) {
#  print(acc_history_gre[[i]])
#}

##print average accuracy
sum_acc = 0
for (i in 1:num_iterations) {
  sum_acc = sum_acc + acc_history[[i]]
}
ave_acc = sum_acc/num_iterations
print(ave_acc)

sum_acc_1 = 0
for (i in 1:num_iterations) {
  sum_acc_1 = sum_acc_1 + acc_history_gre[[i]]
}
ave_acc_1 = sum_acc_1/num_iterations
print(ave_acc_1)

df1 <- data.frame(matrix(unlist(acc_history), nrow=length(acc_history), byrow=T))
df1$group = 1
df1$i <- seq.int(nrow(df1))
names(df1)[1] <- "accuracy"
df2 <- data.frame(matrix(unlist(acc_history_gre), nrow=length(acc_history_gre), byrow=T))
df2$group = 0
df2$i <- seq.int(nrow(df2))
names(df2)[1] <- "accuracy"
df3 <- rbind(df1, df2)
View(df3)
ggplot(data = df3, aes(x=i, y=accuracy, color=factor(group))) + xlab("round of test") +
 ylab("accuracy score") + geom_point() + labs(color="group")
t.test(accuracy~group, data=df3)
##
