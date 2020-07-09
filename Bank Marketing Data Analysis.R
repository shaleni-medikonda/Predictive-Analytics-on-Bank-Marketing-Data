#1st Part
#install.packages("gmodels")
library(gmodels)
nim <- read.csv(file.choose())
head(nim)
nim <-na.omit(nim)
nim
str(nim)

nim$y <- ifelse(nim$y == "y", 1, 0)
nim$y <- factor(nim$y, levels = c(0, 1))
nim$y

set.seed(1) 
row.number <- sample(x=1:nrow(nim), size=0.8*nrow(nim))
train = nim[row.number,]
test = nim[-row.number,]
head(train)
head(test)

logistic_model <- glm(y ~ 1, family=binomial(link="logit"),data=train)
summary(logistic_model)

summary(logistic_model)$coeff
logistic_model$coefficients[1]

sample <- exp(logistic_model$coefficients[1])/(1+exp(logistic_model$coefficients[1]))
sample

pred1 <-predict(logistic_model,newdata = test,type='response')
#pred1
#test
#test$y
CrossTable(pred1,test$y)
############################################################################################################

#2nd Part
education <- glm(y ~ factor(education), family=binomial(link="logit"), data=train)
summary(education)
exp(coef(education))

education$coefficients[2]
exp(education$coefficients[1]+education$coefficients[2])/(1+exp(education$coefficients[1]+education$coefficients[2]))
Pred2 <- predict(education,test,type="response")
pred_ex <- predict(education, newdata = train[train$education=='0', ], type = "response")
head(pred_ex, 1)
#pred_ex
pred_ex1 <- predict(education, newdata = train[train$education=='2', ], type = "response")
head(pred_ex, 1)
#pred_ex1
############################################################################################################

#3rd Part
logistic_model_day = glm(y ~ factor(day), family = binomial(link = "logit"), data = train)
summary(logistic_model_day)
logistic_model_day$coefficients[2]
pred3 <-predict(logistic_model_day,test,type ="response")
#pred3
#test
exp(education$coefficients[1]+education$coefficients[2])/(1+exp(education$coefficients[1]+education$coefficients[2]))
############################################################################################################

#4th Part
set.seed(1) 
row.number <- sample(x=1:nrow(nim), size=0.8*nrow(nim))
train = nim[row.number,]
test = nim[-row.number,]
head(train)
head(test)
logistic_model_prompt <- glm(y ~ ., family=binomial(link="logit"), data=train)
summary(logistic_model_prompt)
pred <- predict(logistic_model_prompt, newdata = test, type = "response")
pred
y_pred_num <- ifelse(pred > 0.5, 1, 0)
test$y <- ifelse(test$y == "y", 1, 0)
y_predicted <- factor(y_pred_num, levels=c(0, 1))
y_observed <- factor(test$y,levels=c(1,0))
y_observed
mean(y_predicted == y_observed) #0.93
#confusion matrix to test the accuracy of the model
table(Actualvalue=test$y,predictedValue=pred>0.5)
(6797+342)/(6797+342+556+162)
############################################################################################################

#5th Part
#Logitics model using optimalcutoff()
logistic_model_prompt <- glm(y ~ factor(education), family=binomial(link="logit"), data=train)
summary(logistic_model_prompt)
library(InformationValue)
pred <- predict(logistic_model_prompt, newdata = test, type = "response")
optCutOff <- optimalCutoff(test$y, pred)[1] 
y_pred_num <- ifelse(pred > optCutOff, 1, 0)
y_predicted <- factor(y_pred_num, levels=c(0, 1))
y_observed <- test$y
mean(y_predicted == y_observed) #0.5560

#Classification Tree model
alpha     <- 0.7 # percentage of training set
inTrain   <- sample(1:nrow(nim), alpha * nrow(nim))
train.set <- nim[inTrain,]
test.set  <- nim[-inTrain,]
#install.packages("tree")
library(tree)
#install.packages("rpart")
library(rpart)
fit <- rpart(y ~.,method="class",data= train.set) #creating the tree
printcp(fit)
plotcp(fit) # cross-validation  
summary(fit) 

plot(fit,uniform=TRUE,main="Classification Tree") #plotting the tree
text(fit, use.n=TRUE, all=TRUE, cex=.8)
pred3 <-predict(fit,test)
head(pred3)

pfit<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]) #Prune the tree
plot(pfit, uniform=TRUE, 
     main="Pruned Classification Tree") #Plotting the tree
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
