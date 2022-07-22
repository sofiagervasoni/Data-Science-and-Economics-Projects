# Packages----------------------------------------------------------------------
library("psych")
library("ggplot2")
library("DataExplorer")
library("gplots")
library("patchwork")
library("dplyr")
library("ggthemes")
library("tidyr")
library("data.table")
library("ggpubr")
library("scales")
library("corrplot")
library("ggplot2")
library("GGally")  
library("rstatix")
library("performanceEstimation")

packages <- c(
  "tidyverse", 
  "lubridate", 
  "timeDate",
  "readxl",
  "skimr",
  "DataExplorer",
  "plotly",
  "tidymodels",
  "stacks",
  "broom.mixed",
  "rstanarm",
  "vip",
  "rpart.plot",
  "dotwhisker",
  "nycflights13",
  "modeldata", 
  "treesnip",
  "rules",
  "Cubist",
  "baguette",
  "discrim",
  "dslabs", 
  "patchwork",
  "DALEX",
  "ggmosaic",
  "ggplot2",
  "ggplot",
  "ggpubr",
  "GoodmanKruskal"
)

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

rm(installed_packages, packages)
#Dataset------------------------------------------------------------------------
library(readxl)
sat=read_excel(file.choose()) #satisfaction
sat=data.frame(sat)
sat%>% glimpse() #129,880 and 24 columns

sat %>% skimr::skim() #393 NA in Arrival Delay in Minutes
#sat=sat[-which(is.na(sat$Arrival.Delay.in.Minutes)),]
#sat %>% skimr::skim() #drop the missing values in Arrival Delay in Minutes
#(1-(129487/129880))*100 -> lose 0.302587% of of observations

#Data manipulation--------------------------------------------------------------

sat=sat %>% 
  mutate(
    satisfaction_v2=ifelse(satisfaction_v2=="satisfied", 1, 0)
  ) %>% 
  mutate(across(where(is.character), as.factor)) %>% 
  select( # remove some "useless" variables
    -c(id)
  ) 

colnames(sat)=c("satisfied","gender", "customer_type", "age", "type_of_travel", 
                "class", "flight_distance","seat_comfort","time_convenient",
                "food_drink","gate_location", "Wifi","entrateinment",
                "online_support","online_booking","onboard_service",
                "legroom_service", "baggage_handling","checkin", 
                "cleanliness","online_boarding",
                "departure_delay", "arrival_delay")
summary(sat)
#Data Visualization ------------------------------------------------------------

barplot(table(sat$satisfied)/length(sat$satisfied), 
        xlab = "Satisfied (Y)", ylab="Frequences")
(1-(table(sat$satisfied)[1]/table(sat$satisfied)[2]))*100
#balanced dataset


(length(which(sat$satisfied==1))/length(which(sat$satisfied==0)))-1 #21%
#more than 20% but close to it-> more or less BALANCED DATASET
#The rule of thumb which I follow is that when the minority class 
#in a binary classification is not less than 20%, 
#class imbalance would not impact the model performance much.

ggplot(sat, aes(x=as.factor(satisfied),y=age))+
  geom_boxplot(fill= "darkred", alpha= 0.7)

barplot(table(sat$satisfied, sat$age), beside=T, cex.names=0.7, 
        legend.text=T, xlab = "Age", ylab="Frequency")
#as the age increase the satisfaction seems to increase

ggplot(sat, aes(x=as.factor(satisfied),y=flight_distance))+
  geom_boxplot(fill= "darkred", alpha= 0.7)
#no evident relation between flight distance and satisfaction

#ggplot(sat, aes(x=as.factor(satisfied),y=arrival_delay))+
#  geom_boxplot(fill= "darkred", alpha= 0.7)

barplot(table(sat$gender)) #balanced
g=table( sat$satisfied,sat$gender)
barplot(g, beside=T, legend.text=T)
#female seems to be more satisfied than male

barplot(table(sat$customer_type)) #unbalanced
t=table(sat$satisfied,sat$customer_type)
barplot(t, beside=T, legend.text=T)
#loyal customers seems to be more satisfied

barplot(table(sat$type_of_travel)) #unbalanced 
tt=table(sat$satisfied,sat$type_of_travel)
barplot(tt, beside=T, legend.text=T)
#who travel for business seems to be more satisfied

barplot(table(sat$class)) #unbalanced 
c=table(sat$satisfied,sat$class)
barplot(c, beside=T, legend.text=T)
#people that travel in business class seems to be mostly satisfied
#people that travel in Eco class seems to be mostly satisfied
#people that travel in Eco plus class seems to be mostly unsatisfied

#distribution of the variables about satisfaction
x11()
par(mfrow=c(3,5))
for(i in 8:21){
  barplot(table(sat[,i])/length(sat), xlab=colnames(sat)[i], ylab="")
}
par(mfrow=c(1,1))

# Deal with NAs ----------------------------------------------------------------
sum(is.na(sat)) #393
library(VIM)
aggr(sat, col = c("white", "grey", "black"), cex.axis=.5)


sat = as_tibble(sat)
sum(is.na(sat$arrival_delay)) #393
(393/129880)*100 #0.3%
missing_index = which(is.na(sat$arrival_delay))
X= sat[missing_index,]
train_v = sat[-c(missing_index),]

library("caret")

tree = caret::train(arrival_delay ~ ., 
                    data=train_v, 
                    method="rpart")

arrival_delay_pred = predict(tree, newdata = X)

sat[missing_index,"arrival_delay"]=arrival_delay_pred
sum(is.na(sat$arrival_delay)) #no more NAs
sum(is.na(sat))


# clean Glob_Env
rm(train_v,X,tree, arrival_delay_pred,missing_index)

#Outliers-----------------------------------------------------------------------
library(rstatix)
sum(is_extreme(sat$age)) #0
sum(is_extreme(sat$flight_distance)) #61
sum(is_extreme(sat$departure_delay)) #11608
sum(is_extreme(sat$arrival_delay)) #10825
sat=sat[-unique(c(which(is_extreme(sat$flight_distance)), 
                  which(is_extreme(sat$departure_delay)),
       which(is_extreme(sat$arrival_delay)))),]
nrow(sat)
#all the extreme values has been dropped

boxplot(sat$flight_distance)
boxplot(sat$departure_delay)
boxplot(sat$arrival_delay)
#I am left with some outliers but no more extreme outliers
#116933 observation left

#Qualitative Correlation--------------------------------------------------------

dt.gk=sat[,c(2,3,5,6)]
library("GoodmanKruskal")

plot(GKtauDataframe(dt.gk))
#the highest correlation is between class and type of travel

#Quantitative Correlation-------------------------------------------------------
library(corrplot)
correl=cor(sat[,-c(2,3,5,6)])
corrplot(correl)
#correl[which(cor(sat[,-c(2,3,5,6)])>0.75&cor(sat[,-c(2,3,5,6)])<1)]

x11()
corPlot(sat[,-c(1,2,3,5,6)], cex = 0.4, xlas=2,show.legend=F)
#strong positive correlation between arrival delay and departure delay (0.96)
#strong positive correlation between online boarding and wifi (0.63),
#online support (0.67) and online booking (0.68)

#problems of collinearity between departure and arrival delay

#Train and Test-----------------------------------------------------------------

sat=as.data.frame(sat)
set.seed(123)
split_train_test=createDataPartition(y = sat$satisfied, p=0.8, list = F) 
#80% training 20% test
train=sat[split_train_test,]
test=sat[-split_train_test,]

par(mfrow=c(1,2))
barplot(table(train$satisfied), xlab="Satisfied", main="Train")
tabtrain=table(train$satisfied)
(1-(tabtrain[1]/tabtrain[2]))*100 #20.85% (BALANCED)

barplot(table(test$satisfied), xlab="Satisfied", main="Test")
tabtest=table(test$satisfied)
(1-(tabtest[1]/tabtest[2]))*100 #20.20% (BALANCED)
par(mfrow=c(1,1))

#Logistic Regression------------------------------------------------------------

logit=glm(as.factor(satisfied)~., data=train, family = binomial(link = 'logit'))
summary(logit)

library("DescTools")
PseudoR2(logit, which = NULL) #0.4438084
#McFadden's pseudo R2 ranging from 0.2 to 0.4 indicates very good model fit
#our McFadden's pseudo R2 very close to 0.4 -> good fit 

#plot(logit)

library("regclass")
round(VIF(logit),2)


#Lasso--------------------------------------------------------------------------

library(glmnet)
x=model.matrix(satisfied~.,data=train)[,-1] 
y=train$satisfied
fit.lass=glmnet(x, y, family = "binomial", alpha = 1, lambda = NULL) 
summary(fit.lass)
#alpha=0 -> Ridge regression
#alpha=1 -> Lasso regression

#fit the lasso penalized model
#library(glmnet)
# Find the best lambda using cross-validation
set.seed(123) 
cv.lasso = cv.glmnet(x, y, alpha = 1, family = "binomial")
# Fit the final model on the training data
model = glmnet(x, y, alpha = 1, family = "binomial",
                lambda = cv.lasso$lambda.min)
# Display regression coefficients
coef(model)

# Make predictions on the test data
x.test = model.matrix(satisfied ~., data=test)[,-1]
probabilities = model %>% predict(newx = x.test)
predicted.classes = ifelse(probabilities > 0.5, 1, 0)
# Model accuracy
observed.classes = test$satisfied
mean(predicted.classes == observed.classes) #accuracy: 0.836013


#library(glmnet)
set.seed(123)
cv.lasso = cv.glmnet(x, y, alpha = 1, family = "binomial")
plot(cv.lasso) #log lambda more or less -7
cv.lasso$lambda.min
cv.lasso$lambda.1se

coef(cv.lasso, cv.lasso$lambda.min)
coef(cv.lasso, cv.lasso$lambda.1se)

### Final model with lambda.min
lasso.model = glmnet(x, y, alpha = 1, family = "binomial",
                      lambda = cv.lasso$lambda.min)
summary(lasso.model)
# Make prediction on test data
x.test = model.matrix(satisfied ~., data=test)[,-1]
probabilities = lasso.model %>% predict(newx = x.test)
predicted.classes = ifelse(probabilities > 0.5, 1, 0)
# Model accuracy
observed.classes = test$satisfied
mean(predicted.classes == observed.classes) #0.836013


### Final model with lambda.1se
lasso.model = glmnet(x, y, alpha = 1, family = "binomial",
                      lambda = cv.lasso$lambda.1se)
coefficients(lasso.model)
summary(lasso.model)
# Make prediction on test data
x.test = model.matrix(satisfied ~., data=test)[,-1]
probabilities = lasso.model %>% predict(newx = x.test)
predicted.classes = ifelse(probabilities > 0.5, 1,0)
# Model accuracy rate
observed.classes <- test$satisfied
mean(predicted.classes == observed.classes) #0.8354571

#keep the penalized lasso model with lambda.1se (Drop Departure delay)

#Logistic Regression (no Departure delay)--------------------------------------

#Lasso set Departure delay to zero, 
#try to refit the logistic regression without this variable
logit_noMulty=glm(as.factor(satisfied)~.-departure_delay, data=train, 
                  family = binomial(link = 'logit'))
summary(logit_noMulty)

#library("DescTools")
PseudoR2(logit_noMulty, which = NULL) #0.4437999
#McFadden's pseudo R2 improved a little (closer to 0.4) 

#plot(logit)
#library("regclass")

#Assumptions check------------------
#1. Multicollinearity
VIF(logit_noMulty) #no problems of collinearity

#2. linearity

# Select only numeric predictors
probabilities <- predict(logit_noMulty, type = "response")
mydata <- train %>%
  dplyr::select_if(is.numeric) 
predictors <- colnames(mydata)
# Bind the logit and tidying the data for plot
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

#linearity holds

#3.Influential values
plot(logit_noMulty, which = 4, id.n = 3)
model.data <- augment(logit_noMulty) %>% 
  mutate(index = 1:n()) 

model.data %>% top_n(3, .cooksd)

ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = train$satisfied), alpha = .5) +
  theme_bw()

model.data %>% 
  filter(abs(.std.resid) > 3) #there are influential points

#remove influential values
new.train=train[-which(abs(model.data$.std.resid)>3),]
barplot(table(as.factor(new.train$satisfied))) #balanced


logit_noMulty_noout=glm(as.factor(satisfied)~.-departure_delay, data=new.train, 
                  family = binomial(link = 'logit'))
probabilities <- predict(logit_noMulty_noout, type = "response")

plot(logit_noMulty_noout, which = 4, id.n = 3)
model.data <- augment(logit_noMulty_noout) %>% 
  mutate(index = 1:n()) 

model.data %>% top_n(3, .cooksd)

ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = satisfied), alpha = .5) +
  theme_bw()

model.data %>% 
  filter(abs(.std.resid) > 3) #there are few influential points (problem solved)



lr_prob1 <- predict(logit_noMulty_noout, newdata = test)

lr_preds_test <- rep(0, 12)
i<-1
for (thresh in seq(0.25,0.75,0.05)){
  lr_pred <- ifelse(lr_prob1 > thresh,1,0)
  cm <- table(
    as.factor(lr_pred),
    as.factor(test$satisfied)
  )[2:1, 2:1]
  lr_preds_test[i] <- F_meas(cm) # f1 score
  i<-i+1
}
names(lr_preds_test) <- seq(0.25,0.75,0.05)
lr_preds_test
lr_pred <- as.numeric(ifelse(lr_prob1 > 0.5,"1","0"))
tb <- table(Predicted = lr_pred, Actual = test$satisfied)[2:1, 2:1]
tb
(tb[1:1,1:1] + tb[2:2, 2:2])/(tb[1:1,2:2] + tb[2:2, 1:1] + tb[1:1,1:1] + tb[2:2, 2:2]) 
#Accuracy
F_meas(tb) # F1
recall(tb)  # Recall
precision(tb) # Precision


library(yardstick)
library(ggplot2)


truth_predicted <- data.frame(
  obs = test$satisfied,
  pred = lr_pred
)
truth_predicted$obs <- as.factor(truth_predicted$obs)
truth_predicted$pred <- as.factor(truth_predicted$pred)

cm <- conf_mat(truth_predicted, obs, pred)

autoplot(cm, type = "heatmap") +
  scale_fill_gradient(low = "pink", high = "cyan")
#9106 true negative
#10455 true positive
#2552 false negative
#1273 false positive
(9106+10455)/(9106+10455+2552+1273) # 0.8364406 correctly predicted

par(mfrow=c(1,1))
library("pROC")
test_roc=roc(as.numeric(test$satisfied)~lr_prob1 , plot = TRUE, 
             print.auc = TRUE,percent=TRUE, ci=TRUE)
#very good predictive ability of the model

cbind("Coefficients (exp)"=round(exp(coefficients(logit_noMulty_noout)),2), 
      "Coefficients (%)"=round((exp(coefficients(logit_noMulty_noout))-1)*100, 2))

#check linearity assumption

# Decision tree ----------------------------------------------------------------

library(datasets)
library(caTools)
library("party")
library(dplyr)
library(magrittr)
library("tree")
library("ISLR")
library(yardstick)
library(ggplot2)

tree.sat=tree(as.factor(satisfied)~.,train)
tree.pred=predict(tree.sat,test,type="class")

truth_predicted <- data.frame(
  obs = as.factor(test$satisfied),
  pred = tree.pred
)
truth_predicted$obs <- as.factor(truth_predicted$obs)
truth_predicted$pred <- as.factor(truth_predicted$pred)

cm <- conf_mat(truth_predicted, obs, pred)

autoplot(cm, type = "heatmap") +
  scale_fill_gradient(low = "pink", high = "cyan") 
#1457 false positive
#1451 false negative
#11556 true positive
#8922 true negative
(11556+8922)/(11556+8922+1457+1451) #0.8756521 (Accurancy)


#Pruning
cv.sat=cv.tree(tree.sat,FUN=prune.misclass)
names(cv.sat)
par(mfrow=c(1,2))
plot(cv.sat$size,cv.sat$dev,type="b",ylab="CV-RMSE", xlab="Tree size")
plot(cv.sat$k,cv.sat$dev,type="b")
prune.sat=prune.misclass(tree.sat,best=10)
?prune.misclass
par(mfrow=c(1,1))
plot(prune.sat)
text(prune.sat,pretty=0)
tree.pred=predict(prune.sat,test,type="class")
table(tree.pred,test$satisfied)

truth_predicted <- data.frame(
  obs = as.factor(test$satisfied),
  pred = tree.pred
)
truth_predicted$obs <- as.factor(truth_predicted$obs)
truth_predicted$pred <- as.factor(truth_predicted$pred)

cm <- conf_mat(truth_predicted, obs, pred)

autoplot(cm, type = "heatmap") +
  scale_fill_gradient(low = "pink", high = "cyan")
#same results as before

#Partitioning

library("rpart")
library("rpart.plot")
library("partykit")
library("party")

tree3=part(as.factor(satisfied)~.,sat)
tree.pred.part=predict(tree3,test,type="class")
table(tree.pred.part,test$satisfied)

truth_predicted=data.frame(
  obs = as.factor(test$satisfied),
  pred = tree.pred.part
)
truth_predicted$obs=as.factor(truth_predicted$obs)
truth_predicted$pred=as.factor(truth_predicted$pred)

cm=conf_mat(truth_predicted, obs, pred)

autoplot(cm, type = "heatmap") +
  scale_fill_gradient(low = "pink", high = "cyan")
#worse results
#1537 false positive
#1261 false negative
#11746 true positive
#8942 true negative
(8942+11746)/(8942+11746+1537+1261) #0.8808652

printcp(tree3)
rpart.plot(tree3)

#Random Forest------------------------------------------------------------------
library(randomForest)
#library(caret)
#hyperparameter tuning
mtry=tuneRF(sat[-1],as.factor(sat$satisfied), ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m=mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m) #mtry=9

set.seed(123)
rf=randomForest(as.factor(satisfied)~.,data=train, mtry=best.m, importance=TRUE,
                ntree=500)
print(rf)
#Evaluate variable importance
importance(rf)
#x11()
varImpPlot(rf)

yhat = predict(rf,newdata=test)

truth_predicted = data.frame(
  obs = as.factor(test$satisfied),
  pred = yhat
)
truth_predicted$obs=as.factor(truth_predicted$obs)
truth_predicted$pred=as.factor(truth_predicted$pred)

cm=conf_mat(truth_predicted, obs, pred)

autoplot(cm, type = "heatmap") +
  scale_fill_gradient(low = "pink", high = "cyan")

#10037 true negative
#12437 true positive
#570 false negative
#342 false positive
(12437+10037)/(12437+10037+570+342) # 0.9610023 (accuracy)