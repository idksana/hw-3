# sana abbasi
# uin 662260656
# hw 3

library(dplyr)
library(tidyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(caTools)
library(ISLR)
library(ROCR)

#---------------------- number 1 ----------------------
#a
analytics_dep <- c("Y", "N", "Y", "Y", "N", "N", "Y", "N")
age = c("Y", "Y", "N", "Y", "N", "Y", "N", "N")
tenure = c("Y", "Y", "Y", "N", "Y", "Y", "N", "N", "N")
less_50k = c(25, 15, 10, 0, 0, 25, 0, 0)
more_50k = c(0, 0, 5, 0, 0, 15, 10, 20)

#if Analytics Department = Y Then Income > 50K
support1 = (4 + 75)/125
confindence1 = (4 + 75) / 75
support1 # .632
confindence1 #1.053

#b
#if Analytics Department = Y and Tenure > 5 Then Income > 50K
support2 = (4 + 5 + 75) / 125
confindence2 = (4 + 5) / 125
support2 #.672
confindence2 #.072

#c
#Using the 1-rule method discussed in class, find the relevant sets of 
#classification rules for the target variable by testing each of the input 
#attributes Analytics Department, Age > 30, and Tenure > 5. Which of these three 
#sets of rules has the lowest misclassification rate?

instanceY=  25 + 15 + 10 #equals 50
instanceN= 5 + 10 + 25 + 15 #50
instanceR= 50/75 # equals.66


#d
#Considering Income>50K as the target variable, which of the attributes would 
#you select as theroot in a decision tree that is constructed using the information 
#gain impurity measure?

entropy = (-4/8) * log2(4/8) - (4/8) * log2(4/8) # low purity
# equals 1

#e
#Use the Gini index impurity measure and construct the full decision tree 
#for this data set.
Parentnode =1 - (75/125)^2 - (50/125)^2 # equals .48
ChildnodeY = (50/55)^2 - (5/55)^2 # for Y 
#equals 0.8181818
childnodeN = (25/70)^2 - (45/70)^2 #for N
# equals -0.2857143
Gini = ((55/125) * ChildnodeY) + ((70/175) * childnodeN) # equals 0.2457143

#---------------------- number 2 ----------------------

inc <- read.csv("Inc_Exp_Data.csv")
str(inc)

#a
nrow(inc) #50 
ncol(inc) #7

#b
#Convert the variable “Highest Qualified Member” to a factor variable. Print the summary of dataset
#and explain the key points of the summary for “Mthly HH Income” and “Highest Qualified Member”.

table_hqm <- table(inc$Highest_Qualified_Member) # to see the highesr qual member table here
#Graduate     Illiterate  Post-Graduate   Professional Under-Graduate 
#19              5              6             10             10 
highqual = factor(table_hqm) # factor vars
summary(highqual) # printing out the numbers of how many members have a certain 

table_hh <- table(inc$Mthly_HH_Income)
hhIncome = factor(table_hh)
summary(hhIncome)

#c
#Calculate the mean and standard deviation of all numeric columns

Mthly_HH_IncomeMean = mean(inc$Mthly_HH_Income) #41558
inc %>% summarise(sd_points = sd(Mthly_HH_Income, na.rm=TRUE)) #26097.91

Mthly_HH_ExpenseMean = mean(inc$Mthly_HH_Expense) #18818
inc %>% summarise(sd_points = sd(Mthly_HH_Expense, na.rm=TRUE))#12090.22

No_of_Fly_MembersMean = mean(inc$No_of_Fly_Members) #4.06
inc %>% summarise(sd_points = sd(No_of_Fly_Members, na.rm=TRUE)) #1.517382

Emi_or_Rent_AmtMean = mean(inc$Emi_or_Rent_Amt) #3060
inc %>% summarise(sd_points = sd(Emi_or_Rent_Amt, na.rm=TRUE)) #6241.435

Annual_HH_IncomeMean = mean(inc$Annual_HH_Income) #490019
inc %>% summarise(sd_points = sd(Annual_HH_Income, na.rm=TRUE)) #320135.8

No_of_Earning_MembersMean = mean(inc$No_of_Earning_Members) #1.46
inc %>% summarise(sd_points = sd(No_of_Earning_Members, na.rm=TRUE)) #0.7342913

#d
table_mhe = table(inc$Mthly_HH_Expense)
hist(table_hh, breaks = 10)  #Mthly_HH_Income histogram
hist(table_mhe, breaks = 10) #Mthly_HH_Expense histogram

#e
boxplot(highqual, hhIncome)

#f
mem = table(inc$No_of_Fly_Members)
inc %>% 
  select(No_of_Fly_Members, Highest_Qualified_Member) %>%  
  filter(No_of_Fly_Members <= 4) %>%
  select(No_of_Fly_Members, Highest_Qualified_Member) %>%  filter(No_of_Fly_Members <= 4)
ggplot(data = inc, mapping = aes(x = "Highest_Qualified_Member", y = "No_of_Fly_Members"))+ 
  geom_point(size = 1)+
  geom_bar(stat = "identity") +
  geom_line(color = "red")+
  ggtitle("average monthly household income by highest qualified member ")+
  labs(x = "highest qualified member", y = "income")

inc %>% 
  select(No_of_Fly_Members, Highest_Qualified_Member) %>%  
  filter(No_of_Fly_Members <= 4)
  group_by(Mthly_HH_Income) 

 

#---------------------- number 3 ----------------------
#a
diabetes <- read.csv("diabetes2.csv")
str(diabetes)

#b
diabetes_table <- table(diabetes$Outcome) # to see the highesr qual member table here
factorD = as.factor(diabetes_table)

#c
set.seed(123)
indx <- sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
train <- data[indx == 1, ]
test <- data[indx == 2, ]

#d
set.seed(123)
sample = sample.split(diabetes$Outcome, SplitRatio = 0.7)
train = subset (diabetes, sample == TRUE)
test = subset (diabetes, sample == FALSE)

model <- glm(Outcome ~ Pregnancies + Glucose + BloodPressure + Insulin + BMI + DiabetesPedigreeFunction + Age, data = test, family = "binomial")
summary(model)
# significant code Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Pregnancies, Glucose, BloodPressure, BMI,DiabetesPedigreeFunction are all significant 

#e
tester = predict(model,newdata = test, type = "response")
predtest =  ifelse(tester >= .25,1,0)
table(test$Outcome, predtest) # confusion matrix 

#f
?ROCR::plot.performance
?ROCR::performance

#ROC 
pred1 = prediction(tester, test$Outcome)
perf = performance(pred1, "tpr", "fpr")
plot(perf)
perf
pred <- prediction(model$fitted.values,train$Outcome)
pref2 = performance(pred1, "tpr", "fpr")
plot(pref2)

auc <- performance(pred1, "auc")
auc <- unlist(slot(auc, "y.values"))
auc #area under the curve 

#PR curve
pred1 = prediction(tester, test$Outcome)
perf = performance(pred1, "prec", "rec")
plot(perf)
pred <- prediction(model$fitted.values,train$Outcome)
pref2 = performance(pred1, "prec", "rec")
plot(pref2)

auc <- performance(pred1, "auc")
auc <- unlist(slot(auc, "y.values"))
auc #0.804

#training
pred1 = prediction(tester, test$Outcome)
perf = performance(pred1, "ppv", "rpp")
plot(perf)
pred <- prediction(model$fitted.values,train$Outcome)
pref2 = performance(pred1, "ppv", "rpp")
plot(pref2)

auc <- performance(pred1, "auc")
auc <- unlist(slot(auc, "y.values"))
auc

#g
coefficients(model)
exp(1.231054) # for diabetes 
# eqauls 3.42483


#---------------------- number 4 ----------------------
#a
set.seed(123)
sample = sample.split(diabetes$Outcome, SplitRatio = 0.7)
train = subset (diabetes, sample == TRUE)
test = subset (diabetes, sample == FALSE)
diabetes_model = rpart(formula = Outcome ~., method = "class", data = train,)
summary(diabetes_model)

#b
model25 = rpart(formula = Outcome ~., data = train, control = rpart.control(minbucket = 25))
summary(model25)

#c
rpart.plot(model25)

#d
data(ROCR.simple)
modellm <- lm(formula= Outcome ~ Pregnancies + Glucose + BloodPressure + Insulin + BMI + DiabetesPedigreeFunction + Age, data = test)
pred1 = prediction(modellm, test$Outcome)

pred <- prediction(ROCR.simple$predictions, ROCR.simple$labels, )
perf2 <- performance(pred,"tpr","fpr")
perf2
plot(perf)

#e
#in 4d its more consistent and doesnt have that many dips on the graph 
# 3f is more scsttered 
perf  # has more points - 231
perf2 # less points - 201

#f
coefficients(modellm)
exp(0.2127428041)  #for diabetes
#equals 1.237066

