#Read the csv that we will use it as dataset
urlfile="https://github.com/kroinas/2ndproject_edx/raw/main/adult.csv"
income<-read.csv(url(urlfile))

#Lets see the dimensions of this dataset and its class
dim(income)
class(income)

#Lets exam its columns now
head(income)

#Lets see what can be the contents of this column.
unique(income$income)




#Load package tidyverse in case is not installed.
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
library(tidyverse)
#Lets try produce some graphs now to check distributions of our samples.
income%>%group_by(race)%>%summarize(race,count=n())%>%ggplot(aes(race))+geom_bar()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
income%>%group_by(sex)%>%summarize(sex,count=n())%>%ggplot(aes(sex))+geom_bar()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
income%>%group_by(education)%>%summarize(education,count=n())%>%ggplot(aes(education))+geom_bar()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
income%>%group_by(marital.status)%>%summarize(marital.status,count=n())%>%ggplot(aes(marital.status))+geom_bar()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
income%>%group_by(age)%>%summarize(age,count=n())%>%ggplot(aes(age,width=10))+geom_bar()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



#Lets see if age affects income
income%>%group_by(age)%>%summarize(age,income)%>%ggplot(aes(income,age))+geom_boxplot()


#Lets examine sex now.We will create a 2x2 table setting the dimension names
income_sex<- table(income$sex,income$income)
names(dimnames(income_sex)) <- c("sex", "income")
income_sex%>%plot(sex,income)


#Regarding native country we observed that 90% of dataset is 'United States'.
dim(income%>%filter(native.country=="United-States"))


#We will examine now if race affects income
income_race<- table(income$race,income$income)
names(dimnames(income_race)) <- c("race", "income")
#Trying to find the percentage of low income (<=50K) in each race
income_race<-cbind(income_race,rowSums(income_race))
percentage_low_income<-income_race[,1]/income_race[,3]*100
#and appearing it in a table format
as.data.frame(percentage_low_income)


#Another column that we suspect as predictor is education. Lets plot
#high and low incomes appearances in our dataset depending on education
income_education<- table(income$education,income$income)
income_education<-as.data.frame(income_education)
colnames(income_education)<-c("education","income","Freq")
income_education%>%group_by(education)%>%ggplot(aes(education,Freq,fill=income))+geom_point(aes(color=income), size=5)+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#A final variable we suspect is occupation. Lets plot a figure to realize if could be predictor.
income%>%group_by(occupation)%>%summarize(occupation,income)%>%ggplot(aes(occupation,income))+geom_count()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#It is now time build our models using the predictors announced before. We will use 
#2 models and these ones will be knn and rpart (decision tree).
#First of all we have to load caret library if it is not existing as well as create our
#train and test data sets. 
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
library(caret)
test_index <- createDataPartition(income$income, times = 1, p = 0.2, list = FALSE)
train_set <- income%>% slice(-test_index)
test_set<-income%>% slice(test_index)

#RPART TRAINING
train_rpart <- train(income~ education+age+race+sex+occupation,method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.02, len = 4)),data = train_set)
#cp is a parameter used in dec. trees that more or less tells us till where will continue splitting
#It is used to control the size of the decision tree and to select the optimal tree size. If the cost of adding another variable to the decision tree from the current node is above the value of cp, then tree building does not continue
plot(train_rpart)
train_rpart$bestTune
#We see the optimal value of cp is 0.006666667 and gives an accuracy just above 80%
#To visualize our tree we give the following commands
plot(train_rpart$finalModel,margin = 0.1)
text(train_rpart$finalModel,cex = 0.75)


#KNN TRAINING
train_knn <- train(income ~ education+age+race+sex+occupation,method = "knn", tuneGrid = data.frame(k = seq(15, 51, 2)),data = train_set)
train_knn$bestTune
y_hat_knn<-predict(train_knn,test_set)
confusionMatrix(y_hat_knn,as.factor(test_set$income))$overall["Accuracy"]
#Here we see the best k is 51 the last value. In general we set odd values not too big not too small
#Then we use our trained model to predict the income for test set and finally we calculate accuracy
#we used as.factor inside matrix for test_set$income, as it is required to be of this class, the values we compare (while its normal class is 'character')
class(test_set$income)
#This model also gives us 80% accuracy as we see

#In order identify variables that affect income we run a simple rf (random forest) to take advantage of its varImp feature.
#This feature indicates which variable affects more our result
train_rf <- train(income ~ ., method = "rf", data=income)
varImp(train_rf)

#But lets create first the profit in both train and test sets


train_set<-train_set%>%mutate(capital.profit=capital.gain-capital.loss)
test_set<-test_set%>%mutate(capital.profit=capital.gain-capital.loss)


#It is time train our first model (rpart) with the predictor changes
train_rpart <- train(income~ education+age+occupation+capital.profit+marital.status+hours.per.week,method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.02, len = 4)),data = train_set)
plot(train_rpart)
y_hat_rpart<-predict(train_rpart,test_set)
confusionMatrix(y_hat_rpart,as.factor(test_set$income))
plot(train_rpart$finalModel,margin = 0.1)
text(train_rpart$finalModel,cex = 0.75)

#We reached 85% much better than before (cp same as before).

#We will repeat now for our second model knn. Only that we will change a bit k to be always odd but include higher values.
#We remember last time was the max value 51.
train_knn <- train(income~ education+age+occupation+marital.status+capital.profit+hours.per.week,method = "knn",tuneGrid = data.frame(k = seq(5, 101, 8)), data=train_set)
train_knn$bestTune
#We see this time best k is 21. In general while in rpart cp is very stable, k in knn we have seen varying.
#Also while there is a rule saying best k is sqrt(N) and here N is close to 30000, we did not see is true (as it had to be k= 170)
#Lets calculate the prediction in test set and check the accuracy
y_hat_knn<-predict(train_knn,test_set)
confusionMatrix(y_hat_knn,as.factor(test_set$income))

#Analysis and interpretation of the results can be found on rmd or pdf report.







                  





