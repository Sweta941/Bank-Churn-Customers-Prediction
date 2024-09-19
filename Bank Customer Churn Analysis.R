###################### Data Description ##########################

#customer_id : Account Number of the customer is the customer_id
#credit_score: The credit score of the customer
#country : Country of Residence
#gender : Gender of the customer
#age : Age of the customer
#tenure : For how many years the customer is having account with the bank
#balance : Account Balance of the customer
#products_number : Number of bank products the customer has (eg: savings a/c, credit card, etc)
#credit_card : Is this customer have credit card?
#active_member: Is the customer active member of the bank?
#estimated_salary: Estimated salary of the customer
#churn : Is the customer churn or not, 1 represents customer has left the bank and 0 represents he/she has not left.

#########################################################################################

# Not allowing R to use scientific notation till specified number
options(scipen=1000000)

# Loading the dataset
ChurnData<-read.csv('C:\\Users\\User\\Desktop\\Data Analysis Projects\\Bank Customer Churn Dataset (R Programming)\\Bank Customer Churn Prediction.csv',na.strings=c(""," ","NA","NULL"))
head(ChurnData)

# Exploring the dataset
head(ChurnData)
str(ChurnData)
summary(ChurnData)

# Removing Useless columns
UselessColumns<-c('customer_id', 'country')
ChurnData[ , UselessColumns]=NULL
head(ChurnData)

########################################################################################

# Exploring multiple continuous features
ColsForHist<-c("credit_score","age","balance","estimated_salary")

#Splitting the plot window into four parts
par(mfrow=c(2,2))

# library to generate professional colors
library(RColorBrewer) 

# looping to create the histograms for each column
for (ColumnName in ColsForHist){
  hist(ChurnData[,c(ColumnName)], main=paste('Histogram of:', ColumnName), 
       col=brewer.pal(8,"Paired"))
}

# Finding the outliers 
table(ChurnData$credit_score < 850)
table(ChurnData$age < 80)
table(ChurnData$balance < 250000)


##Treating outliers for credit_score column
#finding nearest value for outliers
max(ChurnData$credit_score[ChurnData$credit_score < 850])

##Replacing all values greater than 849 with 849
ChurnData$credit_score[ChurnData$credit_score > 849] <- 849

##Treating outliers for age column
#finding nearest value for outliers
max(ChurnData$age[ChurnData$age < 80])

##Replacing all values greater than 79 with 79
ChurnData$age[ChurnData$age > 79] <- 79

##Treating outliers for balance column
# Finding the nearest value for outliers
max(ChurnData$balance[ChurnData$balance < 250000])

##Replacing all values greater than 238387.6 with 238387.6
ChurnData$balance[ChurnData$balance > 238387.6] <- 238387.6


#Checking the histograms after treating the outliers 
ColsForHist<-c("credit_score","age","balance")

#Splitting the plot window into four parts
par(mfrow<-c(2,1))

# library to generate professional colors
library(RColorBrewer) 

# looping to create the histograms for each column
for (ColumnName in ColsForHist){
  hist(ChurnData[,c(ColumnName)], main=paste('Histogram of:', ColumnName), 
       col=brewer.pal(8,"Paired"))
}


#####################################################################################

# Exploring multiple categorical features
ColsForBar<-c("churn","gender","tenure","products_number","credit_card","active_member")

par(mfrow = c(2, 1))
par(mar = c(4, 4, 2, 1))
layout(matrix(1:6, nrow = 3, ncol = 2))
# looping to create the Bar-Plots for each column
for (ColumnName in ColsForBar){
  barplot(table(ChurnData[,c(ColumnName)]), main=paste('Barplot of:', ColumnName), 
          col=brewer.pal(8,"Paired"))
}

#####################################################################################

# Checking missing values
colSums(is.na(ChurnData))

#####################################################################################

# Categorical Vs Continuous Visual analysis: Boxplot
boxplot(age~churn, data = ChurnData, col=brewer.pal(8,"Paired"))
boxplot(credit_score~churn, data = ChurnData, col=brewer.pal(8,"Paired"))
boxplot(balance~churn, data = ChurnData, col=brewer.pal(8,"Paired"))
boxplot(estimated_salary~churn, data = ChurnData, col=brewer.pal(8,"Paired"))

# Categorical Vs Continuous relationship strength: ANOVA
# Analysis of Variance(ANOVA)
# F-Statistic is Mean Sq error/residual MeanSq error
# H0: Variables are NOT correlated
# Small P-Value--> Variables are correlated(H0 is rejected)
# Large P-Value--> Variables are NOT correlated (H0 is accepted)

### ANOVA test for age
summary(aov(age~churn, data = ChurnData))
P_Value=summary(aov(age~churn, data = ChurnData))[[1]][[5]][1]
print(paste('P-value for the test is:', round(P_Value,3)))


### ANOVA test for credit_score
summary(aov(credit_score~churn, data = ChurnData))
P_Value=summary(aov(credit_score~churn, data = ChurnData))[[1]][[5]][1]
print(paste('P-value for the test is:', round(P_Value,3)))

### ANOVA test for balance
summary(aov(balance~churn, data = ChurnData))
P_Value=summary(aov(balance~churn, data = ChurnData))[[1]][[5]][1]
print(paste('P-value for the test is:', round(P_Value,3)))


### ANOVA test for estimated salary
summary(aov(estimated_salary~churn, data = ChurnData))
P_Value=summary(aov(estimated_salary~churn, data = ChurnData))[[1]][[5]][1]
print(paste('P-value for the test is:', round(P_Value,3)))

## All continuous variables are related with churn (target variable) except "estimated_salary"

####################################################################################
####################################################################################

# Categorical Vs Categorical Visual analysis: Barplot

## Churn Vs Gender
CrossTabResult=table(ChurnData[ , c('churn', 'gender')])
CrossTabResult
barplot(CrossTabResult, beside=T, col=c('Red','Green'), legend=T)

## Churn Vs tenure
CrossTabResult=table(ChurnData[ , c('churn', 'tenure')])
CrossTabResult
barplot(CrossTabResult, beside=T, col=c('Red','Green'), legend=T)

## Churn Vs products_number
CrossTabResult=table(ChurnData[ , c('churn', 'products_number')])
CrossTabResult
barplot(CrossTabResult, beside=T, col=c('Red','Green'), legend=T)

## Churn Vs credit_card
CrossTabResult=table(ChurnData[ , c('churn', 'credit_card')])
CrossTabResult
barplot(CrossTabResult, beside=T, col=c('Red','Green'), legend=T)

## Churn Vs active_member
CrossTabResult=table(ChurnData[ , c('churn', 'active_member')])
CrossTabResult
barplot(CrossTabResult, beside=T, col=c('Red','Green'), legend=T)

# Categorical Vs Categorical relationship strength: Chi-Square test
# H0: Variables are NOT correlated
# Small P-Value--> Variables are correlated(H0 is rejected)
# Large P-Value--> Variables are NOT correlated (H0 is accepted)
chisq.test(CrossTabResult)


# Writing a for loop for checking each categorical predictor against Target variable
ColsForBar=c("gender","tenure","products_number","credit_card","active_member")
for(CatColumn in ColsForBar){
  
  CrossTabResult=table(ChurnData[ , c("churn", CatColumn)])
  print(CrossTabResult)
  print(chisq.test(CrossTabResult))
  
}

## "Gender", "products_number" and "active_member" are correlated with the target variable 
## while "tenure" and "credit_card" are non_correlated with the target variable

###############################################################################################
##############################################################################################

InputData=ChurnData

# Specifying the Target Variable
TargetVariableName='churn'

# Making sure the class of Target variable is FACTOR
InputData[, c(TargetVariableName)]=as.factor(InputData[, c(TargetVariableName)])
class(InputData[, c(TargetVariableName)])

# Summarizing the Target Variable
summary(InputData[, c(TargetVariableName)])

# Converting all the string variables in factor format for ML
InputData$gender=as.factor(InputData$gender)
str(InputData)

##########################################################################################

# Extracting Target and predictor variables from data to create a generic dataset
TargetVariable=InputData[, c(TargetVariableName)]
str(TargetVariable)

# Selecting all other columns as Predictors apart from target variable
# You should delete useless columns like name, id, address by manually inspecting the data
#PredictorVariables=InputData[, !names(InputData) %in% TargetVariableName]
PredictorVariables=InputData[, c("age","credit_score","balance","gender","products_number","active_member")]
str(PredictorVariables)

DataForML<-data.frame(TargetVariable,PredictorVariables)
str(DataForML)

###########################################################################################

# Sampling | Splitting data into 70% for training 30% for testing
TrainingSampleIndex<-sample(1:nrow(DataForML), size=0.7 * nrow(DataForML) )
DataForMLTrain<-DataForML[TrainingSampleIndex, ]
DataForMLTest<-DataForML[-TrainingSampleIndex, ]
dim(DataForMLTrain)
dim(DataForMLTest)

###########################################################################################
###########################################################################################
###########################################################################################

# Creating Predictive models on training data to check the accuracy on test data

############################ Logistic Regression ##########################################

startTime=Sys.time()
LR_Model<-glm(TargetVariable ~.-products_number, data=DataForMLTrain, family='binomial')
LR_Model
summary(LR_Model)
endTime=Sys.time()
endTime-startTime

# 2nd itleration
LR_Model<-glm(TargetVariable ~.-credit_score, data=DataForMLTrain, family='binomial')
LR_Model
summary(LR_Model)

# 3rd itleration
LR_Model<-glm(TargetVariable ~.products_number, data=DataForMLTrain, family='binomial')
LR_Model
summary(LR_Model)

# 4th ilteration
LR_Model<-glm(TargetVariable ~.-credit_score-products_number, data=DataForMLTrain, family='binomial')
LR_Model
summary(LR_Model)

# Checking Accuracy of model on Testing data
DataForMLTest$PredictionProb=predict(LR_Model, DataForMLTest, type = "response")
DataForMLTest$Prediction=ifelse(DataForMLTest$PredictionProb>0.6, 1, 0)
DataForMLTest$Prediction=as.factor(DataForMLTest$Prediction)
head(DataForMLTest)

# Creating the Confusion Matrix to calculate overall accuracy, precision and recall on TESTING data
#install.packages('caret')
library(caret)
AccuracyResults<-confusionMatrix(DataForMLTest$Prediction, DataForMLTest$TargetVariable, mode = "prec_recall")
AccuracyResults

# Since AccuracyResults is a list of multiple items, fetching useful components only
AccuracyResults[['table']]
AccuracyResults[['byClass']]

################################################################################################
################################################################################################

##################################### Ctree Decision Tree ######################################
install.packages('party')
library(party)
startTime=Sys.time()
DT_Model=ctree(TargetVariable ~ . , data=DataForMLTrain)
DT_Model
plot(DT_Model)

endTime=Sys.time()
endTime-startTime

# Checking Accuracy of model on Testing data
DataForMLTest$Prediction =predict(DT_Model, DataForMLTest, type = "response")
head(DataForMLTest)

# Creating the Confusion Matrix to calculate overall accuracy, precision and recall on TESTING data
library(caret)
AccuracyResults=confusionMatrix(DataForMLTest$Prediction, DataForMLTest$TargetVariable, mode = "prec_recall")
AccuracyResults

# Since AccuracyResults is a list of multiple items, fetching useful components only
AccuracyResults[['table']]
AccuracyResults[['byClass']]