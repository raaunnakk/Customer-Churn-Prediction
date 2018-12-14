rm(list=ls(all=T))
setwd("C:/Users/RAUNAK/Desktop/edwisor/workspace")

#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees','usdm','devtools')


lapply(x, require, character.only = TRUE) 

install_github("kassambara/easyGgplot2")
library(easyGgplot2)




## Read the data
churn_telecom1 = read.csv("Train_data.csv", header = T, na.strings = c(" ", "", "NA"))
churn_telecom2 = read.csv("Test_data.csv", header = T, na.strings = c(" ", "", "NA"))


#Combine the given datasets into one dataset
churn_telecom<- rbind(churn_telecom1, churn_telecom2)

#Save the combined dataset as CSV
write.csv(churn_telecom, "churn_telecom.csv", row.names = F)

#Read the saved dataset
churn_telecom = read.csv("churn_telecom.csv", header = T, na.strings = c(" ", "", "NA"))



###########################################Explore the data##########################################

#Check Datatypes
str(churn_telecom)

#Remove state, area code,phone_number  columns because we are predicting churn based on Usage and Plans
churn_telecom= subset(churn_telecom, select = -c(state,area.code, phone.number))

#Re-arrange numeric variables and factor variables
churn_telecom <- churn_telecom[, c(1,4,5,6,7,8,9,10,11,12,13,14,15,16,17,2,3,18)]

#Convert all integer/numeric variables to numeric
churn_telecom[1:15] <- sapply(churn_telecom[1:15] , as.numeric)

#Convert factor variables to numeric levels

for(i in 1:ncol(churn_telecom)){
  
  if(class(churn_telecom[,i]) == 'factor'){
    
    churn_telecom[,i] = factor(churn_telecom[,i],labels=0:(length(levels(factor(churn_telecom[,i])))-1))
    
  }
}

#Visualise Histograms for Numeric Variables.

require(ggplot2)
require(scales)


ggplot2.histogram(data=churn_telecom$account.length, xtitle='account.length',
                    fill="#FFAAD4", color="#FFAAD4",
                    addMeanLine=TRUE, meanLineColor="red",
                    meanLineType="dashed", meanLineSize=1, binwidth=8,
                    axisLine=c(0.5, "solid", "black")
                    )
  
  ggplot2.histogram(data=churn_telecom$international.plan, xtitle='international.plan',
                    fill="#FFAAD4", color="#FFAAD4",
                    addMeanLine=TRUE, meanLineColor="red",
                    meanLineType="dashed", meanLineSize=1, binwidth=8,
                    axisLine=c(0.5, "solid", "black"))
  
  ggplot2.histogram(data=as.numeric(churn_telecom$voice.mail.plan), xtitle='international.plan',
                    fill="#FFAAD4", color="#FFAAD4",
                    addMeanLine=TRUE, meanLineColor="red",
                    meanLineType="dashed", meanLineSize=1, binwidth=0.1,
                    axisLine=c(0.5, "solid", "black"))
  
  ggplot2.histogram(data=as.numeric(churn_telecom$total.intl.charge), xtitle='total.intl.charge',
                    fill="#FFAAD4", color="#FFAAD4",
                    addMeanLine=TRUE, meanLineColor="red",
                    meanLineType="dashed", meanLineSize=1, binwidth=0.5,
                    axisLine=c(0.5, "solid", "black"))
  
#Percentage plots for Categorical Variables
  
 ggplot(churn_telecom, aes(x = international.plan)) +  
    geom_bar(aes(y = (..count..)/sum(..count..))) + 
   geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
   scale_y_continuous(labels=percent) + 
   labs(title = "", y = "Percent", x = "International.Plan")
 
 ggplot(churn_telecom, aes(x = voice.mail.plan)) +  
   geom_bar(aes(y = (..count..)/sum(..count..))) + 
   geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
   scale_y_continuous(labels=percent) + 
   labs(title = "", y = "Percent", x = "voice.mail.plan")
 
 ggplot(churn_telecom, aes(x = Churn)) +  
   geom_bar(aes(y = (..count..)/sum(..count..))) + 
   geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
   scale_y_continuous(labels=percent) + 
   labs(title = "", y = "Percent", x = "Churn")
 




##################################Missing Values Analysis###############################################
#Check for null fields
sum(is.na(churn_telecom))





##################################Outlier Analysis#############################################


#selecting only numeric index
numeric_index = sapply(churn_telecom,is.numeric) 

numeric_index 

numeric_data =churn_telecom[,numeric_index]

cnames =  colnames(numeric_data)

 ## BoxPlots - Distribution and Outlier Check

#Generate Box Plots for Numeric variables. The same code has been used for Univariate Analysis
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i])), data = subset(churn_telecom))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="Churn")
           )
}


## Plotting plots together

gridExtra::grid.arrange(gn1,gn2,gn3,ncol=3)

gridExtra::grid.arrange(gn4,gn5,gn6,ncol=3)

gridExtra::grid.arrange(gn7,gn8,gn9,ncol=3)

gridExtra::grid.arrange(gn10,gn11,gn12,ncol=3)

gridExtra::grid.arrange(gn13,gn14,gn15,ncol=3)




#Replace outliers with maximum and minimum values
for(i in cnames){
  quantiles <- quantile( churn_telecom[,i], c(.25, .75 ) )
  churn_telecom[,i][ churn_telecom[,i] > (quantiles[2]+1.5*(quantiles[2]-quantiles[1])) ] <- (quantiles[2]+1.5*(quantiles[2]-quantiles[1]))
  churn_telecom[,i][ churn_telecom[,i] < (quantiles[1]-1.5*(quantiles[2]-quantiles[1])) ] <- (quantiles[1]-1.5*(quantiles[2]-quantiles[1]))
  
}


##############################Feature Scaling#######################################33

#Normalisation
cnames_norm = c("number.vmail.messages",
               "total.intl.calls",
               "number.customer.service.calls")

for(i in cnames_norm){
  print(i)
  churn_telecom[,i] = (churn_telecom[,i] - min(churn_telecom[,i]))/
    (max(churn_telecom[,i] - min(churn_telecom[,i])))
}



# #Standardisation
cnames_stand = c("account.length",
                 "total.day.minutes",
                 "total.day.calls",
                 
                 "total.eve.minutes",
                 "total.eve.calls",
                 
                 "total.night.minutes",
                 "total.night.calls",

                 "total.intl.minutes"
                 )


for(i in cnames_stand){
   print(i)
   churn_telecom[,i] = (churn_telecom[,i] - mean(churn_telecom[,i]))/
                                  sd(churn_telecom[,i])
 }


##################################Feature Selection################################################


## Correlation Plot 
corrgram(churn_telecom[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")



## Chi-squared Test of Independence
factor_index = sapply(churn_telecom,is.factor)
factor_data = churn_telecom[,factor_index]


for (i in 1:2)
{
  print(names(factor_data)[i])
  print(chisq.test(table(factor_data$voice.mail.plan,factor_data[,i])))
}

for (i in 1:2)
{
  print(names(factor_data)[i])
  print(chisq.test(table(factor_data$Churn,factor_data[,i])))
}

#VIF Test

vifcor(churn_telecom[1:15], th = 0.9)



## Dimension Reduction

#Remove Highly collinear variables.
churn_telecom= subset(churn_telecom, select = -c(total.day.charge,total.eve.charge,total.night.charge,total.intl.charge))





###################################Model Development#######################################
#Clean the environment 
rmExcept(c("churn_telecom"))


#Check Distribution of Factor Variables
table(churn_telecom$international.plan)

#0    1 
#4527  473

table(churn_telecom$voice.mail.plan)

#0    1 
#3677 1323

# ##Stratified Sampling
stratas = strata(churn_telecom, c("international.plan"), size = c(3600, 360), method = "srswor")
train=getdata(churn_telecom,stratas)
train.index = createDataPartition(train$Churn, p = 1, list = FALSE)
test  = churn_telecom[-train.index,]
train= subset(train, select = -c(ID_unit, Prob,Stratum))



##Decision tree for classification

C50_model = C5.0(Churn ~., train, trials = 100, rules = TRUE)

#Summary of DT model
summary(C50_model)

#predict for test cases
C50_Predictions = predict(C50_model, test[,-14], type = "class")

summary(C50_Predictions)

##Evaluate the performance of classification model
ConfMatrix_C50 = table(test$Churn, C50_Predictions)

ConfMatrix_C50


 

###Random Forest for Classification

RF_model = randomForest(Churn ~ ., train, importance = TRUE, ntree = 6000)


#Predict test data using random forest model
RF_Predictions = predict(RF_model, test[,-14])

##Evaluate the performance of classification model
ConfMatrix_RF = table(test$Churn, RF_Predictions)
confusionMatrix(ConfMatrix_RF)





#Logistic Regression
logit_model = glm(Churn ~ ., data = train, family = "binomial")

#summary of the model
summary(logit_model)

#predict using logistic regression
logit_Predictions = predict(logit_model, newdata = test, type = "response")

#convert prob
logit_Predictions = ifelse(logit_Predictions > 0.5, 1, 0)


##Evaluate the performance of classification model
ConfMatrix_LP = table(test$Churn, logit_Predictions)

confusionMatrix(ConfMatrix_LP)




#naive Bayes
library(e1071)

#Develop model
NB_model = naiveBayes(Churn ~ ., data = train)

#predict on test cases #raw
NB_Predictions = predict(NB_model, test[,1:13], type = 'class')

#Look at confusion matrix
Conf_matrix = table(observed = test[,14], predicted = NB_Predictions)
confusionMatrix(Conf_matrix)



