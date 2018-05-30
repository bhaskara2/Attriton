# Attriton
library(ggplot2)
library(caret)
library(caTools)
library(dplyr)
####################1.DATA EXPLORATION & PRE-PROCESSING#########################################

#Check for missing values in dataset
t(apply(is.na(df), 2, sum))

#Check for NULL values
t(apply(df, 2, is.null))


#Change datatypes of some variables to numeeric and viceversa
t(lapply(df, class))
df$Tenure = as.numeric(df$Tenure)
df$CivilStatusID = as.factor(df$CivilStatusID)
df$PresentCityID = as.factor(df$PresentCityID)
df$EducationalAttainmentID =  as.factor(df$EducationalAttainmentID)
df$FieldStudyID = as.factor(df$FieldStudyID)

df$Status = as.factor(df$Status)#0:Active and 1:Non-Active
df$Status <- ifelse(df$Status == "Inactive",1,0)


#Check for outliers in Tenure(Outliers increae variance in model and cause overfitting).
#There are no outliers in dataframe.
library(ggplot2)
boxplot(df$Tenure, main="Tenure", boxwex=0.1)

#Checking the proprtion of output classes in dataframe
prop.table(table(df$Status)) # The output classes are highly imbalanced, which leads to biased model.

df_fin = subset(df, select = c(Status,CivilStatusID,Gender,Account,FieldStudyID,Consecutive,Industry,Modality,EducationalAttainmentID,IsPrev1BPO))
df_fin$Status = as.factor(df_fin$Status)

#Other than output class imbalance daatset seems to be pretty clean.
###############################################################################################

#Sample the dataset with decent proportion of output classes.
library(ROSE)
data_balanced_under = ovun.sample(Status ~ ., data = df, method = "under", N = 2182, seed = 1)

data_ub = data_balanced_under$data

prop.table(table(data_ub$Status)) # The output classes are equally balanced now.

#####################################Visualizations###########################################

library(ggplot2)
g1 <- ggplot(df, 
             aes(x = Tenure, fill = Status)) + 
  geom_density(alpha = 0.7) + 
  scale_fill_manual(values = c("#386cb0","#fdb462"))

g1


g4 <- ggplot(df, 
             aes(x = Tenure, fill = Weekend)) + 
  geom_density(alpha = 0.7) + 
  scale_fill_manual(values = c("#386cb0","#fdb462"))

g4

#From the above graph g1, as the tenure increases the number of non-active people Increase.
#The trend continues to be same irrespective of class imbalance.


library(dplyr)
df_left = df_fin %>%
  filter(Status == 1) #Employees who left the organization./inactive

vis_2<-table(df$Account,df$Status)
d_vis_2<-as.data.frame(vis_2)
d_vis_2<-subset(d_vis_2,Var2==1)


library(ggplot2)
d_vis_2$Var1 <- factor(d_vis_2$Var1, levels = d_vis_2$Var1[order(-d_vis_2$Freq)])

p<-ggplot(d_vis_2, aes(x=Var1,y=Freq,fill=Var1)) +
  geom_bar(stat='identity') +theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(p)
p

#Majority of Inactive agents belong to Expedia Account, followed by Macquarie bank.

vis_3<-table(df$Account,df$Status)
d_vis_3<-as.data.frame(vis_3)
d_vis_3<-subset(d_vis_3,Var2==0)


library(ggplot2)
d_vis_3$Var1 <- factor(d_vis_3$Var1, levels = d_vis_3$Var1[order(-d_vis_3$Freq)])

p2<-ggplot(d_vis_3, aes(x=Var1,y=Freq,fill=Var1)) +
  geom_bar(stat='identity') +theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(p2)
#Pearson educaion,Bank of America,Hotelbeds has lesser number of Inactive agents when compatred to other accounts

#Trends in Expedia accout may be due to high volume of records belonging to that account.
#The trends from Pearson Education, Bank fo America and Hotelbeds may true. 

################################################################################################

library(randomForest)

control <- rfeControl(functions=rfFuncs, method="cv", number=10)

results <- rfe(df[,1:20], df[,21], sizes=c(1:21), rfeControl=control)

# list the chosen features
predictors(results)

# plot the results
plot(results, type=c("g", "o"))
#After observing this pot, around 9 features tend to give 93% acvcuracy, adding more variables is 
#overfitting the model and making it less powerful on test data.

fin_var=varImp(results)

df_fin = subset(data_ub, select = c(Status,CivilStatusID,Gender,Account,FieldStudyID,Consecutive,Industry,Modality,EducationalAttainmentID,IsPrev1BPO))


######################################################################################################
sample = sample.split(df_fin$Status, SplitRatio = .7)
x_train = subset(df_fin, sample == TRUE)
x_test = subset(df_fin, sample == FALSE)

df$Status <- ifelse(df$Status == "Inactive",1,0)


y_train<-x_train$Status
y_test <- x_test$Status

x_train$Status<-NULL
x_test$Status<-NULL


#CREATE TRAIN CONTROL OBJECT FOR 10 FOLD CV
ctrl.1<-trainControl(method="repeatedcv",number=10)

#Train RANDOM FOREST 
rf.cv<-train(x=x_train,y=y_train,method="rf",trControl=ctrl.1,tuneLength=3)

#Validate the RANDOM FOREST model
y_predicted<-predict(rf.cv,x_test)

df1<-data.frame(Orig=y_test,Pred=y_predicted)

confusionMatrix(table(df1$Orig,df1$Pred))

######################################################################################################
df_fin = subset(df, select = c(Status,CivilStatusID,Gender,Account,FieldStudyID,Consecutive,Industry,Modality,EducationalAttainmentID,IsPrev1BPO))
df_fin$Status = as.factor(df_fin$Status)


n = nrow(df_fin)
n


trainIndex = sample(1:n, size = round(0.7*n), replace=FALSE)
train = df[trainIndex ,]
test = df[-trainIndex ,]



logreg = glm(Status ~ ., family=binomial(logit), data=train)

summary(logreg)
test = test %>%
  filter(Industry != 'Social Media')

test = test %>%
  filter(Modality != 'Voice/Non-voice Back Office/Non-voice Email') 

# Make predictions on the out-of-sample data
predres=predict(logreg,newdata=test,type = 'response')

plot(predres)

logres = ifelse(predres>=0.5,1,0)
logres = as.factor(logres)

test$intervention = ifelse(predres>=0.7,1,0)
test$prob = predres

confusionMatrix(logres,test$Status)

#Check AUC/ to find if class imbalane effects.

##########################################################################################################
#Insights from model
#Acoount Tagged,Zoosk and Citi have postive effect on the termination of agents.

#Account fund BPO and pearson education has negative impact on employee termination. only 1/9 agents from these accounts.

#ModalityNon-Voice Back-office/Non-voice email/voice  has positve impact on emplpoyee attrition
#whereas ModalityVoice/Non-voice Email has negative impact on employee attrition.

#Education Attainment ID has no effect on agent being active/non active.
