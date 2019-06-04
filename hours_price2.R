library(dplyr)
library(mice)
library(caret)
library(randomForest)

tr<-read.csv("train (2).csv")
#Preprocess the data, get rid of the NA values
#If factor variable, turn NA into a string "NA" and 0 if numeric turn NA to 0
tr<-tr[,2:81]
ind<-c()
for (i in 1:length(names(tr))){
        if (is.numeric(tr[[i]])==T){
                tr[[i]][which(is.na(tr[[i]]))]<-0
        }
        else{
                tr[[i]]<-as.character(tr[[i]])
                tr[[i]][which(is.na(tr[[i]]))]<-"None"
                ind<-c(ind,i)
                
        }
        for (i in ind){
                tr[[i]]<-as.factor(tr[[i]])
        }
}
#

te<-read.csv("test.csv")
te<-te[,2:80]
ind<-c()
nind<-c()
for (i in 1:length(names(te))){
        if (is.numeric(te[[i]])==T){
                te[[i]][which(is.na(te[[i]]))]<-0
                nind<-c(nind,i)
        }
        else{
                te[[i]]<-as.character(te[[i]])
                te[[i]][which(is.na(te[[i]]))]<-"None"
                ind<-c(ind,i)
                
        }
        for (i in ind){
                te[[i]]<-as.factor(te[[i]])
        }
}

#The problem we get is that there NA values in the test set that do not show up in the training se
# we will use mice to replace these values

# This code determines which factor variables in the test set have different levels than
#the training set
sub<-c()
for (i in 1:79){
        sub<-c(sub,all(levels(te[,i]) %in% levels(tr[,i])))
}

#Getting the index where the levels do not match
ind<-which(!sub)

for (i in ind){
        te[[i]][te[[i]]=="None"]<-NA
}



imputed_Data <- mice(te, m=1, maxit = 20, method = 'pmm', seed = 500,remove_collinear=F)
completeData<-complete(imputed_Data,1)
# However mice does not impute all the data, as it removes colinear variables
# we will manually impute the data 
isn<-is.na(completeData)
ind1<-which(isn,arr.ind=T)
completeData[ind1[1,1],ind1[1,2]]="AllPub"
completeData[ind1[2,1],ind1[2,2]]="AllPub"
for (i in ind){
        completeData[[i]]<-factor(completeData[[i]])
}

# To match the training and test data we must make sure the levels for all vairables is the same
for (i in 1:79){
        levels(completeData[[i]])<-levels(tr[[i]])}

#Expolatory analysis of the data, shows that much of the continous data is not normally distributed
# we will use  a Box-Cox transformation to give the data a continous distribution

for (i in nind){
        bc<-BoxCoxTrans(tr[[i]][tr[[i]]!=0])
        tr[[i]][tr[[i]]!=0]<-predict(bc,tr[[i]][tr[[i]]!=0])
        completeData[[i]][completeData[[i]]!=0]<-predict(bc,completeData[[i]][completeData[[i]]!=0])}

#We shoudl also look to remove variables that have zero or near zero variance, we will remove 
nv<-nearZeroVar(tr)
tr<-tr[,-nv]
completeData<-completeData[,-nv]

#We are los predicting the log of house prices
tr<-mutate(tr,SalePrice=log(SalePrice))
#Thus now we have preprocessed the training and test data sets

rf<-randomForest(x=tr[,1:dim(tr)[2]-1],y=tr[,dim(tr)[2]])

# To match the training and test data we must make sure the levels for all vairables is the same
for (i in 1:56){
        levels(completeData[[i]])<-levels(tr[[i]])}

#Using caret to find the optimal hyperparameters for random forest and gradient boosting
# These models will be used to create a stacked model
rf<-train(SalePrice~.,data=tr,method="rf",trControl=trainControl(method="cv", number = 5),tuneGrid=data.frame(mtry=c(18,50,200)))
gbm<-train(SalePrice~.,data=tr,method="gbm",trControl=trainControl(method="cv", number = 5))

#Using elasticnet regression
lr<-train(SalePrice~.,data=tr,method="glmnet",trControl=trainControl(method="cv", number = 5) )

#Now we will stack the models using the optimal hyperparamters to re-train the models,

cvst<-createFolds(tr[,57],k=5)

#First we must create a training set for the stacked model, this involves predicting
# on cross-validated sets

p1<-NULL
p2<-NULL
p3<-NULL

for (i in 1:5){
        rf_cv<-train(SalePrice~.,data=tr[-cvst[[i]],],method="rf",trControl=trainControl(method="cv", number = 5),tuneGrid=data.frame(rf$bestTune))
        gbm_cv<-train(SalePrice~.,data=tr[-cvst[[i]],],method="gbm",trControl=trainControl(method="cv", number = 5),tuneGrid=data.frame(gbm$bestTune))
        lr_cv<-train(SalePrice~.,data=tr[-cvst[[i]],],method="glmnet",trControl=trainControl(method="cv", number = 5),tuneGrid=data.frame(lr$bestTune))
        p1<-c(p1,predict.train(rf_cv,newdata=tr[cvst[[i]],]))
        p2<-c(p2,predict.train(gbm_cv,newdata=tr[cvst[[i]],]))
        p3<-c(p3,predict.train(lr_cv,newdata=tr[cvst[[i]],]))
}

#To do the training of the stacked model, we use the original dataset, plus the
#predicted values

total_df=tr[c(cvst[[1]],cvst[[2]],cvst[[3]],cvst[[4]],cvst[[5]]),]
total_df$rf=p1
total_df$gbm=p2
total_df$lr=p3

# Now we train a random forest on the stacked training set ( we use random forest as it gave 
# us the best performance on the stacked data compared to gradient boosting and regression)
rf_stack<-train(SalePrice ~.,data=total_df,method="rf",trControl=trainControl(method="cv", number = 5) )


# Now we build the stacked testing set
pt1<-predict.train(rf,newdata=completeData)
pt2<-predict.train(gbm,newdata=completeData)
pt3<-predict.train(lr,newdata=completeData)

completeData_stack<-mutate(completeData,rf=pt1,gbm=pt2,lr=pt3)

#And now we used the randomForest model trained on the data to make the final prediction
#on the testing data

final_p<-predict.train(rf_stack,completeData_stack)

# We find the final stacked model has a smaller RMSE than any of the individual models

