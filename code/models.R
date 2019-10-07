##################data cleaning##################
setwd("/Users/jinchunyuan/Desktop/628/module2/project")
BodyFatData <- read.csv('BodyFat.csv')
BodyFatData <- BodyFatData[-which(BodyFatData$BODYFAT==0), ]
bodyfat_siri <- 495/BodyFatData$DENSITY - 450 #siri's equation
BodyFatData$BODYFAT[76] <- bodyfat_siri[76]
BodyFatData$BODYFAT[48] <- bodyfat_siri[48]
BodyFatData[42, "HEIGHT"] <- sqrt((703*BodyFatData$WEIGHT[42])/BodyFatData$ADIPOSITY[42])
check.index <- c(163, 220)
BodyFatData <- BodyFatData[-check.index,]
dim(BodyFatData)

############stepwise backward and forward LR##############
library(MASS)
base=lm(BODYFAT~1,data=BodyFatData[, c("BODYFAT","AGE","WEIGHT","HEIGHT","ADIPOSITY",
                                       "NECK", "CHEST","ABDOMEN","HIP","THIGH","KNEE",
                                       "ANKLE","BICEPS","FOREARM","WRIST" )])
full<-lm(BODYFAT~.,data=BodyFatData[, c("BODYFAT","AGE","WEIGHT","HEIGHT","ADIPOSITY",
                                        "NECK", "CHEST","ABDOMEN","HIP","THIGH","KNEE",
                                        "ANKLE","BICEPS","FOREARM","WRIST" )])
#  use forward stepwise method with BIC, and select four variables in linear regression 
stepAIC(base,scope=list(lower=~1,upper=full),direction = "forward",
        k=log(249),trace = F)
fit_for<-lm(formula = BODYFAT ~ ABDOMEN + WEIGHT + WRIST + FOREARM, 
         data = BodyFatData[, c("BODYFAT", "AGE", "WEIGHT", "HEIGHT", "ADIPOSITY", "NECK", 
                                "CHEST", "ABDOMEN", "HIP", "THIGH", "KNEE", "ANKLE", 
                                "BICEPS", "FOREARM", "WRIST")])
summary(fit_for)
stepAIC(full,scope=list(lower=~1,upper=full),direction = "backward",
        k=log(249),trace = F)
fit_bac<-lm(formula = BODYFAT ~ ABDOMEN + WEIGHT + WRIST + FOREARM, 
            data = BodyFatData[, c("BODYFAT", "AGE", "WEIGHT", "HEIGHT", "ADIPOSITY", "NECK", 
                                   "CHEST", "ABDOMEN", "HIP", "THIGH", "KNEE", "ANKLE", 
                                   "BICEPS", "FOREARM", "WRIST")])
summary(fit_bac)

#  Adjusted R-squared:  0.7317, fit the data well
#  check the collinearity of these variables
library(car)
vif(fit_bac)
sqrt(vif(fit_bac))>2
#  obvious collinearity in these variables, which possibly makes the model unstable
#  so we will use cross_validation to check whether this model has a large variance
#  to avoid collinearity, add regulization L1 to LR
data.frame(fitted(fit_bac),BodyFatData$BODYFAT)
(mse_lr<-sum((fitted(fit_bac)-y)^2))
################## lasso LR#####################
library(lars)
y=as.matrix(BodyFatData[,"BODYFAT"])
x=as.matrix(BodyFatData[, c("AGE","WEIGHT","HEIGHT","ADIPOSITY",
                            "NECK", "CHEST","ABDOMEN","HIP","THIGH","KNEE",
                            "ANKLE","BICEPS","FOREARM","WRIST" )])
model.lasso <- lars(x,y,type='lasso') 
plot(model.lasso)
set.seed(123)
CV.lasso <- cv.lars(x,y, K=5) 
(best <- CV.lasso$index[which.min(CV.lasso$cv)])#lambda=0.323
(coef.lasso <- coef.lars(model.lasso, mode='fraction', s=best))# coefficients with lasso
fit_val<-predict(model.lasso,x,s=best,type = 'fit',mode = 'fraction')
(mse_lasso<-sum((fit_val$fit-y)^2))

##################xgboost#################
library(xgboost)
library(Matrix)
set.seed(1234)
# leave 40 samples as testset
test<-sample(1:249,size=40,replace=F)
traindata<-BodyFatData[-test, c("AGE","BODYFAT","WEIGHT","HEIGHT","ADIPOSITY",
                           "NECK", "CHEST","ABDOMEN","HIP","THIGH","KNEE",
                           "ANKLE","BICEPS","FOREARM","WRIST" )]
traindata1 <- data.matrix(traindata[,c(1,3:8)])
traindata2 <- Matrix(traindata1,sparse=T)
traindata3 <- as.numeric(as.character(traindata[,"BODYFAT"])) 
traindata4 <- list(data=traindata2,label=traindata3) 
dtrain <- xgb.DMatrix(data = traindata4$data, label = traindata4$label) 

testset<-BodyFatData[test, c("AGE","BODYFAT","WEIGHT","HEIGHT","ADIPOSITY",
                              "NECK", "CHEST","ABDOMEN","HIP","THIGH","KNEE",
                              "ANKLE","BICEPS","FOREARM","WRIST" )]
testset1 <- data.matrix(testset[,c(1,3:8)]) 
testset2 <- Matrix(testset1,sparse=T) 
testset3 <- as.numeric(as.character(testset[,"BODYFAT"])) 
testset4 <- list(data=testset2,label=testset3) 
dtest <- xgb.DMatrix(data = testset4$data, label = testset4$label) 

# use cv tuning parameters
cv_tune<-function(max_dep,eta_,nround_){
  a=1:length(max_dep)
  for (i in 1:length(max_dep)){
    set.seed(45)
    param <- list(max_depth=max_dep[i], eta=eta_[i], silent=1, subsample=0.8) 
    bst = xgb.train(params = param, data = dtrain, nrounds = nround_, nthread = 2)
    model <- xgb.dump(bst,with_stats = T) 
    names <- dimnames(data.matrix(traindata[,c(1,3:8)]))[[2]] 
    importance_matrix <- xgb.importance(names,model=bst) 
    xgb.plot.importance(importance_matrix[,])
    # score the importance of variables
    pre_xgb = round(predict(bst,newdata = dtest))
    a[i]=sum((pre_xgb-testset3)^2)
    
  }
  return(a)
}
cv_tune(max_dep = c(3,4,4),eta_ = c(0.5,0.4,0.3),nround_ = c(5,5,3))

# max_depth=3,eta=0.5,nround=5
# build xgboost model 
param <- list(max_depth=3, eta=0.5, silent=1, subsample=0.8) 
bst = xgb.train(params = param, data = dtrain, nrounds = 5, nthread = 2)
model <- xgb.dump(bst,with_stats = T) # see all the process produced by xgboost
model 
names <- dimnames(data.matrix(traindata[,c(1,3:8)]))[[2]] 
importance_matrix <- xgb.importance(names,model=bst) # feature importance
options(repr.plot.width=3,repr.plot.height=1)
xgb.plot.importance(importance_matrix[,])

# calculate MSE of the whole dataset
testset<-BodyFatData[, c("AGE","BODYFAT","WEIGHT","HEIGHT","ADIPOSITY",
                             "NECK", "CHEST","ABDOMEN","HIP","THIGH","KNEE",
                             "ANKLE","BICEPS","FOREARM","WRIST" )]
testset1 <- data.matrix(testset[,c(1,3:8)]) 
testset2 <- Matrix(testset1,sparse=T) 
testset3 <- as.numeric(as.character(testset[,"BODYFAT"])) 
testset4 <- list(data=testset2,label=testset3) 
dtest <- xgb.DMatrix(data = testset4$data, label = testset4$label) 
pre_xgb = round(predict(bst,newdata = dtest))
(mse_xgb<-sum((testset$BODYFAT-pre_xgb)^2))

# compare 3 MSE, xgboost may be the best model to predict body fat.
# for rule of thumb, use linear regression model with two variables.
fit1<-lm(formula = BODYFAT ~ ABDOMEN + WEIGHT, 
            data = BodyFatData[, c("BODYFAT", "WEIGHT", "ABDOMEN")])
summary(fit1)
fit2<-lm(formula = BODYFAT ~ ABDOMEN + WRIST, 
         data = BodyFatData[, c("BODYFAT", "WRIST", "ABDOMEN")])
summary(fit2)
fit3<-lm(formula = BODYFAT ~ ABDOMEN + ADIPOSITY, 
         data = BodyFatData[, c("BODYFAT", "ADIPOSITY", "ABDOMEN")])
summary(fit3)
vif(fit1)
plot(fit1,which = 2)
par(mfrow=c(1,3))
plot(fitted(fit1),residuals(fit1),xlab = "fitted values",ylab = "residuals")
plot(BodyFatData$ABDOMEN,residuals(fit1),xlab = "ABDOMEN", ylab = "residuals")
plot(BodyFatData$WEIGHT,residuals(fit1),xlab = "WEIGHT", ylab = "residuals")
par(mfrow=c(1,2))
plot(fit1,which = 2)
plot(fit1,which = 5)





