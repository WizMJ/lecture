### Data: Seoul Data from MASS Package
### Applied Algorithm: linear model, random forest, boosting, ridge, lasso, knn, pls, svm
## Preprocess
setwd("d:\\R/Seoul")
library(MASS)
library(randomForest)
library(caret)
library(corrplot)
library(car)
library(ggmap)
library(dplyr)
library(FNN)
library(glmnet)
library(gbm)
data(Boston)
Seoul <- Boston; rm(Boston)
str(Seoul)

set.seed(100)
train <- createDataPartition(y=Seoul$medv, p=0.75, list=FALSE) 
trainSet <- Seoul[train,]
testSet <- Seoul[-train,]
trainSetX <- trainSet[,-14]
trainSetY <- trainSet[,14]
testSetX <- testSet[,-14]
testSetY <- testSet[,14]


### EDA ###
ggplot(trainSet, aes(x =medv)) + geom_histogram(binwidth =0.5, col="black", alpha =0.3, fill="lightblue", aes(y=..density..))

cm <- cor(trainSet[,-4])  #chas 제거
corrplot(cm, type="upper", tl.pos ="d")
corrplot(cm, add=TRUE, type ="lower", method ="number", diag =FALSE, tl.pos ="n", cl.pos ="n")

corrplot(cm, order ="hclust", tl.cex =1.5, tl.col="black",
         addrect = 8, type ="full", diag = FALSE)  #hierachial clust

# Linear Regression 1
lm <- lm(medv~ ., data = trainSet)
final.lm <- step(lm)
summary(final.lm)
plot(final.lm)
par(mfrow =c(1,1))

for (i in 1:12){
boxplot(trainSet[,i])
}
boxplot(log(trainSet[,1]))

# Linear Regression 2 (Filtered)
temp_Seoul <- trainSet[,-c(4,14)]
highCorr <- findCorrelation(cor(temp_Seoul), cutoff= 0.75)
rm_var <- colnames(temp_Seoul)[highCorr]
filteredSeoul <- trainSet %>% select(-rm_var)

lm <- lm(medv~ ., data = filteredSeoul)
final.lm_filtered <- step(lm)
summary(final.lm_filtered)


## Validation Method
ctrl <- trainControl(method="repeatedcv", number=10, repeats =5)

## Model Tuning
### KNN   k=3 selection
set.seed(100)
knnTune <- train(medv ~., data =trainSet, method ="knn",
                 preProcess = c("center", "scale"),
                 tuneGrid =data.frame(.k =1:10),
                 trControl = ctrl)

### ridge regression
set.seed(100) # lambda =0.016
ridgeTune <- train(medv ~., data =trainSet, method ="ridge",
                   tuneGrid = data.frame(.lambda =seq(0.001, 0.05, length =20)),
                   preProcess = c("center", "scale"),
                   trControl = ctrl)

## random forest
set.seed(100) # mrty =5
rfTune <- train(medv ~ ., data =trainSet, method ="rf", tuneLength =10, ntrees =1000, 
                importance =TRUE, trControl = ctrl)
#save(list=ls(), file = "rfTune.RData") #저장 
load("rfTune.RData")
rfTune$finalModel$importance


## gradient boosting machine  shrinkage =0.01, depth = 5, ntree=  1500
gbmGrid <- expand.grid(.interaction.depth =seq(1,7, by =2),
                       .n.trees =seq(500, 2000, by =500),
                       .shrinkage =c(0.01, 0.02, 0.03),
                       .n.minobsinnode =(10))
gbmTune <- train(medv ~ ., data =trainSet, method ="gbm", tuneGrid =gbmGrid, verbose =FALSE)
#save(list=ls(), file = "gbmTune.RData") #저장 
load("gbmTune.RData")

#Model Selection

# linear model 1
pred <- predict(final.lm, newdata = Seoul[-train,])
RMSE(pred,Seoul[-train,"medv"])

# linear model 2
pred <- predict(final.lm_filtered, newdata = Seoul[-train,])
RMSE(pred,Seoul[-train,"medv"])

# knn
knnFit <- knnreg(trainSetX, trainSetY, k =3)
pred <- predict(knnFit, testSetX)
RMSE(pred,Seoul[-train,"medv"])

# ridge
x <- model.matrix(medv~., Seoul)[,-1]
y <- Seoul$medv
grid <- 10^seq(10, -3, length =100)
ridgeFit <- glmnet(x[train,], y[train], alpha =0, lambda = grid, thresh = 1e-12 )
pred <- predict(ridgeFit, s =0.016, newx=x[-train,])
RMSE(pred,Seoul[-train,"medv"])

# random Forest
rfFit <- randomForest(medv~., data =trainSet, mtry =5, importance =TRUE)
pred <- predict(rfFit, newdata = Seoul[-train,])
RMSE(pred,Seoul[-train,"medv"])
varImpPlot(rfFit)

# gradient boosting machine
gbmFit <- gbm(medv~., data =Seoul[train,], distribution ="gaussian", n.trees =1500, 
              interaction.depth =5, shrinkage= 0.01, verbose =F)
summary(gbmFit)
pred <- predict(gbmFit, newdata = Seoul[-train,], n.trees =1500)
RMSE(pred,Seoul[-train,"medv"])
                   
                         


