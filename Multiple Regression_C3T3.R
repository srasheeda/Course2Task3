library(caret)
library(readr)
library(gbm)
library(corrplot)
library(e1071)

#Import Data
existing_prods <- read_csv("existing_prods.csv")
newproductattributes2017 <- read_csv("newproductattributes2017.csv")
str(existing_prods)
print()
# dummify the data
newDataFrame <- dummyVars(" ~ .", data = existing_prods)
readyData <- data.frame(predict(newDataFrame, newdata = existing_prods))
summary(readyData)
str(readyData)
is.na(readyData)
# Deleting feature with NA
readyData$BestSellersRank <- NULL

readyData$ProductNum <- NULL
readyData$Price <- NULL
readyData$ExtendedWarranty <- NULL
readyData[!duplicated(readyData$ExtendedWarranty)]

corrData <- cor(readyData)
corrData
corrplot(corrData)
inTrain  <- createDataPartition(existing_prods$Volume,p = 0.75, list = FALSE)
training <- readyData[inTrain,]
testing  <- readyData[-inTrain,]
fitControl     <- trainControl(method = "repeatedcv", number = 10)


# Random forest model with 5 different mtry values manually tuned

rfGrid <- expand.grid(mtry = c(1, 2, 3, 4, 5))
rfFit1 <- train(Volume~., data = training, method = "rf", trControl = fitControl, tuneGrid = rfGrid)
rfFit1
#Random Forest 

#61 samples
#25 predictors

#No pre-processing
#Resampling: Cross-Validated (10 fold, repeated 1 times) 
#Summary of sample sizes: 55, 55, 56, 56, 56, 54, ... 
#Resampling results across tuning parameters:
  
#mtry  RMSE      Rsquared   MAE     
#1     912.8136  0.8409799  578.4638
#2     835.6080  0.8956282  464.3991
#3     828.4489  0.8957829  429.5522
#4     825.2532  0.9060126  417.2476
#5     820.9278  0.9147256  401.7401

#RMSE was used to select the optimal model using the smallest value.
#The final value used for the model was mtry = 5.
imp <- varImp(rfFit1)
print(imp)
plot(varImp(object = rfFit1), main = "Random Forest - Variable Importance")

#Support Vector Machines
set.seed(123)

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1, search="random")
svmGrid <- expand.grid(cost = c(0.25, .5, 1, 10))
svmFit <- train(Volume ~.,data = training,  method = 'svmLinear2', trControl=fitControl, tuneGrid=svmGrid, preProc = c('center','scale'))
svmFit

#Support Vector Machines with Linear Kernel 

#61 samples
#25 predictors

#Pre-processing: centered (25), scaled (25) 
#Resampling: Cross-Validated (10 fold, repeated 1 times) 
#Summary of sample sizes: 55, 53, 55, 55, 55, 56, ... 
#Resampling results across tuning parameters:
  
 # cost   RMSE      Rsquared   MAE     
#0.25  341.5610  0.8356156  231.6197
#0.50  341.5318  0.8356360  231.5723
#1.00  341.5318  0.8356360  231.5723
#10.00  341.5318  0.8356360  231.5723

#RMSE was used to select the optimal model using the smallest value.
#The final value used for the model was cost = 0.5.
svmFit
imp <- varImp(svmModel)
print(imp)

#Gradient Boosting Model
set.seed(123)
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
gbmFit<-train(Volume ~ ., data=training, method='gbm',trControl=fitControl, verbose=FALSE)
gbmFit
#Stochastic Gradient Boosting 

#61 samples
#25 predictors

#No pre-processing
#Resampling: Cross-Validated (10 fold, repeated 1 times) 
#Summary of sample sizes: 54, 56, 53, 55, 54, 55, ... 
#Resampling results across tuning parameters:
  
# interaction.depth  n.trees  RMSE       Rsquared   MAE     
#1                   50       982.1873  0.8443620  586.5491
#1                  100      1042.8870  0.7799745  649.1788
#1                  150      1089.1901  0.7754051  667.9829
#2                   50      1039.6114  0.7899454  625.5481
#2                  100      1093.9953  0.7524462  675.4121
#2                  150      1135.1492  0.7147135  701.2020
#3                   50       951.4659  0.8437583  561.2779
#3                  100      1116.8087  0.7668456  688.8619
#3                  150      1152.7769  0.7157181  708.9904

#Tuning parameter 'shrinkage' was held constant at a value of 0.1
#Tuning parameter 'n.minobsinnode' was held constant at a value of 10
#RMSE was used to select the optimal model using the smallest value.
#The final values used for the model were n.trees = 50, interaction.depth = 3, shrinkage = 0.1 and n.minobsinnode = 10.
#PREDICTIONS
  #Random Forest
Pred_rf<-predict(rfFit1, newdata= testing)
Pred_rf
##3          6         12         14         15         16         24         26         31         33 
#29.72724  528.01147  269.36160 1194.08987 1105.11669   92.59897  144.33547 1107.62455   23.67197   76.13446 
##34         42         46         49         52         63         65         74         79 
#1232.69573  102.90964 1245.70602   68.00200  304.86773   39.31373  192.59960  612.84895 1139.94507 
  #Support Vector Machine
Pred_svm<-predict(svmFit, newdata = testing)
Pred_svm
#3          6         12         14         15         16         24         26         31         33 
#84.23882  201.33758  150.92148 1252.22607 1326.56738  -60.21064  -24.56379  994.58460  183.16472  167.38051 
#34         42         46         49         52         63         65         74         79 
#1403.03549   84.89020 1432.06784   13.84364  238.02116   60.18741   77.75979  468.66947 1369.06079 
  #Gradient Boosting
pred_gbm<-predict(gbmFit, newdata =testing)
pred_gbm
#[1]  262.004769  764.439341  335.756167 2085.844148 1684.864467    9.014462 -170.953594 2088.326507   88.992317
#[10]  114.752806 1538.868090  346.158241 1895.351634  431.667832  928.488942  131.319828  532.924031  426.357846
#[19] 1804.863580
postResample (Pred_rf, testing$Volume)
#RMSE        Rsquared         MAE 
#145.5545060   0.9572494 101.7827990 
postResample (Pred_svm, testing$Volume)
#RMSE        Rsquared         MAE 
#112.0145794   0.9628383  90.9707990
postResample (pred_gbm, testing$Volume)
#RMSE        Rsquared         MAE 
#411.9126844   0.8912241 328.9957724 

#Import New Prods

new_prods <- read_csv("new_prods.csv")
str(new_prods)
# dummify the data
newDataFrame <- dummyVars(" ~ .", data = new_prods)
readyData2 <- data.frame(predict(newDataFrame, newdata = new_prods))
summary(readyData2)
str(readyData2)
is.na(readyData2)
# Deleting feature with NA
readyData2$BestSellersRank <- NULL

readyData2$ProductNum <- NULL
readyData2$Price <- NULL
readyData2$ExtendedWarranty <- NULL
readyData2[!duplicated(readyData$ExtendedWarranty)]
FinalPred<-predict(rfFit1, newdata= readyData2)
FinalPred
#1          2          3          4          5          6          7          8          9         10 
#432.93080  255.35173  242.07040   32.64539   44.85000  115.13880 1298.08722  328.31160   45.78413 1047.82722 
#11         12         13         14         15         16         17         18         19         20 
#4951.90880  409.47507  778.59853  113.47333  313.21627 1566.40562   40.50155   77.84200  115.11960  144.33547 
#21         22         23         24 
#144.63347   23.67197   71.24160 4114.00882 
output<-new_prods
output$predictions<-FinalPred
write.csv(output, file="C2.T3output.csv",row.names=TRUE)
