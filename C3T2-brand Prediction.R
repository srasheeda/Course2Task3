library(caret)
library(readr)
library(C50)
library(gbm)
library(forcats)
# Import Dataset
CompleteResponses <- read_csv("~/Desktop/SurveyData/CompleteResponses.csv")
SurveyIncomplete <- read_csv("~/Desktop/SurveyData/SurveyIncomplete.csv")
#Pre Processing
str(CompleteResponses)
#Convert to factor
CompleteResponses$brand <- as.factor(CompleteResponses$brand)
CompleteResponses$car <- as.factor(CompleteResponses$car)
CompleteResponses$elevel <- as.ordered(CompleteResponses$elevel)
CompleteResponses$zipcode <- as.factor(CompleteResponses$zipcode)
str(CompleteResponses)
#Checking for missing data
sum(is.na(CompleteResponses))
#[1] 0
# Model Training/Testing sets
set.seed(123)
inTraining <- createDataPartition(CompleteResponses$brand, p = .75, list = FALSE)
training <- CompleteResponses[inTraining,]
testing <- CompleteResponses[-inTraining,]
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
c50Fit1 <- train(brand~., data = training, method = "C5.0", trControl = fitControl)
c50Fit1
#7424 samples
#6 predictor
#2 classes: '0', '1' 

#No pre-processing
#Resampling: Cross-Validated (10 fold, repeated 1 times) 
#Summary of sample sizes: 6682, 6682, 6682, 6681, 6682, 6682, ... 
#Resampling results across tuning parameters:
  
 # model  winnow  trials  Accuracy   Kappa    
#rules  FALSE    1      0.8605883  0.7152208
#rules  FALSE   10      0.9004682  0.7903796
#rules  FALSE   20      0.8997976  0.7894101
#rules   TRUE    1      0.8550620  0.7021169
#rules   TRUE   10      0.9148687  0.8186698
#rules   TRUE   20      0.9151391  0.8193717
#tree   FALSE    1      0.8587017  0.7061657
#tree   FALSE   10      0.9154083  0.8212128
#tree   FALSE   20      0.9154076  0.8210016
#tree    TRUE    1      0.8465716  0.6769847
#tree    TRUE   10      0.9159470  0.8218726
#tree    TRUE   20      0.9158121  0.8213872

#Accuracy was used to select the optimal model using the largest value.
#The final values used for the model were trials = 10, model = tree and winnow = TRUE.

# use VarImp() to assess how the model prioritized each feature during training
varImp(object = c50Fit1)
#C5.0 variable importance

#only 20 most important variables shown (out of 34)

#Overall
#salary    100.00
#age        85.51
#car6       23.63
#credit     18.80
#elevel.L   14.72
#car8       14.00
#car12       9.50
#zipcode1    9.35
#car3        8.19
#car19       7.38
#zipcode3    4.66
#car4        2.46
#car18       2.25
#car16       1.63
#car2        1.54
#car5        0.43
#car20       0.00
#elevel.C    0.00
#car7        0.00
#car10       0.00

plot(varImp(object = c50Fit1), main = "C5.0 - Variable Importance")
# random forest model with 5 different mtry values manually tuned
rfGrid <- expand.grid(mtry = c(1, 2, 3, 4, 5))
rfFit1 <- train(brand~., data = training, method = "rf", trControl = fitControl, tuneGrid = rfGrid)
rfFit1
#No pre-processing
#Resampling: Cross-Validated (10 fold, repeated 1 times) 
#Summary of sample sizes: 6681, 6683, 6681, 6682, 6682, 6681, ... 
#Resampling results across tuning parameters:
  
 # mtry  Accuracy   Kappa       
#1     0.6217673  0.0000000000
#2     0.6220368  0.0008828796
#3     0.7358595  0.3721837054
#4     0.8437528  0.6604894422
#5     0.8868524  0.7596324510

#Accuracy was used to select the optimal model using the largest value.
#The final value used for the model was mtry = 5.

# use VarImp() to assess variable importance during training
varImp(object = rfFit1)

#rf variable importance

#only 20 most important variables shown (out of 34)#

#Overall
#salary   100.0000
#age       40.0013
#credit    20.0600
#elevel.C   1.5901
#elevel^4   1.5846
#elevel.L   1.5426
#elevel.Q   1.0315
#zipcode6   0.6303
#zipcode2   0.5721
#zipcode1   0.5715
#zipcode4   0.5619
#zipcode7   0.4970
#zipcode5   0.4824
#zipcode3   0.4719
#zipcode8   0.4226
#car15      0.3082
#car17      0.2606
#car7       0.2402
#car4       0.2382
#car2       0.2334
plot(varImp(object = rfFit1), main = "Random Forest - Variable Importance")
#Gradient Boosting Model
set.seed(123)
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
gbmFit1 <-train(brand~., data = training, method= "gbm", trControl= fitControl, verbose=FALSE)
gbmFit1
#Stochastic Gradient Boosting 

#7424 samples
#6 predictor
#2 classes: '0', '1' 

#No pre-processing
#Resampling: Cross-Validated (10 fold, repeated 1 times) 
#Summary of sample sizes: 6681, 6681, 6681, 6682, 6681, 6683, ... 
#Resampling results across tuning parameters:
  
  #interaction.depth  n.trees  Accuracy   Kappa    
#1                   50      0.7304699  0.4317212
#1                  100      0.7292588  0.4278803
#1                  150      0.7272376  0.4220247
#2                   50      0.8127736  0.6067972
#2                  100      0.8840257  0.7575230
#2                  150      0.9057099  0.8012752
#3                   50      0.8779632  0.7471118
#3                  100      0.9008596  0.7920974#
#3                  150      0.9179649  0.8269911

#Tuning parameter 'shrinkage' was held constant at a value of 0.1
#Tuning parameter 'n.minobsinnode' was
#held constant at a value of 10
#Accuracy was used to select the optimal model using the largest value.
#The final values used for the model were n.trees = 150, interaction.depth = 3, shrinkage = 0.1
#and n.minobsinnode = 10.

#Predicting Brand Preference
str(SurveyIncomplete)
# convert to factor
SurveyIncomplete$brand <- as.factor(SurveyIncomplete$brand)
SurveyIncomplete$car <- as.factor(SurveyIncomplete$car)
SurveyIncomplete$elevel <- as.ordered(SurveyIncomplete$elevel)
SurveyIncomplete$zipcode <- as.factor(SurveyIncomplete$zipcode)
str(SurveyIncomplete)
PredictionsBrand <- predict(c50Fit1, SurveyIncomplete)
summary(PredictionsBrand)
PredictionsBrand <- predict(gbmFit1, SurveyIncomplete)
summary(PredictionsBrand)
# 0    1 
#1949 3051
PredictionsBrand <- predict(rfFit1, SurveyIncomplete)
summary(PredictionsBrand)

resample_results <-resamples(list(RandomForest = rfFit1, C5.0= c50Fit1, GBM=gbmFit1))
resample_results
summary(resample_results)
#Call:
  #summary.resamples(object = resample_results)

#Models: RandomForest, C5.0, GBM 
#Number of resamples: 10 

Accuracy 
               #Min.   1st Qu.    Median      Mean   3rd Qu.      Max.    NA's
#RandomForest 0.8771930 0.8795424 0.8881402 0.8868524 0.8925202 0.8963661    0
#C5.0         0.9056604 0.9094591 0.9138056 0.9159470 0.9198916 0.9313594    0
#GBM          0.9016173 0.9137757 0.9211045 0.9179649 0.9246299 0.9286676    0

#Kappa 
                #Min.   1st Qu.    Median      Mean   3rd Qu.      Max.    NA's
#RandomForest 0.7389845 0.7452231 0.7617969 0.7596325 0.7725031 0.7804213    0
#C5.0         0.7989596 0.8078052 0.8174655 0.8218726 0.8317570 0.8539561    0
#GBM          0.7946510 0.8180841 0.8328106 0.8269911 0.8420563 0.8482288    0

bwplot(resample_results)

diff_results<- diff(resample_results)
summary(diff_results, metric="accuracy")
#Call:
  #summary.diff.resamples(object = diff_results, metric = "accuracy")

#p-value adjustment: bonferroni 
#Upper diagonal: estimates of the difference
#Lower diagonal: p-value for H0: difference = 0

#Accuracy 
              #RandomForest C5.0      GBM      
#RandomForest              -0.029095 -0.031113
#C5.0         3.361e-05              -0.002018
#GBM          5.162e-05    1                  

#Kappa 
            #RandomForest C5.0      GBM      
#RandomForest              -0.062240 -0.067359
#C5.0         3.229e-05              -0.005118
#GBM          3.751e-05    1                  
testPredgbm1 <- predict(gbmFit1,testing)
postResample(testPredgbm1, testing$brand)
#Accuracy     Kappa 
#0.9284559 0.8497599 
#Confusion Matrix
confusionMatrix(testPredgbm1, testing$brand)
#Confusion Matrix and Statistics

#Reference
#Prediction    0    1
#0  877  118
#1   59 1420

#Accuracy : #0.9285          
#95% CI : (0.9176, 0.9383)
#No Information Rate : #0.6217          
#P-Value [Acc > NIR] : #< 2.2e-16       

#Kappa :# 0.8498          

#Mcnemar's Test P-Value : 1.303e-05       
                                          
#            Sensitivity : 0.9370          
#            Specificity : 0.9233          
#        Pos Pred Value : 0.8814          
#         Neg Pred Value : 0.9601          
#             Prevalence : 0.3783          
#         Detection Rate : 0.3545          
#   Detection Prevalence : 0.4022          
#      Balanced Accuracy : 0.9301          
                                          
#       'Positive' Class : 0               
                                          

finalPredgbm1 <- predict(gbmFit1, SurveyIncomplete)
summary(finalPredgbm1)
#0    1 
#1949 3051 
postResample(finalPredgbm1, SurveyIncomplete$brand)
