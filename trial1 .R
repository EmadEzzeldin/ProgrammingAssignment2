### 1- GETTING DATA ==================================

getting _libraries = {
library(AppliedPredictiveModeling)
library(caret)
library(randomForest)
library (rms)}

getting_Data = {
library(MASS)
library(pROC)
set.seed(975)
training <-  quadBoundaryFunc(500)
testing <-  quadBoundaryFunc(1000)
testing$class2 <-  ifelse(testing$class == "Class1", 1, 0)
testing$ID <- 1:nrow(testing)
}


### 2- FITING MODELS ====
=============================
Fit_qda_rf_models = {
  qdaFit <-  qda(class ~ X1 + X2, data = training)
  qdaFit
  ##
  rfFit <-  randomForest(class ~ X1 + X2, data = training, ntree = 2000)
  rfFit
}
fit_glm           =  {
  set.seed(1056)
  logisticReg <-train(class ~ X1 + X2, data = training, method = "glm", trControl = trainControl(method = "repeatedcv", repeats = 5))
  logisticReg
}
fit_lrm           = {
  set.seed(1057)
  rcsFit <- lrm(class ~., data = training)
  #rcsFit <- lrm(Class ~ rcs(Day), data = training[pre2008,])
  nonlinearlogisticReg
}
fit_SVM= {
  library(kernlab)
  set.seed(1056)
  svmFit <- train(class ~ ., data = training , method = "svmRadial", preProc = c("center", "scale"), tuneLength = 10,trControl = trainControl(method = "repeatedcv", repeats = 5))
  svmFit
}
#1- logisticReg, 2- rcsFit , qdaFit , rfFit 
#comparemodels = function () {


### 3- PREDICTING ====================================
Predict_testset_qda_rf_linearlogistic_nonlinear = function () {
  testing$qda     <-  predict(qdaFit, testing)$posterior[,1]
  testing$rf      <-  predict(rfFit, testing, type = "prob")[,1]
  testing$rfclass <-  predict(rfFit, testing)
  testing$lr       =  predict (logisticReg, testing , type = "prob")[,1]
  testing$lrclass  =  predict(logisticReg, testing)
  testing$nonllr   =  predict (rcsFit, testing)[,1]
  testing$nonllrclass = predict(rcsFit, testing)
  head(testing)
}
predictTestset = function () {
  creditResults       <-  data.frame(obs = GermanCreditTest$Class)
  creditResults$prob  <-  predict(logisticReg, GermanCreditTest, type = "prob")[, "Bad"]
  creditResults$pred  <-  predict(logisticReg, GermanCreditTest)
  creditResults$Label <-  ifelse(creditResults$obs == "Bad", "True Outcome: Bad Credit",  "True Outcome: Good Credit")
}

### 4- EVALUATION ====================================
performance1 = function () {
  
  ## Let class 1 be the event of interest -------------------------
  
  sensitivity(data = testing$rfclass, reference = testing$class, positive = "Class1")
  specificity(data = testing$rfclass, reference = testing$class, negative = "Class2")
  
  ## Posterior (PPV or NPV) -----------------------
  PPV = posPredValue(data = testing$rfclass, reference = testing$class, positive = "Class1")
  PPV
  NPV = negPredValue(data = testing$rfclass, reference = testing$class, positive = "Class2")
  NPV
  ## change prior -------------------
  PPV = posPredValue(data = testing$rfclass, reference = testing$class, positive = "Class1", prevalence = 0.9)
  PPV
  ## Confusion matrix ---------------
  confusionMatrix(data = testing$rfclass, reference = testing$class, positive = "Class1")
  
  ## ROC curve,  --------------------------
  ## this function assumes the second class is the event of interest, so we reverse it
  rocCurve = roc(response = testing$class, predictor = testing$rf, levels = rev(levels(testing$class)))
  
  auc(rocCurve)         # area under the curve
  
  # by default the x-axis go backward, use the legacy.axes=TRUE to modify it
  
  plot(rocCurve, legacy.axes = TRUE)
}
#perfomance2 = function (){ ### Create the confusion matrix from the test set.
  confusionMatrix(data = creditResults$pred, reference = creditResults$obs)
  
  performance_plots = function () {
    ### Plot the probability of bad credit
    histogram(~prob|Label,
              data = creditResults,
              layout = c(2, 1),
              nint = 20,
              xlab = "Probability of Bad Credit",
              type = "count")
    
    ### Calculate and plot the calibration curve
    creditCalib <-   calibration(obs ~ prob, data = creditResults)
    xyplot(creditCalib)
    
    ### ROC curves:
    
    ### Like glm(), roc() treats the last level of the factor as the event
    ### of interest so we use relevel() to change the observed class data
    
    creditROC <-   roc(relevel(creditResults$obs, "Good"), creditResults$prob)
    auc(creditROC)
    ci.auc(creditROC)
    ### Note the x-axis is reversed
    #plot(creditROC)
    plot(creditROC, legacy.axes = TRUE)
    
    ### Lift charts
    creditLift <-   lift(obs ~ prob, data = creditResults)
    xyplot(creditLift)
  }
}





