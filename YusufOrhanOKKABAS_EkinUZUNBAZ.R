# ---------------------------------- GETTING DATA FROM DATA SOURCE  ----------------------------------

# Reading .csv file into data frame
obesityDataSetFile <- "C:\\Users\\Ekin\\Desktop\\2022-2023-GUZ\\CME4403\\PROJECT\\ObesityDataSet_raw_and_data_sinthetic.csv"

BASEDATASET <- read.csv (obesityDataSetFile, header = TRUE, quote ="\"", sep = ",", dec = ".", 
                         fill = TRUE, comment.char = "#", na.strings = "NA", skip = 0,
                         colClasses=c('factor', 'numeric','numeric','numeric', 
                                      'factor', 'factor','numeric','numeric',
                                      'factor', 'factor','numeric','factor',
                                      'numeric','numeric','factor','factor','factor'))

# -------------> Feature Selection by Boruta Package
# Install necessary packages
# install.packages("Boruta")

# Import necessary libraries
library(Boruta)

# -------------------- NOTES --------------------
# 1-) In our dataset, there is no missing values. If it exists in your dataset,
# you need to impute them before implementing boruta package!
# 2-) Tentative Attributes refers to importance score so close to their best shadow  
# attributes that Boruta is unable to decide in default number of random forest runs.
# -------------------- NOTES --------------------

set.seed(1827492)

borutaTrain <- Boruta(NObeyesdad ~., data = BASEDATASET, doTrace = 2)
print(borutaTrain)

# FOR BETTER VISUALIZATION
plot(borutaTrain, xlab = "", xaxt = "n")
history <-lapply(1:ncol(borutaTrain$ImpHistory),function(i)
  borutaTrain$ImpHistory[is.finite(borutaTrain$ImpHistory[,i]),i])
names(history) <- colnames(borutaTrain$ImpHistory)
Labels <- sort(sapply(history,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(borutaTrain$ImpHistory), cex.axis = 0.7)

# If any tentative attributes exist, you need to treat them. 
# Call TentativeRoughFix function to take decision on tentative attributes.
finalBoruta <- TentativeRoughFix(borutaTrain)
print(finalBoruta)

print(attStats(finalBoruta)) # give the results in form of data frame

selectedAttributes <- getSelectedAttributes(finalBoruta, withTentative = F)

# Remove columns of data frame that doesn't exist in selected attributes vector
for (attributeIndex in 1:length(colnames(BASEDATASET))) {
  element <- colnames(BASEDATASET)[attributeIndex]
  
  # Remove operations are done in the logic of the "index of" function
  if (element!="NObeyesdad" & is.na(match(element, selectedAttributes))) {
    # Remove if related column is different from selected attribute
    index <-match(element,colnames(BASEDATASET))
    BASEDATASET <- BASEDATASET[, -index]
    print(paste(element, "REMOVE OPERASYONUNA GIRDI")) # for debug
  }
  
}

# Display the final columns of base data set 
print("***FINAL COLUMNS FOR BASE DATA AFTER FEATURE SELECTION***")
print(colnames(BASEDATASET))

#  ------------------------------ END OF GETTING DATA FROM DATA SOURCE  ------------------------------

# ---------------------------------- OBESITY DATA WITH NUMERICAL VALUES ----------------------------------

convertToNumerical <- function(obesityDataSetNumeric){
  # Install necessary packages
  # install.packages('fastDummies')
  
  # Import necessary libraries
  library(fastDummies)
  
  # -------------------- NOTES --------------------
  # 1-) If there is more than one level for categorical values, use fastDummies library.
  # Otherwise if-else statements work well. No need for external library.
  # 2-) There is no N/A value in our dataset (it is stated in the website).
  # So, there is no need to handle mechanism for N/A values.
  # -------------------- NOTES --------------------
  
  # -------------> Convert categorical values to numerical values
  # If Gender is Female, convert to 1. If Gender is Male, convert to 0
  obesityDataSetNumeric$Gender <- ifelse(obesityDataSetNumeric$Gender == 'Female', 1, 0)
  
  # If family_history_with_overweight is yes, convert to 1. Otherwise, convert to 0
  obesityDataSetNumeric$family_history_with_overweight  <- ifelse(obesityDataSetNumeric$family_history_with_overweight  == 'yes', 1, 0)
  
  # If FAVC is yes, convert to 1. Otherwise, convert to 0
  obesityDataSetNumeric$FAVC  <- ifelse(obesityDataSetNumeric$FAVC  == 'yes', 1, 0)
  
  # If SMOKE is yes, convert to 1. Otherwise, convert to 0
  obesityDataSetNumeric$SMOKE <- ifelse(obesityDataSetNumeric$SMOKE  == 'yes', 1, 0)
  
  # If SCC is yes, convert to 1. Otherwise, convert to 0
  obesityDataSetNumeric$SCC <- ifelse(obesityDataSetNumeric$SCC  == 'yes', 1, 0)
  
  # Convert categorical values to numerical values by using dummy attributes
  obesityDataSetNumeric <- dummy_cols(obesityDataSetNumeric, select_columns = c('CAEC','CALC','MTRANS'),
                                      remove_selected_columns = TRUE)
  
  # -------------> Reorder the columns to place the target feature at last column. This makes the operations easier in next steps.
  targetFeature <- "NObeyesdad" # variable to hold target feature's name
  
  targetIndex <- which(colnames(obesityDataSetNumeric) == targetFeature) # get the index of target feature
  
  tempdata <- obesityDataSetNumeric$NObeyesdad # get the data of target column (dont lose it!)
  
  obesityDataSetNumeric <- obesityDataSetNumeric[,-targetIndex] # remove the target column that isn't at last column
  
  # Place the target feature as a new column. So, it appears at last column.
  obesityDataSetNumeric[targetFeature] <- tempdata
  
  # ------------->  There is big differences among numeric values of some columns.
  # Apply MIN-MAX (RANGE) NORMALIZATION. As a result, every numeric value presents in [0,1] range
  
  lastIndex <- ncol(obesityDataSetNumeric) # get the lastIndex that is the target feature exists
  lastColumn <- obesityDataSetNumeric[,lastIndex] # get the target feature's values
  
  # Function for MIN-MAX (RANGE) NORMALIZATION
  min_max_norm  <- function(x){
    (x- min(x)) /(max(x)-min(x))
  }
  
  # Normalize the values in columns except the target feature column (last column in the data frame)
  obesityDataSetNumeric <- as.data.frame(lapply(obesityDataSetNumeric[1:lastIndex-1], min_max_norm))
  
  # Place the last column (target feature column) back
  obesityDataSetNumeric[targetFeature] <- lastColumn
  
  # *** THE DATASET IS FULL NUMERIC NOW. IT CAN BE USED IN MACHINE LEARNING MODELS THAT REQUIRES NUMERICAL VALUES ***
  
  return(obesityDataSetNumeric)
}

# ---------------------------------- END OF THE OBESITY DATA WITH NUMERICAL VALUES ----------------------------------


# ---------------------------------- OBESITY DATA WITH CATEGORICAL VALUES ----------------------------------

convertToCategorical <- function(obesityDataSetCategoric){
  beforeDiscretization <- obesityDataSetCategoric
  
  # Install necessary packages
  # install.packages('arules')
  
  # Import necessary libraries
  library('arules')
  
  # -------------------- NOTES --------------------
  # 1-) discretizeDF function applies discretization to each numeric column.
  # The special method "none" can be specified to suppress discretization for a column.
  # 2-) Used resources during discretization of weight and height: 
  # https://en.wikipedia.org/wiki/Human_body_weight
  # https://tr.wikipedia.org/wiki/%C3%9Clkeye_g%C3%B6re_ortalama_insan_boyu
  # -------------------- NOTES --------------------
  
  # average Weight values for Male and Female that is provided in Internet
  bmi_calculator  <- function(height, weight){
    (weight / (height*height))
  }
  
  # calculate BMI values for each record and place it into Weight column for future use (discretization)
  for (recordIndex in 1:nrow(beforeDiscretization)) {
    bmiValue <- bmi_calculator(beforeDiscretization[recordIndex, 'Height'], beforeDiscretization[recordIndex, 'Weight'])
    beforeDiscretization[recordIndex, 'Weight'] <- bmiValue
  }
  
  # -------------> Basic discretization operations
  afterDiscretization <- discretizeDF(beforeDiscretization, methods = list(
    Age = list(method = "fixed", breaks = c(-Inf,25,50,+Inf), 
               labels = c("Young", "Middle-aged", "Old")),
    FCVC = list(method = "interval", breaks = 3, 
                labels = c("Rarely", "Sometimes","Usually")),
    NCP = list(method = "interval", breaks = 4, 
               labels = c("One", "Two","Three","Four")),
    CH2O = list(method = "interval", breaks = 3, 
                labels = c("Rarely", "Sometimes","Usually")),
    FAF = list(method = "interval", breaks = 4, 
               labels = c("Never", "Rarely","Sometimes","Usually")),
    TUE = list(method = "interval", breaks = 3, 
               labels = c("Rarely", "Sometimes","Usually"))
  ),default = list(method = "none"))
  
  # -------------> Discretization for Height column based on genders
  # average Height values for Male and Female that is provided in Internet
  averageHeightMale <- 1.724
  
  averageHeightFemale <- 1.589
  
  # limit variability of Height values for Male and Female that is provided in Internet
  limitHeightMale <- 0.0597
  
  limitHeightFemale <- 0.0683
  
  # discretization of Height column based on genders
  discHeightMale<-discretizeDF(afterDiscretization[afterDiscretization$Gender=="Male",],methods=list(
    Height = list(method = "fixed", breaks = c(-Inf, averageHeightMale - limitHeightMale, averageHeightMale + limitHeightMale,+Inf), 
                  labels = c("Short", "Medium-height", "Tall"))
  ),default = list(method = "none"))
  
  discHeightFemale<-discretizeDF(afterDiscretization[afterDiscretization$Gender=="Female",],methods=list(
    Height = list(method = "fixed", breaks = c(-Inf, averageHeightFemale - limitHeightFemale, averageHeightFemale + limitHeightFemale,+Inf), 
                  labels = c("Short", "Medium-height", "Tall"))
  ),default = list(method = "none"))
  
  afterDiscretization <- rbind(discHeightFemale,discHeightMale)
  
  # -------------> Discretization for Weight column (BMI ranges are used)
  discWeight <-discretizeDF(afterDiscretization,methods=list(
    Weight = list(method = "fixed", breaks = c(-Inf, 18.5, 25, 30, +Inf), 
                  labels = c("Thin", "Healthy","Over-weighted", "Obese"))
  ),default = list(method = "none"))
  
  afterDiscretization <- discWeight
  
  # Discretization operations are done, all features are categoric now.
  obesityDataSetCategoric <- afterDiscretization
  
  # *** THE DATASET IS FULL CATEGORICAL NOW. IT CAN BE USED IN MACHINE LEARNING MODELS THAT REQUIRES CATEGORICAL VALUES ***
  
  return(obesityDataSetCategoric)
}

# ---------------------------------- END OF THE OBESITY DATA WITH CATEGORICAL VALUES ----------------------------------

# ---------------------------------------- BUILDING MODELS AND EVALUATIONS OF THEM ----------------------------------------

# -------------------- NOTES --------------------
# 1-) We used Random Forest (together with full categorical version of dataset), KNN and Logistic Regression (together with   
# full numeric version of dataset). We evaluated the models in terms of accuracy, precision and recall.
# 2-) When we researched we found that random forest gives the best result with categorical
# values (when working with a data set like ours in terms of number of records and features).
# -------------------- NOTES --------------------

# Install necessary packages
# install.packages("caret")
# install.packages("class")
# install.packages("randomForest")
# install.packages("nnet")

# Import necessary libraries
library(caret) # for Confusion Matrix
library(class) # for knn() function
library(randomForest)  # for Random Forest
library(nnet) # for Multinomial Logistic Regression 

# Vectors to hold accuracy values for each model
rf_acc <- numeric()
knn_acc <- numeric()
lr_acc <- numeric()

# Vectors to hold precision values for each model
rf_prec <- numeric()
knn_prec <- numeric()
lr_prec <- numeric()

# Vectors to hold recall values for each model
rf_reca <- numeric()
knn_reca <- numeric()
lr_reca <- numeric()

set.seed(1815850)

# Convert raw dataset (BASEDATASET) into numerical and categorical versions
obesityDataSetNumeric <- convertToNumerical(BASEDATASET)
obesityDataSetCategoric <- convertToCategorical(BASEDATASET)

# Order the numerical and categorical versions by row names to provide consistency between each other in the future
obesityDataSetNumeric <- obesityDataSetNumeric[ order(as.numeric(row.names(obesityDataSetNumeric))), ]
obesityDataSetCategoric <- obesityDataSetCategoric[ order(as.numeric(row.names(obesityDataSetCategoric))), ]

# Exclude the target column when giving datasets in knn() function. Otherwise, it gives an error.
targetFeature <- "NObeyesdad" # variable to hold target feature's name
targetFeatureIndex <- which(colnames(obesityDataSetNumeric) == targetFeature) # get the index of target feature

# Function to calculate Precision (after confusionMatrix() function is used)
precisionCalculator<-function(cm){
  precisionValues<-cm[["byClass"]][ , "Precision"]
  precision<-sum(precisionValues)/length(precisionValues)
  return(precision)
}

# Function to calculate Recall (after confusionMatrix() function is used)
recallCalculator<- function(cm){
  recallValues<-cm[["byClass"]][ , "Recall"]
  recall<-sum(recallValues)/length(recallValues)
  return(recall)
}

# Cross-Validation (20 iterations)

for(i in 1:20){
  sub <- sample(1:nrow(BASEDATASET), size=nrow(BASEDATASET)*0.75) # use 75% of samples for training
  
  # Prepare the training subsets
  obesityDataSetCategoricTrain <- obesityDataSetCategoric[sub,]
  obesityDataSetNumericTrain <- obesityDataSetNumeric[sub,]
  
  # Prepare the testing subsets
  obesityDataSetCategoricTest <- obesityDataSetCategoric[-sub,]
  obesityDataSetNumericTest <- obesityDataSetNumeric[-sub,]
  
  # -------------> Random Forest
  
  # Build the model and make prediction
  rForest <- randomForest(NObeyesdad ~ ., data = obesityDataSetCategoricTrain, proximity = TRUE,ntree=500)
  
  predict2 <- predict(rForest, obesityDataSetCategoricTest, type = "class")
  
  predictTable <- table(predict2,obesityDataSetCategoricTest[, "NObeyesdad"])
  
  # Accuracy
  accuracy <- sum(diag(predictTable)) / sum(predictTable)
  rf_acc <- c(rf_acc, accuracy)
  
  # Build the confusion Matrix
  cm<-confusionMatrix(predict2,obesityDataSetCategoricTest$NObeyesdad)
  
  # Precision
  precision <- precisionCalculator(cm)
  rf_prec <- c(rf_prec, precision)
  
  # Recall
  recall <- recallCalculator(cm)
  rf_reca <- c(rf_reca, recall)
  
  
  # -------------> KNN
  
  classifier_knn <- knn(train = obesityDataSetNumericTrain[,-targetFeatureIndex], test = obesityDataSetNumericTest[,-targetFeatureIndex], 
                        cl=obesityDataSetNumericTrain$NObeyesdad, 
                        k = 5)
  
  confMatrix <- table(obesityDataSetNumericTest$NObeyesdad, classifier_knn)
  
  # Accuracy
  accuracy <- sum(diag(confMatrix)) / sum(confMatrix)
  knn_acc <- c(knn_acc, accuracy)
  
  # Precision and Recall calculation from table
  prec_vector<-numeric()
  recall_vector<-numeric()
  
  for(i in 1:nrow(confMatrix)){
    precision <-confMatrix[i,i]/sum(confMatrix[,i])
    recall <- confMatrix[i,i]/sum(confMatrix[i,])
    prec_vector<-c(prec_vector,precision)
    recall_vector<-c(recall_vector,recall)
  }
  
  knn_prec <- c(knn_prec,mean(prec_vector))
  knn_reca <- c(knn_reca,mean(recall_vector))
  
  # -------------> Multinomial Logistic Regression
  
  # Build the model and make prediction
  logRegres <- multinom(NObeyesdad~., data=obesityDataSetNumericTrain,maxit=100)
  
  predict3 <- predict(logRegres, obesityDataSetNumericTest, type = "class")
  
  predictTable3 <- table(predict3,obesityDataSetNumericTest[, "NObeyesdad"])
  
  # Accuracy
  accuracy <- sum(diag(predictTable3)) / sum(predictTable3)
  lr_acc <- c(lr_acc, accuracy)
  
  # Build the confusion Matrix
  cm <- confusionMatrix(predict3,obesityDataSetNumericTest$NObeyesdad)
  
  # Precision
  precision <- precisionCalculator(cm)
  lr_prec <- c(lr_prec, precision)
  
  # Recall
  recall <- recallCalculator(cm)
  lr_reca <- c(lr_reca, recall)
  
}

# -------------> GIVING THE FINAL RESULTS FOR EACH MODEL

print("***** RANDOM FOREST *****")
print(paste("Average accuracy of RF after 20 iterations", mean(rf_acc)))
print(paste("Average precision of RF after 20 iterations", mean(rf_prec)))
print(paste("Average recall of RF after 20 iterations", mean(rf_reca)))

plot(rf_acc, type="l", ylab="Accuracy Rate", xlab="Iterations", main="Accuracy Rate for Random Forest With Different Subsets of obesityDataSet")
sprintf("Highest accuracy is %f (in the %.0fth iteration)", max(rf_acc), which.max(rf_acc))

plot(rf_prec, type="l", ylab="Precision Rate", xlab="Iterations", main="Precision Rate for Random Forest With Different Subsets of obesityDataSet")
sprintf("Highest precision is %f (in the %.0fth iteration)", max(rf_prec), which.max(rf_prec))

plot(rf_reca, type="l", ylab="Recall Rate", xlab="Iterations", main="Recall Rate for Random Forest With Different Subsets of obesityDataSet")
sprintf("Highest recall is %f (in the %.0fth iteration)", max(rf_reca), which.max(rf_reca))


print("***** KNN *****")
print(paste("Average accuracy of KNN after 20 iterations", mean(knn_acc)))
print(paste("Average precision of KNN after 20 iterations", mean(knn_prec)))
print(paste("Average recall of KNN after 20 iterations", mean(knn_reca)))

plot(knn_acc, type="l", ylab="Accuracy Rate", xlab="Iterations", main="Accuracy Rate for KNN With Different Subsets of obesityDataSet")
sprintf("Highest accuracy is %f (in the %.0fth iteration)", max(knn_acc), which.max(knn_acc))

plot(knn_prec, type="l", ylab="Precision Rate", xlab="Iterations", main="Precision Rate for KNN With Different Subsets of obesityDataSet")
sprintf("Highest precision is %f (in the %.0fth iteration)", max(knn_prec), which.max(knn_prec))

plot(knn_reca, type="l", ylab="Recall Rate", xlab="Iterations", main="Recall Rate for KNN With Different Subsets of obesityDataSet")
sprintf("Highest recall is %f (in the %.0fth iteration)", max(knn_reca), which.max(knn_reca))


print("***** MULTINOMIAL LOGISTIC REGRESSION *****")
print(paste("Average accuracy of LR after 20 iterations", mean(lr_acc)))
print(paste("Average precision of LR after 20 iterations", mean(lr_prec)))
print(paste("Average recall of LR after 20 iterations", mean(lr_reca)))

plot(lr_acc, type="l", ylab="Accuracy Rate", xlab="Iterations", main="Accuracy Rate for Logistic Regression With Different Subsets of obesityDataSet")
sprintf("Highest accuracy is %f (in the %.0fth iteration)", max(lr_acc), which.max(lr_acc))

plot(lr_prec, type="l", ylab="Precision Rate", xlab="Iterations", main="Precision Rate for Logistic Regression With Different Subsets of obesityDataSet")
sprintf("Highest precision is %f (in the %.0fth iteration)", max(lr_prec), which.max(lr_prec))

plot(lr_reca, type="l", ylab="Recall Rate", xlab="Iterations", main="Recall Rate for Logistic Regression With Different Subsets of obesityDataSet")
sprintf("Highest recall is %f (in the %.0fth iteration)", max(lr_reca), which.max(lr_reca))

# ------------------------------------- END OF BUILDING MODELS AND EVALUATIONS OF THEM ------------------------------------

# DEVELOPED BY EKIN UZUNBAZ, YUSUF ORHAN OKKABAS, 2022

