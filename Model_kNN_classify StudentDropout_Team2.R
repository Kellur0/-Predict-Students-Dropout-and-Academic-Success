rm(list=ls()); gc()

setwd("/home/mcj/Documents/Edu/ISDS 574/Group Proj")
# Load the data
load(file="df_dropOut_clean.rda")
names(df_dropOut_clean)

#Standardize and validate continuous (predictor) variables
{
  #Assign continuous variable names to a list
  varCont <-  c("Admission.grade",
                "Previous.qualification..grade.",
                "Curricular.units.1st.sem..grade.",
                "Curricular.units.2nd.sem..grade.",
                "Unemployment.rate",
                "Inflation.rate",
                "GDP")
  
  # Standardize continuous variables
  df_dropOut_clean <- df_dropOut_clean %>%
    mutate(across(
      varCont,
      scale
    ))

  # Validate the standardization---Check the mean and standard deviation of the scaled columns
  for (col in varCont) {
    cat("Validate Stanardization for:", col, ":\n")
    cat("Mean:", mean(df_dropOut_clean[[col]]), "---NOTE Should be approximately 0 \n")
    cat("SD:", sd(df_dropOut_clean[[col]]), "---NOTE Should be approximately 1 \n\n")
  }
  rm(col,varCont); gc()  
}


#Train-Test Split. Split the dataset into 75% training and 25% testing subsets. 
set.seed(1)                                        #Random seed of 1 is set for reproducability
n.train <- floor( nrow(df_dropOut_clean)*0.75 )          #Calculate training size
ind.train <- sample(1:nrow(df_dropOut_clean), n.train)   #Randomly sample training indices
ind.test <- setdiff(1:nrow(df_dropOut_clean), ind.train) #Use the remaining indices for testing

#Prepare Training and Testing Data. Exclude the outcome variable "Target_Dropout". 
require(class)
Xtrain <- df_dropOut_clean[ind.train, !(names(df_dropOut_clean) %in% "Target_Dropout")]
Xtest <- df_dropOut_clean[ind.test, !(names(df_dropOut_clean) %in% "Target_Dropout")]
ytrain <- df_dropOut_clean[ind.train,"Target_Dropout"]

#Fit thekNN model
ypred = knn(Xtrain, Xtest, ytrain, k=round(sqrt(n.train)), prob=T)
table(ypred)

#Evaluate predictions. Compare the predicted values with the actual values from the test set.
ytest = df_dropOut_clean[ind.test, "Target_Dropout"]
table(ytest, ypred)

#Extract precition probabilities
#Define a function to extract probabilities from the kNN output
get.prob = function(x) {
  prob = attr(x, "prob")
  ind = which(x == 0)
  prob[ind] = 1 - prob[ind]
  return(prob)
}

#Define function to find the best k (optimize by finding the lowest value k).
knn.bestK = function(train, test, y.train, y.test, k.grid = 1:20, ct = .5) {
  # browser()
  fun.tmp = function(x) {
    y.tmp = knn(train, test, y.train, k = x, prob=T) # run knn for each k in k.grid
    prob = get.prob(y.tmp)
    y.hat = as.numeric( prob > ct )
    return( sum(y.hat != as.numeric(y.test)) )
  }
  ## create a temporary function (fun.tmp) that we want to apply to each value in k.grid
  error = unlist(lapply(k.grid, fun.tmp))
  names(error) = paste0("k=", k.grid)
  ## it will return a list so I need to unlist it to make it to be a vector
  out = list(k.optimal = k.grid[which.min(error)], 
             error.min = min(error)/length(y.test),
             error.all = error/length(y.test))
  return(out)
}

#Run optimization for k. Identify the optimal k using the training and testing data.
bestK_dropOUt = knn.bestK(Xtrain, Xtest, ytrain, ytest, seq(1, 18, 2), .5)
## Rerun kNN with the optimal k. Use the best k to make final predictions.
ypred1 <- knn(Xtrain, Xtest, ytrain, k = bestK_dropOUt$k.optimal, prob = TRUE)
table(ytest, ypred1)

#Evaluate model performance
#Define Functions for measures: Sensitivity, Specificity, FPR, and FNR
{
sen = function(ytest, ypred) {
  ind1 = which(ytest == 1)
  mean(ytest[ind1] == ypred[ind1])
}

spe = function(ytest, ypred) {
  ind1 = which(ytest == 0)
  mean(ytest[ind1] == ypred[ind1])
}

fpr = function(ytest, ypred) {
  ind1 = which(ytest == 0)
  mean(ytest[ind1] != ypred[ind1])
}

fnr = function(ytest, ypred) {
  ind1 = which(ytest == 1)
  mean(ytest[ind1] != ypred[ind1])
}

sen(ytest, ypred1)
spe(ytest, ypred1)
fpr(ytest, ypred1)
fnr(ytest, ypred1)
}

#Define function that reports on measures: Sensitivity, Specificity, FPR, and FNR
performance = function(ytest, ypred) {
  measures = c(mean(ytest == ypred),
               sen(ytest, ypred),
               spe(ytest, ypred),
               fpr(ytest, ypred),
               fnr(ytest, ypred))
  names(measures) = c("Accuracy", "Sensitivity", "Specificity", "FPR", "FNR")
  return(measures)
}

#Display performance summary
performance(ytest, ypred1)

# Evaluate and Print Metrics at Multiple Cutoffs
cutoff_values <- c(0.5, 0.4, 0.3, 0.2, 0.1)
metrics <- list()
prob <- get.prob(ypred1)
for (cutoff in cutoff_values) {
  # Make probabilities binary based off cutoff
  yhat_class <- as.factor(ifelse(prob > cutoff, 1, 0))
  
  # Calculate metrics
  acc <- mean(yhat_class == ytest)
  sens <- sen(ytest, yhat_class)
  spec <- spe(ytest, yhat_class)
  fpr_val <- fpr(ytest, yhat_class)
  fnr_val <- fnr(ytest, yhat_class)
  
  # Store metrics for this cutoff
  metrics[[paste0("Cutoff_", cutoff)]] <- c(Accuracy = acc,
                                            Sensitivity = sens,
                                            Specificity = spec,
                                            FPR = fpr_val,
                                            FNR = fnr_val)
  
  # Print metrics
  cat("\nCutoff:", cutoff, "\n")
  cat("Accuracy:", acc, "\n")
  cat("Sensitivity:", sens, "\n")
  cat("Specificity:", spec, "\n")
  cat("FPR:", fpr_val, "\n")
  cat("FNR:", fnr_val, "\n")
}

