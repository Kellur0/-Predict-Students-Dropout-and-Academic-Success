rm(list=ls()); gc()
library(dplyr)
setwd("/home/mcj/Documents/Edu/ISDS 574/Group Proj")
# Load the data
load(file="df_dropOut_clean.rda")
str(df_dropOut_clean)

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
      all_of(varCont),
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

#Convert target variable to a factor versus integer. The model is interpreting it as a continuous variable and can't do cross validation.
df_dropOut_clean$Target_Dropout <- as.factor(df_dropOut_clean$Target_Dropout)
# Check levels to confirm conversion
cat("Levels-Target_Dropout:\n")
print(levels(df_dropOut_clean$Target_Dropout))

#All variables are important for analysis in terms of domain knowledge. Target_Dropout will be used as the binary predictor.
#Variables removed will be based off of evaluation.

#Step 1: take 60% of data randomly as training
set.seed(1) # set a seed so that people get the same 60% next time they run the same code
id.train = sample(1:nrow(df_dropOut_clean), nrow(df_dropOut_clean)*.6) # ncol() gives number of columns
id.test = setdiff(1:nrow(df_dropOut_clean), id.train) # setdiff gives the set difference
df_dropOut_clean.train = df_dropOut_clean[id.train,]
df_dropOut_clean.test = df_dropOut_clean[id.test,]

#No PCA. It causes multicollinearity issues.

min.model <- glm(Target_Dropout ~ 1, data = df_dropOut_clean.train, family = 'binomial')
max.model <- glm(Target_Dropout ~ ., data = df_dropOut_clean.train, family = 'binomial')
max.formula <- formula(max.model)

## Run models Forward, Backward, and Stepwise
# Forward selection
forward.model <- step(min.model, direction = 'forward', scope = max.formula)
summary(forward.model)

# Backward selection
backward.model <- step(max.model, direction = 'backward')
summary(backward.model)

# Stepwise selection
stepwise.model <- step(min.model, direction = 'both', scope = max.formula)
summary(stepwise.model)

#List of models stored in a variable. This will be referenced later
models <- list(forward = forward.model, backward = backward.model, stepwise = stepwise.model)

#Summarize all models
for (i in names(models)) {
  cat("\n Summary: ",i,"\n")
  print(summary(models[[i]]))
}

# Check Variance Inflation Factor (VIF) for multicollinearity
library(car)
vif(forward.model)  # Replace with the appropriate model
vif(backward.model)  # Replace with the appropriate model
vif(stepwise.model)  # Replace with the appropriate model

# Function to calculate and remove high VIF variables. This are extremely high VIF when reviewed, used function for systematic approach
#rather than manual approach. The results for reducing would have been the same.
del_highVif <- function(model, threshold = 5) {
  while (TRUE) {
    # Calculate VIFs
    vif_values <- vif(model)
    
    # Identify the predictor with the highest VIF
    max_vif <- max(vif_values)
    
    # Check if it exceeds the threshold
    if (max_vif < threshold) {
      break
    }
    
    # Find the variable with the highest VIF and remove it
    max_vif_var <- names(which.max(vif_values))
    cat("Removing variable with high VIF:", max_vif_var, "with VIF =", max_vif, "\n")
    
    # Update the formula of the model by removing the variable with the highest VIF
    formula_updated <- as.formula(
      paste(". ~ . -", max_vif_var)
    )
    model <- update(model, formula = formula_updated)
  }
  
  return(model)
}

#Apply VIF removal function to all models
del_highVif(forward.model)
del_highVif(backward.model)
del_highVif(stepwise.model)

#The function falculates odds ratio with confidence intervals.
get.or = function(sobj, alpha=.05) {
  b = sobj$coef[-1, 'Estimate']
  se.b = sobj$coef[-1, 'Std. Error']
  pval = sobj$coef[-1, 'Pr(>|z|)']
  or = exp(b); se.or = exp(b)*se.b
  lb = b + qnorm(alpha/2)*se.b; lb.or = exp(lb)
  ub = b + qnorm(1-alpha/2)*se.b; ub.or = exp(ub)
  out = cbind(or, se.or, lb.or, ub.or, pval)
  colnames(out) = c('OR', 'SE', paste0((1-alpha)*100, '% CI, lower'),
                    paste0((1-alpha)*100, '% CI, upper'), 'p value')
  return(out)
}

#Calculate odds ratio for each model
get.or(summary(forward.model))
get.or(summary(backward.model))
get.or(summary(stepwise.model))

# Predict using forward.model
yhat_forward = predict(forward.model, newdata = df_dropOut_clean.test, type = 'response')
# Visualize predictions
hist(yhat_forward, main = "Forward Model", xlab = "Predicted Probability", col = "red")

# Predict using backward.model
yhat_backward = predict(backward.model, newdata = df_dropOut_clean.test, type = 'response')
# Visualize predictions
hist(yhat_backward, main = "Backward Model", xlab = "Predicted Probability", col = "white")

# Predict using stepwise.model
yhat_stepwise = predict(stepwise.model, newdata = df_dropOut_clean.test, type = 'response')
# Visualize predictions
hist(yhat_stepwise, main = "Stepwise Model", xlab = "Predicted Probability", col = "blue")

# Create functions to dichotomize and calculate sensitivity and specificity
dichotomize = function(yhat, cutoff=.5) {
  out = rep(0, length(yhat))
  out[yhat > cutoff] = 1
  out
}

sen = function(ytrue, yhat) {
  ind.true1 = which(ytrue == 1)
  mean( ytrue[ind.true1] == yhat[ind.true1] )
}

spe = function(ytrue, yhat) {
  ind.true0 = which(ytrue == 0)
  mean( ytrue[ind.true0] == yhat[ind.true0] )
}

# Libraries
library(pROC)
library(caret)

# List of models
models <- list(forward = forward.model, backward = backward.model, stepwise = stepwise.model)

# Define cutoff values
cutoff_values <- c(0.5, 0.4, 0.3, 0.2, 0.1)

# Initialize storage for metrics
model_metrics <- list()

# Loop through each model and loop through a nested loop for cutoffs. Print the metrics for all.
for (i in names(models)) {
  model <- models[[i]]
  cat("\nEvaluating Model:", i, "\n")
  
  # Predict probabilities on test set
  yhat_probs <- predict(model, newdata = df_dropOut_clean.test, type = "response")
  
  # Initialize storage for metrics at different cutoffs
  cutoff_metrics <- list()
  
  # Loop through each cutoff value
  for (cutoff in cutoff_values) {
    # Dichotomize predictions
    yhat_class <- dichotomize(yhat_probs, cutoff = cutoff)
    
    # Confusion matrix
    cm <- confusionMatrix(as.factor(yhat_class), df_dropOut_clean.test$Target_Dropout, positive = "1")
    
    # Calculate misclassification error rate
    error_rate <- 1 - cm$overall["Accuracy"]
    
    # Calculate ROC and AUC (only once, as AUC is cutoff-independent)
    if (cutoff == 0.5) {
      roc_curve <- roc(df_dropOut_clean.test$Target_Dropout, yhat_probs)
      auc_value <- auc(roc_curve)
    }
    
    # Store metrics for this cutoff
    cutoff_metrics[[paste0("Cutoff_", cutoff)]] <- list(
      Accuracy = cm$overall["Accuracy"],
      MisclassificationErrorRate = error_rate,
      Kappa = cm$overall["Kappa"],
      Sensitivity = cm$byClass["Sensitivity"],
      Specificity = cm$byClass["Specificity"],
      Precision = cm$byClass["Precision"],
      F1 = cm$byClass["F1"],
      AUC = ifelse(cutoff == 0.5, auc_value, NA)  # AUC only once
    )
    
    # Print metrics for this cutoff
    cat("\nCutoff:", cutoff, "\n")
    cat("Accuracy:", cm$overall["Accuracy"], "\n")
    cat("Misclassification Error Rate:", error_rate, "\n")
    cat("Kappa:", cm$overall["Kappa"], "\n")
    cat("Sensitivity:", cm$byClass["Sensitivity"], "\n")
    cat("Specificity:", cm$byClass["Specificity"], "\n")
    cat("Precision:", cm$byClass["Precision"], "\n")
    cat("F1-Score:", cm$byClass["F1"], "\n")
    if (cutoff == 0.5) cat("AUC:", auc_value, "\n")
    
    # Print confusion matrix
    cat("Confusion Matrix:\n")
    print(cm$table)
  }
  
  # Store metrics for this model
  model_metrics[[i]] <- cutoff_metrics
  
  # Plot ROC Curve for the model (only once at cutoff 0.5)
  if (exists("roc_curve")) {
    plot(roc_curve, main = paste("ROC Curve -", i), col = "blue", lwd = 2)
  }
  cat("\n", rep("-", 30), "\n")
}

