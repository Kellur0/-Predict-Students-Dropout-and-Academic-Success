rm(list=ls()); gc()
library(dplyr)
library(rpart)
library(rpart.plot)
library(caret)
#Note needed, SMOTE did not improve the model.
#library(smotefamily)

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
  #Convert target variable to a factor versus integer. The model is interpreting it as a continuous variable and can't do cross validation.
  df_dropOut_clean$Target_Dropout <- as.factor(df_dropOut_clean$Target_Dropout)
  # Check levels to confirm conversion
  cat("Levels-Target_Dropout:\n")
  print(levels(df_dropOut_clean$Target_Dropout))
  
  rm(col,varCont); gc()  
}

#Split the data into training and testing sets
set.seed(1)
id.train <- sample(1:nrow(df_dropOut_clean), nrow(df_dropOut_clean) * 0.6)
id.test <- setdiff(1:nrow(df_dropOut_clean), id.train)
df_dropOut_clean.train <- df_dropOut_clean[id.train, ]
df_dropOut_clean.test <- df_dropOut_clean[id.test, ]


## NOTE: This did not improve the model. The whole code block below starting and ending with the bracket can
##       be removed and model results will still be the same. 
#Smote
{
# # Separate predictors and target
# predictors <- df_dropOut_clean.train[, setdiff(names(df_dropOut_clean.train), "Target_Dropout")]
# target <- df_dropOut_clean.train$Target_Dropout
# 
# # Convert target to numeric (required by smotefamily)
# target_numeric <- as.numeric(as.character(target))
# 
# # Apply SMOTE
# smote_result <- SMOTE(X = predictors, target = target_numeric, K = 5, dup_size = 2)
# 
# # Recombine the balanced data
# balanced_data <- data.frame(smote_result$data)
# names(balanced_data) <- c(names(predictors), "Target_Dropout")
# balanced_data$Target_Dropout <- factor(balanced_data$Target_Dropout, levels = levels(df_dropOut_clean.train$Target_Dropout))
# 
# # Check class distribution after SMOTE
# cat("Class distribution after SMOTE:\n")
# print(table(balanced_data$Target_Dropout))
}


# Classification Tree with rpart
fit <- rpart(Target_Dropout ~ ., method = "class", data = df_dropOut_clean.train, minsplit = 5) #change data = to df_dropOut_clean.train if not applying SMOTE
# Identify nonsignificant predictors from variable importance

# Plot the initial tree
rpart.plot(fit, main = "Initial CART Model")

# Attempt Address class imbalance further by filtering by importance.
## NOTE: This made the model worse. Do not run.
{  
  # #Calculate Variable Importance
  # importance <- varImp(fit)
  # #Extract Row Names (Feature Names) and Column Values Separately. The varImp(fit) won't convert directly to a data frame.
  # feature_names <- rownames(importance)  # Extract row names as feature names
  # importance_values <- as.numeric(importance$Overall)  # Extract importance values
  # 
  # #Combine into a Data Frame. The output for varImp(fit) doesn't easily fit so you need to extract both columns separately and then append.
  # importance_df <- data.frame(
  #   Feature = feature_names,
  #   Importance = importance_values,
  #   stringsAsFactors = FALSE  # Ensure character column remains as-is
  # )
  # 
  # # Inspect the Data Frame. Make sure both columns have a match and are not null.
  # print(head(importance_df))  # Check the structure of the resulting data frame
  # 
  # #Filter Based on Threshold
  # importance_threshold <- 5  # Set a threshold for importance
  # filtered_features <- importance_df %>%
  #   filter(Importance > importance_threshold) %>%
  #   pull(Feature)
  # 
  # # Add the Target Variable back to the list of variables to KEEP
  # filtered_features <- c(filtered_features, "Target_Dropout")
  # 
  # #Subset the Dataset based off of filtered importance
  # df_dropOut_clean.train <- df_dropOut_clean.train[, filtered_features, drop = FALSE]
  # df_dropOut_clean.test <- df_dropOut_clean.test[, filtered_features, drop = FALSE]
  # 
  # #Verify Subset Columns. They should both be the same.
  # print(colnames(df_dropOut_clean.train))
  # print(colnames(df_dropOut_clean.test))
  # 
  # # Verify class balance
  # table(df_dropOut_clean.train$Class)  # New "Class" column replaces Target_Dropout
  # 
  # #Refit the model
  # fit <- rpart(Target_Dropout ~ ., method = "class", data = df_dropOut_clean.train, minsplit = 5)
  # rpart.plot(fit, main = "Refitted CART Model")
  # fit <- rpart(
  #   Target_Dropout ~ ., 
  #   data = df_dropOut_clean.train, 
  #   method = "class", 
  #   parms = list(loss = matrix(c(0, 1, 5, 0), 2, 2)))

}



# Minimum Error and Best Pruned Tree
{
  pfit.me <- prune(fit, cp = fit$cptable[which.min(fit$cptable[, "xerror"]), "CP"])
  rpart.plot(pfit.me, main = "Minimum Error Tree")
  
  ind <- which.min(fit$cptable[, "xerror"])
  se1 <- fit$cptable[ind, "xstd"] / sqrt(nrow(df_dropOut_clean.train))
  xer1 <- min(fit$cptable[, "xerror"]) + fit$cptable[ind, "xstd"]
  ind0 <- which.min(abs(fit$cptable[1:ind, "xerror"] - xer1))
  pfit.bp <- prune(fit, cp = fit$cptable[ind0, "CP"])
  rpart.plot(pfit.bp, main = "Best Pruned Tree")
}

# Create models list
models <- list("Minimum Error Tree" = pfit.me, "Best Pruned Tree" = pfit.bp)

# Define cutoff values
cutoff_values <- c(0.5, 0.4, 0.3, 0.2, 0.1)
all_metrics <- list()

# Evaluate models at multiple cutoffs
for (model_name in names(models)) {
  model <- models[[model_name]]
  cat("\nEvaluating Model:", model_name, "\n")
  
  yhat_probs <- predict(model, newdata = df_dropOut_clean.test, type = "prob")[, 2]
  
  # Initialize metrics data frame
  model_metrics <- data.frame(
    Cutoff = numeric(),
    Accuracy = numeric(),
    MisclassificationErrorRate = numeric(),
    Sensitivity = numeric(),
    Specificity = numeric(),
    Precision = numeric(),
    F1 = numeric()
  )
  
  for (cutoff in cutoff_values) {
    # Dichotomize predictions
    yhat_class <- as.factor(as.numeric(yhat_probs > cutoff))
    
    # Align factor levels with the target variable
    yhat_class <- factor(yhat_class, levels = levels(df_dropOut_clean.test$Target_Dropout))
    
    # Generate confusion matrix
    cm <- confusionMatrix(yhat_class, df_dropOut_clean.test$Target_Dropout, positive = "1")
    
    # Calculate metrics
    error_rate <- 1 - cm$overall["Accuracy"]
    acc <- cm$overall["Accuracy"]
    sens <- cm$byClass["Sensitivity"]
    spec <- cm$byClass["Specificity"]
    precision <- cm$byClass["Precision"]
    f1 <- cm$byClass["F1"]
    
    # Append metrics
    model_metrics <- rbind(
      model_metrics,
      data.frame(
        Cutoff = cutoff,
        Accuracy = round(acc, 4),
        MisclassificationErrorRate = round(error_rate, 4),
        Sensitivity = round(sens, 4),
        Specificity = round(spec, 4),
        Precision = round(precision, 4),
        F1 = round(f1, 4)
      )
    )
  }
  
  # Store metrics for this model
  all_metrics[[model_name]] <- model_metrics
  
  # Print metrics table for this model
  cat("\nMetrics for", model_name, ":\n")
  print(model_metrics)
  cat("\n", rep("-", 30), "\n")
  print(cm)
  library(pROC)
  yhat_probs <- predict(fit, newdata = df_dropOut_clean.test, type = "prob")[, 2]
  roc_curve <- roc(df_dropOut_clean.test$Target_Dropout, yhat_probs)
  plot(roc_curve, main = "ROC Curve")
  cat("AUC:", auc(roc_curve), "\n")
  
}


