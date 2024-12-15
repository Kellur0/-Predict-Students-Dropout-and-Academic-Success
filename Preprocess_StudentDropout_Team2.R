

rm(list=ls()); gc()
#data set link: https://archive.ics.uci.edu/dataset/697/predict+students+dropout+and+academic+success
###Integer descriptions can be found in this link.

setwd("/home/mcj/Documents/Edu/ISDS 574/Group Proj")
# Load the data
df_dropOut_raw <- read.csv("data.csv", head=TRUE, sep = ";")

### Visual analysis
{
  # Check dimensions and structure
  dim(df_dropOut_raw)
  str(df_dropOut_raw)
  
  # Display names and data types of each column in the data frame
  data_info <- data.frame(Column = names(df_dropOut_raw), Type = sapply(df_dropOut_raw, class))
  print(data_info) #To view the column names and data types
  
  
  # Check missing data
  matrix.na <- is.na(df_dropOut_raw)
  pmiss <- colMeans(matrix.na) # proportion of missing for each column
  nmiss <- rowMeans(matrix.na) # proportion of missing for each row
  plot(pmiss)
  plot(nmiss)
  # Count of missing values in the dataset
  sum(is.na(df_dropOut_raw))  #Should return 0, meaning there is no missing data
  
  ## simple heatmap of correlations (without values)
  heatmap(cor(matrix.na), Rowv = NA, Colv = NA) #the heatmap will be blank. If blank, R will return Warning messages in the console.
  
  
  # Check distributions of continuous variables (adjusting based on known columns in your dataset)
  par(mfrow = c(1, 2))
  hist(df_dropOut_raw$"Previous.qualification..grade.")
  boxplot(df_dropOut_raw$"Previous.qualification..grade.")
  
  par(mfrow = c(1, 2))
  hist(df_dropOut_raw$"Admission.grade")
  boxplot(df_dropOut_raw$"Admission.grade")
  
  par(mfrow = c(1, 2))
  hist(df_dropOut_raw$"Curricular.units.1st.sem..grade.")
  boxplot(df_dropOut_raw$"Curricular.units.1st.sem..grade.")
  
  par(mfrow = c(1, 2))
  hist(df_dropOut_raw$"Curricular.units.2nd.sem..grade.")
  boxplot(df_dropOut_raw$"Curricular.units.2nd.sem..grade.")
  
  par(mfrow = c(1, 2))
  hist(df_dropOut_raw$"Unemployment.rate")
  boxplot(df_dropOut_raw$"Unemployment.rate")
  
  par(mfrow = c(1, 2))
  hist(df_dropOut_raw$"Inflation.rate")
  boxplot(df_dropOut_raw$"Inflation.rate")
  
  par(mfrow = c(1, 2))
  hist(df_dropOut_raw$"GDP")
  boxplot(df_dropOut_raw$"GDP")
  
  # Additional continuous variables can be added as needed
}

### Separate Outliers from Data
{
  #Define IQR variables with column
  Q1 <- quantile(df_dropOut_raw$"Admission.grade", 0.25)
  Q3 <- quantile(df_dropOut_raw$"Admission.grade", 0.75)
  IQR_value <- IQR(df_dropOut_raw$"Admission.grade")
  
  # Define outlier boundaries
  ll <- Q1 - 1.5 * IQR_value
  ul <- Q3 + 1.5 * IQR_value
  
  # Filter out rows with outliers
  df_dropOut_processed <- df_dropOut_raw[df_dropOut_raw$"Admission.grade" >= ll & df_dropOut_raw$"Admission.grade" <= ul, ]
  # Rows with outliers
  df_dropOut_raw.Outliers <- df_dropOut_raw[df_dropOut_raw$"Admission.grade" < ll | df_dropOut_raw$"Admission.grade" > ul, ]
  dim(df_dropOut_processed)
}

### Frequency table for all categorical variables 
#Assign all categorical variable names to a variable
  {
  varCat <- c("Marital.status", 
              "Application.mode",
              "Application.order",
              "Course",
              "Daytime.evening.attendance.",
              "Previous.qualification",
              "Nacionality",
              "Mother.s.qualification",
              "Father.s.qualification",
              "Mother.s.occupation",
              "Father.s.occupation",
              "Displaced",
              "Educational.special.needs",
              "Debtor",
              "Tuition.fees.up.to.date",
              "Gender",
              "Scholarship.holder",
              "Age.at.enrollment",
              "International",
              "Curricular.units.1st.sem..credited.",
              "Curricular.units.1st.sem..enrolled.",
              "Curricular.units.1st.sem..evaluations.",
              "Curricular.units.1st.sem..approved.",
              "Curricular.units.1st.sem..without.evaluations.",
              "Curricular.units.2nd.sem..credited.",
              "Curricular.units.2nd.sem..enrolled.",
              "Curricular.units.2nd.sem..evaluations.",
              "Curricular.units.2nd.sem..approved.",
              "Curricular.units.2nd.sem..without.evaluations.",
              "Target")
} 
#Loop through all categorical variables listed in varCat
for (i in varCat) {
  cat(i,":")
  print(table(df_dropOut_processed[[i]], useNA = "ifany"))  #print outliers 
  cat("\n")
}

### Combine categorical variables and label
{
  library(dplyr)
    
  #Marital status to either 1=single or 2=married
  id = which(df_dropOut_processed$Marital.status %in% c(3, 4, 5, 6)) #1=single, 2=married, 3=widower, 4=divorced, 5=factounion, 6=legallyseparated
  df_dropOut_processed$Marital.status[id] = 1
  df_dropOut_processed$Marital.status <-factor(df_dropOut_processed$Marital.status,levels = c(1,2), labels = c(0,1)) #Use labels instead of integers
  table(df_dropOut_processed$Marital.status)
  sum(table(df_dropOut_processed$Marital.status))
  
  #Label Application.mode based on the categories
  df_dropOut_processed <- df_dropOut_processed %>%
    mutate(Application.mode = factor(
      Application.mode,
      levels = c(1, 17, 18, 2, 5, 10, 16, 26, 27, 42, 43, 51, 57, 7, 15, 39, 44, 53),
      labels = c(
        rep("General.Admission", 3),
        rep("Special.Quotas and Ordinances", 6),
        rep("Transfers and Changes", 4),
        rep("Adult and Non-Traditional.Admissions", 5)
      )
    ))
  table(df_dropOut_processed$Application.mode)
  sum(table(df_dropOut_processed$Application.mode))
  
  #Application.order group to smaller categories and label
  df_dropOut_processed <- df_dropOut_processed %>%
    mutate(
      Application.order = cut(
        Application.order,
        breaks = c(-Inf, 1, 3, Inf),  # Define the breaks for grouping
        labels = c("1st", "2nd&3rd", "4th+"),  # Group labels
        right = TRUE
      )
    )
  table(df_dropOut_processed$Application.order)
  sum(table(df_dropOut_processed$Application.order))
  
  # Label Course based on the categories 
  df_dropOut_processed <- df_dropOut_processed %>%
    mutate(Course = factor(
      Course,
      levels = c(
        33, 9119,                          # Technology&Engineering
        171, 9070,                         # Design&Multimedia
        9003, 9085, 9130,                  # Agriculture&Veterinary
        9147, 9670, 9991,                  # Management&Business
        8014, 9238, 9254,                  # Social.Sciences&Services
        9500, 9556,                        # Health.Sciences
        9773, 9853                         # Education&Communication
      ),
      labels = c(
        rep("Technology&Engineering", 2),
        rep("Design&Multimedia", 2),
        rep("Agriculture&Veterinary", 3),
        rep("Management&Business", 3),
        rep("Social.Sciences&Services", 3),
        rep("Health.Sciences", 2),
        rep("Education&Communication", 2)
      )
    ))
  table(df_dropOut_processed$Course)
  sum(table(df_dropOut_processed$Course))
  
  #Datime or evening attendance  0=evening 1 = daytime
  df_dropOut_processed$Daytime.evening.attendance. <-factor(df_dropOut_processed$Daytime.evening.attendance.,levels = c(0,1), labels = c(0,1)) #Use labels instead of integers
  table(df_dropOut_processed$Daytime.evening.attendance.)
  sum(table(df_dropOut_processed$Daytime.evening.attendance.))
  
  #Previous qualification replace integers with labels
  df_dropOut_processed <- df_dropOut_processed %>%
    mutate(Previous.qualification = factor(
      Previous.qualification,
      levels = c(
        19,38,           # Basic Education
        1,9,10,12,14,15, # Secondary Education
        2,3,4,5,6,39,     # Higher Education
        40,42,43         # Other
      ),
      labels = c(
        rep("Basic Education", 2),
        rep("Secondary Education", 6),
        rep("Higher Education", 6),
        rep("Other",3)
      )
    ))
  table(df_dropOut_processed$Previous.qualification)
  sum(table(df_dropOut_processed$Previous.qualification))
  
  #Nationality filter by infrequent and then replace integers with labels, 0=not Portuguese 1=Portuguese
  freqTblNacion <- table(df_dropOut_processed$Nacionality)
  freqTblNacionFilt <- as.numeric(names(freqTblNacion[freqTblNacion<1000]))
  id = which(df_dropOut_processed$Nacionality %in% freqTblNacionFilt)
  df_dropOut_processed$Nacionality[id] = 0
  df_dropOut_processed$Nacionality <-factor(df_dropOut_processed$Nacionality,levels = c(0,1), labels = c(0,1)) #Use labels instead of integers
  table(df_dropOut_processed$Nacionality)
  sum(table(df_dropOut_processed$Nacionality))
  
  #Mothers qualifications replace integers with labels
  df_dropOut_processed <- df_dropOut_processed %>%
    mutate(Mother.s.qualification = factor(
      Mother.s.qualification,
      levels = c(
        9,10,11,12,14,18,19,26,27,29,30,37,38, # Basic Education
        1,                                     # Secondary Education
        2,3,4,5,6,40,41,42,43,44,              # Higher Education
        22,39,34,35,36                         # Other 
      ),
      labels = c(
        rep("Basic Education", 13),
        rep("Secondary Education", 1),
        rep("Higher Education", 10),
        rep("Other Education",5)
      )
    ))
  table(df_dropOut_processed$Mother.s.qualification)
  sum(table(df_dropOut_processed$Mother.s.qualification))
  
  #Fathers qualifications replace integers with labels
  df_dropOut_processed <- df_dropOut_processed %>%
    mutate(Father.s.qualification = factor(
      Father.s.qualification,
      levels = c(
        9,10,11,12,13,14,18,19,20,25,26,27,29,30,37,38, # Basic Education
        1,   # Secondary Education
        2,3,4,5,6,40,41,42,43,44,   # Higher Education
        22,31,33,34,35,36,39        # Other
      ),
      labels = c(
        rep("Basic Education", 16),
        rep("Secondary Education", 1),
        rep("Higher Education", 10),
        rep("Other", 7)
      )
    ))
  table(df_dropOut_processed$Father.s.qualification)
  sum(table(df_dropOut_processed$Father.s.qualification))
  
  #Mother.s.occupation replace integers with labels
  df_dropOut_processed <- df_dropOut_processed %>%
    mutate(Mother.s.occupation = factor(
      Mother.s.occupation,
      levels = c(
        0, 90, 99,                              # Students and Unemployed
        1, 2, 122, 123, 125,                    # Managers and Professionals
        3, 131, 132, 134, 4, 141, 143, 144, 175, # Technicians and Clerical Workers
        5, 151, 152, 153,                       # Service and Sales Workers
        6, 7, 171, 173, 8,                      # Skilled Manual Workers
        9, 191, 192, 193, 194,                  # Unskilled Workers
        10                                      # Armed Forces
      ),
      labels = c(
        rep("Students & Unemployed", 3),
        rep("Managers & Professionals", 5),
        rep("Technicians & Clerical Workers", 9),
        rep("Service & Sales Workers", 4),
        rep("Skilled Manual Workers", 5),
        rep("Unskilled Workers", 5),
        "Armed Forces"
      )
    ))
  table(df_dropOut_processed$Mother.s.occupation)
  sum(table(df_dropOut_processed$Mother.s.occupation))
      
  #Father.s.occupation replace integers with labels
  df_dropOut_processed <- df_dropOut_processed %>%
    mutate(Father.s.occupation = factor(
      Father.s.occupation,
      levels = c(
        0, 90, 99,                                    # Students and Unemployed
        1, 2, 122, 123,112,114,121,122,124,           # Managers and Professionals
        3, 131, 132, 134, 4, 141, 143, 144,135,       # Technicians and Clerical Workers
        5, 151, 152, 153,154,                         # Service and Sales Workers
        6, 7, 171, 8,161,163,172,174,175,181,182,183, # Skilled Manual Workers
        9, 192, 193, 194,195,                         # Unskilled Workers
        10,101,102,103                                # Armed Forces
      ),
      labels = c(
        rep("Students & Unemployed", 3),
        rep("Managers & Professionals", 9),
        rep("Technicians & Clerical Workers", 9),
        rep("Service & Sales Workers", 5),
        rep("Skilled Manual Workers", 12),
        rep("Unskilled Workers", 5),
        rep("Armed Forces",4)
      )
    ))
  table(df_dropOut_processed$Father.s.occupation)
  sum(table(df_dropOut_processed$Father.s.occupation))
  
  # #Admission.grade group to smaller categories based on quantiles (standardize) and label
  # quantile_breaks_admission <- quantile(df_dropOut_processed$Admission.grade, probs = seq(0,1,by=0.25),na.rm=TRUE) #na.rm includes the rightmost values.
  # quantile_breaks_admission[1] <- min(df_dropOut_processed$Admission.grade, na.rm = TRUE) - 1  #The "-1" is to slightly lower than min so that the breaks picks up the minimum values
  # 
  # df_dropOut_processed <- df_dropOut_processed %>%
  #   mutate(
  #     Admission.grade = as.numeric(Admission.grade),
  #     Admission.grade = cut(
  #       Admission.grade,
  #       breaks = quantile_breaks_admission, 
  #       labels = c("Low", "Average", "Good", "Best"), 
  #       right = TRUE  # Include the upper bound
  #     )
  #   )
  # table(df_dropOut_processed$Admission.grade)
  # sum(table(df_dropOut_processed$Admission.grade))
  # 
  # #Previous.qualification..grade. group to smaller categories based on quantiles (standardize) and label
  # quantile_breaks_prevGrade <- quantile(df_dropOut_processed$Previous.qualification..grade., probs = seq(0,1,by=0.25),na.rm=TRUE) #na.rm includes the rightmost values.
  # quantile_breaks_prevGrade[1] <- min(df_dropOut_processed$Previous.qualification..grade., na.rm = TRUE) - 1  #The "-1" is to slightly lower than min so that the breaks picks up the minimum values
  # 
  # df_dropOut_processed <- df_dropOut_processed %>%
  #   mutate(
  #     Previous.qualification..grade. = as.numeric(Previous.qualification..grade.),
  #     Previous.qualification..grade. = cut(
  #       Previous.qualification..grade.,
  #       breaks = quantile_breaks_prevGrade,
  #       labels = c("Low", "Average", "Good", "Best"),
  #       right = TRUE  # Include the upper bound
  #     )
  #   )
  # table(df_dropOut_processed$Previous.qualification..grade.)
  # sum(table(df_dropOut_processed$Previous.qualification..grade.))
  
  
  #Age.at.enrollment group to smaller categories and label
  df_dropOut_processed <- df_dropOut_processed %>%
    mutate(
      Age.at.enrollment = cut(
        Age.at.enrollment,
        breaks = c(-Inf, 20, 25, 30, 40, Inf), 
        labels = c("--20", "21-25", "26-30", "31-40", "41+"),
        right = TRUE # Include the upper bound
      )
    )
  table(df_dropOut_processed$Age.at.enrollment)
  sum(table(df_dropOut_processed$Age.at.enrollment))
  
  #Curricular.units.1st.sem..credited group to smaller categories and label
  df_dropOut_processed <- df_dropOut_processed %>%
    mutate(
      Curricular.units.1st.sem..credited. = cut(
        Curricular.units.1st.sem..credited.,
        breaks = c(-Inf, 0, 5, 10, Inf),  # Define the breaks for grouping
        labels = c("0", "1-5", "6-10", "11+"),  # Labels for each group
        right = TRUE
      )
    )
  table(df_dropOut_processed$Curricular.units.1st.sem..credited.)
  sum(table(df_dropOut_processed$Curricular.units.1st.sem..credited.))
  
  #Curricular.units.1st.sem..enrolled. group to smaller categories and label
  df_dropOut_processed <- df_dropOut_processed %>%
    mutate(
      Curricular.units.1st.sem..enrolled. = cut(
        Curricular.units.1st.sem..enrolled.,
        breaks = c(-Inf, 0, 4, 7, 10, Inf),
        labels = c("0", "1-4", "5-7", "8-10", "11+"),
        right = TRUE
      )
    )
  table(df_dropOut_processed$Curricular.units.1st.sem..enrolled.)
  sum(table(df_dropOut_processed$Curricular.units.1st.sem..enrolled.))
  
  #Curricular.units.1st.sem..enrolled. group to smaller categories and label
  df_dropOut_processed <- df_dropOut_processed %>%
    mutate(
      Curricular.units.1st.sem..evaluations. = cut(
        Curricular.units.1st.sem..evaluations.,
        breaks = c(-Inf, 0, 5, 10, 15, Inf),  # Define the breaks for grouping
        labels = c("0", "1-5", "6-10", "11-15", "16+"),  # Range-based labels for each group
        right = TRUE
      )
    )
  table(df_dropOut_processed$Curricular.units.1st.sem..evaluations.)
  sum(table(df_dropOut_processed$Curricular.units.1st.sem..evaluations.))
  
  #Curricular.units.1st.sem..approved. group to smaller categories and label
  df_dropOut_processed <- df_dropOut_processed %>%
    mutate(
      Curricular.units.1st.sem..approved. = cut(
        Curricular.units.1st.sem..approved.,
        breaks = c(-Inf, 0, 2, 5, 8, Inf),  # Define the breaks for grouping
        labels = c("0", "1-2", "3-5", "6-8", "9+"),  # Range-based labels for each group
        right = TRUE
      )
    )
  table(df_dropOut_processed$Curricular.units.1st.sem..approved.)
  sum(table(df_dropOut_processed$Curricular.units.1st.sem..approved.))
  
  #Curricular.units.1st.sem..approved. group to smaller categories and label
  df_dropOut_processed <- df_dropOut_processed %>%
    mutate(
      Curricular.units.1st.sem..without.evaluations. = cut(
        Curricular.units.1st.sem..without.evaluations.,
        breaks = c(-Inf, 0, 2, 5, Inf),  # Define the breaks for grouping
        labels = c("0", "1-2", "3-5", "6+"),  # Range-based labels for each group
        right = TRUE
      )
    )
  table(df_dropOut_processed$Curricular.units.1st.sem..without.evaluations.)
  sum(table(df_dropOut_processed$Curricular.units.1st.sem..without.evaluations.))
  
  #Curricular.units.2nd.sem..credited. group to smaller categories and label
  df_dropOut_processed <- df_dropOut_processed %>%
    mutate(
      Curricular.units.2nd.sem..credited. = cut(
        Curricular.units.2nd.sem..credited.,
        breaks = c(-Inf, 0, 5, 10, Inf),  # Define the breaks for grouping
        labels = c("0", "1-5", "6-10", "11+"),  # Labels for each group
        right = TRUE
      )
    )
  table(df_dropOut_processed$Curricular.units.2nd.sem..credited.)
  sum(table(df_dropOut_processed$Curricular.units.2nd.sem..credited.))
  
  #Curricular.units.2nd.sem..enrolled. group to smaller categories and label
  df_dropOut_processed <- df_dropOut_processed %>%
    mutate(
      Curricular.units.2nd.sem..enrolled. = cut(
        Curricular.units.2nd.sem..enrolled.,
        breaks = c(-Inf, 0, 4, 7, 10, Inf),
        labels = c("0", "1-4", "5-7", "8-10", "11+"),
        right = TRUE
      )
    )
  table(df_dropOut_processed$Curricular.units.2nd.sem..enrolled.)
  sum(table(df_dropOut_processed$Curricular.units.2nd.sem..enrolled.))
  
  #Curricular.units.2nd.sem..evaluations. group to smaller categories and label
  df_dropOut_processed <- df_dropOut_processed %>%
    mutate(
      Curricular.units.2nd.sem..evaluations. = cut(
        Curricular.units.2nd.sem..evaluations.,
        breaks = c(-Inf, 0, 5, 10, 15, Inf),  # Define the breaks for grouping
        labels = c("0", "1-5", "6-10", "11-15", "16+"),  # Range-based labels for each group
        right = TRUE
      )
    )
  table(df_dropOut_processed$Curricular.units.2nd.sem..evaluations.)
  sum(table(df_dropOut_processed$Curricular.units.2nd.sem..evaluations.))
  
  #Curricular.units.2nd.sem..approved. group to smaller categories and label
  df_dropOut_processed <- df_dropOut_processed %>%
    mutate(
      Curricular.units.2nd.sem..approved. = cut(
        Curricular.units.2nd.sem..approved.,
        breaks = c(-Inf, 0, 2, 5, 8, Inf),  # Define the breaks for grouping
        labels = c("0", "1-2", "3-5", "6-8", "9+"),  # Range-based labels for each group
        right = TRUE
      )
    )
  table(df_dropOut_processed$Curricular.units.1st.sem..approved.)
  sum(table(df_dropOut_processed$Curricular.units.1st.sem..approved.))
  
  #Curricular.units.2nd.sem..without.evaluations. group to smaller categories and label
  df_dropOut_processed <- df_dropOut_processed %>%
    mutate(
      Curricular.units.2nd.sem..without.evaluations. = cut(
        Curricular.units.2nd.sem..without.evaluations.,
        breaks = c(-Inf, 0, 2, 5, Inf),  # Define the breaks for grouping
        labels = c("0", "1-2", "3-5", "6+"),  # Range-based labels for each group
        right = TRUE
      )
    )
  table(df_dropOut_processed$Curricular.units.2nd.sem..without.evaluations.)
  sum(table(df_dropOut_processed$Curricular.units.2nd.sem..without.evaluations.))
  
  #Target Combine categories and create dummy variables
  df_dropOut_processed <- df_dropOut_processed %>%
    mutate(Target = case_when(
      Target %in% c("Graduate", "Enrolled") ~ "Not-Dropout",
      TRUE ~ Target # Keep other values unchanged
    ))
}

### Create dummy columns for categorical columns and confirm all 2 variable columns are binary
{
  
  #Assign continuous variable names to a list
  varCont <-  c("Admission.grade",
                "Previous.qualification..grade.",
                "Curricular.units.1st.sem..grade.",
                "Curricular.units.2nd.sem..grade.",
                "Unemployment.rate",
                "Inflation.rate",
                "GDP")

  binary_int_vars <- names(df_dropOut_processed)[sapply(df_dropOut_processed, function(x) {
    is.integer(x) && length(unique(x)) == 2
  })]
  dum_var <- setdiff(names(df_dropOut_processed), c(varCont, binary_int_vars))  
  library(fastDummies)
  df_dropOut_processed = dummy_columns(df_dropOut_processed, select_columns = dum_var,
                       remove_most_frequent_dummy = T,
                       remove_selected_columns = T)
  
  # Convert all two-level factors to integer 0/1 binary columns. This makes the dataset consistent with integers and binary variables.
  df_dropOut_processed <- df_dropOut_processed %>%
    mutate(across(where(~ is.factor(.) && nlevels(.) == 2), ~ as.integer(as.numeric(.) - 1)))
}


### Clean Data from Columns that are highly correlated, or have low variance
{
  #Remove low variance
  low_variance_cols <- sapply(df_dropOut_processed, function(x) var(as.numeric(x), na.rm = TRUE) <= 0.01)
  df_dropOut_clean <- df_dropOut_processed[, !low_variance_cols]
  #Sparse variables (remove columns with mostly one value)
  sparse_cols <- sapply(df_dropOut_processed, function(x) mean(x == 0, na.rm = TRUE) > 0.95)
  df_dropOut_clean <- df_dropOut_processed[, !sparse_cols]
  
  #Remove highly correlated variables
  # Check if the package is installed, install if it isn't, and load it
  if (!requireNamespace("caret", quietly = TRUE)) {
    install.packages("caret")
  }
  library(caret)
  #Search for highly correlated variables
  numeric_vars <- sapply(df_dropOut_processed, is.numeric)
  cor_matrix <- cor(df_dropOut_processed[, numeric_vars], use = "complete.obs")
  high_corr <- findCorrelation(cor_matrix, cutoff = 0.9)  # Remove variables with correlation > 0.9
  df_dropOut_clean <- df_dropOut_processed[, -high_corr]
  
  #Select significant predictors????? Does this happen during preprocessing or applying algorithms?
}
ncol(df_dropOut_processed)
ncol(df_dropOut_clean)
str(df_dropOut_clean)

#Create Heatmaps for Original Data and Raw Data. To get a before and after
library(ggplot2)
library(reshape2)

# Heatmap for original data
# Select only numeric columns from the original dataset
numeric_vars_OG <- sapply(df_dropOut_raw, is.numeric)
cor_matrix_orig <- cor(df_dropOut_raw[, numeric_vars_OG], use = "complete.obs")
cor_data_OG <- melt(cor_matrix_orig)

# Plot the heatmap for original data
ggplot(data = cor_data_OG, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, 
                       limit = c(-1, 1), space = "Lab", name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Correlation Heatmap: Original Data",
       x = "Variables",
       y = "Variables")

# Heatmap for cleaned data
# Select only numeric columns from the cleaned dataset
numeric_vars_clean <- sapply(df_dropOut_clean, is.numeric)
cor_matrix_clean <- cor(df_dropOut_clean[, numeric_vars_clean], use = "complete.obs")
cor_data_clean <- melt(cor_matrix_clean)

# Plot the heatmap for cleaned data
ggplot(data = cor_data_clean, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, 
                       limit = c(-1, 1), space = "Lab", name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Correlation Heatmap: Cleaned Data",
       x = "Variables",
       y = "Variables")



#Remove everything from R environment except df_dropOut_clean
rm(list = setdiff(ls(), "df_dropOut_clean")); gc()

save(df_dropOut_clean, file = "df_dropOut_clean.rda")
write.csv(df_dropOut_clean,"df_dropOut_clean.csv", row.names = FALSE)