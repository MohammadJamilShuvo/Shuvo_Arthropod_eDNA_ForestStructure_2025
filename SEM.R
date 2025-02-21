# Load necessary libraries
library(readxl)      # For reading Excel files
library(ggplot2)     # For visualization
library(lavaan)      # For SEM modeling
library(semPlot)     # For visualizing SEM
library(tidyverse)
library(car)
library(mgcv)

# Step 1: Load the data from Excel
data <- read_excel("Metadata_SEM.xlsx", sheet = "Sheet1")

# Scale all continuous variables in the dataset (except categorical variables)
data <- data %>% mutate(across(where(is.numeric), scale)) 

# List of response variables
responses <- c("Arachnida", "Collembola", "Diplopoda", "Coleoptera", "Hymenoptera", "Hemiptera")

# Loop through each response variable
for (response in responses) {
  # Build the GAM formula for each response variable
  formula <- as.formula(paste(response, "~ s(SSCI) + s(FMI) + s(alt) + s(DBH) + 
                              s(bA) + s(pc) + s(cc) + Des + 
                              s(sng, k=3) + s(ly, k=3) + s(nd, k=3) + 
                              s(TreMs, k=3) + s(TS, k=5)"))
  
  # Fit the GAM model
  gam_model <- gam(formula, data = data)
  
  # Extract the predicted values from the GAM model
  data[[paste(response, "_pred", sep = "")]] <- predict(gam_model, newdata = data)
  
  # Print the summary for each model
  cat("\nSummary for", response, "model:\n")
  print(summary(gam_model))
}

# Check the first few rows of the data to verify that the predicted values have been added
head(data)

# SEM model with predicted GAM values
sem_model_with_gam <- '
Arachnida_pred ~ sng + TreMs + Des
Diplopoda_pred ~ TreMs + Des
Collembola_pred ~ sng + ly + TreMs + Des
Hemiptera_pred ~ sng + TreMs + Des
Hymenoptera_pred ~ sng + ly + TreMs + Des
Coleoptera_pred ~ sng + ly + TreMs + Des

# Indirect effects: Forest characteristics -> Arthropod groups via Deadwood
Arachnida_pred ~ DBH + alt + cc + pc + FMI
Diplopoda_pred ~ DBH + alt + bA + pc + FMI
Collembola_pred ~ alt + cc + pc + FMI
Hemiptera_pred ~ bA + cc + pc + FMI
Hymenoptera_pred ~ DBH + bA + pc + FMI
Coleoptera_pred ~ DBH + alt + bA + cc + pc + FMI


'

# Fit the SEM model with GAM predicted values
sem_fit_with_gam <- sem(sem_model_with_gam, data = data, estimator = "ML", missing = "FIML")

# View the summary of the SEM model
summary(sem_fit_with_gam, rsquare = TRUE)

# Model fit indices
fitMeasures(sem_fit_with_gam, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))

# Get the coefficients and their p-values
coef_summary <- parameterEstimates(sem_fit_with_gam)

# Filter the coefficients to include only those with p-value < 0.05
significant_paths <- coef_summary[coef_summary$pvalue < 0.05, ]

# Extract R-squared values
rsq_values <- summary(sem_fit_with_gam, rsquare = TRUE)$rsq

# Publication-Ready SEM Plot with significant paths and R-squared values
semPaths(
  sem_fit_with_gam,
  what = "std",                      # Use standardized coefficients
  layout = "tree",                   # Arrange in a tree layout for logical flow
  residuals = FALSE,                 # Hide residual variances for simplicity
  edge.label.cex = 1.5,              # Adjust font size for path coefficients
  edge.color = c("darkgreen", "red"), # Green for positive, red for negative paths
  edge.width = 1.0,                  # Increase edge thickness for visibility
  color = list(lat = "lightblue", man = "lightgray"), # Use subtle colors for nodes
  style = "lisrel",                  # Use LISREL style for clear arrows
  sizeMan = 7,                       # Increase the size of manifest variable boxes
  sizeLat = 11,                      # Increase the size of latent variable boxes
  nCharNodes = 4,                    # Shorten labels to avoid overlap (edit node names if needed)
  mar = c(4, 4, 4, 4),               # Margins to prevent text cutoff
  edgeLabels = significant_paths$label, # Only include edges with significant paths
)

