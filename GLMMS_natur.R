# Load required packages
library(readxl)   # For reading Excel files
library(dplyr)    # For data manipulation
library(tidyr)    # For formatting results
install.packages("writexl")
library(writexl)
library(lavaan)
library(car)
library(mgcv)

# Step 1: Load the data from Excel
data <- read_excel("Metadata_SEM.xlsx", sheet = "Sheet1")

#multicoloniarity check
vif(lm(Arachnida ~ sng + ly + TreMs + nd, data = data))

#small SEM model check
model <- '
  Arachnida ~ sng + ly + TreMs + nd
'
fit <- sem(model, data = data, estimator = "ML")
summary(fit, fit.measures = TRUE, standardized = TRUE)

fitMeasures(fit, c("cfi", "rmsea", "srmr", "tli", "chi2"))

#data check
sapply(data[, c("SSCI", "FMI", "alt", "DBH", "bA", "pc", "cc", "Des", "sng", "ly", "nd", "TreMs", "TS")], function(x) length(unique(x)))
data$Des <- as.factor(data$Des)

#GAM model for non-linierity check and key predictor identification
gam_model <- gam(Arachnida ~ s(SSCI) + s(FMI) + s(alt) + s(DBH) +
                   s(bA) + s(pc) + s(cc) + Des + 
                   s(sng, k=3) + s(ly, k=3) + s(nd, k=3) + s(TreMs, k=3) + 
                   s(TS, k=5), data = data)

summary(gam_model)

#Repeate the process 

# List of response variables
responses <- c("Arachnida", "Collembola", "Diplopoda", "Coleoptera", "Hymenoptera", "Hemiptera")

# Loop through each response variable
for (response in responses) {
  # Build the GAM model for each response variable
  formula <- as.formula(paste(response, "~ s(SSCI) + s(FMI) + s(alt) + s(DBH) + 
                              s(bA) + s(pc) + s(cc) + Des + 
                              s(sng, k=3) + s(ly, k=3) + s(nd, k=3) + 
                              s(TreMs, k=3) + s(TS, k=5)"))
  
  # Fit the GAM model
  gam_model <- gam(formula, data = data)
  
  # Print the summary for each model
  cat("\nSummary for", response, "model:\n")
  print(summary(gam_model))
}





