# Load necessary libraries
library(readxl)      # For reading Excel files
library(ggplot2)     # For visualization
library(lavaan)      # For SEM modeling
library(semPlot)     # For visualizing SEM
library(tidyverse)

# Step 1: Load the data from Excel
data <- read_excel("Metadata_SEM.xlsx", sheet = "Sheet1")

# Scale relevant variables
data <- data %>% mutate_at(c("bA"), 
                           ~(log(.) %>% as.vector))

# Scale relevant variables
data <- data %>% mutate_at(c("bA", "alt", "DBH", "sng", "TreMs"), 
                           ~(scale(.) %>% as.vector))

# Define SEM model with both direct and indirect effects
sem_model <- '
Arachnida ~ ly + sng + TreMs + Des
Diplopoda ~ ly + sng + TreMs + Des
Collembola ~ ly + sng + TreMs + Des
Hemiptera ~ ly + sng + TreMs
Hymenoptera ~ TreMs
Coleoptera ~ ly + sng +TreMs + Des

# Indirect effects: Forest characteristics -> Arthropod groups via Deadwood
Arachnida ~ DBH + alt + bA + cc + pc + FMI
Diplopoda ~ DBH + alt + bA + cc + pc + FMI
Collembola ~ DBH + alt + bA + cc + pc + FMI
Hemiptera ~ DBH + alt + bA + cc + pc + FMI
Hymenoptera ~ DBH + alt + bA + cc + pc + FMI
Coleoptera ~ DBH + alt + bA + cc + pc + FMI
'

# Fit the SEM model
sem_fit <- sem(sem_model, data = data, estimator = "ML", missing = "FIML")

# Display model summary (including direct and indirect effects)
summary(sem_fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


# Visualize SEM model with both direct and indirect effects
semPaths(sem_fit, whatLabels = "std", layout = "tree", 
         edge.label.cex = 0.8, fade = FALSE, 
         sizeMan = 8, sizeLat = 8, residuals = FALSE)

# Model fit indices
fitMeasures(sem_fit, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))



# Extract standardized estimates and significance levels
std_estimates <- standardizedSolution(sem_fit)
std_estimates <- std_estimates[std_estimates$op == "~", ]  # Only direct effects

# Display the relevant columns (Path Coefficients and p-values)
std_estimates[, c("lhs", "rhs", "est.std", "pvalue")]

# Define colors based on sign and significance
std_estimates$color <- ifelse(std_estimates$pvalue < 0.05 & std_estimates$est.std > 0, "green", 
                              ifelse(std_estimates$pvalue < 0.05 & std_estimates$est.std < 0, "red", "black"))

# Define arrow width proportional to absolute standardized coefficients
std_estimates$width <- abs(std_estimates$est.std) * 5  # Adjust multiplier as needed

# Create a named vector for edge colors
edge_colors <- setNames(std_estimates$color, paste(std_estimates$lhs, std_estimates$rhs, sep="~"))

# Create a named vector for edge widths
edge_widths <- setNames(std_estimates$width, paste(std_estimates$lhs, std_estimates$rhs, sep="~"))

# Generate SEM plot with modifications
semPaths(sem_fit, 
         whatLabels = "std",  # Show standardized path coefficients
         edge.color = edge_colors,  # Color arrows
         edge.width = edge_widths,  # Widths proportional to effect size
         layout = "tree", 
         curvePivot = TRUE,  # Show curved arrows for covariation
         residuals = FALSE, 
         sizeMan = 8, sizeLat = 8,  # Adjust node sizes
         fade = FALSE, 
         label.scale = FALSE)

# Print R² values (variance explained)
summary(sem_fit, rsquare = TRUE)



##################

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
Arachnida ~ Arachnida_pred + sng + TreMs + Des
Diplopoda ~ Diplopoda_pred + sng + TreMs + Des
Collembola ~ Collembola_pred + sng + TreMs + Des
Hemiptera ~ Hemiptera_pred + sng + TreMs
Hymenoptera ~ TreMs
Coleoptera ~ Coleoptera_pred + sng + TreMs + Des

# Indirect effects: Forest characteristics -> Arthropod groups via Deadwood
Arachnida ~ DBH + alt + bA + cc + pc + FMI
Diplopoda ~ DBH + alt + bA + cc + pc + FMI
Collembola ~ DBH + alt + bA + cc + pc + FMI
Hemiptera ~ DBH + alt + bA + cc + pc + FMI
Hymenoptera ~ DBH + alt + bA + cc + pc + FMI
Coleoptera ~ DBH + alt + bA + cc + pc + FMI
'

# Fit the SEM model with GAM predicted values
sem_fit_with_gam <- sem(sem_model_with_gam, data = data, estimator = "ML", missing = "FIML")

# View the summary of the SEM model
summary(sem_fit_with_gam)
