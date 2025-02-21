install.packages("lme4")  # For mixed models
library(lme4)
library(readxl)  # For reading Excel files

# Load required packages
install.packages(c("MASS", "pscl", "mgcv", "performance"))
library(MASS)       # For Negative Binomial models
library(pscl)       # For Zero-Inflated Models
library(mgcv)       # For GAMs
library(performance) # For model diagnostics
library(ggplot2)
library(tidyverse)
library(reshape2)

# Step 1: Read the Excel file
data <- read_excel("eDNA_and_Metadata.xlsx")

#Examine Correlations:
data_numeric <- data %>% select(where(is.numeric))
cor_matrix <- cor(data_numeric, method = "pearson")
print(cor_matrix)

# Convert to long format
cor_long <- as.data.frame(as.table(cor_matrix))

# Rename columns for clarity
names(cor_long) <- c("Variable1", "Variable2", "Correlation")

ggplot(cor_long, aes(x = Variable1, y = Variable2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 2)), color = "black", size = 3) + # Add text
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, 
                       limit = c(-1, 1), name = "Correlation") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid = element_blank()
  ) +
  coord_fixed()
# Ensure square tiles

#####################################################################################
# Convert categorical variables
data$Decaying_stage <- as.factor(data$Decaying_stage)

#(a) Generalized Linear Model (Poisson)
glm_poisson <- glm(Arachnida ~ avg_alt + basal_A + pc_conifer + can_closure + SSCI + Decaying_stage,
                   family = poisson(link = "log"), data = data)

summary(glm_poisson)

#(b) Negative Binomial Model
glm_nb <- glm.nb(Arachnida ~ avg_alt + basal_A + pc_conifer + can_closure + SSCI + Decaying_stage,
                 data = data)

summary(glm_nb)

#(d) Generalized Additive Model (GAM)

gam_model <- gam(Arachnida ~ s(avg_alt) + s(basal_A) + s(pc_conifer) + s(can_closure) + SSCI + Decaying_stage,
                 family = poisson, data = data)

summary(gam_model)


# Check model performance
check_model(glm_nb)
check_model(gam_model)

# Compare models using AIC
AIC(glm_poisson, glm_nb, gam_model)


###############################################

# Arthropod groups as response variables
arthropod_vars <- c("Arachnida", "Collembola", "Diplopoda", "Coleoptera", "Hymenoptera", "Hemiptera")

# Create a long-format dataset for easier iteration
library(tidyr)
long_data <- data %>%
  pivot_longer(cols = all_of(arthropod_vars), names_to = "Group", values_to = "Counts")

# Function to fit GAM for each group
library(mgcv)
gam_results <- list()

for (group in arthropod_vars) {
  gam_results[[group]] <- gam(as.formula(paste(group, "~ s(avg_alt) + s(can_closure) + SSCI + Decaying_stage")),
                              family = poisson, data = data)
}

# View summaries
lapply(gam_results, summary)


# Check AIC for all GAMs
gam_aic <- sapply(gam_results, AIC)
print(gam_aic)

# Visualize smooth terms for a specific group (e.g., Arachnida)
plot(gam_results[["Arachnida"]], pages = 1, rug = TRUE, se = TRUE, shade = TRUE)

##################
# Install and load the package
install.packages("mvabund")
library(mvabund)

# Combine arthropod groups into a matrix
arthropod_matrix <- data[, arthropod_vars]
arthropod_matrix <- as.matrix(data[, arthropod_vars])

# Fit a multivariate GAM
mv_gam <- manyglm(arthropod_matrix ~ avg_alt + can_closure + SSCI + Decaying_stage,
                  family = "negative.binomial", data = data)

summary(mv_gam)
anova(mv_gam)

##################################################
#Examine Correlations:
cor_matrix <- cor(data[, c("avg_alt", "basal_A", "pc_conifer", "can_closure", "SSCI")])
print(cor_matrix)

#Simplify the Model:
mv_gam_simple <- manyglm(arthropod_matrix ~ avg_alt + basal_A, family = "negative.binomial", data = data)
anova(mv_gam_simple)

#Test for Interactions: 
mv_gam_interaction <- manyglm(arthropod_matrix ~ avg_alt * Decaying_stage, family = "negative.binomial", data = data)
anova(mv_gam_interaction)

#Check the Model Fit:
plot(mv_gam)

###########################################################
library(randomForest)

# Make sure response is a vector
y <- data$Collembola

# Make sure predictors are a data frame
X <- data.frame(data[, c("avg_alt", "basal_A", "pc_conifer", "can_closure", "SSCI", "Decaying_stage")])

# Run the Random Forest model again
rf_model <- randomForest(x = X, y = y, importance = TRUE)
print(rf_model)


rf_model <- randomForest(Collembola ~ avg_alt + basal_A + pc_conifer + can_closure + SSCI + Decaying_stage, 
                         data = data, importance = TRUE)
print(rf_model)

######solutions
importance(rf_model)

rf_model_reduced <- randomForest(Collembola ~ avg_alt + can_closure + SSCI + Decaying_stage, 
                                 data = data, importance = TRUE)
print(rf_model_reduced)


###########################




