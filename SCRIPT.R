###############################################################################
#  MASTER SCRIPT: Forest Structure and Arthropod Diversity Analysis
#  Author: Mohammad Jamil Shuvo
#  Project: Influence of deadwood, TreMs, and forest structure on arthropod diversity
#  Journal: Ecology and Evolution (2025)
#  ---------------------------------------------------------------------------
#  Description:
#  This script integrates all analyses from the study, including:
#    1. Correlation analysis of environmental and biodiversity variables
#    2. Generalized models (GLM / GAM) and Random Forests
#    3. Structural Equation Modeling (SEM)
#    4. Richness heatmap visualization
#
#  Requirements:
#    - R version ≥ 4.3
#    - Excel data files: Metadata_SEM.xlsx, OTUs_info_97.xlsx, richness_df.xlsx
###############################################################################

## ---- 0. Setup: Load or install required packages ----------------------------

packages <- c(
  "tidyverse", "readxl", "corrplot", "MASS", "pscl", "mgcv", "performance",
  "mvabund", "lavaan", "semPlot", "randomForest", "car", "reshape2", "RColorBrewer",
  "writexl"
)

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)
}

# Set working directory (adjust this to your project folder)
# setwd("path/to/Shuvo_Arthropod_eDNA_ForestStructure_2025")

###############################################################################
# 1. CORRELATION ANALYSIS
###############################################################################

cat("\nStep 1: Correlation analysis\n")

# Load metadata file
metadata <- read_excel("Metadata_SEM.xlsx")

# Select numeric variables for correlations
data_numeric_1 <- metadata %>%
  select(SSCI, ForMI, Avarage_elevation, DBHMean, Basal_area,
         Percentage_coniferous, Canopy_closure, Decaying_stage,
         Snags, Lying_DW_volume, Dead_tree, TreMs, Tree_species)

data_numeric_2 <- metadata %>%
  select(Arachnida, Collembola, Diplopoda, Coleoptera,
         Hymenoptera, Hemiptera, Richness)

# Compute correlation matrices
cor_matrix_1 <- cor(data_numeric_1, method = "pearson", use = "pairwise.complete.obs")
cor_matrix_2 <- cor(data_numeric_2, method = "pearson", use = "pairwise.complete.obs")

# Plot correlation matrices
par(mfrow = c(1, 2))

corrplot(cor_matrix_1, order = "AOE", method = "pie",
         title = "Forest Structure & Environmental Variables", mar = c(0,0,2,0))

corrplot(cor_matrix_2, order = "AOE", method = "pie",
         title = "Arthropod Richness Variables", mar = c(0,0,2,0))

###############################################################################
# 2. GENERALIZED MODELS (GLM, GAM) AND RANDOM FOREST
###############################################################################

cat("\nStep 2: GLM, GAM, and Random Forest modeling\n")

# Load full eDNA + metadata dataset
data <- read_excel("eDNA_and_Metadata.xlsx")

# Convert categorical variables
data$Decaying_stage <- as.factor(data$Decaying_stage)

## (a) Poisson GLM
glm_poisson <- glm(Arachnida ~ avg_alt + basal_A + pc_conifer +
                     can_closure + SSCI + Decaying_stage,
                   family = poisson, data = data)
summary(glm_poisson)

## (b) Negative Binomial GLM
glm_nb <- MASS::glm.nb(Arachnida ~ avg_alt + basal_A + pc_conifer +
                         can_closure + SSCI + Decaying_stage,
                       data = data)
summary(glm_nb)

## (c) GAM (Poisson)
gam_model <- mgcv::gam(Arachnida ~ s(avg_alt) + s(basal_A) +
                         s(pc_conifer) + s(can_closure) + SSCI + Decaying_stage,
                       family = poisson, data = data)
summary(gam_model)

# Model diagnostics and comparison
performance::check_model(glm_nb)
AIC(glm_poisson, glm_nb, gam_model)

## (d) GAMs for each arthropod group
arthropod_vars <- c("Arachnida", "Collembola", "Diplopoda",
                    "Coleoptera", "Hymenoptera", "Hemiptera")
gam_results <- list()

for (group in arthropod_vars) {
  formula_str <- as.formula(paste(group,
                                  "~ s(avg_alt) + s(can_closure) + SSCI + Decaying_stage"))
  gam_results[[group]] <- gam(formula_str, family = poisson, data = data)
  cat("\nSummary for", group, ":\n")
  print(summary(gam_results[[group]]))
}

# Extract AIC values
gam_aic <- sapply(gam_results, AIC)
print(gam_aic)

## (e) Random Forest example (Collembola)
rf_model <- randomForest(Collembola ~ avg_alt + basal_A +
                           pc_conifer + can_closure + SSCI + Decaying_stage,
                         data = data, importance = TRUE)
print(rf_model)
importance(rf_model)

###############################################################################
# 3. STRUCTURAL EQUATION MODELING (SEM)
###############################################################################

cat("\nStep 3: Structural Equation Modeling\n")

# Load metadata again
sem_data <- read_excel("Metadata_SEM.xlsx", sheet = "Sheet1")

# Scale all numeric variables
sem_data <- sem_data %>% mutate(across(where(is.numeric), scale))

# Fit GAMs for each response and use predicted values in SEM
responses <- c("Arachnida", "Collembola", "Diplopoda",
               "Coleoptera", "Hymenoptera", "Hemiptera")

for (r in responses) {
  formula_gam <- as.formula(paste(r, "~ s(SSCI) + s(FMI) + s(alt) + s(DBH) +
                                   s(bA) + s(pc) + s(cc) + Des +
                                   s(sng, k=3) + s(ly, k=3) + s(nd, k=3) +
                                   s(TreMs, k=3) + s(TS, k=5)"))
  model_gam <- gam(formula_gam, data = sem_data)
  sem_data[[paste0(r, "_pred")]] <- predict(model_gam, newdata = sem_data)
}

# Define SEM model using predicted GAM values
sem_model <- '
Arachnida_pred ~ sng + TreMs + Des
Diplopoda_pred ~ TreMs + Des
Collembola_pred ~ sng + ly + TreMs + Des
Hemiptera_pred ~ sng + TreMs + Des
Hymenoptera_pred ~ sng + ly + TreMs + Des
Coleoptera_pred ~ sng + ly + TreMs + Des

# Indirect forest effects
Arachnida_pred ~ DBH + alt + cc + pc + FMI
Diplopoda_pred ~ DBH + alt + bA + pc + FMI
Collembola_pred ~ alt + cc + pc + FMI
Hemiptera_pred ~ bA + cc + pc + FMI
Hymenoptera_pred ~ DBH + bA + pc + FMI
Coleoptera_pred ~ DBH + alt + bA + cc + pc + FMI
'

# Fit SEM model
sem_fit <- sem(sem_model, data = sem_data, estimator = "ML", missing = "FIML")
summary(sem_fit, rsquare = TRUE, standardized = TRUE)
fitMeasures(sem_fit, c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))

# Extract significant paths and plot
coef_summary <- parameterEstimates(sem_fit)
significant_paths <- coef_summary[coef_summary$pvalue < 0.05, ]

semPlot::semPaths(
  sem_fit, what = "std", layout = "tree", residuals = FALSE,
  edge.label.cex = 1.3, edge.color = c("darkgreen","red"),
  color = list(lat = "lightblue", man = "lightgray"),
  style = "lisrel", sizeMan = 6, sizeLat = 10, nCharNodes = 4,
  mar = c(3,3,3,3)
)
# ggsave("figures/SEM_v2_final.png", width = 10, height = 8, dpi = 300)

###############################################################################
# 4. RICHNESS HEATMAP VISUALIZATION
###############################################################################

cat("\nStep 4: Richness heatmap\n")

df <- read_excel("richness_df.xlsx", sheet = 1)

required_cols <- c("Arachnida", "Collembola", "Diplopoda",
                   "Coleoptera", "Hymenoptera", "Hemiptera", "DW_species")
stopifnot(all(required_cols %in% colnames(df)))

df_long <- reshape2::melt(df, id.vars = "DW_species",
                          variable.name = "Arthropod_Group",
                          value.name = "Richness")

df_long <- df_long %>%
  group_by(DW_species, Arthropod_Group) %>%
  summarise(Richness = max(Richness, na.rm = TRUE))

ggplot(df_long, aes(x = Arthropod_Group, y = DW_species, fill = Richness)) +
  geom_tile(color = "gray") +
  geom_text(aes(label = round(Richness, 1)), size = 4, color = "black") +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, "Blues"),
                       name = "Average Richness") +
  labs(x = "Arthropod Groups", y = "Deadwood Species") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())

###############################################################################
# END OF SCRIPT
###############################################################################

cat("\nAnalysis complete. All results generated successfully.\n")
