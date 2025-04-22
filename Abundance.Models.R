# Step 1: Load the POPULATION data
POPULATION <- read.csv("~/Desktop/Research/MetaAnalysis/DATA_BASES/POPULATIONS.csv")

# Step 2: Ensure the necessary variables are factors and properly formatted
POPULATION$EFFECT_CLEAN_JULIA <- factor(POPULATION$EFFECT_CLEAN_JULIA)
POPULATION$Focal.Megafauna.Group <- factor(POPULATION$Focal.Megafauna.Group)
POPULATION$Ocean <- factor(POPULATION$Ocean)
POPULATION$LRR <- as.numeric(POPULATION$LRR)
POPULATION$LRR <- abs(POPULATION$LRR)  # Ensure LRR values are non-negative
POPULATION$IUCN.Status <- factor(POPULATION$IUCN.Status, 
                                 levels = c("Not Evaluated", "Data Deficient", "Least Concern", 
                                            "Near Threatened", "Vulnerable", "Endangered", 
                                            "Critically Endangered"))

# Step 3: Apply sum coding (effect coding) to all categorical variables
contrasts(POPULATION$EFFECT_CLEAN_JULIA) <- contr.sum(levels(POPULATION$EFFECT_CLEAN_JULIA))
contrasts(POPULATION$Focal.Megafauna.Group) <- contr.sum(levels(POPULATION$Focal.Megafauna.Group))
contrasts(POPULATION$Ocean) <- contr.sum(levels(POPULATION$Ocean))

# Step 4: Check the levels of the factors (optional)
levels(POPULATION$EFFECT_CLEAN_JULIA)
levels(POPULATION$Focal.Megafauna.Group)
levels(POPULATION$Ocean)

# Step 5: Fit Generalized Linear Models (GLMs)
# Full model including all variables
model_full_POPULATION <- glm(LRR ~ IUCN.Status + EFFECT_CLEAN_JULIA + Focal.Megafauna.Group + Ocean, 
                             data = POPULATION, family = gaussian)

# Drop-one models to assess the effect of each variable by omitting them one at a time
model_no_iucn <- glm(LRR ~ EFFECT_CLEAN_JULIA + Focal.Megafauna.Group + Ocean, 
                     data = POPULATION, family = gaussian)
model_no_effect <- glm(LRR ~ IUCN.Status + Focal.Megafauna.Group + Ocean, 
                       data = POPULATION, family = gaussian)
model_no_megafauna <- glm(LRR ~ IUCN.Status + EFFECT_CLEAN_JULIA + Ocean, 
                          data = POPULATION, family = gaussian)
model_no_ocean <- glm(LRR ~ IUCN.Status + EFFECT_CLEAN_JULIA + Focal.Megafauna.Group, 
                      data = POPULATION, family = gaussian)
model_no_iucn_effect <- glm(LRR ~ Focal.Megafauna.Group + Ocean, 
                            data = POPULATION, family = gaussian)
model_no_iucn_megafauna <- glm(LRR ~ EFFECT_CLEAN_JULIA + Ocean, 
                               data = POPULATION, family = gaussian)
model_no_iucn_ocean <- glm(LRR ~ EFFECT_CLEAN_JULIA + Focal.Megafauna.Group, 
                           data = POPULATION, family = gaussian)
model_no_effect_megafauna <- glm(LRR ~ IUCN.Status + Ocean, 
                                 data = POPULATION, family = gaussian)
model_no_effect_ocean <- glm(LRR ~ IUCN.Status + Focal.Megafauna.Group, 
                             data = POPULATION, family = gaussian)
model_no_megafauna_ocean <- glm(LRR ~ IUCN.Status + EFFECT_CLEAN_JULIA, 
                                data = POPULATION, family = gaussian)

# Step 6: Compare AIC values for all models to determine the best fit
aic_values <- AIC(model_full_POPULATION, model_no_iucn, model_no_effect, 
                  model_no_megafauna, model_no_ocean, model_no_iucn_effect, 
                  model_no_iucn_megafauna, model_no_iucn_ocean, 
                  model_no_effect_megafauna, model_no_effect_ocean, 
                  model_no_megafauna_ocean)
print(aic_values)

# Step 7: Analyze the top model (lowest AIC)
summary(model_no_megafauna_ocean)

 # Step 8: Model Diagnostics
# Diagnostic plots to check model fit:
# Residuals vs. Fitted plot
plot(model_no_megafauna_ocean, which = 1, main = "Residuals vs Fitted - Top Model")
# Q-Q plot for normality
plot(model_no_megafauna_ocean, which = 2, main = "Q-Q Plot - Top Model")


tab_model(model_full_POPULATION)# Step 9: Visualize Model Results
# Load required libraries
library(broom)
library(ggplot2)
library(sjPlot)

# Tidy the model to get coefficients, standard errors, and confidence intervals
tidy_model <- tidy(model_no_iucn, conf.int = TRUE)
# Remove the intercept for clearer plotting (optional)
tidy_model <- tidy_model[tidy_model$term != "(Intercept)", ]

# Custom labels for predictors for clarity in the plot
labels <- c(
  "IUCN.StatusLeast Concern" = "IUCN: Least Concern",
  "IUCN.StatusCritically Endangered" = "IUCN: Critically Endangered",
  "Focal.Megafauna.Group1" = "Focal Group: Cetaceans"
)

# Generate the plot with custom labels
POPULATION_MODEL <- ggplot(tidy_model, aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  scale_x_discrete(labels = labels) +  # Use custom labels
  theme_classic() +
  ggtitle("POPULATION Model Estimates") +
  xlab("Predictors") +
  ylab("Estimate (LRR)") +
  coord_flip()

# Display the plot
print(POPULATION_MODEL)
