# Step 1: Load the BEHAVIORLOGY data
BEHAVIOR <- read.csv("BEHAVIOR.csv")

BEHAVIOR$LRR <- abs(BEHAVIOR$LRR)

# Step 2: Ensure the necessary variables are factors (as we did previously)
BEHAVIOR$EFFECT_CLEAN_JULIA <- factor(BEHAVIOR$EFFECT_CLEAN_JULIA)
BEHAVIOR$Focal.Megafauna.Group <- factor(BEHAVIOR$Focal.Megafauna.Group)
BEHAVIOR$Ocean <- factor(BEHAVIOR$Ocean)
BEHAVIOR$LRR <- as.numeric(BEHAVIOR$LRR)
BEHAVIOR$IUCN.Status <- factor(BEHAVIOR$IUCN.Status , levels = c("Not Evaluated", "Data Deficient", "Least Concern", "Near Threatened", "Vulnerable", "Endangered", "Critically Endangered"))


# Apply sum coding to all categorical variables
contrasts(BEHAVIOR$EFFECT_CLEAN_JULIA) <- contr.sum(levels(BEHAVIOR$EFFECT_CLEAN_JULIA))
contrasts(BEHAVIOR$Focal.Megafauna.Group) <- contr.sum(levels(BEHAVIOR$Focal.Megafauna.Group))
contrasts(BEHAVIOR$Ocean) <- contr.sum(levels(BEHAVIOR$Ocean))


# Check the levels of EFFECT_CLEAN_JULIA
levels(BEHAVIOR$EFFECT_CLEAN_JULIA)


# Check the levels of Focal.Megafauna.Group
levels(BEHAVIOR$Focal.Megafauna.Group)

# Check the levels of Ocea
levels(BEHAVIOR$Ocean)




# Step 3: Fit the models


# Step 4: Global model
model_full_BEHAVIOR <- glm(LRR ~ IUCN.Status + EFFECT_CLEAN_JULIA + Focal.Megafauna.Group + Ocean, data = BEHAVIOR, family = gaussian)

# Step 5: Drop-one models
# Remove IUCN.Status
model_no_iucn <- glm(LRR ~ EFFECT_CLEAN_JULIA + Focal.Megafauna.Group + Ocean, data = BEHAVIOR, family = gaussian)

# Remove EFFECT_CLEAN_JULIA
model_no_effect <- glm(LRR ~ IUCN.Status + Focal.Megafauna.Group + Ocean, data = BEHAVIOR, family = gaussian)

# Remove Focal.Megafauna.Group
model_no_megafauna <- glm(LRR ~ IUCN.Status + EFFECT_CLEAN_JULIA + Ocean, data = BEHAVIOR, family = gaussian)

# Remove Ocean
model_no_ocean <- glm(LRR ~ IUCN.Status + EFFECT_CLEAN_JULIA + Focal.Megafauna.Group, data = BEHAVIOR, family = gaussian)

# Remove IUCN.Status and EFFECT_CLEAN_JULIA
model_no_iucn_effect <- glm(LRR ~ Focal.Megafauna.Group + Ocean, data = BEHAVIOR, family = gaussian)

# Remove IUCN.Status and Focal.Megafauna.Group
model_no_iucn_megafauna <- glm(LRR ~ EFFECT_CLEAN_JULIA + Ocean, data = BEHAVIOR, family = gaussian)

# Remove IUCN.Status and Ocean
model_no_iucn_ocean <- glm(LRR ~ EFFECT_CLEAN_JULIA + Focal.Megafauna.Group, data = BEHAVIOR, family = gaussian)

# Remove EFFECT_CLEAN_JULIA and Focal.Megafauna.Group
model_no_effect_megafauna <- glm(LRR ~ IUCN.Status + Ocean, data = BEHAVIOR, family = gaussian)

# Remove EFFECT_CLEAN_JULIA and Ocean
model_no_effect_ocean <- glm(LRR ~ IUCN.Status + Focal.Megafauna.Group, data = BEHAVIOR, family = gaussian)

# Remove Focal.Megafauna.Group and Ocean
model_no_megafauna_ocean <- glm(LRR ~ IUCN.Status + EFFECT_CLEAN_JULIA, data = BEHAVIOR, family = gaussian)

# Step 6: Compare AIC values for all models
aic_values <- AIC(model_full_BEHAVIOR, model_no_iucn, model_no_effect, model_no_megafauna, model_no_ocean, 
                  model_no_iucn_effect, model_no_iucn_megafauna, model_no_iucn_ocean, 
                  model_no_effect_megafauna, model_no_effect_ocean, model_no_megafauna_ocean)

# Print AIC values
print(aic_values)

# Note Top Model 

# Step 7: Analyze Top Model 
summary(model_no_ocean)

# Step 8: Model Diagnostics 

# Residuals vs Fitted plot
plot(model_no_effect_ocean, which = 1, main = "Residuals vs Fitted - Top Model")

# Q-Q Plot
plot(model_no_effect_ocean, which = 2, main = "Q-Q Plot - Top Model")

# Step 9 Visualize Model 

library(broom)
library(ggplot2)
library(sjPlot)

# Tidy the model (get coefficients, standard errors, confidence intervals)
tidy_model <- tidy(model_no_effect_ocean, conf.int = TRUE)

# Filter out the intercept (optional, you may not want to plot the intercept)
tidy_model <- tidy_model[tidy_model$term != "(Intercept)", ]

# Custom labels for predictors
labels <- c(
  "IUCN.StatusLeast Concern" = "IUCN: Least Concern",
  "IUCN.StatusCritically Endangered" = "IUCN: Critically Endangered",
  "Focal.Megafauna.Group1" = "Focal Group: Cetaceans")

# Plot with custom labels
BEHAVIOR_MODEL <- ggplot(tidy_model, aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  scale_x_discrete(labels = labels) +  # Use custom labels
  theme_classic() +
  ggtitle("BEHAVIORlogy") +
  xlab("Predictors") +
  ylab("Estimate (LRR)") +
  coord_flip()
