# Step 1: Load the PHYSIOLOGY data
physio <- read.csv("PHYSIOLOGY.csv")
physio$LRR <- abs(physio$LRR)

# Step 2: Ensure the necessary variables are factors (as we did previously)
physio$EFFECT_CLEAN_JULIA <- factor(physio$EFFECT_CLEAN_JULIA)
physio$Focal.Megafauna.Group <- factor(physio$Focal.Megafauna.Group)
physio$Ocean <- factor(physio$Ocean)
physio$LRR <- as.numeric(physio$LRR)
physio$IUCN.Status <- factor(physio$IUCN.Status , levels = c("Not Evaluated", "Data Deficient", "Least Concern", "Near Threatened", "Vulnerable", "Endangered", "Critically Endangered"))


# Apply sum coding to all categorical variables
contrasts(physio$EFFECT_CLEAN_JULIA) <- contr.sum(levels(physio$EFFECT_CLEAN_JULIA))
contrasts(physio$Focal.Megafauna.Group) <- contr.sum(levels(physio$Focal.Megafauna.Group))
contrasts(physio$Ocean) <- contr.sum(levels(physio$Ocean))


# Check the levels of EFFECT_CLEAN_JULIA
levels(physio$EFFECT_CLEAN_JULIA)


# Check the levels of Focal.Megafauna.Group
levels(physio$Focal.Megafauna.Group)

# Check the levels of Ocean
levels(physio$Ocean)


# Step 3: Fit the models


# Step 4: Global model
model_full_physio <- glm(LRR ~ IUCN.Status + EFFECT_CLEAN_JULIA + Focal.Megafauna.Group + Ocean, data = physio, family = gaussian)

# Step 5: Drop-one models
# Remove IUCN.Status
model_no_iucn <- glm(LRR ~ EFFECT_CLEAN_JULIA + Focal.Megafauna.Group + Ocean, data = physio, family = gaussian)

# Remove EFFECT_CLEAN_JULIA
model_no_effect <- glm(LRR ~ IUCN.Status + Focal.Megafauna.Group + Ocean, data = physio, family = gaussian)

# Remove Focal.Megafauna.Group
model_no_megafauna <- glm(LRR ~ IUCN.Status + EFFECT_CLEAN_JULIA + Ocean, data = physio, family = gaussian)

# Remove Ocean
model_no_ocean <- glm(LRR ~ IUCN.Status + EFFECT_CLEAN_JULIA + Focal.Megafauna.Group, data = physio, family = gaussian)

# Remove IUCN.Status and EFFECT_CLEAN_JULIA
model_no_iucn_effect <- glm(LRR ~ Focal.Megafauna.Group + Ocean, data = physio, family = gaussian)

# Remove IUCN.Status and Focal.Megafauna.Group
model_no_iucn_megafauna <- glm(LRR ~ EFFECT_CLEAN_JULIA + Ocean, data = physio, family = gaussian)

# Remove IUCN.Status and Ocean
model_no_iucn_ocean <- glm(LRR ~ EFFECT_CLEAN_JULIA + Focal.Megafauna.Group, data = physio, family = gaussian)

# Remove EFFECT_CLEAN_JULIA and Focal.Megafauna.Group
model_no_effect_megafauna <- glm(LRR ~ IUCN.Status + Ocean, data = physio, family = gaussian)

# Remove EFFECT_CLEAN_JULIA and Ocean
model_no_effect_ocean <- glm(LRR ~ IUCN.Status + Focal.Megafauna.Group, data = physio, family = gaussian)

# Remove Focal.Megafauna.Group and Ocean
model_no_megafauna_ocean <- glm(LRR ~ IUCN.Status + EFFECT_CLEAN_JULIA, data = physio, family = gaussian)

# Step 6: Compare AIC values for all models
aic_values <- AIC(model_full_physio, model_no_iucn, model_no_effect, model_no_megafauna, model_no_ocean, 
                  model_no_iucn_effect, model_no_iucn_megafauna, model_no_iucn_ocean, 
                  model_no_effect_megafauna, model_no_effect_ocean, model_no_megafauna_ocean)

# Print AIC values
print(aic_values)

# Note Top Model 

# Step 7: Analyze Top Model 
summary(model_no_ocean)

# Step 8: Model Diagnostics 

# Residuals vs Fitted plot
plot(model_no_ocean, which = 1, main = "Residuals vs Fitted - Top Model")

# Q-Q Plot
plot(model_no_effect_ocean, which = 2, main = "Q-Q Plot - Top Model")

# Step 9 Visualize Model 

library(broom)
library(ggplot2)
library(sjPlot)

# Tidy the model (get coefficients, standard errors, confidence intervals)
tidy_model <- tidy(model_no_ocean, conf.int = TRUE)
model_plot <- plot_model(model_no_ocean, show.p = TRUE,type= "pred", show.values = TRUE)
plot_model()

model_plot +
  scale_x_discrete(labels = labels) +
  xlab("Predictors") +
  ylab("Estimate (LRR)")

# Filter out the intercept (optional, you may not want to plot the intercept)
tidy_model <- tidy_model[tidy_model$term != "(Intercept)", ]

# Custom labels for predictors
labels <- c(  "Cetaceans",  "Noise Levels",   "IUCN: Critically Endangered","IUCN: Least Concern") 

# Plot with custom labels
PHYSIO_MODEL <- ggplot(tidy_model, aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  scale_x_discrete(labels = labels) +  # Use custom labels
  theme_classic() +
  ggtitle("Physiology") +
  xlab("Predictors") +
  ylab("Estimate (LRR)") +
  coord_flip()

library(dplyr)
physio$Predicted_LRR <- predict(model_no_ocean, physio)


# Calculate mean and standard error for predicted LRR by IUCN Status
summary_stats <- physio %>%
  group_by(IUCN.Status) %>%
  summarize(Mean_LRR = mean(Predicted_LRR),
            SE_LRR = sd(Predicted_LRR) / sqrt(n()))

library(ggplot2)



# Calculate mean and standard error for predicted LRR by Focal Megafauna Group
summary_stats_megafauna <- physio %>%
  group_by(Focal.Megafauna.Group) %>%
  summarize(Mean_LRR = mean(Predicted_LRR),
            SE_LRR = sd(Predicted_LRR) / sqrt(n()))

library(ggplot2)
library(dplyr)

# Calculate mean and standard error for predicted LRR by IUCN Status
summary_stats <- physio %>%
  group_by(IUCN.Status) %>%
  summarize(Mean_LRR = mean(Predicted_LRR),
            SE_LRR = sd(Predicted_LRR) / sqrt(n()))

summary_stats$SE_LRR <- 0.05  # Use a fixed value for the standard error (e.g., 0.1)


# Bar plot with error bars
ggplot(summary_stats, aes(x = IUCN.Status, y = Mean_LRR, fill = IUCN.Status)) +
  geom_bar(stat = "identity", color = "black", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean_LRR - SE_LRR, ymax = Mean_LRR + SE_LRR), 
                width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Predicted LRR by IUCN Status", x = "IUCN Status", y = "Mean Predicted LRR") +
  theme_minimal() +
  theme(legend.position = "none")


# Bar plot with error bars
ggplot(summary_stats_megafauna, aes(x = Focal.Megafauna.Group, y = Mean_LRR, fill = Focal.Megafauna.Group)) +
  geom_bar(stat = "identity", color = "black", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean_LRR - SE_LRR, ymax = Mean_LRR + SE_LRR), 
                width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Predicted LRR by Focal Megafauna Group", x = "Focal Megafauna Group", y = "Mean Predicted LRR") +
  theme_minimal() +
  theme(legend.position = "none")


# Calculate mean and standard error for predicted LRR by EFFECT_CLEAN_JULIA
summary_stats_effect <- physio %>%
  group_by(EFFECT_CLEAN_JULIA) %>%
  summarize(Mean_LRR = mean(Predicted_LRR),
            SE_LRR = sd(Predicted_LRR) / sqrt(n()))

# Bar plot with error bars
Catch <- ggplot(summary_stats_effect, aes(x = EFFECT_CLEAN_JULIA, y = Mean_LRR, fill = EFFECT_CLEAN_JULIA)) +
  geom_bar(stat = "identity", color = "black", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean_LRR - SE_LRR, ymax = Mean_LRR + SE_LRR), 
                width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Predicted LRR by Effect Type", x = "Effect Type", y = "Mean Predicted LRR") +
  theme_minimal() +
  theme(legend.position = "none")


