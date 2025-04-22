library(ggplot2)
library(ggpubr)
library(dplyr)


# PHYSIOLOGY  -------------------------------------------------------------


## Effect Type  -----------------------------------------------------------

# Calculate the mean and standard error for each EFFECT_CLEAN_JULIA
PHYSIOLOGY_EFFECT<- PHYSIOLOGY %>%
  group_by(EFFECT_CLEAN_JULIA) %>%
  summarise(
    mean_LRR = mean(LRR_ABSVAL, na.rm = TRUE),
    se_LRR = sd(LRR_ABSVAL, na.rm = TRUE) / sqrt(n())
  )

# Perform ANOVA
anova_PHYSIOLOGY <- aov(LRR_ABSVAL ~ EFFECT_CLEAN_JULIA, data = PHYSIOLOGY)

# Display ANOVA table
summary(anova_PHYSIOLOGY)


# Perform Tukey's HSD post-hoc test
tukey_result <- TukeyHSD(anova_PHYSIOLOGY)

# Display Tukey HSD results
print(tukey_result)

# Convert Tukey results to a data frame for easier handling
tukey_df <- as.data.frame(tukey_result$EFFECT_CLEAN_JULIA)
tukey_df <- tukey_df %>% mutate(group = rownames(tukey_df))

# Create the histogram with error bars
EFFECT_PHYSIO <- ggplot(PHYSIOLOGY_EFFECT, aes(x = EFFECT_CLEAN_JULIA, y = mean_LRR)) +
  geom_bar(stat = "identity", fill = "#29Af7FFF", alpha = 0.5) +
  geom_errorbar(aes(ymin = mean_LRR - se_LRR, ymax = mean_LRR + se_LRR), width = 0.2) +
  theme_classic() +
  ylim(0, 2) +
  xlab("Effect Measured") +
  ylab("Absolute Value LRR (Physiology)") +
  theme(axis.text.x = element_text(face = "bold", angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(face = "bold")) + 
  scale_x_discrete(labels = c("Vessel Noise Levels", "Vessel Present/Absent")) 

## FOCAL GROUP  ----------------------------------------------------------



# Calculate the mean and standard error for each EFFECT_CLEAN_JULIA
PHYSIOLOGY_GROUP <- PHYSIOLOGY %>%
  group_by(Focal.Megafauna.Group) %>%
  summarise(
    mean_LRR = mean(LRR_ABSVAL, na.rm = TRUE),
    se_LRR = sd(LRR_ABSVAL, na.rm = TRUE) / sqrt(n())
  )

# Perform ANOVA
anova_PHYSIO_group <- aov(LRR_ABSVAL ~ Focal.Megafauna.Group, data = PHYSIOLOGY)

# Display ANOVA table
summary(anova_PHYSIO_group)


# Perform Tukey's HSD post-hoc test
tukey_result <- TukeyHSD(anova_PHYSIO_group)

# Display Tukey HSD results
print(tukey_result)

# Create the histogram with error bars
group_PHYSIO <- ggplot(PHYSIOLOGY_GROUP, aes(x = Focal.Megafauna.Group, y = mean_LRR)) +
  geom_bar(stat = "identity", fill = "#29Af7FFF", alpha = 0.5) +
  geom_errorbar(aes(ymin = mean_LRR - se_LRR, ymax = mean_LRR + se_LRR), width = 0.2) +
  theme_classic() +
  ylim(0, 2) +
  xlab("Focal Group") +
  ylab("Absolute Value LRR (Physiology)") +
  theme(axis.text.x = element_text(face = "bold", angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(face = "bold")) 



## IUCN ----------------------------------------------------------

PHYSIOLOGY$IUCN.Status <- factor(PHYSIOLOGY$IUCN.Status , levels = c("Not Evaluated", "Data Deficient", "Least Concern", "Near Threatened", "Vulnerable", "Endangered", "Critically Endangered"))


# Calculate the mean and standard error for each EFFECT_CLEAN_JULIA
PHYSIOLOGY_IUCN <- PHYSIOLOGY%>%
  group_by(IUCN.Status) %>%
  summarise(
    mean_LRR = mean(LRR_ABSVAL, na.rm = TRUE),
    se_LRR = sd(LRR_ABSVAL, na.rm = TRUE) / sqrt(n())
  )

# Perform ANOVA
anova_PHYSIOLOGY_iucn <- aov(LRR_ABSVAL ~ IUCN.Status, data = PHYSIOLOGY)

# Display ANOVA table
summary(anova_PHYSIOLOGY_iucn)


# Perform Tukey's HSD post-hoc test
tukey_result <- TukeyHSD(anova_PHYSIOLOGY_iucn)

# Display Tukey HSD results
print(tukey_result)

# Convert Tukey results to a data frame for easier handling
tukey_df <- as.data.frame(tukey_result$IUCN.Status)
tukey_df <- tukey_df %>% mutate(group = rownames(tukey_df))

# Create the histogram with error bars
IUCN_PHYSIO <- ggplot(PHYSIOLOGY_IUCN, aes(x = IUCN.Status, y = mean_LRR)) +
  geom_bar(stat = "identity", fill = "#29Af7FFF", alpha = 0.5) +
  geom_errorbar(aes(ymin = mean_LRR - se_LRR, ymax = mean_LRR + se_LRR), width = 0.2) +
  theme_classic() +
  ylim(0, 2) +
  xlab("IUCN Status") +
  ylab("Absolute Value LRR (Physiology)") +
  theme(axis.text.x = element_text(face = "bold", angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(face = "bold")) 




## physiology graphs  -------------------------------------------------
ggarrange(EFFECT_PHYSIO, group_PHYSIO, IUCN_PHYSIO, nrow=1, labels= "AUTO", align = "hv")



# VOCALIZATION -----------------------------------------------------------

## Effect Type  -----------------------------------------------------------

# Calculate the mean and standard error for each EFFECT_CLEAN_JULIA

# Calculate the mean and standard error for each EFFECT_CLEAN_JULIA
VOCALIZATION_EFFECT<- VOCALIZATION %>%
  group_by(EFFECT_CLEAN_JULIA) %>%
  summarise(
    mean_LRR = mean(LRR_ABSVAL, na.rm = TRUE),
    se_LRR = sd(LRR_ABSVAL, na.rm = TRUE) / sqrt(n())
  )

# Perform ANOVA
anova_result <- aov(LRR_ABSVAL ~ EFFECT_CLEAN_JULIA, data = VOCALIZATION)

# Display ANOVA table
summary(anova_result)


# Perform Tukey's HSD post-hoc test
tukey_result <- TukeyHSD(anova_result)

# Display Tukey HSD results
print(tukey_result)

# Convert Tukey results to a data frame for easier handling
tukey_df <- as.data.frame(tukey_result$EFFECT_CLEAN_JULIA)
tukey_df <- tukey_df %>% mutate(group = rownames(tukey_df))

# Create the histogram with error bars
effect_vocal <- ggplot(VOCALIZATION_EFFECT, aes(x = EFFECT_CLEAN_JULIA, y = mean_LRR)) +
  geom_bar(stat = "identity", fill = "#381567FF", alpha = 0.5) +
  geom_errorbar(aes(ymin = mean_LRR - se_LRR, ymax = mean_LRR + se_LRR), width = 0.2) +
  theme_classic() +
  ylim(0, 2) +
  xlab("Effect Measured") +
  ylab("Absolute Value LRR (Vocalization)") +
  theme(axis.text.x = element_text(face = "bold", angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(face = "bold")) + 
  scale_x_discrete(labels = c("Distance of Vessel", "Vessel Noise Levels", "Vessel Present/Absent", "Regulations Present/Absent")) 



## FOCAL GROUP  ----------------------------------------------------------



#\ Calculate the mean and standard error for each EFFECT_CLEAN_JULIA
VOCALIZATION_GROUP <- VOCALIZATION %>%
  group_by(Focal.Megafauna.Group) %>%
  summarise(
    mean_LRR = mean(LRR_ABSVAL, na.rm = TRUE),
    se_LRR = sd(LRR_ABSVAL, na.rm = TRUE) / sqrt(n())
  )

# Perform ANOVA
anova_vocalization_group <- aov(LRR_ABSVAL ~ Focal.Megafauna.Group, data = VOCALIZATION)

# Display ANOVA table
summary(anova_vocalization_group)


# Perform Tukey's HSD post-hoc test
tukey_result <- TukeyHSD(anova_vocalization_group)

# Display Tukey HSD results
print(tukey_result)

# Convert Tukey results to a data frame for easier handling
tukey_df <- as.data.frame(tukey_result$EFFECT_CLEAN_JULIA)
tukey_df <- tukey_df %>% mutate(group = rownames(tukey_df))

# Create the histogram with error bars
vocal_group <- ggplot(VOCALIZATION_GROUP, aes(x = Focal.Megafauna.Group, y = mean_LRR)) +
  geom_bar(stat = "identity", fill = "#381567FF", alpha = 0.5) +
  geom_errorbar(aes(ymin = mean_LRR - se_LRR, ymax = mean_LRR + se_LRR), width = 0.2) +
  theme_classic() +
  ylim(0, 2) +
  xlab("Focal Group") +
  ylab("Absolute Value LRR (Vocalization)") +
  theme(axis.text.x = element_text(face = "bold", angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(face = "bold")) 



## IUCN ----------------------------------------------------------



# Calculate the mean and standard error for each EFFECT_CLEAN_JULIA
VOCALIZATION_IUCN <- VOCALIZATION %>%
  group_by(IUCN.Status) %>%
  summarise(
    mean_LRR = mean(LRR_ABSVAL, na.rm = TRUE),
    se_LRR = sd(LRR_ABSVAL, na.rm = TRUE) / sqrt(n())
  )

# Perform ANOVA
anova_vocalization_iucn <- aov(LRR_ABSVAL ~ IUCN.Status, data = VOCALIZATION)

# Display ANOVA table
summary(anova_vocalization_iucn)


# Perform Tukey's HSD post-hoc test
tukey_result <- TukeyHSD(anova_vocalization_iucn)

# Display Tukey HSD results
print(tukey_result)

# Convert Tukey results to a data frame for easier handling
tukey_df <- as.data.frame(tukey_result$IUCN.Status)
tukey_df <- tukey_df %>% mutate(group = rownames(tukey_df))

# Create the histogram with error bars
iucn_vocal <- ggplot(VOCALIZATION_IUCN, aes(x = IUCN.Status, y = mean_LRR)) +
  geom_bar(stat = "identity", fill = "#381567FF", alpha = 0.5) +
  geom_errorbar(aes(ymin = mean_LRR - se_LRR, ymax = mean_LRR + se_LRR), width = 0.2) +
  theme_classic() +
  ylim(0, 2) +
  xlab("IUCN Status") +
  ylab("Absolute Value LRR (Vocalization)") +
  theme(axis.text.x = element_text(face = "bold", angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(face = "bold")) 



## vocalization graphs  -------------------------------------------------
ggarrange(effect_vocal,vocal_group, iucn_vocal, nrow=1, labels= "AUTO", align = "hv")








# BEHAVIOR  ---------------------------------------------------------------

## Effect Type  -----------------------------------------------------------

# Calculate the mean and standard error for each EFFECT_CLEAN_JULIA

# Calculate the mean and standard error for each EFFECT_CLEAN_JULIA
BEHAVIOR_EFFECT<- BEHAVIOR %>%
  group_by(EFFECT_CLEAN_JULIA) %>%
  summarise(
    mean_LRR = mean(LRR_ABSVAL, na.rm = TRUE),
    se_LRR = sd(LRR_ABSVAL, na.rm = TRUE) / sqrt(n())
  )

# Perform ANOVA
anova_result <- aov(LRR_ABSVAL ~ EFFECT_CLEAN_JULIA, data = BEHAVIOR)

# Display ANOVA table
summary(anova_result)


# Perform Tukey's HSD post-hoc test
tukey_result <- TukeyHSD(anova_result)

# Display Tukey HSD results
print(tukey_result)

# Create the histogram with error bars
effect_BEHAVE <- ggplot(BEHAVIOR_EFFECT, aes(x = EFFECT_CLEAN_JULIA, y = mean_LRR)) +
  geom_bar(stat = "identity", fill = "#39568CFF", alpha = 0.5) +
  geom_errorbar(aes(ymin = mean_LRR - se_LRR, ymax = mean_LRR + se_LRR), width = 0.2) +
  theme_classic() +
  ylim(0, 1.5) +
  xlab("Effect Measured") +
  ylab("Absolute Value LRR (Behavior)") +
  theme(axis.text.x = element_text(face = "bold", angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(face = "bold")) + 
  scale_x_discrete(labels = c("Distance of Vessel", "Vessel Noise Levels", "Vessel Present/Absent", "Regulations Present/Absent")) 



## FOCAL GROUP  ----------------------------------------------------------



# Calculate the mean and standard error for each EFFECT_CLEAN_JULIA
BEHAVE_GROUP <- BEHAVIOR %>%
  group_by(Focal.Megafauna.Group) %>%
  summarise(
    mean_LRR = mean(LRR_ABSVAL, na.rm = TRUE),
    se_LRR = sd(LRR_ABSVAL, na.rm = TRUE) / sqrt(n())
  )

# Perform ANOVA
anova_BEHAVIOR_group <- aov(LRR_ABSVAL ~ Focal.Megafauna.Group, data = BEHAVIOR)

# Display ANOVA table
summary(anova_BEHAVIOR_group)


# Perform Tukey's HSD post-hoc test
tukey_result <- TukeyHSD(anova_BEHAVIOR_group)

# Display Tukey HSD results
print(tukey_result)

# Create the histogram with error bars
BEHAVE_GROUP <- ggplot(BEHAVE_GROUP, aes(x = Focal.Megafauna.Group, y = mean_LRR)) +
  geom_bar(stat = "identity", fill = "#39568CFF", alpha = 0.5) +
  geom_errorbar(aes(ymin = mean_LRR - se_LRR, ymax = mean_LRR + se_LRR), width = 0.2) +
  theme_classic() +
  ylim(0, 2) +
  xlab("Focal Group") +
  ylab("Absolute Value LRR (Behavior)") +
  theme(axis.text.x = element_text(face = "bold", angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(face = "bold")) 

# Assuming 'BEHAVE_GROUP' is your data frame with mean_LRR and se_LRR
# Assign a placeholder SE for "Marine Reptiles"
BEHAVE_GROUP <- BEHAVE_GROUP %>%
  mutate(se_LRR = ifelse(Focal.Megafauna.Group == "Marine Reptiles", 0.1, se_LRR))  # Replace 0.1 with your preferred placeholder value

# Create the histogram with error bars, including the placeholder SE for Marine Reptiles
BEHAVE_GROUP_PLOT <- ggplot(BEHAVE_GROUP, aes(x = Focal.Megafauna.Group, y = mean_LRR)) +
  geom_bar(stat = "identity", fill = "#39568CFF", alpha = 0.5) +
  geom_errorbar(aes(ymin = mean_LRR - se_LRR, ymax = mean_LRR + se_LRR), width = 0.2) +
  theme_classic() +
  ylim(0, 2) +
  xlab("Focal Group") +
  ylab("Absolute Value LRR (Behavior)") +
  theme(axis.text.x = element_text(face = "bold", angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(face = "bold"))

# Display the plot
print(BEHAVE_GROUP_PLOT)



# behavior graphs  --------------------------------------------------------


ggarrange(effect_BEHAVE, BEHAVE_GROUP_PLOT, iucn_BEHAVIOR, nrow=1, labels= "AUTO", align = "hv")



## IUCN ----------------------------------------------------------



# Calculate the mean and standard error for each EFFECT_CLEAN_JULIA
IUCN <- BEHAVIOR %>%
  group_by(IUCN.Status) %>%
  summarise(
    mean_LRR = mean(LRR_ABSVAL, na.rm = TRUE),
    se_LRR = sd(LRR_ABSVAL, na.rm = TRUE) / sqrt(n())
  )

# Perform ANOVA
anova_BEHAVIOR_iucn <- aov(LRR_ABSVAL ~ IUCN.Status, data = BEHAVIOR)

# Display ANOVA table
summary(anova_BEHAVIOR_iucn)


# Perform Tukey's HSD post-hoc test
tukey_result <- TukeyHSD(anova_BEHAVIOR_iucn )

# Display Tukey HSD results
print(tukey_result)

# Convert Tukey results to a data frame for easier handling
tukey_df <- as.data.frame(tukey_result$IUCN.Status)
tukey_df <- tukey_df %>% mutate(group = rownames(tukey_df))

# Create the histogram with error bars
iucn_BEHAVIOR <- ggplot(IUCN, aes(x = IUCN.Status, y = mean_LRR)) +
  geom_bar(stat = "identity", fill = "#39568CFF", alpha = 0.5) +
  geom_errorbar(aes(ymin = mean_LRR - se_LRR, ymax = mean_LRR + se_LRR), width = 0.2) +
  theme_classic() +
  ylim(0, 2) +
  xlab("IUCN Status") +
  ylab("Absolute Value LRR (Vocalization)") +
  theme(axis.text.x = element_text(face = "bold", angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(face = "bold")) 



## vocalization graphs  -------------------------------------------------
ggarrange(iucn_vocal, group_vocal,effect_vocal, nrow=1, labels= "AUTO", align = "hv")






# POPULATION --------------------------------------------------------------


## Effect Type  -----------------------------------------------------------

POPULATION$LRR_ABSVAL <- abs(POPULATION$LRR)
# Calculate the mean and standard error for each EFFECT_CLEAN_JULIA

# Calculate the mean and standard error for each EFFECT_CLEAN_JULIA
POPULATION_EFFECT<- POPULATION %>%
  group_by(EFFECT_CLEAN_JULIA) %>%
  summarise(
    mean_LRR = mean(LRR_ABSVAL, na.rm = TRUE),
    se_LRR = sd(LRR_ABSVAL, na.rm = TRUE) / sqrt(n())
  )

# Perform ANOVA
anova_result <- aov(LRR_ABSVAL ~ EFFECT_CLEAN_JULIA, data = POPULATION)

# Display ANOVA table
summary(anova_result)


# Perform Tukey's HSD post-hoc test
tukey_result <- TukeyHSD(anova_result)

# Display Tukey HSD results
print(tukey_result)

# Create the histogram with error bars
POP_EFFECT <- ggplot(POPULATION_EFFECT, aes(x = EFFECT_CLEAN_JULIA, y = mean_LRR)) +
  geom_bar(stat = "identity", fill = "#dce319ff", alpha = 0.5) +
  geom_errorbar(aes(ymin = mean_LRR - se_LRR, ymax = mean_LRR + se_LRR), width = 0.2) +
  theme_classic() +
  ylim(0, 4) +
  xlab("Effect Measured") +
  ylab("Absolute Value LRR (Population)") +
  theme(axis.text.x = element_text(face = "bold", angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(face = "bold")) + 
  scale_x_discrete(labels = c("Distance of Vessel", "Vessel Noise Levels", "Vessel Present/Absent", "Regulations Present/Absent")) 



## FOCAL GROUP  ----------------------------------------------------------



# Calculate the mean and standard error for each EFFECT_CLEAN_JULIA
POP_GROUP <- POPULATION %>%
  group_by(Focal.Megafauna.Group) %>%
  summarise(
    mean_LRR = mean(LRR_ABSVAL, na.rm = TRUE),
    se_LRR = sd(LRR_ABSVAL, na.rm = TRUE) / sqrt(n())
  )

# Perform ANOVA
anova_POP_GROUP  <- aov(LRR_ABSVAL ~ Focal.Megafauna.Group, data = POPULATION)

# Display ANOVA table
summary(anova_POP_GROUP)


# Perform Tukey's HSD post-hoc test
tukey_result <- TukeyHSD(anova_POP_GROUP)

# Display Tukey HSD results
print(tukey_result)

# Create the histogram with error bars
POP_GROUP <- ggplot(POP_GROUP, aes(x = Focal.Megafauna.Group, y = mean_LRR)) +
  geom_bar(stat = "identity", fill = "#dce319ff", alpha = 0.5) +
  geom_errorbar(aes(ymin = mean_LRR - se_LRR, ymax = mean_LRR + se_LRR), width = 0.2) +
  theme_classic() +
  ylim(0, 4) +
  xlab("Focal Group") +
  ylab("Absolute Value LRR (Population)") +
  theme(axis.text.x = element_text(face = "bold", angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(face = "bold")) 



## IUCN ----------------------------------------------------------



# Calculate the mean and standard error for each EFFECT_CLEAN_JULIA
IUCN <- POPULATION %>%
  group_by(IUCN.Status) %>%
  summarise(
    mean_LRR = mean(LRR_ABSVAL, na.rm = TRUE),
    se_LRR = sd(LRR_ABSVAL, na.rm = TRUE) / sqrt(n())
  )

# Perform ANOVA
anova_POP_iucn <- aov(LRR_ABSVAL ~ IUCN.Status, data = POPULATION)

# Display ANOVA table
summary(anova_POP_iucn)


# Perform Tukey's HSD post-hoc test
tukey_result <- TukeyHSD(anova_POP_iucn )

# Display Tukey HSD results
print(tukey_result)

# Convert Tukey results to a data frame for easier handling
tukey_df <- as.data.frame(tukey_result$IUCN.Status)
tukey_df <- tukey_df %>% mutate(group = rownames(tukey_df))

# Create the histogram with error bars
POP_IUCN <- ggplot(IUCN, aes(x = IUCN.Status, y = mean_LRR)) +
  geom_bar(stat = "identity", fill = "#dce319ff", alpha = 0.5) +
  geom_errorbar(aes(ymin = mean_LRR - se_LRR, ymax = mean_LRR + se_LRR), width = 0.2) +
  theme_classic() +
  ylim(0, 4) +
  xlab("IUCN Status") +
  ylab("Absolute Value LRR (Population)") +
  theme(axis.text.x = element_text(face = "bold", angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(face = "bold")) 





ggarrange(POP_EFFECT, POP_GROUP, POP_IUCN, nrow=1, labels= "AUTO", align = "hv")





glm