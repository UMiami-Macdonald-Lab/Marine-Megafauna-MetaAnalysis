# Load Required Libraries
library(ggplot2)
library(dplyr)
library(ggpubr)
library(viridis)
library(sf)

# Function to calculate summary stats
summarize_lrr <- function(data, group_var) {
  data %>%
    group_by({{ group_var }}) %>%
    summarise(
      mean_LRR = mean(abs(LRR), na.rm = TRUE),
      se_LRR = sd(abs(LRR), na.rm = TRUE) / sqrt(n())
    )
}

# Function to create bar plot with error bars
plot_lrr <- function(summary_df, xvar, fill_color, ylabel, labels_vec = NULL, ymax = 2) {
  p <- ggplot(summary_df, aes(x = {{ xvar }}, y = mean_LRR)) +
    geom_bar(stat = "identity", fill = fill_color, alpha = 0.6) +
    geom_errorbar(aes(ymin = mean_LRR - se_LRR, ymax = mean_LRR + se_LRR), width = 0.2) +
    theme_classic() +
    ylim(0, ymax) +
    ylab(paste0("|LRR| (", ylabel, ")")) +
    xlab(NULL) +
    theme(
      axis.text.x = element_text(face = "bold", angle = 45, hjust = 1, size = 11),
      axis.text.y = element_text(face = "bold", size = 11),
      axis.title.y = element_text(size = 13)
    )
  if (!is.null(labels_vec)) {
    p <- p + scale_x_discrete(labels = labels_vec)
  }
  return(p)
}


# Colors for different response types
colors <- list(
  "POPULATION" = "#CC3363",
  "PHYSIOLOGY" = "#9AD1D4",
  "VOCALIZATION" = "#436436",
  "BEHAVIOR" = "#20063B"
)

# Function to create all plots for a dataset
generate_plots <- function(data, type_label, fill_color, effect_labels = NULL, ymax = 2) {
  data$LRR_ABSVAL <- abs(data$LRR)
  
  effect_summary <- summarize_lrr(data, EFFECT_CLEAN_JULIA)
  group_summary  <- summarize_lrr(data, Focal.Megafauna.Group)
  iucn_summary   <- summarize_lrr(data, IUCN.Status)
  
  p1 <- plot_lrr(effect_summary, EFFECT_CLEAN_JULIA, fill_color, type_label, labels_vec = effect_labels, ymax = ymax)
  p2 <- plot_lrr(group_summary, Focal.Megafauna.Group, fill_color, type_label, ymax = ymax)
  p3 <- plot_lrr(iucn_summary, IUCN.Status, fill_color, type_label, ymax = ymax)
  
  return(list(effect = p1, group = p2, iucn = p3))
}


# Replace these with your actual data frames
# POPULATION, PHYSIOLOGY, VOCALIZATION, BEHAVIOR must be loaded beforehand

plots_population <- generate_plots(POPULATION, "Abundance", colors$POPULATION,
                                   effect_labels = c("Distance of Vessel", "Vessel Present/Absent", "Regulations Present/Absent"))

plots_physiology <- generate_plots(PHYSIOLOGY, "Physiology", colors$PHYSIOLOGY,
                                   effect_labels = c("Vessel Noise Levels", "Vessel Present/Absent"))

plots_vocal <- generate_plots(VOCALIZATION, "Vocalization", colors$VOCALIZATION,
                              effect_labels = c("Distance of Vessel", "Vessel Noise Levels", "Vessel Present/Absent", "Regulations Present/Absent"),
                              ymax = 1.5)

plots_behavior <- generate_plots(BEHAVIOR, "Behavior", colors$BEHAVIOR,
                                 effect_labels = c("Distance of Vessel", "Vessel Noise Levels", "Vessel Present/Absent", "Regulations Present/Absent"))


# Arrange all subplots into a grid
final_plot <- ggarrange(
  plots_population$effect, plots_population$group, plots_population$iucn,
  plots_physiology$effect, plots_physiology$group, plots_physiology$iucn,
  plots_vocal$effect, plots_vocal$group, plots_vocal$iucn,
  plots_behavior$effect, plots_behavior$group, plots_behavior$iucn,
  ncol = 3, nrow = 4, labels = "auto", align = "hv", common.legend = TRUE, legend = "bottom"
)

ggsave("final_meta_analysis_plot.png", final_plot, width = 22, height = 16, dpi = 300)
