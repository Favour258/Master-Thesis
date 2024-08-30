#Descriptive Statistics of Heavy Metals
# Load necessary libraries
install.packages(c("dplyr", "ggplot2", "cowplot"))
library(dplyr)
library(ggplot2)
library(cowplot)

# Filter and preprocess the data
Heavy_metals_long6 <- Heavy_metals %>%
    filter(urbimpact != "Forest" & urbimpact != "Rural") %>%  # Filter out specific urbimpact values
    filter(sample_id != 317  & sample_id != 432) %>%  # Filter out specific rows based on sample_id
    select(sample_id, field_id, urbimpact, Cr, Cd, Zn, Cu, Pb, As) %>%  # Select relevant columns
    droplevels()  # Drop unused factor levels

# Create individual plots for each heavy metal
plot1 <- ggplot(Heavy_metals_long6, aes(x = urbimpact, y = Zn)) +
    geom_boxplot(alpha = 0.5, width = 0.5) +  # Reduce the opacity of the boxplot
    geom_jitter(width = 0.1, alpha = 0.5) +  # Jittered points with reduced opacity
    stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +  # Mean points
    stat_summary(fun = min, geom = "point", shape = 8, size = 3, color = "blue") +  # Min points
    stat_summary(fun = max, geom = "point", shape = 8, size = 3, color = "green") +  # Max points
    theme_classic() +
    labs(title = "Zn", y = "Concentration (mg/kg)", x = "Urban Intensity") + 
    geom_hline(yintercept = 50, linetype = "dashed", color = "red", size = 0.7)

plot2 <- ggplot(Heavy_metals_long6, aes(x = urbimpact, y = Cu)) +
    geom_boxplot(alpha = 0.5, width = 0.5) +  # Reduce the opacity of the boxplot
    geom_jitter(width = 0.1, alpha = 0.5) +  # Jittered points with reduced opacity
    stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +  # Mean points
    stat_summary(fun = min, geom = "point", shape = 8, size = 3, color = "blue") +  # Min points
    stat_summary(fun = max, geom = "point", shape = 8, size = 3, color = "green") +  # Max points
    theme_classic() +
    labs(title = "Cu", y = "Concentration (mg/kg)", x = "Urban Intensity") + 
    geom_hline(yintercept = 36, linetype = "dashed", color = "red", size = 0.7)

plot3 <- ggplot(Heavy_metals_long6, aes(x = urbimpact, y = Cd)) +
    geom_boxplot(alpha = 0.5, width = 0.5) +  # Reduce the opacity of the boxplot
    geom_jitter(width = 0.1, alpha = 0.5) +  # Jittered points with reduced opacity
    stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +  # Mean points
    stat_summary(fun = min, geom = "point", shape = 8, size = 3, color = "blue") +  # Min points
    stat_summary(fun = max, geom = "point", shape = 8, size = 3, color = "green") +  # Max points
    theme_classic() +
    labs(title = "Cd", y = "Concentration (mg/kg)", x = "Urban Intensity") + 
    geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", size = 0.7)

plot4 <- ggplot(Heavy_metals_long6, aes(x = urbimpact, y = Cr)) +
    geom_boxplot(alpha = 0.5, width = 0.5) +  # Reduce the opacity of the boxplot
    geom_jitter(width = 0.1, alpha = 0.5) +  # Jittered points with reduced opacity
    stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +  # Mean points
    stat_summary(fun = min, geom = "point", shape = 8, size = 3, color = "blue") +  # Min points
    stat_summary(fun = max, geom = "point", shape = 8, size = 3, color = "green") +  # Max points
    theme_classic() +
    labs(title = "Cr", y = "Concentration (mg/kg)", x = "Urban Intensity") + 
    geom_hline(yintercept = 100, linetype = "dashed", color = "red", size = 0.7)

plot5 <- ggplot(Heavy_metals_long6, aes(x = urbimpact, y = Pb)) +
    geom_boxplot(alpha = 0.5, width = 0.5) +  # Reduce the opacity of the boxplot
    geom_jitter(width = 0.1, alpha = 0.5) +  # Jittered points with reduced opacity
    stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +  # Mean points
    stat_summary(fun = min, geom = "point", shape = 8, size = 3, color = "blue") +  # Min points
    stat_summary(fun = max, geom = "point", shape = 8, size = 3, color = "green") +  # Max points
    theme_classic() +
    labs(title = "Pb", y = "Concentration (mg/kg)", x = "Urban Intensity") + 
    geom_hline(yintercept = 85, linetype = "dashed", color = "red", size = 0.7)

plot6 <- ggplot(Heavy_metals_long6, aes(x = urbimpact, y = As)) +
    geom_boxplot(alpha = 0.5, width = 0.5) +  # Reduce the opacity of the boxplot
    geom_jitter(width = 0.1, alpha = 0.5) +  # Jittered points with reduced opacity
    stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +  # Mean points
    stat_summary(fun = min, geom = "point", shape = 8, size = 3, color = "blue") +  # Min points
    stat_summary(fun = max, geom = "point", shape = 8, size = 3, color = "green") +  # Max points
    theme_classic() +
    labs(title = "As", y = "Concentration (mg/kg)", x = "Urban Intensity") + 
    geom_hline(yintercept = 12, linetype = "dashed", color = "red", size = 0.7)

# Create a base plot to extract the legend
plot_base <- ggplot(Heavy_metals_long6, aes(x = urbimpact, y = Cr)) +
    geom_boxplot(alpha = 0.5, width = 0.5) +  # Reduce the opacity of the boxplot
    geom_jitter(width = 0.1, alpha = 0.5) +  # Jittered points with reduced opacity
    stat_summary(fun = mean, geom = "point", shape = 20, size = 3, aes(color = "Mean")) +  # Mean points
    stat_summary(fun = min, geom = "point", shape = 8, size = 3, aes(color = "Min")) +  # Min points
    stat_summary(fun = max, geom = "point", shape = 8, size = 3, aes(color = "Max")) +  # Max points
    geom_hline(aes(yintercept = 12, linetype = "WHO"), color = "red", size = 0.7) +  # WHO guideline
    scale_color_manual(name = "Summary Points",
                       values = c("Min" = "blue", "Max" = "green", "Mean" = "red")) +
    scale_linetype_manual(name = "Guidelines",
                          values = c("WHO" = "dashed")) +
    theme_classic() +
    labs(title = "As", y = "Concentration (mg/kg)", x = "Urban Intensity") +
    guides(color = guide_legend(order = 1), linetype = guide_legend(order = 2))

# Function to extract the legend
g_legend <- function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}

# Combine plots into a grid
plots_grid <- cowplot::plot_grid(plot1, plot3, plot6,
                                 plot4, plot2, plot5,
                                 nrow = 2, ncol = 3,
                                 align = 'hv')

# Extract the legend
legend <- g_legend(plot_base + theme(legend.position = "bottom"))

# Combine the plot grid with the legend
final_plot <- cowplot::plot_grid(
    plots_grid,
    legend,
    ncol = 1,
    rel_heights = c(10, 1)
)

# Print the final combined plot
print(final_plot)

###################################################################################
#Enrichment Factor 
plot_EF <- Heavy_metals_long %>%
    filter(sample_id != 334) %>%  # Remove the specific sample id from the plot
    filter(!is.na(Concentration)) %>% 
    ggplot(aes(x = Metal_EF, y = Concentration, fill = Metal_EF)) + 
    geom_boxplot() + 
    theme_classic() +
    theme(legend.position = "none",
          strip.text = element_text(size = 12)) +  # Adjust facet label text size if needed
    scale_x_discrete(labels = function(x) gsub("_EF", "", x)) +  # Remove "_EF" from x-axis labels
    labs(
        x = "Heavy Metals",
        y = "Enrichment Factor") +
    geom_hline(yintercept = 2, linetype = "dashed", color = "blue") +  # Deficiency to minimal enrichment threshold
    geom_hline(yintercept = 5, linetype = "dashed", color = "green") +  # Moderate enrichment threshold
    geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +  # Significant enrichment threshold
    geom_hline(yintercept = 40, linetype = "dashed", color = "red") +  # Very high enrichment threshold
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 2, ymax = 5, alpha = 0.2, fill = "blue") +  # Moderate enrichment
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 5, ymax = 20, alpha = 0.2, fill = "green") +  # Significant enrichment
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 20, ymax = 40, alpha = 0.2, fill = "orange") +  # Very high enrichment
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 40, ymax = Inf, alpha = 0.2, fill = "red")  # Extremely high enrichment
print(plot_EF)


#Contamination Factor
plot_CF <- Heavy_metals_long2 %>%
    filter(sample_id != 334) %>%
    filter(!is.na(Concentration)) %>%  # Remove rows with NA values in Concentration
    ggplot(aes(x = urbimpact, y = Concentration, fill = urbimpact)) +
    geom_boxplot() +
    theme_classic() +
    theme(
        legend.position = "none",
        strip.text = element_text(size = 12),
        axis.title.x = element_blank()  # Remove x-axis label
    ) +
    scale_x_discrete(labels = function(x) gsub("_CF", "", x)) +  # Remove "_CF" from x-axis labels
    scale_fill_discrete(labels = function(x) gsub("_CF", "", x)) +  # Remove "_CF" from legend labels
    facet_wrap(~Metal_CF, scales = "free_x", labeller = labeller(Metal_CF = function(x) gsub("_CF", "", x))) +  # Facet by Metal_CF with custom labels
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 1, ymax = 3, alpha = 0.2, fill = "yellow") +  # Moderate contamination (1-3)
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 3, ymax = 6, alpha = 0.2, fill = "orange") +  # Significant contamination (3-6)
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 6, ymax = Inf, alpha = 0.2, fill = "red") +  # High contamination (>=6)
    labs(y = "Contamination Factor") +  # Only y-axis label
    geom_hline(yintercept = 1, linetype = "dashed", color = "blue") +  # Threshold for minimal contamination
    geom_hline(yintercept = 3, linetype = "dashed", color = "orange") +  # Threshold for moderate contamination
    geom_hline(yintercept = 6, linetype = "dashed", color = "red")  # Threshold for significant contamination

print(plot_CF)

#PLI
Heavy_metals_filtered <- Heavy_metals %>%
    filter( urbimpact != "Rural")
plot_PLI <- ggplot(Heavy_metals_filtered, aes(x = urbimpact, y = PLI, fill = urbimpact)) +
    geom_boxplot() +
    theme_classic() +
    labs(
        y = "Pollution Load Index (PLI)",  # Only y-axis label
        x = NULL  # Remove x-axis title
    ) +
    theme(
        legend.position = "none",
        strip.text = element_text(size = 12),
        axis.title.x = element_blank()  # Remove x-axis title
    ) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "blue") +  # Threshold for unpolluted
    geom_hline(yintercept = 3, linetype = "dashed", color = "green") +  # Threshold for moderately polluted
    geom_hline(yintercept = 5, linetype = "dashed", color = "orange") +  # Threshold for highly polluted
    geom_hline(yintercept = Inf, linetype = "dashed", color = "red") +  # Threshold for very highly polluted
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 1, ymax = 3, alpha = 0.1, fill = "blue") +  # Moderately polluted
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 3, ymax = 5, alpha = 0.1, fill = "green") +  # Highly polluted
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 5, ymax = Inf, alpha = 0.1, fill = "orange")  # Very highly polluted

print(plot_PLI)

# Create the plot for PERI
plot_PERI <- Heavy_metals_long88 %>%
    filter(!is.na(Concentration)) %>%  # Remove rows with NA values in Concentration
    ggplot(aes(x = urbimpact, y = Concentration, fill = urbimpact)) +
    geom_boxplot() +
    theme_classic() +
    theme(
        legend.position = "none",
        strip.text = element_text(size = 12),
        axis.title.x = element_blank()  # Remove x-axis label
    ) +
    scale_x_discrete(labels = function(x) gsub("_PERI", "", x)) +  # Remove "_PERI" from x-axis labels
    facet_wrap(~Metal_PERI, scales = "free_x", labeller = labeller(Metal_PERI = function(x) gsub("_PERI", "", x))) +  # Facet by Metal_PERI with custom labels
    labs(y = "Potential Ecological Risk (PERI)") +  # Only y-axis label
    geom_hline(yintercept = 40, linetype = "dashed", color = "blue") +  # Threshold for low risk
    geom_hline(yintercept = 80, linetype = "dashed", color = "green") +  # Threshold for moderate risk
    geom_hline(yintercept = 160, linetype = "dashed", color = "orange") +  # Threshold for considerable risk
    geom_hline(yintercept = 320, linetype = "dashed", color = "red") +  # Threshold for high risk
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 40, ymax = 80, alpha = 0.2, fill = "blue") +  # Moderate risk
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 80, ymax = 160, alpha = 0.2, fill = "green") +  # Considerable risk
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 160, ymax = 320, alpha = 0.2, fill = "orange") +  # High risk
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 320, ymax = Inf, alpha = 0.2, fill = "red")  # Very high risk

print(plot_PERI)

#################################################################################################################
#Human Health Assessment 
#Visualize Hazard Index in Adults 
Heavy_metals_long3 <- Heavy_metals %>% 
    pivot_longer(cols = c(HI_Pb_Adt, HI_As_Adt, HI_Ni_Adt, HI_Zn_Adt, HI_Cr_Adt, HI_Mn_Adt, HI_Cd_Adt, HI_Cu_Adt),
                 names_to = "Metals",
                 values_to = "Index")

ggplot(Heavy_metals_long3, aes(x=Metals, y=Index, fill=Metals)) +
    geom_boxplot() +
    theme_classic()

#Visualize hazard Quotient
library(dplyr)
library(ggplot2)
library(tidyr)

# Step 1: Pivot longer and separate the Metals column into Metal and Group
Heavy_metals_long20 <- Heavy_metals %>%
    pivot_longer(cols = c(HQing_As_Adt,HQing_Pb_Adt,HQing_Zn_Adt,HQing_Cr_Adt,HQing_Cu_Adt,HQing_Cd_Adt,
                          HQing_As_Chd, HQing_Cd_Chd, HQing_Cr_Chd, HQing_Cu_Chd, HQing_Pb_Chd, HQing_Zn_Chd),
                 names_to = "Metals",
                 values_to = "HQ_Value") %>%
    separate(Metals, into = c("HQ", "Metal", "Group"), sep = "_")

Heavy_metals_long22 <- Heavy_metals %>%
    pivot_longer(cols = c(HQinhl_As_Adt, HQinhl_Cd_Adt, HQinhl_Cr_Adt, HQinhl_Cu_Adt, HQinhl_Pb_Adt,
                          HQinhl_Zn_Adt, HQinhl_As_Chd, HQinhl_Cd_Chd, HQinhl_Cr_Chd, HQinhl_Cu_Chd, HQinhl_Pb_Chd,
                          HQinhl_Zn_Chd), 
                 names_to = "Metals",
                 values_to = "HQ_Value") %>%
    separate(Metals, into = c("HQ", "Metal", "Group"), sep = "_" )

Heavy_metals_long23 <- Heavy_metals %>%
    pivot_longer(cols = c(HQderm_Pb_Adt, HQderm_As_Adt, HQderm_Zn_Adt, HQderm_Cr_Adt, HQderm_Cu_Adt,
                          HQderm_Cd_Adt, HQderm_As_Chd, HQderm_Cd_Chd, HQderm_Cr_Chd, HQderm_Cu_Chd,
                          HQderm_Pb_Chd, HQderm_Zn_Chd),
                 names_to = "Metals",
                 values_to = "HQ_Value") %>%
    separate(Metals, into = c("HQ", "Metal", "Group"), sep = "_" )

# Step 2: Create a boxplot faceted by Adult/Child
ggplot(Heavy_metals_long23, aes(x = Metal, y = HQ_Value, fill = Group)) +
    geom_boxplot() +
    facet_wrap(~ Group, labeller = as_labeller(c("Adt" = "Adult", "Chd" = "Child"))) +
    theme_classic() +
    labs(title = "Hazard Quotients by Dermal Contact Route",
         x = "Heavy Metal",
         y = "Hazard Quotient (HQ Value)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
