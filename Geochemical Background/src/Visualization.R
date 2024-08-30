# Readings without missing values for each element
Readings <- Background$Zn

# Step 2: Define the Caldist function
Caldist <- function(data) {
    data <- na.omit(data)
    
    min_val <- min(data)
    median_val <- median(data)
    reduceddata <- data[data >= min_val & data <= median_val]
    dist_val <- median_val - reduceddata
    mirrorvalues <- median_val + dist_val
    backgroundvalue <- mean(mirrorvalues) + 2 * sd(mirrorvalues)
    
    return(backgroundvalue)
}

# Step 3: Calculate the Geochemical Background Threshold Value (GBTV) directly on original data
GBTV <- Caldist(Readings)

# Step 4: Divide the dataset into "Geochemical Background" and "Outlier Data"
Geochemical_Background <- Readings[Readings < GBTV]
Outlier_Data <- Readings[Readings >= GBTV]

# Step 5: Calculate statistics for each group
background_stats <- list(
    n = length(Geochemical_Background),
    dataset_percentage = round(length(Geochemical_Background) / length(Readings) * 100, 2),
    mean = round(mean(Geochemical_Background, na.rm = TRUE), 2),
    sd = round(sd(Geochemical_Background, na.rm = TRUE), 2)
)

outlier_stats <- list(
    n = length(Outlier_Data),
    dataset_percentage = round(length(Outlier_Data) / length(Readings) * 100, 2),
    mean = round(mean(Outlier_Data, na.rm = TRUE), 2),
    sd = round(sd(Outlier_Data, na.rm = TRUE), 2)
)

windows(width = 6, height = 6)
# Step 6: Create the histogram with annotations
ggplot(data.frame(Readings), aes(x = Readings)) +
    geom_histogram(aes(y = ..count.., fill = "Zn Concentrations"), bins = 15, color = "black", alpha = 0.7) +
    geom_vline(aes(xintercept = GBTV, color = "GBTV"), linetype = "dashed", size = 1) +
    scale_fill_manual(name = "Legend", values = "white") +
    scale_color_manual(name = "Legend", values = c("GBTV" = "black")) +
    annotate("text", x = GBTV, y = 100, label = paste0("GBTV = ", round(GBTV, 2), " mg/kg"),
             hjust = 0.5, vjust = 1.2, angle = 90, color = "black", size = 4) +
    annotate("text", x = GBTV + 100, y = 150, label = paste0("Geochemical Background\nn = ", background_stats$n, " (", background_stats$dataset_percentage,  "%)",
                                                             "\nmean = ", background_stats$mean,
                                                             "\nSD = ", background_stats$sd),
             hjust = 0, vjust = 1) +
    annotate("text", x = GBTV + 100, y = 50, label = paste0("Outliers Data\nn = ", outlier_stats$n, " (", outlier_stats$dataset_percentage, "%)",
                                                            "\nmean = ", outlier_stats$mean,
                                                            "\nSD = ", outlier_stats$sd),
             hjust = 0, vjust = 1) +
    scale_x_continuous(
        breaks = scales::pretty_breaks(n = 10)    # Ensure appropriate number of breaks
    ) +
    labs( x = "Zn Concentration (mg/kg)", y = "Frequency") +
    theme_classic() +
    theme(legend.position = "none")

####################################################################################
# Readings without missing values
Readings <- na.omit(Background$Pb)

# Step 2: Calculate the Geochemical Background Threshold Value (GBTV) directly on original data
GBTV <- Caldist(Background$Pb)

# Step 3: Divide the dataset into "Geochemical Background" and "Outlier Data"
Geochemical_Background <- Readings[Readings < GBTV]
Outlier_Data <- Readings[Readings >= GBTV]

# Step 4: Calculate statistics for each group
background_stats <- list(
    n = length(Geochemical_Background),
    dataset_percentage = round(length(Geochemical_Background) / length(Readings) * 100, 2),
    mean = round(mean(Geochemical_Background), 2),
    sd = round(sd(Geochemical_Background), 2)
)

outlier_stats <- list(
    n = length(Outlier_Data),
    dataset_percentage = round(length(Outlier_Data) / length(Readings) * 100, 2),
    mean = round(mean(Outlier_Data), 2),
    sd = round(sd(Outlier_Data), 2)
)

windows(width = 6, height = 6)
# Step 6: Create the histogram with annotations
ggplot(data.frame(Readings), aes(x = Readings)) +
    geom_histogram(aes(y = ..count.., fill = "Pb Concentrations"), bins = 15, color = "black", alpha = 0.7) +
    geom_vline(aes(xintercept = GBTV, color = "GBTV"), linetype = "dashed", size = 1) +
    scale_fill_manual(name = "Legend", values = "white") +
    scale_color_manual(name = "Legend", values = c("GBTV" = "black")) +
    annotate("text", x = GBTV, y = 100, label = paste0("GBTV = ", round(GBTV, 2), " mg/kg"),
             hjust = 0.5, vjust = 1.2, angle = 90, color = "black", size = 4) +
    annotate("text", x = GBTV + 50, y = 150, label = paste0("Geochemical Background\nn = ", background_stats$n, " (", background_stats$dataset_percentage,  "%)",
                                                            "\nmean = ", background_stats$mean,
                                                            "\nSD = ", background_stats$sd),
             hjust = 0, vjust = 1) +
    annotate("text", x = GBTV + 50, y = 50, label = paste0("Outliers Data\nn = ", outlier_stats$n, " (", outlier_stats$dataset_percentage, "%)",
                                                           "\nmean = ", outlier_stats$mean,
                                                           "\nSD = ", outlier_stats$sd),
             hjust = 0, vjust = 1) +
    scale_x_continuous(
        breaks = scales::pretty_breaks(n = 10)    # Ensure appropriate number of breaks
    ) +
    labs( x = "Pb Concentration (mg/kg)", y = "Frequency") +
    theme_classic() +
    theme(legend.position = "none")

################################################################################
# Readings without missing values
Readings <- na.omit(Background$Cu)

# Remove the maximum value from the Readings vector
Readings <- Readings[-which.max(Readings)]
length(Readings)
length(Background$Cu)

# Step 2: Calculate the Geochemical Background Threshold Value (GBTV) directly on original data
GBTV <- Caldist(Background$Cu)

# Step 3: Divide the dataset into "Geochemical Background" and "Outlier Data"
Geochemical_Background <- Readings[Readings < GBTV]
Outlier_Data <- Readings[Readings >= GBTV]

# Step 4: Calculate statistics for each group
background_stats <- list(
    n = length(Geochemical_Background),
    dataset_percentage = round(length(Geochemical_Background) / length(Readings) * 100, 2),
    mean = round(mean(Geochemical_Background), 2),
    sd = round(sd(Geochemical_Background), 2)
)

outlier_stats <- list(
    n = length(Outlier_Data),
    dataset_percentage = round(length(Outlier_Data) / length(Readings) * 100, 2),
    mean = round(mean(Outlier_Data), 2),
    sd = round(sd(Outlier_Data), 2)
)

windows(width = 6, height = 6)
library(ggplot2)
# Step 6: Create the histogram with annotations
ggplot(data.frame(Readings), aes(x = Readings)) +
    geom_histogram(aes(y = ..count.., fill = "Pb Concentrations"), bins = 15, color = "black", alpha = 0.7) +
    geom_vline(aes(xintercept = GBTV, color = "GBTV"), linetype = "dashed", size = 1) +
    scale_fill_manual(name = "Legend", values = "white") +
    scale_color_manual(name = "Legend", values = c("GBTV" = "black")) +
    annotate("text", x = GBTV, y = 100, label = paste0("GBTV = ", round(GBTV, 2), " mg/kg"),
             hjust = 0.5, vjust = 1.2, angle = 90, color = "black", size = 4) +
    annotate("text", x = GBTV - 15, y = 200, label = paste0("Geochemical\nBackground\nn = ", background_stats$n, " (", background_stats$dataset_percentage,  "%)",
                                                            "\nmean = ", background_stats$mean,
                                                            "\nSD = ", background_stats$sd),
             hjust = 0, vjust = 1) +
    annotate("text", x = GBTV + 5, y = 50, label = paste0("Outliers Data\nn = ", outlier_stats$n, " (", outlier_stats$dataset_percentage, "%)",
                                                          "\nmean = ", outlier_stats$mean,
                                                          "\nSD = ", round(outlier_stats$sd, 2)),
             hjust = 0, vjust = 1) +
    scale_x_continuous(
        breaks = scales::pretty_breaks(n = 10)    # Ensure appropriate number of breaks
    ) +
    labs(x = "Cu Concentration (mg/kg)", y = "Frequency") +
    theme_classic() +
    theme(legend.position = "none")

#################################################################################
# Readings without missing values
Readings <- na.omit(Background$Cd)

# Step 2: Calculate the Geochemical Background Threshold Value (GBTV) directly on original data
GBTV <- Caldist(Background$Cd)

# Step 3: Divide the dataset into "Geochemical Background" and "Outlier Data"
Geochemical_Background <- na.omit(Background$Cd[Background$Cd < GBTV])
Outlier_Data <- na.omit(Background$Cd[Background$Cd >= GBTV])

# Step 4: Calculate statistics for each group
background_stats <- list(
    n = length(Geochemical_Background),
    dataset_percentage = round(length(Geochemical_Background) / length(Readings) * 100, 2),
    mean = round(mean(Geochemical_Background), 2),
    sd = round(sd(Geochemical_Background), 2)
)

outlier_stats <- list(
    n = length(Outlier_Data),
    dataset_percentage = round(length(Outlier_Data) / length(Readings) * 100, 2),
    mean = round(mean(Outlier_Data), 2),
    sd = round(sd(Outlier_Data), 2)
)

windows(width = 6, height = 6)
# Step 6: Create the histogram with annotations
ggplot(data.frame(Readings), aes(x = Readings)) +
    geom_histogram(aes(y = ..count.., fill = "Pb Concentrations"), bins = 15, color = "black", alpha = 0.7) +
    geom_vline(aes(xintercept = GBTV, color = "GBTV"), linetype = "dashed", size = 1) +
    scale_fill_manual(name = "Legend", values = "white") +
    scale_color_manual(name = "Legend", values = c("GBTV" = "black")) +
    annotate("text", x = GBTV, y = 150, label = paste0("GBTV = ", round(GBTV, 2), " mg/kg"),
             hjust = 0.5, vjust = 1.2, angle = 90, color = "black", size = 4) +
    annotate("text", x = GBTV - 1.5, y = 200, label = paste0("Geochemical Background\nn = ", background_stats$n, " (", background_stats$dataset_percentage,  "%)",
                                                             "\nmean = ", background_stats$mean,
                                                             "\nSD = ", background_stats$sd),
             hjust = 0, vjust = 1) +
    annotate("text", x = GBTV + 0.2, y = 60, label = paste0("Outliers Data\nn = ", outlier_stats$n, " (", outlier_stats$dataset_percentage, "%)",
                                                            "\nmean = ", outlier_stats$mean,
                                                            "\nSD = ", outlier_stats$sd),
             hjust = 0, vjust = 1) +
    scale_x_continuous(
        breaks = scales::pretty_breaks(n = 10)    # Ensure appropriate number of breaks
    ) +
    labs( x = "Cd Concentration (mg/kg)", y = "Frequency") +
    theme_classic() +
    theme(legend.position = "none")

################################################################################
# Readings without missing values
Readings <- na.omit(BackgroundpXRF$As)
length(Readings)

# Step 23: Calculate the Geochemical Background Threshold Value (GBTV) directly on original data
GBTV <- Caldist(Readings)

# Step 3: Divide the dataset into "Geochemical Background" and "Outlier Data"
Geochemical_Background <- Readings[Readings < GBTV]
Outlier_Data <- Readings[Readings >= GBTV]

# Step 4: Calculate statistics for each group
background_stats <- list(
    n = length(Geochemical_Background),
    dataset_percentage = round(length(Geochemical_Background) / length(Readings) * 100, 2),
    mean = round(mean(Geochemical_Background), 2),
    sd = round(sd(Geochemical_Background), 2)
)

outlier_stats <- list(
    n = length(Outlier_Data),
    dataset_percentage = round(length(Outlier_Data) / length(Readings) * 100, 2),
    mean = round(mean(Outlier_Data), 2),
    sd = round(sd(Outlier_Data), 2)
)

windows(width = 6, height = 6)
# Step 6: Create the histogram with annotations
ggplot(data.frame(Readings), aes(x = Readings)) +
    geom_histogram(aes(y = ..count.., fill = "Pb Concentrations"), bins = 15, color = "black", alpha = 0.7) +
    geom_vline(aes(xintercept = GBTV, color = "GBTV"), linetype = "dashed", size = 1) +
    scale_fill_manual(name = "Legend", values = "white") +
    scale_color_manual(name = "Legend", values = c("GBTV" = "black")) +
    annotate("text", x = GBTV, y = 120, label = paste0("GBTV = ", round(GBTV, 2), " mg/kg"),
             hjust = 0.5, vjust = 1.2, angle = 90, color = "black", size = 4) +
    annotate("text", x = GBTV - 13, y = 200, label = paste0("Geochemical Background\nn = ", background_stats$n, " (", background_stats$dataset_percentage,  "%)",
                                                            "\nmean = ", background_stats$mean,
                                                            "\nSD = ", background_stats$sd),
             hjust = 0, vjust = 1) +
    annotate("text", x = GBTV + 7, y = 60, label = paste0("Outliers Data\nn = ", outlier_stats$n, " (", outlier_stats$dataset_percentage, "%)",
                                                          "\nmean = ", outlier_stats$mean,
                                                          "\nSD = ", outlier_stats$sd),
             hjust = 0, vjust = 1) +
    scale_x_continuous(
        breaks = scales::pretty_breaks(n = 10)    # Ensure appropriate number of breaks
    ) +
    labs( x = "As Concentration (mg/kg)", y = "Frequency") +
    theme_classic() +
    theme(legend.position = "none")

################################################################################
Readings <- na.omit(BackgroundpXRF$Cr)
length(Readings)

# Step 2: Calculate the Geochemical Background Threshold Value (GBTV) directly on original data
GBTV <- Caldist(Readings)

# Step 3: Divide the dataset into "Geochemical Background" and "Outlier Data"
Geochemical_Background <- Readings[Readings < GBTV]
Outlier_Data <- Readings[Readings >= GBTV]

# Step 4: Calculate statistics for each group
background_stats <- list(
    n = length(Geochemical_Background),
    dataset_percentage = round(length(Geochemical_Background) / length(Readings) * 100, 2),
    mean = round(mean(Geochemical_Background), 2),
    sd = round(sd(Geochemical_Background), 2)
)

outlier_stats <- list(
    n = length(Outlier_Data),
    dataset_percentage = round(length(Outlier_Data) / length(Readings) * 100, 2),
    mean = round(mean(Outlier_Data), 2),
    sd = round(sd(Outlier_Data), 2)
)

windows(width = 6, height = 6)
# Step 6: Create the histogram with annotations
ggplot(data.frame(Readings), aes(x = Readings)) +
    geom_histogram(aes(y = ..count.., fill = "Pb Concentrations"), bins = 15, color = "black", alpha = 0.7) +
    geom_vline(aes(xintercept = GBTV, color = "GBTV"), linetype = "dashed", size = 1) +
    scale_fill_manual(name = "Legend", values = "white") +
    scale_color_manual(name = "Legend", values = c("GBTV" = "black")) +
    annotate("text", x = GBTV, y = 120, label = paste0("GBTV = ", round(GBTV, 2), " mg/kg"),
             hjust = 0.5, vjust = 1.2, angle = 90, color = "black", size = 4) +
    annotate("text", x = GBTV + 10, y = 200, label = paste0("Geochemical Background\nn = ", background_stats$n, " (", background_stats$dataset_percentage,  "%)",
                                                            "\nmean = ", background_stats$mean,
                                                            "\nSD = ", background_stats$sd),
             hjust = 0, vjust = 1) +
    annotate("text", x = GBTV + 100, y = 60, label = paste0("Outliers Data\nn = ", outlier_stats$n, " (", outlier_stats$dataset_percentage, "%)",
                                                            "\nmean = ", outlier_stats$mean,
                                                            "\nSD = ", outlier_stats$sd),
             hjust = 0, vjust = 1) +
    scale_x_continuous(
        breaks = scales::pretty_breaks(n = 10)    # Ensure appropriate number of breaks
    ) +
    labs( x = "Cr Concentration (mg/kg)", y = "Frequency") +
    theme_classic() +
    theme(legend.position = "none")
