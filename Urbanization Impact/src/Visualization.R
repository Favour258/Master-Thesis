# Load necessary libraries
library(ggplot2)
library(dplyr)
library(agricolae)


#Pb
par(mfrow = c(2, 2))
plot(lm(Pb_sqrt ~ urbimpact, data = Cleaned_Pb))

#Add fitted values from the model to the dataframe
Cleaned_Pb$fitted_sqrt <- fitted(pb_model)
Cleaned_Pb$fitted <- Cleaned_Pb$fitted_sqrt^2

# Create a numeric version of the urbanization factor
Cleaned_Pb$urbimpact_num <- as.numeric(Cleaned_Pb$urbimpact)

# Perform ANOVA and Tukey HSD test
anova_result <- aov(Pb_sqrt ~ urbimpact, data = Cleaned_Pb)
hsd <- HSD.test(anova_result, "urbimpact", group = TRUE)

# Extract letters and convert to a data frame
letters_df <- data.frame(urbimpact = rownames(hsd$groups), Letters = hsd$groups$groups)
rownames(letters_df) <- NULL

# Calculate maximum values for each category to place significance letters
max_values <- Cleaned_Pb %>%
    group_by(urbimpact) %>%
    summarise(Max = max(Pb_sqrt))

# Merge max values with letters
summary_stats <- left_join(letters_df, max_values, by = "urbimpact")

# Merge letters with the main data frame
Cleaned_Pb <- merge(Cleaned_Pb, letters_df, by = "urbimpact", all.x = TRUE)

# Plot observed values as a boxplot and overlay raw fitted values with dodging and quadratic fit line
p1 <- ggplot(Cleaned_Pb, aes(x = urbimpact, y = Pb_sqrt)) +
    geom_boxplot(width = 0.5, alpha = 0.6) +  # Boxplot for transformed data, reduced width
    geom_jitter(aes(y = Pb_sqrt), color = "blue", size = 1.5, width = 0.1, alpha = 0.6) +  # Raw observed values with minimal spread
    geom_point(aes(y = fitted_sqrt), color = "red", size = 2, position = position_dodge(width = 0.75)) +  # Fitted values in sqrt scale with dodging
    geom_smooth(aes(x = urbimpact_num, y = Pb_sqrt), method = "lm", formula = y ~ poly(x, 2), color = "red", linetype = "solid") +  # Quadratic fit line
    geom_text(data = summary_stats, aes(x = urbimpact, y = Max + 0.5, label = Letters), size = 6) +  # Add significance letters
    scale_x_discrete(labels = levels(Cleaned_Pb$urbimpact)) +  # Use original labels for x-axis
    labs(x = "Urbanization Intensity", y = "Square Root Pb Concentration (mg/kg)") +
    theme_classic() +
    theme(legend.position = "none")

print(p1)
##########################################################################################################
#As
#Pairwise Comparison 
anova_result <- aov(As_log~ urbimpact, data = Cleaned_As)
hsd <- HSD.test(anova_result, "urbimpact", group = TRUE)

#Add fitted values from the model to the dataframe
Cleaned_As$fitted_log <- fitted(As_model)
Cleaned_As$fitted <- exp(Cleaned_As$fitted_log)

# Create a numeric version of the urbanization factor
Cleaned_As$urbimpact_num <- as.numeric(Cleaned_As$urbimpact)

#Visualization 
# Extract letters and convert to a data frame
letters_df <- data.frame(urbimpact = rownames(hsd$groups), Letters = hsd$groups$groups)
rownames(letters_df) <- NULL

# Calculate maximum values for each category
max_values <- Cleaned_As %>%
    group_by(urbimpact) %>%
    summarise(Max = max(As))

# Merge max values with summary statistics
summary_stats <- left_join(max_values, letters_df, by = "urbimpact")

# Merging summary_stats with Cleaned_Pb on 'urbimpact' to include the 'Letters' information
Cleaned_As <- merge(Cleaned_As, letters_df, by = "urbimpact", all.x = TRUE)

# Visualization
p2 <- ggplot(Cleaned_As, aes(x = urbimpact, y = As)) +
    geom_boxplot(width = 0.5, alpha = 0.6) +  # Boxplot for observed data, reduced width
    geom_jitter(aes(y = As), color = "blue", size = 1.5, width = 0.1, alpha = 0.6) +  # Raw observed values with minimal spread
    geom_point(aes(y = fitted), color = "red", size = 2, position = position_dodge(width = 0.75)) +  # Raw fitted values with dodging
    geom_smooth(aes(x = urbimpact_num, y = As), method = "lm", formula = y ~ poly(x, 1), color = "red", linetype = "solid") +  # Linear fit line
    #geom_smooth(aes(x = urbimpact_num, y = As), method = "lm", formula = y ~ poly(x, 2), color = "lightcoral", linetype = "dashed") +  # Quadratic fit line
    geom_text(data = summary_stats, aes(x = urbimpact, y = Max + 2, label = Letters), size = 6) +  # Add significance letters
    scale_x_discrete(labels = levels(Cleaned_As$urbimpact)) +  # Use original labels for x-axis
    labs(x = "Urbanization Intensity", y = "Log As Concentration (mg/kg)") +
    theme_classic() +
    theme(legend.position = "none") +
    scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))  # Log10 scale with labels

print(p2)

########################################################################################################
#Zn
# Add fitted values from the model to the dataframe
Cleaned_Zn$fitted_log <- fitted(zn_model)
Cleaned_Zn$fitted <- exp(Cleaned_Zn$fitted_log)

# Create a numeric version of the urbanization factor
Cleaned_Zn$urbimpact_num <- as.numeric(Cleaned_Zn$urbimpact)

# Perform ANOVA and Tukey HSD test
anova_result <- aov(Zn_log ~ urbimpact, data = Cleaned_Zn)
hsd <- HSD.test(anova_result, "urbimpact", group = TRUE)

# Extract letters and convert to a data frame
letters_df <- data.frame(urbimpact = rownames(hsd$groups), Letters = hsd$groups$groups)
rownames(letters_df) <- NULL

# Calculate maximum values for each category to place significance letters
max_values <- Cleaned_Zn %>%
    group_by(urbimpact) %>%
    summarise(Max = max(Zn))

# Merge max values with letters
summary_stats <- left_join(letters_df, max_values, by = "urbimpact")

# Merge letters with the main data frame
Cleaned_Zn <- merge(Cleaned_Zn, letters_df, by = "urbimpact", all.x = TRUE)

# Visualization with mean fitted values
windows(width = 6, height = 6)
p3 <- ggplot(Cleaned_Zn, aes(x = urbimpact, y = Zn)) +
    geom_boxplot(width = 0.5, alpha = 0.6) +  # Boxplot for observed data, reduced width
    geom_jitter(aes(y = Zn), color = "blue", size = 1.5, width = 0.1, alpha = 0.6) +  # Raw observed values with minimal spread
    geom_point(aes(y = fitted), color = "red", size = 2, position = position_dodge(width = 0.75)) +  # Mean fitted values with dodging
    geom_smooth(aes(x = urbimpact_num, y = Zn), method = "lm", formula = y ~ poly(x, 1), color = "red", linetype = "solid") +  # Linear fit line
    geom_text(data = summary_stats, aes(x = urbimpact, y = Max + 100, label = Letters), size = 6) +  # Add significance letters
    scale_x_discrete(labels = levels(Cleaned_Zn$urbimpact)) +  # Use original labels for x-axis
    labs(x = "Urbanization Intensity", y = "Log Zn Concentration (mg/kg)") +
    theme_classic() +
    theme(legend.position = "none") +
    scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))  # Log10 scale with labels

print(p3)

######################################################################################################
#Cu
# Add fitted values from the model to the dataframe
Cleaned_Cu$fitted_log <- fitted(cu_model)
Cleaned_Cu$fitted <- exp(Cleaned_Cu$fitted_log)

# Create a numeric version of the urbanization factor
Cleaned_Cu$urbimpact_num <- as.numeric(Cleaned_Cu$urbimpact)

# Perform ANOVA and Tukey HSD test
anova_result <- aov(Cu_log ~ urbimpact, data = Cleaned_Cu)
hsd <- HSD.test(anova_result, "urbimpact", group = TRUE)

# Extract letters and convert to a data frame
letters_df <- data.frame(urbimpact = rownames(hsd$groups), Letters = hsd$groups$groups)
rownames(letters_df) <- NULL
print(letters_df)

# Calculate maximum values for each category
max_values <- Cleaned_Cu %>%
    group_by(urbimpact) %>%
    summarise(Max = max(Cu))

# Merge max values with letters
summary_stats <- left_join(letters_df, max_values, by = "urbimpact")

# Merge letters with the main data frame
Cleaned_Cu <- merge(Cleaned_Cu, letters_df, by = "urbimpact", all.x = TRUE)

# Visualization
p4 <- ggplot(Cleaned_Cu, aes(x = urbimpact, y = Cu)) +
    geom_boxplot(width = 0.5, alpha = 0.6) +  # Boxplot for observed data, reduced width
    geom_jitter(aes(y = Cu), color = "blue", size = 1.5, width = 0.1, alpha = 0.6) +  # Raw observed values with minimal spread
    geom_point(aes(y = fitted), color = "red", size = 2, position = position_dodge(width = 0.75)) +  # Fitted values with dodging
    geom_smooth(aes(x = urbimpact_num, y = Cu), method = "lm", formula = y ~ poly(x, 1), color = "red", linetype = "solid") +  # Linear fit line
    geom_text(data = summary_stats, aes(x = urbimpact, y = Max + 5, label = Letters), size = 6) +  # Add significance letters
    scale_x_discrete(labels = levels(Cleaned_Cu$urbimpact)) +  # Use original labels for x-axis
    labs(x = "Urbanization Intensity", y = "Log Cu Concentration (mg/kg)") +
    theme_classic() +
    theme(legend.position = "none")+
    scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))  # Log10 scale with labels


print(p4)

#######################################################################################################
#Cd
# Add fitted values from the model to the dataframe
Cleaned_Cd$fitted_log <- fitted(cd_model)
Cleaned_Cd$fitted <- exp(Cleaned_Cd$fitted_log)

# Create a numeric version of the urbanization factor
Cleaned_Cd$urbimpact_num <- as.numeric(Cleaned_Cd$urbimpact)

# Perform ANOVA and Tukey HSD test
anova_result <- aov(Cd_log ~ urbimpact, data = Cleaned_Cd)
hsd <- HSD.test(anova_result, "urbimpact", group = TRUE)

# Extract letters and convert to a data frame
letters_df <- data.frame(urbimpact = rownames(hsd$groups), Letters = hsd$groups$groups)
rownames(letters_df) <- NULL
print(letters_df)

# Calculate maximum values for each category
max_values <- Cleaned_Cd %>%
    group_by(urbimpact) %>%
    summarise(Max = max(Cd))

# Merge max values with letters
summary_stats <- left_join(letters_df, max_values, by = "urbimpact")

# Merge letters with the main data frame
Cleaned_Cd <- merge(Cleaned_Cd, letters_df, by = "urbimpact", all.x = TRUE)

# Visualization
p5 <- ggplot(Cleaned_Cd, aes(x = urbimpact, y = Cd)) +
    geom_boxplot(width = 0.5, alpha = 0.6) +  # Boxplot for observed data, reduced width
    geom_jitter(aes(y = Cd), color = "blue", size = 1.5, width = 0.1, alpha = 0.6) +  # Raw observed values with minimal spread
    geom_point(aes(y = fitted), color = "red", size = 2, position = position_dodge(width = 0.75)) +  # Fitted values with dodging
    geom_smooth(aes(x = urbimpact_num, y = Cd), method = "lm", formula = y ~ poly(x, 1), color = "red", linetype = "solid") +  # Linear fit line
    geom_text(data = summary_stats, aes(x = urbimpact, y = Max + 1, label = Letters), size = 6) +  # Add significance letters
    scale_x_discrete(labels = levels(Cleaned_Cd$urbimpact)) +  # Use original labels for x-axis
    labs(x = "Urbanization Intensity", y = "Log Cd Concentration (mg/kg)") +
    theme_classic() +
    theme(legend.position = "none") +
    scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))  # Log10 scale with labels

print(p5)

###########################################################################################################
#Cr
anova_result <- aov(Cr_log ~ urbimpact, data = Cleaned_Cr)
hsd <- HSD.test(anova_result, "urbimpact", group = TRUE)

# Visualization
# Extract letters and convert to a data frame
letters_df <- data.frame(urbimpact = rownames(hsd$groups), Letters = hsd$groups$groups)
rownames(letters_df) <- NULL

# Calculate maximum values for each category
max_values <- Cleaned_Cr %>%
    group_by(urbimpact) %>%
    summarise(Max = max(Cr))

# Merge max values with letters
summary_stats <- left_join(max_values, letters_df, by = "urbimpact")

# Merge letters with the main data frame
Cleaned_Cr <- merge(Cleaned_Cr, letters_df, by = "urbimpact", all.x = TRUE)

# Add fitted values from the model to the dataframe
Cleaned_Cr$fitted_log <- fitted(cr_model)
Cleaned_Cr$fitted <- exp(Cleaned_Cr$fitted_log)

# Create a numeric version of the urbanization factor
Cleaned_Cr$urbimpact_num <- as.numeric(Cleaned_Cr$urbimpact)

# Visualization
p6 <- ggplot(Cleaned_Cr, aes(x = urbimpact, y = Cr)) +
    geom_boxplot(width = 0.5, alpha = 0.6) +  # Boxplot for observed data, reduced width
    geom_jitter(aes(y = Cr), color = "blue", size = 1.5, width = 0.1, alpha = 0.6) +  # Raw observed values with minimal spread
    geom_point(aes(y = fitted), color = "red", size = 2, position = position_dodge(width = 0.75)) +  # Fitted values with dodging
    geom_smooth(aes(x = urbimpact_num, y = Cr), method = "lm", formula = y ~ poly(x, 2), color = "red", linetype = "solid") +  # Quadratic fit line
    geom_text(data = summary_stats, aes(x = urbimpact, y = Max + 4, label = Letters), size = 6) +  # Add significance letters
    scale_x_discrete(labels = levels(Cleaned_Cr$urbimpact)) +  # Use original labels for x-axis
    labs(x = "Urbanization Intensity", y = "Chromium (Cr) Concentration (mg/kg)") +
    theme_classic() +
    theme(legend.position = "none")+
    scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))  # Log10 scale with labels

print(p6)
