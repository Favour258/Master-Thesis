# Load necessary libraries
install.packages(c("dplyr", "Hmisc", "ggplot2", "reshape2"))
library(dplyr)
library(Hmisc)
library(ggplot2)
library(reshape2)

# Step 1: Remove duplicates by keeping the first occurrence
Heavy_metals_unique <- Heavy_metals %>%
    group_by(sample_id) %>%
    slice(1) %>%
    ungroup()

Background_unique <- Background %>%
    group_by(sample_id) %>%
    slice(1) %>%
    ungroup()

# Step 2: Join the unique datasets to include soil_ph and loi_105_gkg from Background
Corr_subset <- Heavy_metals_unique %>%
    left_join(select(Background_unique, sample_id, soil_ph, loi_105_gkg), by = "sample_id")

# Step 3: Select and rename the columns as needed
numeric_data <- Corr_subset %>%
    select(Pb, As, Zn, Cu, Cr, Cd, soil_ph, loi_105_gkg) %>%
    rename(SOM = loi_105_gkg, pH = soil_ph)

# Step 4: Calculate the correlation matrix and p-values
correlation_results <- rcorr(as.matrix(numeric_data), type = "pearson")

# Step 5: Extract correlation coefficients and p-values
correlation_coefficients <- correlation_results$r
p_values <- correlation_results$P

# Step 6: Create a significance matrix
significance_matrix <- matrix(nrow = nrow(p_values), ncol = ncol(p_values))
for (i in 1:nrow(p_values)) {
    for (j in 1:ncol(p_values)) {
        if (!is.na(p_values[i, j])) {
            if (p_values[i, j] < 0.01) {
                significance_matrix[i, j] <- "**"
            } else if (p_values[i, j] < 0.05) {
                significance_matrix[i, j] <- "*"
            } else {
                significance_matrix[i, j] <- ""
            }
        }
    }
}
rownames(significance_matrix) <- rownames(p_values)
colnames(significance_matrix) <- colnames(p_values)

# Step 7: Melt the correlation matrix and significance matrix to long format
melted_correlation <- melt(correlation_coefficients)
melted_significance <- melt(significance_matrix)

# Step 8: Merge the correlation and significance data
melted_correlation$significance <- melted_significance$value

# Ensure no NA values in significance column
melted_correlation$significance[is.na(melted_correlation$significance)] <- ""

# Step 9: Create the correlation matrix plot with significance markers
ggplot(melted_correlation, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "white", high = "darkred", mid = "lightpink", 
                         midpoint = 0, limit = c(-1, 1), space = "Lab", 
                         name = "Pearson\nCorrelation") +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1),
          axis.text.y = element_text(size = 12),
          plot.margin = unit(c(1, 1, 1, 1), "cm"),  # Adjust plot margins
          aspect.ratio = 1) +  # Set aspect ratio
    coord_fixed() +
    labs(x = NULL, y = NULL) +
    geom_text(aes(label = ifelse(value == 1, "1", paste0(round(value, 2), significance))), 
              color = "black", size = 4)
