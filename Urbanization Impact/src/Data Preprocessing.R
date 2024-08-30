#Select relvant columns from combined dataset 
data <- Heavy_metals[, c(1:12)]


#Preprocessing 
library(dplyr)

#Pb
Cleaned_Pb <- data %>%
    filter(!is.na(Pb)) %>%
    filter(sample_id != 626 & sample_id != 374) %>% #remove outliers 
    filter(urbimpact != "Forest" & urbimpact != "Rural") %>%
    dplyr::select(sample_id, field_id, urbimpact, Pb)
Cleaned_Pb$urbimpact <- factor(Cleaned_Pb$urbimpact, ordered = TRUE)

#As
Cleaned_As <- data %>%
    filter(!is.na(As)) %>%
    dplyr::select(sample_id, field_id, urbimpact, As) %>%
    filter(urbimpact != "Forest" & urbimpact != "Rural")%>%
    droplevels()

Cleaned_As$urbimpact <- factor(Cleaned_As$urbimpact, ordered = TRUE)

#Zn
Cleaned_Zn <- data %>%
    filter(!is.na(Zn)) %>%
    filter(sample_id != 334 & sample_id != 626 & sample_id != 572) %>%
    dplyr::select(sample_id, field_id, urbimpact, Zn) %>%
    filter(urbimpact != "Forest" & urbimpact != "Rural") %>%
    droplevels()
Cleaned_Zn$urbimpact <- factor(Cleaned_Zn$urbimpact, ordered = TRUE)

#Cu
Cleaned_Cu <- data %>%
    filter(!is.na(Cu)) %>%
    dplyr::select(sample_id, field_id, urbimpact, Cu) %>%
    filter(urbimpact != "Forest" & urbimpact != "Rural") %>%
    droplevels()
Cleaned_Cu$urbimpact <- factor(Cleaned_Cu$urbimpact, ordered = TRUE)

#Cd
Cleaned_Cd <- data %>%
    filter(!is.na(Cd)) %>%
    filter(urbimpact != "Forest" & urbimpact != "Rural") %>%
    filter(sample_id != 361 & sample_id != 432 & sample_id != 513) %>%
    dplyr::select(sample_id, field_id, urbimpact, Cd) %>%
    droplevels()
Cleaned_Cd$urbimpact <- factor(Cleaned_Cd$urbimpact, ordered = TRUE)

#Cr
Cleaned_Cr <- data %>%
    filter(!is.na(Cr)) %>%
    dplyr::select(sample_id, field_id, urbimpact, Cr) %>%
    filter(urbimpact != "Forest" & urbimpact != "Rural") %>%
    filter(sample_id != 317 & sample_id != 432 & sample_id != 536) %>%
    droplevels()
Cleaned_Cr$urbimpact <- factor(Cleaned_Cr$urbimpact, ordered = TRUE)

#Exploratoy statistics
category_summary <- Cleaned_Pb %>%
    group_by(urbimpact) %>%
    summarise(
        count = n(),
        mean_Pb = mean(Pb, na.rm = TRUE),
        median_Pb = median(Pb, na.rm = TRUE),
        sd_Pb = sd(Pb, na.rm = TRUE),
        min_Pb = min(Pb, na.rm = TRUE),
        max_Pb = max(Pb, na.rm = TRUE))