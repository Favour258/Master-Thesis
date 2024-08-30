# Importing the datasets
ICP_OES <- read.csv("Dataset/data_kumasi_urbansoil_soilchemistry.csv")
pXRF <- read.csv("Dataset/total_elements_Pxrf.csv")

# Data Cleaning for ICP_OES
library(dplyr)

# Step 1: Analyze missing data to inform decisions
# Calculate the percentage of missing values per column
col_missing_pct <- sapply(ICP_OES, function(x) sum(x == "" | is.na(x))) / nrow(ICP_OES)
head(col_missing_pct[order(-col_missing_pct)]) # Display columns with the highest percentage of missing values in descending order

# Step 2: Remove the 'tic_105_gkg' column due to high missing data (78%)
ICP_OES <- ICP_OES %>%
    select(-tic_105_gkg) 

# Step 3: Remove rows with any remaining missing data
ICP_OES <- ICP_OES %>%
    filter(if_all(everything(), ~ !is.na(.)))

# View the cleaned dataset
View(ICP_OES)

# Step 4: Take care of duplicates
library(tidyverse)
ICP_OES %>%
    group_by(sample_id) %>%
    filter(n() > 1) %>%
    summarise(count = n()) %>%
    ungroup() #Looking for sample IDs that have duplicate values.

ICP_OES <- ICP_OES%>%
    group_by(sample_id) %>%mutate(across(where(is.numeric), ~ ifelse(n() > 1, mean(.), .)),
                                  across(where(is.character), ~ ifelse(n() > 1, first(.), .))) %>%
    distinct(sample_id, .keep_all = TRUE) %>%
    ungroup() #create a new df with the mean of duplicate values

#4: Keep only relevant columns for methods analysis
word_to_exclude <- "_40_gkg"
ICP_OES <- ICP_OES  %>%
    select(-contains(word_to_exclude))
ICP_OES <- ICP_OES [, c(1,2,18,19,22:25)]
ICP_OES  <- ICP_OES [, -8]
#colnames(colnames(wet_chemistry$sample_id))
#colnames(Heavymetals)[colnames(Heavymetals) == "Pb"] <- "Pb_pXRF"

#Unit Conversion####
library(dplyr)
#Pb
ICP_OES  <- ICP_OES  %>% 
    mutate(Pb_HNO3 = Pb_105_gkg*1000)
ICP_OES <- ICP_OES  %>% 
    select(-Pb_105_gkg)

#Zn
ICP_OES  <- ICP_OES %>% 
    mutate(Zn_HNO3 = Zn_105_gkg*1000)
ICP_OES  <- ICP_OES  %>% 
    select(-Zn_105_gkg)

#Cu
ICP_OES  <- ICP_OES  %>% 
    mutate(Cu_HNO3 = Cu_105_gkg*1000)
ICP_OES  <- ICP_OES  %>% 
    select(-Cu_105_gkg)

#Fe
ICP_OES  <- ICP_OES %>% 
    mutate(Fe_HNO3 = Fe_105_gkg*1000)
ICP_OES  <- ICP_OES  %>% 
    select(-Fe_105_gkg)

#Mn
ICP_OES <- ICP_OES  %>% 
    mutate(Mn_HNO3 = Mn_105_gkg*1000)
ICP_OES  <- ICP_OES %>% 
    select(-Mn_105_gkg)


#PART 2: Data cleaning for pXRF

# Step 1: Check for missing data in rows and columns 
missing <- sapply(pXRF, function (x) sum(x == ""|is.na(x)))/ nrow(pXRF)
head(missing[order(-missing)]) #highest number of missing values
pXRF <- pXRF[, -c(47:51)]

word_to_exclude <- "Error"
pXRF <- pXRF %>%
    select(-contains(word_to_exclude)) #deleting irrelevant columns


# Step 2: Create subset for further analysis
pXRF <- pXRF[, c(1,2,11,13,14,16,17)] #Pb, Zn, Cu, Fe, Mn

# Step 3: Check mode of dataset
column_types <- sapply(pXRF, function(x) class(x))
print(column_types) #check column types
pXRF <- pXRF %>%
    mutate(across(c(Pb, Zn, Cu, Mn), as.numeric)) #change from character to numeric

# Step 4: create subset of PXRF based on the sample ID column in wet chemistry
pXRF <- pXRF %>% 
    rename(sample_id = Sample_id) %>% 
    filter(sample_id %in% ICP_OES$sample_id)


#PART 3: Merge the data frames 
MethodsComparison <- merge(pXRF, ICP_OES, by= "sample_id", all = TRUE) #keep all rows not just the ones with similar IDs