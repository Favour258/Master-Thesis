#Load necessary libraries
library(tidyr)
library(dplyr)
library(ggplot2)

#Enrichment Factor (EF)####
Heavy_metals$Pb_EF <- (Heavy_metals$Pb/Heavy_metals$Fe)/(Pb_bc/Fe_bc)
Heavy_metals$As_EF <- (Heavy_metals$As/Heavy_metals$Fe)/(As_bc/Fe_bc)
Heavy_metals$Ni_EF <- (Heavy_metals$Ni/Heavy_metals$Fe)/(Ni_bc/Fe_bc)
Heavy_metals$Zn_EF <- (Heavy_metals$Zn/Heavy_metals$Fe)/(Zn_bc/Fe_bc)
Heavy_metals$Cr_EF <- (Heavy_metals$Cr/Heavy_metals$Fe)/(Cr_bc/Fe_bc)
Heavy_metals$Cu_EF <- (Heavy_metals$Cu/Heavy_metals$Fe)/(Cu_bc/Fe_bc)
Heavy_metals$Mn_EF <- (Heavy_metals$Mn/Heavy_metals$Fe)/(Mn_bc/Fe_bc)
Heavy_metals$Cd_EF <- (Heavy_metals$Cd/Heavy_metals$Fe)/(Cd_bc/Fe_bc)
Heavy_metals$Fe_EF <- (Heavy_metals$Fe/Heavy_metals$Fe)/(Fe_bc/Fe_bc)

#Summary statistics for EF
Heavy_metals %>% 
    #select(urbimpact, , , ,Cu_EF, Pb_EF ) %>% 
    group_by(urbimpact) %>%
    summarise(
        minimal = sum(Cr_EF < 2, na.rm = TRUE),
        moderate = sum(Cr_EF > 2 & Cr_EF < 5, na.rm = TRUE),
        significant = sum(Cr_EF > 5 & Cr_EF < 20, na.rm = TRUE),
        veryhigh = sum(Cr_EF > 20 & Cr_EF < 40, na.rm = TRUE),
        extremelyhigh = sum(Cr_EF >= 40, na.rm = TRUE)
    )

#Convert to long format
Heavy_metals_long <- Heavy_metals %>% 
    pivot_longer(cols = c(Pb_EF, Zn_EF, Cu_EF, Cd_EF, As_EF, Cr_EF),
                 names_to = "Metal_EF",
                 values_to = "Concentration")

# Calculate the range and mean EF values for each element in each soil category
EF_summary <- Heavy_metals_long %>%
    filter(!is.na(Concentration)) %>%
    group_by(urbimpact, Metal_EF) %>%
    summarise(
        min_EF = min(Concentration, na.rm = TRUE),
        max_EF = max(Concentration, na.rm = TRUE),
        mean_EF = mean(Concentration, na.rm = TRUE)
    )

print(EF_summary)


#Contamination Factor####
Heavy_metals$Pb_CF <- Heavy_metals$Pb/Pb_bc
Heavy_metals$As_CF <- Heavy_metals$As/As_bc
Heavy_metals$Ni_CF <- Heavy_metals$Ni/Ni_bc
Heavy_metals$Zn_CF <- Heavy_metals$Zn/Zn_bc
Heavy_metals$Cr_CF <- Heavy_metals$Cr/Cr_bc
Heavy_metals$Cu_CF <- Heavy_metals$Cu/Cu_bc
Heavy_metals$Mn_CF <- Heavy_metals$Mn/Mn_bc
Heavy_metals$Cd_CF <- Heavy_metals$Cd/Cd_bc
Heavy_metals$Fe_CF <- Heavy_metals$Fe/Fe_bc


#Pollution Load Index (PLI) 
Heavy_metals <- Heavy_metals %>%
    mutate(
        PLI = (Pb_CF * As_CF * Zn_CF * Cr_CF * Cu_CF * Cd_CF)^(1/6)
    )

#Potential Ecological Risk Index (PERI)
#Calculated by TrF * CF 

toxic_factors <- c(Pb = 5, As = 10, Zn = 1, Cr = 2, Cu = 5, Cd = 30)

# Calculate individual PERI for each element, considering NAs
Heavy_metals$Pb_PERI <- ifelse(is.na(Heavy_metals$Pb_CF), 0, Heavy_metals$Pb_CF * toxic_factors["Pb"])
Heavy_metals$As_PERI <- ifelse(is.na(Heavy_metals$As_CF), 0, Heavy_metals$As_CF * toxic_factors["As"])
Heavy_metals$Zn_PERI <- ifelse(is.na(Heavy_metals$Zn_CF), 0, Heavy_metals$Zn_CF * toxic_factors["Zn"])
Heavy_metals$Cr_PERI <- ifelse(is.na(Heavy_metals$Cr_CF), 0, Heavy_metals$Cr_CF * toxic_factors["Cr"])
Heavy_metals$Cu_PERI <- ifelse(is.na(Heavy_metals$Cu_CF), 0, Heavy_metals$Cu_CF * toxic_factors["Cu"])
Heavy_metals$Cd_PERI <- ifelse(is.na(Heavy_metals$Cd_CF), 0, Heavy_metals$Cd_CF * toxic_factors["Cd"])

#Calculate human health indices 
#Average Daily Dose _ ingestion pathway in Adults 
ADDing_Adt <- function(data) {
    
    BW <- 70 #Body weight for adults
    EF <- 250 #Exposure frequency in adults 
    ED <- 24 #Exposure duration in adults 
    AT <- 8760 #Average time in adults 
    IR <- 100 #Ingestion rate in adults 
    CF <- 10^-6 #kgmg-1 conversion factor
    
    result <- ((data * IR * EF * ED) / (BW *AT)) * CF 
    
    return (result)
}

Heavy_metals$ADDing_Pb_Adt <- ADDing_Adt(Heavy_metals$Pb)
Heavy_metals$ADDing_As_Adt <- ADDing_Adt(Heavy_metals$As)
Heavy_metals$ADDing_Ni_Adt <- ADDing_Adt(Heavy_metals$Ni)
Heavy_metals$ADDing_Zn_Adt <- ADDing_Adt(Heavy_metals$Zn)
Heavy_metals$ADDing_Cr_Adt <- ADDing_Adt(Heavy_metals$Cr)
Heavy_metals$ADDing_Cu_Adt <- ADDing_Adt(Heavy_metals$Cu)
Heavy_metals$ADDing_Mn_Adt <- ADDing_Adt(Heavy_metals$Mn)
Heavy_metals$ADDing_Cd_Adt <- ADDing_Adt(Heavy_metals$Cd)
Heavy_metals$ADDing_Fe_Adt <- ADDing_Adt(Heavy_metals$Fe)

#Calculate Average Daily Dose_ingestion pathway in children 

ADDing_Chd <- function(data) {
    BW <- 15  #Body weight in children
    EF <- 350 #Exposure frequency in children 
    ED <- 6   #Exposure duration in children 
    AT <- 2190 #Average time in children 
    IR <- 200 #Ingestion rate in children 
    CF <- 10^-6 #kgmg-1 conversion factor
    
    result <- data * ((IR * EF * ED) / (BW *AT)) * CF 
    
    return (result)  
}

Heavy_metals$ADDing_Pb_Chd <- ADDing_Chd(Heavy_metals$Pb)
Heavy_metals$ADDing_As_Chd <- ADDing_Chd(Heavy_metals$As)
Heavy_metals$ADDing_Ni_Chd <- ADDing_Chd(Heavy_metals$Ni)
Heavy_metals$ADDing_Zn_Chd <- ADDing_Chd(Heavy_metals$Zn)
Heavy_metals$ADDing_Cr_Chd <- ADDing_Chd(Heavy_metals$Cr)
Heavy_metals$ADDing_Cu_Chd <- ADDing_Chd(Heavy_metals$Cu)
Heavy_metals$ADDing_Mn_Chd <- ADDing_Chd(Heavy_metals$Mn)
Heavy_metals$ADDing_Cd_Chd <- ADDing_Chd(Heavy_metals$Cd)
Heavy_metals$ADDing_Fe_Chd <- ADDing_Chd(Heavy_metals$Fe)

#Calculate RfD for ingestion pathway 

#Initialize parameters: PTE Oral and dermal reference dose 
RfDing_As <- 0.0003
RfDing_Cr <- 0.003 
RfDing_Cu <- 0.037
RfDing_Ni <- 0.00186 
RfDing_Pb <- 0.0036
RfDing_Zn <- 0.3 
RfDing_Mn <- 0.024 
RfDing_Cd <- 0.0005

#PTE inhalation reference dose 
RfDinhl_As <- 0.000086
RfDinhl_Cr <- 0.0000285 
RfDinhl_Cu <- 1
RfDinhl_Ni <- 1
RfDinhl_Pb <- 1
RfDinhl_Zn <- 1
RfDinhl_Mn <- 0.00143
RfDinhl_Cd <- 1

#ADD dermal pathway in adults 
ADDderm_Adt <- function(data) {
    EF <- 250 #exposure frequency
    ED <- 24 #exposure duration
    SA <- 13110 #skin exposed surface area
    AF <- 0.07 #skin adherence factor 
    ABF <- 0.01 #dermal absorption factor 
    AT <- 8760 #average time 
    BW <- 70 #body weight 
    CF <- 10^-6 #kgmg-1 conversion factor 
    
    result <- data * ((EF*ED*SA*AF*ABF)/(AT*BW)) * CF 
    
    return(result)
}

Heavy_metals$ADDderm_Pb_Adt <- ADDderm_Adt(Heavy_metals$Pb)
Heavy_metals$ADDderm_As_Adt <- ADDderm_Adt(Heavy_metals$As)
Heavy_metals$ADDderm_Ni_Adt <- ADDderm_Adt(Heavy_metals$Ni)
Heavy_metals$ADDderm_Zn_Adt <- ADDderm_Adt(Heavy_metals$Zn)
Heavy_metals$ADDderm_Cr_Adt <- ADDderm_Adt(Heavy_metals$Cr)
Heavy_metals$ADDderm_Cu_Adt <- ADDderm_Adt(Heavy_metals$Cu)
Heavy_metals$ADDderm_Mn_Adt <- ADDderm_Adt(Heavy_metals$Mn)
Heavy_metals$ADDderm_Cd_Adt <- ADDderm_Adt(Heavy_metals$Cd)
Heavy_metals$ADDderm_Fe_Adt <- ADDderm_Adt(Heavy_metals$Fe)

#ADD dermal pathway in children 
ADDderm_Chd <- function(data) {
    EF <- 350 #exposure frequency
    ED <- 6 #exposure duration
    SA <- 5800 #skin exposed surface area
    AF <- 0.07 #skin adherence factor 
    ABF <- 0.01 #dermal absorption factor 
    AT <- 2190 #average time 
    BW <- 15 #body weight 
    CF <- 10^-6 #kgmg-1 conversion factor
    
    result <- data * ((EF*ED*SA*AF*ABF)/(AT*BW)) * CF
    return(result)
}

Heavy_metals$ADDderm_Pb_Chd <- ADDderm_Chd(Heavy_metals$Pb)
Heavy_metals$ADDderm_As_Chd <- ADDderm_Chd(Heavy_metals$As)
Heavy_metals$ADDderm_Ni_Chd <- ADDderm_Chd(Heavy_metals$Ni)
Heavy_metals$ADDderm_Zn_Chd <- ADDderm_Chd(Heavy_metals$Zn)
Heavy_metals$ADDderm_Cr_Chd <- ADDderm_Chd(Heavy_metals$Cr)
Heavy_metals$ADDderm_Cu_Chd <- ADDderm_Chd(Heavy_metals$Cu)
Heavy_metals$ADDderm_Mn_Chd <- ADDderm_Chd(Heavy_metals$Mn)
Heavy_metals$ADDderm_Cd_Chd <- ADDderm_Chd(Heavy_metals$Cd)
Heavy_metals$ADDderm_Fe_Chd <- ADDderm_Chd(Heavy_metals$Fe)

#Average daily dose by inhalation in Adults 
ADDinhl_Adt <- function(data) {
    Inhr <- 10.4 #inhalation rate 
    EF <- 250 #exposure frequency 
    ED <- 24 #exposure duration 
    PEF <- 1.3 * 10^9 #particulate emission factor 
    BW <- 70 #body weight 
    AT <- 8760 #average time 
    
    result = data* ((EF*Inhr*ED) / (AT*BW*PEF)) 
    return(result)
}

Heavy_metals$ADDinhl_Pb_Adt <- ADDinhl_Adt(Heavy_metals$Pb)
Heavy_metals$ADDinhl_As_Adt <- ADDinhl_Adt(Heavy_metals$As)
Heavy_metals$ADDinhl_Ni_Adt <- ADDinhl_Adt(Heavy_metals$Ni)
Heavy_metals$ADDinhl_Zn_Adt <- ADDinhl_Adt(Heavy_metals$Zn)
Heavy_metals$ADDinhl_Cr_Adt <- ADDinhl_Adt(Heavy_metals$Cr)
Heavy_metals$ADDinhl_Cu_Adt <- ADDinhl_Adt(Heavy_metals$Cu)
Heavy_metals$ADDinhl_Mn_Adt <- ADDinhl_Adt(Heavy_metals$Mn)
Heavy_metals$ADDinhl_Cd_Adt <- ADDinhl_Adt(Heavy_metals$Cd)
Heavy_metals$ADDinhl_Fe_Adt <- ADDinhl_Adt(Heavy_metals$Fe)

#Averge daily dose in children 
ADDinhl_Chd <- function(data){
    Inhr <- 10 #inhalation rate 
    EF <- 350 #exposure frequency 
    ED <- 6 #exposure duration 
    PEF <- 1.3 * 10^9 #particulate emission factor 
    BW <- 15 #body weight 
    AT <- 2190 #average time 
    
    result = data* ((EF*Inhr*ED) / (AT*BW*PEF)) 
    return(result)
}

Heavy_metals$ADDinhl_Pb_Chd <- ADDinhl_Chd(Heavy_metals$Pb)
Heavy_metals$ADDinhl_As_Chd <- ADDinhl_Chd(Heavy_metals$As)
Heavy_metals$ADDinhl_Ni_Chd <- ADDinhl_Chd(Heavy_metals$Ni)
Heavy_metals$ADDinhl_Zn_Chd <- ADDinhl_Chd(Heavy_metals$Zn)
Heavy_metals$ADDinhl_Cr_Chd <- ADDinhl_Chd(Heavy_metals$Cr)
Heavy_metals$ADDinhl_Cu_Chd <- ADDinhl_Chd(Heavy_metals$Cu)
Heavy_metals$ADDinhl_Mn_Chd <- ADDinhl_Chd(Heavy_metals$Mn)
Heavy_metals$ADDinhl_Cd_Chd <- ADDinhl_Chd(Heavy_metals$Cd)
Heavy_metals$ADDinhl_Fe_Chd <- ADDinhl_Chd(Heavy_metals$Fe)

#Hazard Quotient for three pathways in Adults
#Ingestion
Heavy_metals$HQing_Pb_Adt <- Heavy_metals$ADDing_Pb_Adt/RfDing_Pb 
Heavy_metals$HQing_As_Adt <- Heavy_metals$ADDing_As_Adt/RfDing_As
Heavy_metals$HQing_Ni_Adt <- Heavy_metals$ADDing_Ni_Adt/RfDing_Ni
Heavy_metals$HQing_Zn_Adt <- Heavy_metals$ADDing_Zn_Adt/RfDing_Zn
Heavy_metals$HQing_Cr_Adt <- Heavy_metals$ADDing_Cr_Adt/RfDing_Cr
Heavy_metals$HQing_Cu_Adt <- Heavy_metals$ADDing_Cu_Adt/RfDing_Cu
Heavy_metals$HQing_Mn_Adt <- Heavy_metals$ADDing_Mn_Adt/RfDing_Mn
Heavy_metals$HQing_Cd_Adt <- Heavy_metals$ADDing_Cd_Adt/RfDing_Cd 


#Dermal 
Heavy_metals$HQderm_Pb_Adt <- Heavy_metals$ADDderm_Pb_Adt/RfDing_Pb
Heavy_metals$HQderm_As_Adt <- Heavy_metals$ADDderm_As_Adt/RfDing_As
Heavy_metals$HQderm_Ni_Adt <- Heavy_metals$ADDderm_Ni_Adt/RfDing_Ni
Heavy_metals$HQderm_Zn_Adt <- Heavy_metals$ADDderm_Zn_Adt/RfDing_Zn
Heavy_metals$HQderm_Cr_Adt <- Heavy_metals$ADDderm_Cr_Adt/RfDing_Cr
Heavy_metals$HQderm_Cu_Adt <- Heavy_metals$ADDderm_Cu_Adt/RfDing_Cu
Heavy_metals$HQderm_Mn_Adt <- Heavy_metals$ADDderm_Mn_Adt/RfDing_Mn
Heavy_metals$HQderm_Cd_Adt <- Heavy_metals$ADDderm_Cd_Adt/RfDing_Cd

#Inhalation 
Heavy_metals$HQinhl_Pb_Adt <- Heavy_metals$ADDinhl_Pb_Adt/RfDinhl_Pb
Heavy_metals$HQinhl_As_Adt <- Heavy_metals$ADDinhl_As_Adt/RfDinhl_As
Heavy_metals$HQinhl_Ni_Adt <- Heavy_metals$ADDinhl_Ni_Adt/RfDinhl_Ni
Heavy_metals$HQinhl_Zn_Adt <- Heavy_metals$ADDinhl_Zn_Adt/RfDinhl_Zn
Heavy_metals$HQinhl_Cr_Adt <- Heavy_metals$ADDinhl_Cr_Adt/RfDinhl_Cr
Heavy_metals$HQinhl_Cu_Adt <- Heavy_metals$ADDinhl_Cu_Adt/RfDinhl_Cu
Heavy_metals$HQinhl_Mn_Adt <- Heavy_metals$ADDinhl_Mn_Adt/RfDinhl_Mn
Heavy_metals$HQinhl_Cd_Adt <- Heavy_metals$ADDinhl_Cd_Adt/RfDinhl_Cd

#Hazard Index for Adults 
Heavy_metals$HI_Pb_Adt <- Heavy_metals$HQing_Pb_Adt + Heavy_metals$HQderm_Pb_Adt+
    Heavy_metals$HQinhl_Pb_Adt
Heavy_metals$HI_As_Adt <- Heavy_metals$HQing_As_Adt + Heavy_metals$HQderm_As_Adt +
    Heavy_metals$HQinhl_As_Adt
Heavy_metals$HI_Ni_Adt <- Heavy_metals$HQing_Ni_Adt + Heavy_metals$HQderm_Ni_Adt +
    Heavy_metals$HQinhl_Ni_Adt
Heavy_metals$HI_Zn_Adt <- Heavy_metals$HQing_Zn_Adt + Heavy_metals$HQderm_Zn_Adt + 
    Heavy_metals$HQinhl_Zn_Adt
Heavy_metals$HI_Cr_Adt <- Heavy_metals$HQing_Cr_Adt + Heavy_metals$HQderm_Cr_Adt + 
    Heavy_metals$HQinhl_Cr_Adt
Heavy_metals$HI_Cu_Adt <- Heavy_metals$HQing_Cu_Adt + Heavy_metals$HQderm_Cu_Adt + 
    Heavy_metals$HQinhl_Cu_Adt
Heavy_metals$HI_Mn_Adt <- Heavy_metals$HQing_Mn_Adt + Heavy_metals$HQderm_Mn_Adt + 
    Heavy_metals$HQinhl_Mn_Adt 
Heavy_metals$HI_Cd_Adt <- Heavy_metals$HQing_Cd_Adt + Heavy_metals$HQderm_Cd_Adt +
    Heavy_metals$HQinhl_Cd_Adt 

#Visualize Hazard Index in Adults 
Heavy_metals_long3 <- Heavy_metals %>% 
    pivot_longer(cols = c(HI_Pb_Adt, HI_As_Adt, HI_Ni_Adt, HI_Zn_Adt, HI_Cr_Adt, HI_Mn_Adt, HI_Cd_Adt, HI_Cu_Adt),
                 names_to = "Metals",
                 values_to = "Index")

ggplot(Heavy_metals_long3, aes(x=Metals, y=Index, fill=Metals)) +
    geom_boxplot() +
    theme_classic()
#Hazard Quotient for three pathways in Children
#Ingestion
Heavy_metals$HQing_Pb_Chd <- Heavy_metals$ADDing_Pb_Chd/RfDing_Pb 
Heavy_metals$HQing_As_Chd <- Heavy_metals$ADDing_As_Chd/RfDing_As
Heavy_metals$HQing_Ni_Chd <- Heavy_metals$ADDing_Ni_Chd/RfDing_Ni
Heavy_metals$HQing_Zn_Chd <- Heavy_metals$ADDing_Zn_Chd/RfDing_Zn
Heavy_metals$HQing_Cr_Chd <- Heavy_metals$ADDing_Cr_Chd/RfDing_Cr
Heavy_metals$HQing_Cu_Chd <- Heavy_metals$ADDing_Cu_Chd/RfDing_Cu
Heavy_metals$HQing_Cd_Chd <- Heavy_metals$ADDing_Cd_Chd/RfDing_Cd
Heavy_metals$HQing_Mn_Chd <- Heavy_metals$ADDing_Mn_Chd/RfDing_Mn

#Dermal 
Heavy_metals$HQderm_Pb_Chd <- Heavy_metals$ADDderm_Pb_Chd/RfDing_Pb
Heavy_metals$HQderm_As_Chd <- Heavy_metals$ADDderm_As_Chd/RfDing_As
Heavy_metals$HQderm_Ni_Chd <- Heavy_metals$ADDderm_Ni_Chd/RfDing_Ni
Heavy_metals$HQderm_Zn_Chd <- Heavy_metals$ADDderm_Zn_Chd/RfDing_Zn
Heavy_metals$HQderm_Cr_Chd <- Heavy_metals$ADDderm_Cr_Chd/RfDing_Cr
Heavy_metals$HQderm_Cu_Chd <- Heavy_metals$ADDderm_Cu_Chd/RfDing_Cu
Heavy_metals$HQderm_Cd_Chd <- Heavy_metals$ADDderm_Cd_Chd/RfDing_Cd
Heavy_metals$HQderm_Mn_Chd <- Heavy_metals$ADDderm_Mn_Chd/RfDing_Mn

#Inhalation 
Heavy_metals$HQinhl_Pb_Chd <- Heavy_metals$ADDinhl_Pb_Chd/RfDinhl_Pb
Heavy_metals$HQinhl_As_Chd <- Heavy_metals$ADDinhl_As_Chd/RfDinhl_As
Heavy_metals$HQinhl_Ni_Chd <- Heavy_metals$ADDinhl_Ni_Chd/RfDinhl_Ni
Heavy_metals$HQinhl_Zn_Chd <- Heavy_metals$ADDinhl_Zn_Chd/RfDinhl_Zn
Heavy_metals$HQinhl_Cr_Chd <- Heavy_metals$ADDinhl_Cr_Chd/RfDinhl_Cr
Heavy_metals$HQinhl_Cu_Chd <- Heavy_metals$ADDinhl_Cu_Chd/RfDinhl_Cu
Heavy_metals$HQinhl_Cd_Chd <- Heavy_metals$ADDinhl_Cd_Chd/RfDinhl_Cd
Heavy_metals$HQinhl_Mn_Chd <- Heavy_metals$ADDinhl_Mn_Chd/RfDinhl_Mn

#Hazard Index for Children 
Heavy_metals$HI_Pb_Chd <- Heavy_metals$HQing_Pb_Chd + Heavy_metals$HQderm_Pb_Chd+
    Heavy_metals$HQinhl_Pb_Chd
Heavy_metals$HI_As_Chd <- Heavy_metals$HQing_As_Chd + Heavy_metals$HQderm_As_Chd +
    Heavy_metals$HQinhl_As_Chd
Heavy_metals$HI_Ni_Chd <- Heavy_metals$HQing_Ni_Chd + Heavy_metals$HQderm_Ni_Chd +
    Heavy_metals$HQinhl_Ni_Chd
Heavy_metals$HI_Zn_Chd <- Heavy_metals$HQing_Zn_Chd + Heavy_metals$HQderm_Zn_Chd + 
    Heavy_metals$HQinhl_Zn_Chd
Heavy_metals$HI_Cr_Chd <- Heavy_metals$HQing_Cr_Chd + Heavy_metals$HQderm_Cr_Chd + 
    Heavy_metals$HQinhl_Cr_Chd
Heavy_metals$HI_Cu_Chd <- Heavy_metals$HQing_Cu_Chd + Heavy_metals$HQderm_Cu_Chd + 
    Heavy_metals$HQinhl_Cu_Chd
Heavy_metals$HI_Cd_Chd <- Heavy_metals$HQing_Cd_Chd + Heavy_metals$HQderm_Cd_Chd + 
    Heavy_metals$HQinhl_Cd_Chd 
Heavy_metals$HI_Mn_Chd <- Heavy_metals$HQing_Mn_Chd + Heavy_metals$HQderm_Mn_Chd + 
    Heavy_metals$HQinhl_Mn_Chd 

