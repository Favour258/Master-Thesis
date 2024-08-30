#Use the ICP_OES dataframe for further analyses
Background <- data_kumasi_urbansoil_soilchemistry 

#Exploratory statistics and data cleaning 
Background <- Background[rowSums(is.na(Background)) < 38, ]
summary(Background)
sapply(Background, class) 

#Convert the unit from g/kg to mg/kg of the dataframe 
Background$Pb_105_gkg <- Background$Pb_105_gkg*1000 #from g/kg to mg/kg 
Background$Cd_105_gkg <- Background$Cd_105_gkg*1000
Background$Mn_105_gkg <- Background$Mn_105_gkg*1000
Background$Cu_105_gkg <- Background$Cu_105_gkg*1000
Background$Zn_105_gkg <- Background$Zn_105_gkg*1000
Background$Fe_105_gkg<- Background$Fe_105_gkg*1000

#Change colnames 
colnames(Background)[colnames(Background) == "Mn_105_gkg"] <- "Mn"
colnames(Background)[colnames(Background) == "Fe_105_gkg"] <- "Fe"
colnames(Background)[colnames(Background) == "Cu_105_gkg"] <- "Cu"
colnames(Background)[colnames(Background) == "Zn_105_gkg"] <- "Zn"
colnames(Background)[colnames(Background) == "Pb_105_gkg"] <- "Pb"
colnames(Background)[colnames(Background) == "Cd_105_gkg"] <- "Cd"

#Delete irrelevant columns 
Background <- Background[, -c(14:27)]

#Checking for assumptions of unimodality and positive symmetry 
plot(density(Background$Mn, na.rm = TRUE), main = "Density Plot of Mn", xlab = "Mn", ylab = "Density", col = "blue")
#All datasets are unimodal and have positive symmetry 

#Background dataset for As, Cr, Ni 
BackgroundpXRF <- pXRF
BackgroundpXRF <- BackgroundpXRF[, c(1,2,3,12,15,18)]
summary(BackgroundpXRF)

BackgroundpXRF$As <- as.numeric(BackgroundpXRF$As)
BackgroundpXRF$Ni <- as.numeric(BackgroundpXRF$Ni)
BackgroundpXRF$Cr <- as.numeric(BackgroundpXRF$Cr)