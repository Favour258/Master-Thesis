#Using Boxcox Transformation 

#Pb
dummymodel <- lm(Pb~ urbimpact, data = Cleaned_Pb)
plot(dummymodel)

bc <- boxcox(dummymodel, lambda = seq(-3,3))
best.lam <- bc$x[which(bc$y == max (bc$y))]
best.lam

Cleaned_Pb <- Cleaned_Pb %>%
    mutate(Pb_sqrt = Pb^0.5)

#As
dummymodel <- lm(As~ urbimpact, data = Cleaned_As)
plot(dummymodel)

bc <- boxcox(dummymodel, lambda = seq(-3,3))
best.lam <- bc$x[which(bc$y == max (bc$y))]
best.lam

Cleaned_As <- Cleaned_As %>%
    mutate(As_log = log(As)) 

#Zn
Cleaned_Zn <- Cleaned_Zn %>% 
    mutate(Zn_log = log(Zn))# Log transformation

#Cu 
Cleaned_Cu <- Cleaned_Cu %>% 
    mutate(Cu_log = log(Cu))

#Cd
Cleaned_Cd <- Cleaned_Cd %>%
    mutate(Cd_log = log(Cd))

#Cr
Cleaned_Cr <- Cleaned_Cr %>%
    mutate(Cr_log = log(Cr))
