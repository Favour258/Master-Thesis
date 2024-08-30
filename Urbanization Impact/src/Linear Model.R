#Pb
pb_model <- lm(Pb_sqrt~urbimpact, data= Cleaned_Pb)
summary(pb_model)
#Model validation
shapiro.test(resid(pb_model))
plot (pb_model)

#As
As_model <- lm(As_log ~urbimpact, data = Cleaned_As)
summary(As_model)
#Model validation 
shapiro.test(resid(As_model)) #transformed data is normally distributed. 
plot(As_model)

#Zn
zn_model <- lm(Zn_log ~ urbimpact, data = Cleaned_Zn)
summary(zn_model)

#Cu
cu_model <- lm(Cu_log ~ urbimpact, data = Cleaned_Cu)
summary(cu_model)

#Cd
# Linear Model
cd_model <- lm(Cd_log ~ urbimpact, data = Cleaned_Cd)
summary(cd_model)
# Model validation
shapiro.test(resid(cd_model)) # Normally distributed
plot(cd_model)

#Cr
# Linear Model
cr_model <- lm(Cr_log ~ urbimpact, data = Cleaned_Cr)
summary(cr_model)
# Model validation
shapiro.test(resid(cr_model))
plot(cr_model)
