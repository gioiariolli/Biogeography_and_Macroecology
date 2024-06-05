############## REPRODUCTION STRATEGIES (REPSTRAT) ####################################
Emerus_repstrat <- read.table("RepStrat_Emerus2023.txt", header=TRUE, sep = "")
summary(Emerus_repstrat)

attach(Emerus_repstrat)
## detach(Emerus_repstrat)

require (multcomp)

boxplot((Emerus_repstrat$Seeds[Test=="A"]), (Emerus_repstrat$Seeds[Test== "SS"]), (Emerus_repstrat$Seeds[Test== "C"]),
        names= c("Agamospermy", "Spontaneous selfing", "Control"), main= "Tot seeds")


##################### binomial family ## NON SERVE#####################à
modello <- glm(cbind(Seeds, Abortive_ovules) ~ Test, family = binomial,
               data = Emerus_repstrat)
summary(modello)

ods <- modello$deviance/modello$df.residual
ods ### >1, overdispersal

modello2 <- glm(cbind(Seeds, Abortive_ovules) ~ Test, family = quasibinomial,
                data = Emerus_repstrat)
summary(modello2)

ods2 <- modello2$deviance/modello2$df.residual
ods2

### pairwise comparison
library(emmeans)
pairs(emmeans(modello2, ~ Test))


#### poisson family
modello_try <- glm(Seeds ~ Test, family = poisson,
               data = Emerus_repstrat)
odstry <- modello_try$deviance/modello_try$df.residuals
odstry
summary(modello_try)

pairs(emmeans(modello_try ~ Test))

### per valutare reproduction efficiency: ratio tra numero massimo (medio) di semi totali (sviluppati+abortiti, ovvero 7) e numero di semi sviluppati:
Emerus_repstrat$ratio <- Emerus_repstrat$Seeds/7
Emerus_repstrat$ratio <- round(Emerus_repstrat$ratio, 2)

freq.tab <- table(Emerus_repstrat$ratio [Emerus_repstrat$ratio>0])
prop.table(freq.tab)
barplot(prop.table)  ###vediamo tra i frutti che hanno seed>0 che efficienza ci sia rispetto al massimo di 7


########################### POLLEN SUPPLEMENTATION ####################à
Emerus_pollsup <- read.table("PollSup_Emerus2023.txt", header=TRUE, sep = "")
summary(Emerus_pollsup)
attach(Emerus_pollsup)

boxplot((Emerus_pollsup$Seeds[Test=="PS"]), (Emerus_pollsup$Seeds[Test== "N"]),
        names= c("Pollen Supplementation", "Normal"), main= "Total number of seeds(2023)", col = c("violet", "yellow"))


bartlett.test(Seeds ~ Test) #to test if the variances of seed production under normal conditions and pollen supplementation 
                        #conditions are equal (if p-value>significance level--> null hypotesis accepted)
#H0=variance in each group is the same.
#p-value=0.85 --> no evidence against H0 so the two variances are equal, null hypotesis is true


shapiro.test(Seeds[Test=="N"])#to verify normal distribution
#H0=data come from a normal distribution
#p-value=0.01 --> moderate evidence against H0 [so actually they may not come from a not normal distribution????]
#anna# W=0.8 --> The W statistic measures how closely the sample data follow a normal distribution.
#anna# A value close to 1 indicates that the data are nearly normal. the two indices are not coherent

shapiro.test(Seeds[Test=="PS"])  
#H0=data come from a normal distribution
#p-value=0.07 --> weak evidence against H0 so data come from a normal distribution
#anna# W=0.85 --> data nearly normal

#we are testing that the number of seeds depend from the tests we are doing and the significance of difference between the two tests:
#H0=number of seeds depend from the tests we are doing 
#anna# H0 should be that there is no significant relationship between the two variables (test and seeds)
model1 <- lm(Seeds ~ Test, data  =  Emerus_pollsup)
summary(model1)
#P-value=0.14 no evidence against H0 so the number of seeds depend from the test we are doing
#anna# p-value is pretty high (H0 true) so it should mean that the difference between tests are not that statistically significant
#comment on R^2?
#anna# R-squared statistic provides a measure of how well the model is fitting the dataset
#anna# R^2=0.1085 suggests that about 10.8% of the variability in seed counts can be explained by the indipendent 
#anna# variable (Test) through the model used (Lineal model)

########################### updated dataset (old+new)  ############################
#### reproductive strategies
EmerusUpdate_repstrat <- read.table("RepStrat_EmerusUpdate.txt", header=TRUE, sep = "")
summary(EmerusUpdate_repstrat)
attach(EmerusUpdate_repstrat)

boxplot((EmerusUpdate_repstrat$Seeds[Test=="A"]), (EmerusUpdate_repstrat$Seeds[Test== "SS"]), (EmerusUpdate_repstrat$Seeds[Test== "C"]),
        names= c("Agamospermy", "Spontaneous selfing", "Control"), main= "Total number of seeds (2024)", col = c("blue", "red", "yellow"))

####################model ############### NON SERVE
modelloupdate <- glm(cbind(Seeds, Abortive_ovules) ~ Test, family = binomial,
                     data = EmerusUpdate_repstrat)
odsupdate <- modelloupdate$deviance/modelloupdate$df.residual
odsupdate

modello2update <- glm(cbind(Seeds, Abortive_ovules) ~ Test, family = quasibinomial,
                      data = EmerusUpdate_repstrat)
ods2update <- modello2update$deviance/modello2update$df.residual
ods2update

pairs(emmeans(modello2update, ~ Test))
########################

#### pollen supplementation

EmerusUpdate_pollsup <- read.table("PollSup_EmerusUpdate.txt", header=TRUE, sep = "")
summary(EmerusUpdate_pollsup)
attach(EmerusUpdate_pollsup)

boxplot((EmerusUpdate_pollsup$Seeds[Test=="PS"]), (EmerusUpdate_pollsup$Seeds[Test== "N"]),
        names= c("Pollen Supplementation", "Normal"), main= "Total number of seeds (2024)", col = c("violet","yellow"))

bartlett.test(Seeds ~ Test)       
   #Bartlett's K-squared = 2.1792, df = 1, p-value = 0.1399
#anna# no evidence against H0 so the two variances are equal

shapiro.test(Seeds[Test=="N"])  
   #W = 0.66568, p-value = 2.772e-07
shapiro.test(Seeds[Test=="PS"])  
   #W = 0.71996, p-value = 1.007e-05
#anna# both shapiro tests for normality seem to show that with the updated dataset the variable seed doesn't have a normal 
#anna# distribution with neither normal nor PS conditions

model1update <- lm(Seeds ~ Test, data  =  EmerusUpdate_pollsup)
summary(model1update)
##output:
   #Residuals:
   #  Min     1Q Median     3Q    Max 
   #-2.038 -1.873 -1.375  2.625  5.962 

   #Coefficients:
   #            Estimate Std. Error t value Pr(>|t|)   
   #(Intercept)   1.3750     0.4307   3.192  0.00232 **
   #TestPS        0.6635     0.6433   1.031  0.30683   
   #---
   #Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

   #Residual standard error: 2.437 on 56 degrees of freedom
   #Multiple R-squared:  0.01864,	Adjusted R-squared:  0.001114 
   #F-statistic: 1.064 on 1 and 56 DF,  p-value: 0.3068
#anna# as before, the difference between tests appears to not be statistically significant (high p-value)
#anna# here, R^2 is even lower, showing that the model might not be fitted for the dataset used, since the value
#anna# tells only 1% of variability in seeds production can be explained/caused by the conditions (tests)
