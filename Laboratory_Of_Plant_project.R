############## REPRODUCTION STRATEGIES (REPSTRAT) ####################################
Emerus_repstrat <- read.table("RepStrat_Emerus2023.txt", header=TRUE, sep = "")
summary(Emerus_repstrat)

attach(Emerus_repstrat)
## detach(Emerus_repstrat)

require (multcomp)

boxplot((Emerus_repstrat$Seeds[Test=="A"]), (Emerus_repstrat$Seeds[Test== "SS"]), (Emerus_repstrat$Seeds[Test== "C"]),
        names= c("Agamospermy", "Spontaneous selfing", "Control"), main= "Tot seeds")


##### binomial family ## NON SERVE
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

########################### POLLEN SUPPLEMENTATION ####################à
Emerus_pollsup <- read.table("PollSup_Emerus2023.txt", header=TRUE, sep = "")
summary(Emerus_pollsup)
attach(Emerus_pollsup)

boxplot((Emerus_pollsup$Seeds[Test=="PS"]), (Emerus_pollsup$Seeds[Test== "N"]),
        names= c("Pollen Supplementation", "Normal"), main= "Total number of seeds(2023)", col = c("violet", "yellow"))


bartlett.test(Seeds ~ Test) #verifica che le varianze siano omogenee
#H0=variance in each group is the same.
#p-value=0.85 --> no evidence against H0 so the two variances are equal

shapiro.test(Seeds[Test=="N"])#verifica la normalità
#H0=data come from a normal distribution
#p-value=0.01 --> moderate evidence against H0 [so actually they may not come from a not normal distribution????]
shapiro.test(Seeds[Test=="PS"])  #verifica la normalità
#H0=data come from a normal distribution
#p-value=0.07 --> weak evidence against H0 so data come from a normal distribution

#we are testing that the number of seeds depend from the tests we are doing
#H0=number of seeds depend from the tests we are doing
model1 <- lm(Seeds ~ Test, data  =  Emerus_pollsup)
summary(model1)
#P-value=0.14 no evidence against H0 so the number of seeds depend from the test we are doing
#comment on R^2?

########################### updated dataset (old+new)  ############################
#### reproductive strategies
EmerusUpdate_repstrat <- read.table("RepStrat_EmerusUpdate.txt", header=TRUE, sep = "")
summary(EmerusUpdate_repstrat)
attach(EmerusUpdate_repstrat)

boxplot((EmerusUpdate_repstrat$Seeds[Test=="A"]), (EmerusUpdate_repstrat$Seeds[Test== "SS"]), (EmerusUpdate_repstrat$Seeds[Test== "C"]),
        names= c("Agamospermy", "Spontaneous selfing", "Control"), main= "Total number of seeds (2024)", col = c("blue", "red", "yellow"))

###model ## NON SERVE
modelloupdate <- glm(cbind(Seeds, Abortive_ovules) ~ Test, family = binomial,
                     data = EmerusUpdate_repstrat)
odsupdate <- modelloupdate$deviance/modelloupdate$df.residual
odsupdate

modello2update <- glm(cbind(Seeds, Abortive_ovules) ~ Test, family = quasibinomial,
                      data = EmerusUpdate_repstrat)
ods2update <- modello2update$deviance/modello2update$df.residual
ods2update

pairs(emmeans(modello2update, ~ Test))


#### pollen supplementation

EmerusUpdate_pollsup <- read.table("PollSup_EmerusUpdate.txt", header=TRUE, sep = "")
summary(EmerusUpdate_pollsup)
attach(EmerusUpdate_pollsup)

boxplot((EmerusUpdate_pollsup$Seeds[Test=="PS"]), (EmerusUpdate_pollsup$Seeds[Test== "N"]),
        names= c("Pollen Supplementation", "Normal"), main= "Total number of seeds (2024)", col = c("violet","yellow"))
