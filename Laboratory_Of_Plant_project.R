############## REPRODUCTION STRATEGIES (REPSTRAT) ##################à##################
Emerus_repstrat <- read.table("RepStrat_Emerus2023.txt", header=TRUE, sep = "")
summary(Emerus_repstrat)

attach(Emerus_repstrat)
## detach(Emerus_repstrat)

require (multcomp)

boxplot((Emerus_repstrat$Seeds[Test=="A"]), (Emerus_repstrat$Seeds[Test== "SS"]), (Emerus_repstrat$Seeds[Test== "C"]),
        names= c("Agamospermy", "Spontaneous selfing", "Control"), main= "Tot seeds")


##### binomial family
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
