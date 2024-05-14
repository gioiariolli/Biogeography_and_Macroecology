# 14th May 2024
library(sars)
library(tidyverse)

# Doesn't have anything to do with the lesson, but for the future, if one package you have installed has a new version that is in conflict/masks the past one, than you'll probably be required to select which version/package to use
# To do so you use the following things
library(devtools)
devtools::install_github("r-lib/conflicted")
1
library(conflicted)
library(dplyr)
library(terra)
############

medis <- vect("data/medis.gpkg")
plot(medis) #each island is a poligon with different variables
medis_df <- as.data.frame(medis)
medis_df

data <- read.csv("data/islands_flora.csv")
data

#to calculate the frequency of each species we select only the rows where the species are present (where 1 is found)
freq <- rowSums(data>0)
head(freq) # we see the first 6 frequencies

data[data$Occurrence == 1,]
pres <- data[data$Occurrence ==1,]
pres$Occurrence
plot(pres, cex=0.5)

########
species_freq <- data.frame(
  "taxa" = data$accepted_species,
  "freq" = freq
)

species_freq[species_freq$freq>50,] # To select the most abudant species, those who have more than 50 representations in the different islands
sorted_freq <- species_freq[order(species_freq$freq, decreasing = T),] # to put in order the values in a decreasing order
# The most common species are reducing the beta-diversity because they are increasing the similarity
# whilist the less frequent species, are decreasing similarity and so increasing the island beta-diversity

# To plot the species and have a visual idea of the frequnecy distribution
plot(sorted_freq$freq, ylab= "Number of islands", xlab= "Ranking")
# On the x-axis we have an "index" which is a "rank" from the most to the less common species : "Rank abudance distribution" or "rank frequency distribution" 
# We can see that few species are common while most of them are rare : "being rare is common"
# Most of the diverisity is scattered moving towards the end of the x-axis

isl_rich <- colSums(data>0) # Calculate the sum for all the species except the first column (not numeric values)

# dataframe with species richness for each island
islands_sr <- data.frame(
  "island_id" = colnames (data),
  "sr" = isl_rich
)

medis_df <- medis_df[c(1, 3, 4, 5)]
medis_df <- left_join(medis_df, islands_sr, 
                      join_by("id" == "island_id")) # with the command we call the first and the secon object and see which objects between the two match

medis_df <- medis_df[c(1, 3, 4, 5)]
medis_df <- left_join(medis_df, islands_sr, 
                      join_by("id" == "island_id")) # with the command we call the first and the secon object and see which objects between the two match

medis_df_clean <- medis_df %>% # this is useful to make easy operations in R
  filter(!is.na(sr)) 

# Species area relationship
fit_power <- sar_power(medis_df_clean[c(4,5)])
plot(fit_power) # we obtain a logaritmic plot with area on x-axis and species richnes on y-axis. The dots are the islands
# corse appears to be as an outlier : its values are way to far from others in therms of species richness, 
# this is probably due to the fact that in its data species annex also alien spicies have been included 
# (and not only the native ones as it happens to be for the other islands such as Sicily and Sardinia for example).
