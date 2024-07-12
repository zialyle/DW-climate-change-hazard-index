library(tidyverse)
library(ggplot2)
library(ggridges)
library(dplyr)
library(stringr)
library(usmap)
library(ggmap)
library(maps)
library(cowplot)
library(gridExtra)
library(sf)
library(tigris)
library(formattable)
library(ggpubr)
library(reshape2)
library(factoextra)

options(scipen=999)

#Adding util projections to data frame
ALLpws_withhazards <- read_csv("pws_withhazards.csv")
pws <- data.frame(ALLpws_withhazards)

#Potential risks to water resources (water risk or SLR)
water_resources_risk <- pws %>% filter((SLR_indicator == 1 | water_stress >= 10))

#Potential risks to infrastructure (extreme heat, precipitation or wildfires)
infra_risk <- pws %>% filter(RC_avg_wildfire >= quantile(pws$RC_avg_wildfire, 0.9)| RC_highest_precip_5d >= quantile(pws$RC_highest_precip_5d, 0.9) | Diff_maxtemp_5d >= quantile(pws$Diff_maxtemp_5d, 0.9))

#Potential risks to operations (energy demand, freeze-thaw cycles)
ops_risk <- pws %>% filter((energy_demand >= 8 | RC_FT >= 0))

#Compounded Hazards - Thresholds
compounding <- pws
compounding <- compounding %>% mutate(heat_threshold = if_else(Diff_maxtemp_5d >= quantile(pws$Diff_maxtemp_5d, 0.9), 1, 0))
compounding <- compounding %>% mutate(precip_threshold = if_else(RC_highest_precip_5d >= quantile(pws$RC_highest_precip_5d, 0.9), 1, 0))
compounding <- compounding %>% mutate(SLR_threshold = if_else(SLR_indicator == 1, 1, 0))
compounding <- compounding %>% mutate(wildfire_threshold = if_else(RC_avg_wildfire >= quantile(pws$RC_avg_wildfire, 0.9), 1, 0))
compounding <- compounding %>% mutate(FT_threshold = if_else(RC_FT >= 0, 1, 0))
compounding <- compounding %>% mutate(waterstress_threshold = if_else(water_stress >= 10, 1, 0))
compounding <- compounding %>% mutate(energydemand_threshold = if_else(energy_demand >= 8, 1, 0))

compounding$sum <- compounding$heat_threshold + compounding$precip_threshold + compounding$SLR_threshold + compounding$wildfire_threshold + compounding$FT_threshold + compounding$waterstress_threshold + compounding$energydemand_threshold

#Compounded Hazards - Means
compounding_means <- pws
compounding_means <- compounding_means %>% mutate(heat_threshold = if_else(Diff_maxtemp_5d >= mean(Diff_maxtemp_5d), 1, 0))
compounding_means <- compounding_means %>% mutate(precip_threshold = if_else(RC_highest_precip_5d >= mean(RC_highest_precip_5d), 1, 0))
compounding_means <- compounding_means %>% mutate(SLR_threshold = if_else(SLR_indicator >= 1, 1, 0))
compounding_means <- compounding_means %>% mutate(wildfire_threshold = if_else(RC_avg_wildfire >= mean(RC_avg_wildfire), 1, 0))
compounding_means <- compounding_means %>% mutate(FT_threshold = if_else(RC_FT >= mean(RC_FT), 1, 0))
compounding_means <- compounding_means %>% mutate(waterstress_threshold = if_else(water_stress >= mean(water_stress), 1, 0))
compounding_means <- compounding_means %>% mutate(energydemand_threshold = if_else(energy_demand >= mean(energy_demand), 1, 0))

compounding_means$sum <- compounding_means$heat_threshold + compounding_means$precip_threshold + compounding_means$SLR_threshold + compounding_means$wildfire_threshold + compounding_means$FT_threshold + compounding_means$waterstress_threshold + compounding_means$energydemand_threshold

#Compounded Hazards - Medians
compounding_median <- pws
compounding_median <- compounding_median %>% mutate(heat_threshold = if_else(Diff_maxtemp_5d >= median(Diff_maxtemp_5d), 1, 0))
compounding_median <- compounding_median %>% mutate(precip_threshold = if_else(RC_highest_precip_5d >= median(RC_highest_precip_5d), 1, 0))
compounding_median <- compounding_median %>% mutate(SLR_threshold = if_else(SLR_indicator >= 1, 1, 0))
compounding_median <- compounding_median %>% mutate(wildfire_threshold = if_else(RC_avg_wildfire >= median(RC_avg_wildfire), 1, 0))
compounding_median <- compounding_median %>% mutate(FT_threshold = if_else(RC_FT >= median(RC_FT), 1, 0))
compounding_median <- compounding_median %>% mutate(waterstress_threshold = if_else(water_stress >= median(water_stress), 1, 0))
compounding_median <- compounding_median %>% mutate(energydemand_threshold = if_else(energy_demand >= median(energy_demand), 1, 0))

compounding_median$sum <- compounding_median$heat_threshold + compounding_median$precip_threshold + compounding_median$SLR_threshold + compounding_median$wildfire_threshold + compounding_median$FT_threshold + compounding_median$waterstress_threshold + compounding_median$energydemand_threshold

#Combined Climate Hazard Index - Min Max Linear Aggregation
hazard_index_minmax <- pws
hazard_index_minmax$heat_index <- (hazard_index_minmax$Diff_maxtemp_5d - min(hazard_index_minmax$Diff_maxtemp_5d))/(max(hazard_index_minmax$Diff_maxtemp_5d)-min(hazard_index_minmax$Diff_maxtemp_5d))
hazard_index_minmax$energydemand_index <- (hazard_index_minmax$energy_demand - min(hazard_index_minmax$energy_demand))/(max(hazard_index_minmax$energy_demand)-min(hazard_index_minmax$energy_demand))
hazard_index_minmax$FT_index <- (hazard_index_minmax$RC_FT - min(hazard_index_minmax$RC_FT))/(max(hazard_index_minmax$RC_FT)-min(hazard_index_minmax$RC_FT))
hazard_index_minmax$FT_index <- 1 - hazard_index_minmax$FT_index
hazard_index_minmax$extremeprecip_index <- (hazard_index_minmax$RC_highest_precip_5d - min(hazard_index_minmax$RC_highest_precip_5d))/(max(hazard_index_minmax$RC_highest_precip_5d)-min(hazard_index_minmax$RC_highest_precip_5d))
hazard_index_minmax$waterrisk_index <- (hazard_index_minmax$water_stress - min(hazard_index_minmax$water_stress))/(max(hazard_index_minmax$water_stress)-min(hazard_index_minmax$water_stress))
hazard_index_minmax$SLR_index <- hazard_index_minmax$SLR_indicator
hazard_index_minmax$wildfirerisk_index <- (hazard_index_minmax$RC_avg_wildfire - min(hazard_index_minmax$RC_avg_wildfire))/(max(hazard_index_minmax$RC_avg_wildfire)-min(hazard_index_minmax$RC_avg_wildfire))

hazard_index_minmax$hazard_index <- (hazard_index_minmax$heat_index + hazard_index_minmax$energydemand_index + hazard_index_minmax$FT_index + hazard_index_minmax$extremeprecip_index+hazard_index_minmax$waterrisk_index + hazard_index_minmax$SLR_index + hazard_index_minmax$wildfirerisk_index)/7

#Designating Hazard Index Bins - Min Max Linear Aggregation
hazard_tags <- c("Minimal","Low", "Moderate", "High")
hazard_breaks_minmax <- c(min(hazard_index_minmax$hazard_index),quantile(hazard_index_minmax$hazard_index, 0.25),quantile(hazard_index_minmax$hazard_index, 0.5),quantile(hazard_index_minmax$hazard_index, 0.75), max(hazard_index_minmax$hazard_index))
hazard_index_minmax$hazard_index_group <- cut(hazard_index_minmax$hazard_index, 
                                              breaks=hazard_breaks_minmax, 
                                              include.lowest=TRUE, 
                                              right=FALSE, 
                                              labels=hazard_tags)

#Climate Hazard Thresholds Exceeded - Min Max Linear Aggregation
hazard_index_minmax <- hazard_index_minmax %>% mutate(heat_threshold = if_else(Diff_maxtemp_5d >= quantile(pws$Diff_maxtemp_5d, 0.9), 1, 0))
hazard_index_minmax <- hazard_index_minmax %>% mutate(precip_threshold = if_else(RC_highest_precip_5d >= quantile(pws$RC_highest_precip_5d, 0.9), 1, 0))
hazard_index_minmax <- hazard_index_minmax %>% mutate(SLR_threshold = if_else(SLR_indicator == 1, 1, 0))
hazard_index_minmax <- hazard_index_minmax %>% mutate(wildfire_threshold = if_else(RC_avg_wildfire >= quantile(pws$RC_avg_wildfire, 0.9), 1, 0))
hazard_index_minmax <- hazard_index_minmax %>% mutate(FT_threshold = if_else(RC_FT > 0, 1, 0))
hazard_index_minmax <- hazard_index_minmax %>% mutate(waterstress_threshold = if_else(water_stress >= 10, 1, 0))
hazard_index_minmax <- hazard_index_minmax %>% mutate(energydemand_threshold = if_else(energy_demand >= 8, 1, 0))

hazard_index_minmax$sum <- hazard_index_minmax$heat_threshold + hazard_index_minmax$precip_threshold + hazard_index_minmax$SLR_threshold + hazard_index_minmax$wildfire_threshold + hazard_index_minmax$FT_threshold + hazard_index_minmax$waterstress_threshold + hazard_index_minmax$energydemand_threshold

#Grouping Analysis - Min Max Linear Aggregation
means <- c(mean(hazard_index_minmax$hazard_index),mean(hazard_index_minmax$heat_index), mean(hazard_index_minmax$FT_index), mean(hazard_index_minmax$extremeprecip_index), mean(hazard_index_minmax$waterrisk_index), mean(hazard_index_minmax$energydemand_index), mean(hazard_index_minmax$SLR_index), mean(hazard_index_minmax$wildfirerisk_index))
hazard_index_minmax_means_NOAARegion <- hazard_index_minmax %>%  group_by(NOAA.Region) %>% summarise(hazard_index = mean(hazard_index), heat_index = mean(heat_index), FT_index = mean(FT_index), extremeprecip_index = mean(extremeprecip_index), waterrisk_index = mean(waterrisk_index), energydemand_index = mean(energydemand_index), SLR_index = mean(SLR_index), wildfire_index = mean(wildfirerisk_index))
hazard_index_minmax_means_Population <- hazard_index_minmax %>%  group_by(PopulationBins) %>% summarise(hazard_index = mean(hazard_index), heat_index = mean(heat_index), FT_index = mean(FT_index), extremeprecip_index = mean(extremeprecip_index), waterrisk_index = mean(waterrisk_index), energydemand_index = mean(energydemand_index), SLR_index = mean(SLR_index), wildfire_index = mean(wildfirerisk_index))
hazard_index_minmax_means_source <- hazard_index_minmax %>%  group_by(primary_source_code) %>% summarise(hazard_index = mean(hazard_index), heat_index = mean(heat_index), FT_index = mean(FT_index), extremeprecip_index = mean(extremeprecip_index), waterrisk_index = mean(waterrisk_index), energydemand_index = mean(energydemand_index), SLR_index = mean(SLR_index), wildfire_index = mean(wildfirerisk_index))
hazard_index_minmax_means_States <- hazard_index_minmax %>%  group_by(State) %>% summarise(hazard_index = mean(hazard_index), heat_index = mean(heat_index), FT_index = mean(FT_index), extremeprecip_index = mean(extremeprecip_index), waterrisk_index = mean(waterrisk_index), energydemand_index = mean(energydemand_index), SLR_index = mean(SLR_index), wildfire_index = mean(wildfirerisk_index))

#Combined Climate Hazard Index - Min Max Linear Aggregation, no Sea Level Rise
hazard_index_minmax$hazard_index_noSLR <- (hazard_index_minmax$heat_index + hazard_index_minmax$energydemand_index + hazard_index_minmax$FT_index+ hazard_index_minmax$extremeprecip_index+hazard_index_minmax$waterrisk_index + hazard_index_minmax$wildfirerisk_index)/6
hazard_index_minmax_means_noSLR_NOAARegion <- hazard_index_minmax %>%  group_by(NOAA.Region) %>% summarise(hazard_index_noSLR = mean(hazard_index_noSLR), heat_index = mean(heat_index), FT_index = mean(FT_index), extremeprecip_index = mean(extremeprecip_index), waterrisk_index = mean(waterrisk_index), energydemand_index = mean(energydemand_index), wildfire_index = mean(wildfirerisk_index))

#Combined Climate Hazard Index - Min Max Linear Aggregation, no Freeze-Thaw Cycles
hazard_index_minmax$hazard_index_noFT <- (hazard_index_minmax$heat_index + hazard_index_minmax$energydemand_index + hazard_index_minmax$SLR_index + hazard_index_minmax$extremeprecip_index+hazard_index_minmax$waterrisk_index + hazard_index_minmax$wildfirerisk_index)/6
hazard_index_minmax_means_noFT_NOAARegion <- hazard_index_minmax %>%  group_by(NOAA.Region) %>% summarise(hazard_index_noFT = mean(hazard_index_noFT), heat_index = mean(heat_index), SLR_index = mean(SLR_index), extremeprecip_index = mean(extremeprecip_index), waterrisk_index = mean(waterrisk_index), energydemand_index = mean(energydemand_index), wildfire_index = mean(wildfirerisk_index))

#Exposure
hazard_index_minmax$exposure <- hazard_index_minmax$hazard_index * hazard_index_minmax$population_served_count

#Writing CSV with hazard index values for each utilty
write.table(hazard_index_minmax[, c(1:16,47, 53:69, 72)], file = 'hazard_index_byutility.csv', col.names = TRUE, row.names = FALSE, sep = ",")

#Correlation Analysis
hazard_cor_table <- cor(hazard_index_minmax[, c('heat_index','extremeprecip_index','SLR_index', "wildfirerisk_index", 'energydemand_index', 'FT_index', 'waterrisk_index', 'hazard_index')], use='pairwise.complete.obs', method = "spearman")
round(hazard_cor_table, 2)
hcor <- round(hazard_cor_table, 2)
upper <- hcor
upper[upper.tri(hcor)]<-""
upper<-as.data.frame(upper)
upper


#Combined Climate Hazard Index - Z Score Linear Aggregation
hazard_index_zscore <- pws
hazard_index_zscore$heat_index <- (hazard_index_zscore$Diff_maxtemp_5d-mean(hazard_index_zscore$Diff_maxtemp_5d))/sd(hazard_index_zscore$Diff_maxtemp_5d)
hazard_index_zscore$energydemand_index <- (hazard_index_zscore$energy_demand-mean(hazard_index_zscore$energy_demand))/sd(hazard_index_zscore$energy_demand)
hazard_index_zscore$FT_index <- (hazard_index_zscore$RC_FT - mean(hazard_index_zscore$RC_FT))/sd(hazard_index_zscore$RC_FT)
hazard_index_zscore$FT_index <- 1 - hazard_index_zscore$FT_index
hazard_index_zscore$extremeprecip_index <- (hazard_index_zscore$RC_highest_precip_5d-mean(hazard_index_zscore$RC_highest_precip_5d))/sd(hazard_index_zscore$RC_highest_precip_5d)
hazard_index_zscore$waterrisk_index <- (hazard_index_zscore$water_stress-mean(hazard_index_zscore$water_stress))/sd(hazard_index_zscore$water_stress)
hazard_index_zscore$wildfirerisk_index <- (hazard_index_zscore$RC_avg_wildfire-mean(hazard_index_zscore$RC_avg_wildfire))/sd(hazard_index_zscore$RC_avg_wildfire)
hazard_index_zscore$SLR_index <- (hazard_index_zscore$SLR_indicator-mean(hazard_index_zscore$SLR_indicator))/sd(hazard_index_zscore$SLR_indicator)

hazard_index_zscore$hazard_index <- (hazard_index_zscore$heat_index + hazard_index_zscore$energydemand_index + hazard_index_zscore$FT_index + hazard_index_zscore$extremeprecip_index+hazard_index_zscore$waterrisk_index + hazard_index_zscore$SLR_index + hazard_index_zscore$wildfirerisk_index)/7

#Designating Hazard Index Bins - Z Score Aggregation
hazard_tags <- c("Minimal","Low", "Moderate", "High")
hazard_breaks_zscore <- c(min(hazard_index_zscore$hazard_index),quantile(hazard_index_zscore$hazard_index, 0.25),quantile(hazard_index_zscore$hazard_index, 0.5),quantile(hazard_index_zscore$hazard_index, 0.75), max(hazard_index_zscore$hazard_index))
hazard_index_zscore$hazard_index_group <- cut(hazard_index_zscore$hazard_index, 
                          breaks=hazard_breaks_zscore, 
                          include.lowest=TRUE, 
                          right=FALSE, 
                          labels=hazard_tags)

#Comparison of Min Max Aggregation and Z Score Aggregation Methods for Index
comparison <- hazard_index_minmax[, c("pwsid","hazard_index")]
names(comparison)[names(comparison) == "hazard_index"] <- "hazard_index_minmax"
comparison <- merge(comparison, hazard_index_zscore[, c("pwsid","hazard_index")], by = "pwsid")
names(comparison)[names(comparison) == "hazard_index"] <- "hazard_index_zscore"
cor(comparison$hazard_index_minmax, comparison$hazard_index_zscore,  method = "pearson", use = "complete.obs")

