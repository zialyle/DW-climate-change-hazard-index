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

#Adding util projections to data frame
ALLpws_withhazards <- read_csv("pws_withhazards.csv")
pws <- data.frame(ALLpws_withhazards)

#Adding EPA regions data frame
Regions <- read_csv("Regions.csv")
Regions <- data.frame(Regions)
Regions$NOAA.Region <- as.character(Regions$NOAA.Region)

#Adding shape files of states
us_states <- states(cb = TRUE, resolution = "20m")
us_states<- shift_geometry(us_states)
#remove alaska and hawaii
us_states = us_states[!us_states$STATEFP %in% c("02", "15"),]
#merging with regions
us_states_merged <- merge(us_states, Regions, by.x = "NAME", by.y = "original", all.x = FALSE)
#creating region shape files
regions <- us_states_merged %>% 
  group_by(NOAA.Region) %>% 
  summarise()

#Map of states, with NOAA climate regions shaded
usmap <- ggplot() + 
  geom_sf(data = us_states_merged, fill = "grey80", color = "grey70", alpha = 0.5) +
  geom_sf(data = regions, fill = "transparent", color = "gray30", size = 0.5)  + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())

#Adding utilities to map
# Make the  cols spatial 
pws.SP <- st_as_sf(pws, coords = c("centroid_lon", "centroid_lat"), crs = 4326)
# transform to NAD83
pws.SP<-st_transform(x = pws.SP, crs = "ESRI:102003")
# get coordinates and add back to dataframe
pws.SP$utm_E<-st_coordinates(pws.SP)[,1] # get coordinates
pws.SP$utm_N<-st_coordinates(pws.SP)[,2] # get coordinates
# now switch back to lat-long
pws.SP<-st_transform(x = pws.SP, crs = 4326)
# add coordinates to dataframe
pws.SP$lon<-st_coordinates(pws.SP)[,1] # get coordinates
pws.SP$lat<-st_coordinates(pws.SP)[,2] # get coordinates

#DW Utility Changes in Climate Hazard Figures
#Figure 1 - Extreme Heat
usmap + geom_point(data = pws, aes(x = utm_E,y =utm_N, color = Diff_maxtemp_5d), shape = 16) + labs(title = "Changes in Extreme Heat Events - RCP 4.5 Mid Century Projections") + 
  theme(legend.position = "right", axis.title.x=element_blank(), axis.title.y = element_blank(), axis.ticks = element_blank()) + 
  theme(text=element_text(family="Open Sans", size=10)) + 
  scale_color_gradient(low = "white", high = "#ef6f6a", name ="∆ Max 5-day Temp (°C)") 

#Figure 1 -Extreme Precipitation
usmap + geom_point(data = pws, aes(x = utm_E,y =utm_N, color = RC_highest_precip_5d), shape = 16) + labs(title = "Changes in Excessive Precipitation Events - RCP 4.5 Mid Century Projections") + 
  theme(legend.position = "right", axis.title.x=element_blank(), axis.title.y = element_blank(), axis.ticks = element_blank()) + 
  theme(text=element_text(family="Open Sans", size=10)) + 
  scale_color_gradient(low = "white", high = "#55ad89", name ="Relative Change in 5 day \nPrecipitation Totals") 

#Figure 1 -Freeze-Thaw Days
usmap + geom_point(data = pws, aes(x = utm_E,y =utm_N, color = RC_FT), shape = 16) + labs(title = "Changes in Freeze-Thaw Days - RCP 4.5 Mid Century Projections") + 
  theme(legend.position = "right", axis.title.x=element_blank(), axis.title.y = element_blank(), axis.ticks = element_blank()) + 
  theme(text=element_text(family="Open Sans", size=10)) + 
  scale_color_gradient(low = "white", high = "#bb7693", name ="Relative Change in Freeze-Thaw Days") 

#Figure 1 -Energy Demand
usmap + geom_point(data = pws, aes(x = utm_E,y =utm_N, color = energy_demand), shape = 16) + labs(title = "Increased Energy Demand - RCP 4.5 Mid Century Projections") + 
  theme(legend.position = "right", axis.title.x=element_blank(), axis.title.y = element_blank(), axis.ticks = element_blank()) + 
  theme(text=element_text(family="Open Sans", size=10)) + 
  scale_color_gradient(low = "white", high = "#c3bc3f", name ="% Increase in  \nAnnual Energy Demand") 

#Figure 1 -Water Stress
usmap + geom_point(data = pws, aes(x = utm_E,y =utm_N, color = water_stress), shape = 16) + labs(title = "Changes in Water Supply Stress Index - RCP 4.5 Mid Century Projections") + 
  theme(legend.position = "right", axis.title.x=element_blank(), axis.title.y = element_blank(), axis.ticks = element_blank()) + 
  theme(text=element_text(family="Open Sans", size=10)) + 
  scale_color_gradient(low = "white", high = "#6388b4", name ="Changes in Water Supply Stress Index") 

#Figure 1 -Sea Level Rise
usmap + geom_point(data = pws, aes(x = utm_E,y =utm_N, color = SLR_indicator), shape = 16) + labs(title = "Sea Level Rise - RCP 4.5 Mid Century Projections") + 
  theme(legend.position = "right", axis.title.x=element_blank(), axis.title.y = element_blank(), axis.ticks = element_blank()) + 
  theme(text=element_text(family="Open Sans", size=10)) + 
  scale_color_gradient(low = "white", high = "#8cc2ca", name ="Sea Level Rise", limits=c(0,1)) 

#Figure 1 -Wildfires
usmap + geom_point(data = pws, aes(x = utm_E,y =utm_N, color = RC_avg_wildfire), shape = 16) + labs(title = "Fire Weather Index - RCP 4.5 Mid Century Projections") + 
  theme(legend.position = "right", axis.title.x=element_blank(), axis.title.y = element_blank(), axis.ticks = element_blank()) + 
  theme(text=element_text(family="Open Sans", size=10)) + 
  scale_color_gradient(low = "white", high = "#e79315", name ="Fire Weather Index") 

#Figure 2 - Potential Risks to water resources (water risk or SLR)
source_colors <- c(GWP = "#f2bb7b", SWP = "#f2bb7b", GW = "#fe6567",SW ="#912624", GU = "white", GUP = "white")

water_resources_risk <- pws %>% filter((SLR_indicator == 1 | water_stress >= 10))

usmap + geom_point(data = subset(water_resources_risk, primary_source_code %in% c("GW", "SW", "GWP", "SWP")), aes(x = utm_E,y =utm_N, color = primary_source_code, size = PopulationBins), shape = 16) + labs(title = "Projected Water Supply Issues for Drinking Water Utilities") + 
  theme(legend.position = "right", axis.title.x=element_blank(), axis.title.y = element_blank(), axis.ticks = element_blank()) + 
  theme(text=element_text(family="Open Sans", size=10))+ scale_color_manual(values=source_colors)

#Figure 2 - Potential risks to infrastructure (extreme heat, precipitation or wildfires)
infra_risk <- pws %>% filter(RC_avg_wildfire >= quantile(pws$RC_avg_wildfire, 0.9)| RC_highest_precip_5d >= quantile(pws$RC_highest_precip_5d, 0.9) | Diff_maxtemp_5d >= quantile(pws$Diff_maxtemp_5d, 0.9))

usmap + geom_point(data = subset(infra_risk, primary_source_code %in% c("GW", "SW", "GWP", "SWP")), aes(x = utm_E,y =utm_N, color = primary_source_code, size = PopulationBins), shape = 16) + labs(title = "Projected Infrastructure Issues for Drinking Water Utilities") + 
  theme(legend.position = "right", axis.title.x=element_blank(), axis.title.y = element_blank(), axis.ticks = element_blank()) + 
  theme(text=element_text(family="Open Sans", size=10))+ scale_color_manual(values=source_colors)

#Figure 2 - Potential risks to operations (energy demand, freeze-thaw cycles)
ops_risk <- pws %>% filter((energy_demand >= 8 | RC_FT >= 0))

usmap + geom_point(data = subset(ops_risk, primary_source_code %in% c("GW", "SW", "GWP", "SWP")), aes(x = utm_E,y =utm_N, color = primary_source_code, size = PopulationBins), shape = 16) + labs(title = "Projected Water Supply Issues for Drinking Water Utilities serving < 10,000 Customers") + 
  theme(legend.position = "right", axis.title.x=element_blank(), axis.title.y = element_blank(), axis.ticks = element_blank()) + 
  theme(text=element_text(family="Open Sans", size=10))+ scale_color_manual(values=source_colors)


#Figure 4 - Combined Climate Hazard Index - Min Max Linear Aggregation
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

hazard_boxplot <- hazard_index_minmax %>% 
  ggplot() + geom_density(aes(x= hazard_index), fill = "black") +theme_minimal() + theme(axis.ticks=element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  theme(text=element_text(family="Open Sans")) +  labs( x = "Hazard Index" ) + xlim(0, 1) + ylim(0, 15)

heat_boxplot <- hazard_index_minmax %>% 
  ggplot() + geom_density(aes(x= heat_index), fill="#ef6f6a") +theme_minimal() + theme(axis.ticks=element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  theme(text=element_text(family="Open Sans")) +  labs( x = "Excessive Heat Index" ) + ylim(0, 15)

FT_boxplot <- hazard_index_minmax %>% 
  ggplot() + geom_density(aes(x= FT_index), fill="#bb7693") +theme_minimal() + theme(axis.ticks=element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  theme(text=element_text(family="Open Sans")) +  labs( x = "Freeze-Thaw Cycle Index" ) + ylim(0, 15)

extremeprecip_boxplot <- hazard_index_minmax %>% 
  ggplot() + geom_density(aes(x= extremeprecip_index), fill="#55ad89") +theme_minimal() + theme(axis.ticks=element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  theme(text=element_text(family="Open Sans")) +  labs( x = "Extreme Precipitation Index" ) + ylim(0, 15)

energydemand_boxplot <- hazard_index_minmax %>% 
  ggplot() + geom_density(aes(x= energydemand_index), fill="#c3bc3f") +theme_minimal() + theme(axis.ticks=element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  theme(text=element_text(family="Open Sans")) +  labs( x = "Energy Demand Index" ) + ylim(0, 15)

waterrisk_boxplot <- hazard_index_minmax %>% 
  ggplot() + geom_density(aes(x= waterrisk_index), fill="#6388b4")+theme_minimal() + theme(axis.ticks=element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  theme(text=element_text(family="Open Sans")) +  labs( x = "Water Supply Stress Index" ) + ylim(0, 15)

SLR_boxplot <- hazard_index_minmax %>% 
  ggplot() + geom_density(aes(x= SLR_index), fill="#8cc2ca") +theme_minimal() + theme(axis.ticks=element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  theme(text=element_text(family="Open Sans")) +  labs( x = "Sea Level Rise Index" ) + ylim(0, 15)

wildfire_boxplot <- hazard_index_minmax %>% 
  ggplot() + geom_density(aes(x= wildfirerisk_index), fill="#ffae34")+theme_minimal() + theme(axis.ticks=element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  theme(text=element_text(family="Open Sans")) +  labs( x = "Wildfire Index" ) + ylim(0, 15)

grid.arrange(hazard_boxplot, heat_boxplot, FT_boxplot, extremeprecip_boxplot,energydemand_boxplot, waterrisk_boxplot, SLR_boxplot, wildfire_boxplot, ncol = 1)

#Figure 5
hazard_tags <- c("Minimal","Low", "Moderate", "High")
hazard_cols    <- c( "Minimal" = "yellow", "Low" = "#CEA537", "Moderate" = "#A45E20", "High"="darkred")
hazard_breaks_minmax <- c(min(hazard_index_minmax$hazard_index),quantile(hazard_index_minmax$hazard_index, 0.25),quantile(hazard_index_minmax$hazard_index, 0.5),quantile(hazard_index_minmax$hazard_index, 0.75), max(hazard_index_minmax$hazard_index))
hazard_index_minmax$hazard_index_group <- cut(hazard_index_minmax$hazard_index, 
                                              breaks=hazard_breaks_minmax, 
                                              include.lowest=TRUE, 
                                              right=FALSE, 
                                              labels=hazard_tags)
usmap + geom_point(data = hazard_index_minmax, aes(x = utm_E,y =utm_N, color = hazard_index_group, size = PopulationBins), shape = 16) + 
  labs(title = "Drinking Water Utilities (n = 42,786)", color = "Combined Hazard Index") + 
  theme(legend.position = "right", axis.title.x=element_blank(), axis.title.y = element_blank(), axis.ticks = element_blank()) + 
  scale_color_manual(values = hazard_cols) +
  scale_alpha_manual(values=c(0.1, 0.5, .6, 0.5)) +
  theme(text=element_text(family="Open Sans"))

#Supplemental Information Figures

#Extreme Heat
ggplot(data=subset(pws,!is.na(NOAA.Region)), aes(x = Diff_maxtemp_5d, y = fct_reorder(NOAA.Region,Diff_maxtemp_5d))) + 
  geom_boxplot() + theme_minimal() +
  labs(y= "NOAA Climate Region", x = "Difference in Maximum Average 5-day Period Temperature (degC)") +
  theme(text=element_text(family="Open Sans", size=10)) 

ggplot(data=subset(pws,!is.na(NOAA.Region)), aes(x=Diff_maxtemp_5d, group=PopulationBins, fill=PopulationBins)) + geom_density(adjust=1.5) +
  scale_fill_brewer(palette = "Spectral") + theme_minimal() +
  labs(x = "Difference in Maximum Average 5-day Period Temperature (degC)") + theme(text=element_text(family="Open Sans", size=10)) +
  facet_wrap(~PopulationBins) +
  theme(legend.position="none", panel.spacing = unit(0.1, "lines"), axis.ticks.x=element_blank())

ggplot(subset(pws, primary_source_code %in% c("GW", "SW", "GWP", "SWP")), aes(x=Diff_maxtemp_5d, group=primary_source_code, fill=primary_source_code)) + geom_density(adjust=1.5) +
  scale_fill_brewer(palette = "Spectral") + theme_minimal() +
  labs(x = "Difference in Maximum Average 5-day Period Temperature (degC)") + theme(text=element_text(family="Open Sans", size=10)) + 
  facet_wrap(~primary_source_code) +
  theme(legend.position="none", panel.spacing = unit(0.1, "lines"), axis.ticks.x=element_blank())

#Extreme Precipitation
ggplot(data=subset(pws, !is.na(NOAA.Region)), aes(x = RC_highest_precip_5d, y = fct_reorder(NOAA.Region, RC_highest_precip_5d))) + 
  geom_boxplot() + theme_minimal() +
  labs(y= "NOAA Climate Region", x = "Relative Change in Highest 5-Day Precipitation (%)") +
  theme(text=element_text(family="Open Sans", size=10))

ggplot(data=subset(pws,!is.na(NOAA.Region)), aes(x=RC_highest_precip_5d, group=PopulationBins, fill=PopulationBins)) + geom_density(adjust=1.5) +
  scale_fill_brewer(palette = "Spectral") + theme_minimal() +
  labs(x = "Relative Change in 5 day \nPrecipitation Totals") + theme(text=element_text(family="Open Sans", size=10)) +
  facet_wrap(~PopulationBins) +
  theme(legend.position="none", panel.spacing = unit(0.1, "lines"), axis.ticks.x=element_blank())

ggplot(subset(pws, primary_source_code %in% c("GW", "SW", "GWP", "SWP")), aes(x=RC_highest_precip_5d, group=primary_source_code, fill=primary_source_code)) + geom_density(adjust=1.5) +
  scale_fill_brewer(palette = "Spectral") + theme_minimal() +
  labs(x = "Relative Change in 5 day \nPrecipitation Totals") + theme(text=element_text(family="Open Sans", size=10)) + 
  facet_wrap(~primary_source_code) +
  theme(legend.position="none", panel.spacing = unit(0.1, "lines"), axis.ticks.x=element_blank())

#Freeze-Thaw Days
ggplot(data=subset(pws, !is.na(NOAA.Region)), aes(x = RC_FT, y = fct_reorder(NOAA.Region, RC_FT))) + 
  geom_boxplot() + theme_minimal() +
  labs(y= "NOAA Climate Region", x = "Relative Change in Freeze-Thaw Days") +
  theme(text=element_text(family="Open Sans", size=10))

ggplot(data=subset(pws,!is.na(NOAA.Region)), aes(x=RC_FT, group=PopulationBins, fill=PopulationBins)) + geom_density(adjust=1.5) +
  scale_fill_brewer(palette = "Spectral") + theme_minimal() +
  labs(x = "Relative Change in Freeze-Thaw Days") + theme(text=element_text(family="Open Sans", size=10)) +
  facet_wrap(~PopulationBins) +
  theme(legend.position="none", panel.spacing = unit(0.1, "lines"), axis.ticks.x=element_blank())

ggplot(subset(pws, primary_source_code %in% c("GW", "SW", "GWP", "SWP")), aes(x=RC_FT, group=primary_source_code, fill=primary_source_code)) + geom_density(adjust=1.5) +
  scale_fill_brewer(palette = "Spectral") + theme_minimal() +
  labs(x = "Relative Change in Freeze-Thaw Days") + theme(text=element_text(family="Open Sans", size=10)) + 
  facet_wrap(~primary_source_code) +
  theme(legend.position="none", panel.spacing = unit(0.1, "lines"), axis.ticks.x=element_blank())

#Energy Demand 
ggplot(data=subset(pws, !is.na(NOAA.Region)), aes(x = energy_demand, y = fct_reorder(NOAA.Region, energy_demand))) + 
  geom_boxplot() + theme_minimal() +
  labs(y= "NOAA Climate Region", x = "Relative Change in Energy Demand") +
  theme(text=element_text(family="Open Sans", size=10))

ggplot(data=subset(pws,!is.na(NOAA.Region)), aes(x=energy_demand, group=PopulationBins, fill=PopulationBins)) + geom_density(adjust=1.5) +
  scale_fill_brewer(palette = "Spectral") + theme_minimal() +
  labs(x = "% Increase in Energy Demand") + theme(text=element_text(family="Open Sans", size=10)) + 
  facet_wrap(~PopulationBins) +
  theme(legend.position="none", panel.spacing = unit(0.1, "lines"), axis.ticks.x=element_blank())

ggplot(subset(pws, primary_source_code %in% c("GW", "SW", "GWP", "SWP")), aes(x=energy_demand, group=primary_source_code, fill=primary_source_code)) + geom_density(adjust=1.5) +
  scale_fill_brewer(palette = "Spectral") + theme_minimal() +
  labs(x = "% Increase in Energy Demand") + theme(text=element_text(family="Open Sans", size=10)) + 
  facet_wrap(~primary_source_code) +
  theme(legend.position="none", panel.spacing = unit(0.1, "lines"), axis.ticks.x=element_blank())

#Water Stress 
ggplot(data=subset(pws, !is.na(NOAA.Region)), aes(x = water_stress, y = fct_reorder(NOAA.Region, water_stress))) + 
  geom_boxplot() + theme_minimal() +
  labs(y= "NOAA Climate Region", x = "Changes in Water Supply Stress Index") +
  theme(text=element_text(family="Open Sans", size=10))

ggplot(data=subset(pws,!is.na(NOAA.Region)), aes(x=water_stress, group=PopulationBins, fill=PopulationBins)) + geom_density(adjust=1.5) +
  scale_fill_brewer(palette = "Spectral") + theme_minimal() +
  labs(x = "Changes in Water Supply Stress Index") + theme(text=element_text(family="Open Sans", size=10)) + 
  facet_wrap(~PopulationBins) +
  theme(legend.position="none", panel.spacing = unit(0.1, "lines"), axis.ticks.x=element_blank())

ggplot(subset(pws, primary_source_code %in% c("GW", "SW", "GWP", "SWP")), aes(x=water_stress, group=primary_source_code, fill=primary_source_code)) + geom_density(adjust=1.5) +
  scale_fill_brewer(palette = "Spectral") + theme_minimal() +
  labs(x = "Changes in Water Supply Stress Index") + theme(text=element_text(family="Open Sans", size=10)) + 
  facet_wrap(~primary_source_code) +
  theme(legend.position="none", panel.spacing = unit(0.1, "lines"), axis.ticks.x=element_blank())

#Sea Level Rise
ggplot(data=subset(pws, !is.na(NOAA.Region)), aes(x = SLR_indicator, y = fct_reorder(NOAA.Region, SLR_indicator))) + 
  geom_boxplot() + theme_minimal() +
  labs(y= "NOAA Climate Region", x = "Sea Level Rise") +
  theme(text=element_text(family="Open Sans", size=10))

ggplot(data=subset(pws,!is.na(NOAA.Region)), aes(x=SLR_indicator, group=PopulationBins, fill=PopulationBins)) + geom_density(adjust=1.5) +
  scale_fill_brewer(palette = "Spectral") + theme_minimal() +
  labs(x = "Changes in Sea Level Rise") + theme(text=element_text(family="Open Sans", size=10)) + 
  facet_wrap(~PopulationBins) +
  theme(legend.position="none", panel.spacing = unit(0.1, "lines"), axis.ticks.x=element_blank())

ggplot(subset(pws, primary_source_code %in% c("GW", "SW", "GWP", "SWP")), aes(x=SLR_indicator, group=primary_source_code, fill=primary_source_code)) + geom_density(adjust=1.5) +
  scale_fill_brewer(palette = "Spectral") + theme_minimal() +
  labs(x = "Changes in Sea Level Rise") + theme(text=element_text(family="Open Sans", size=10)) + 
  facet_wrap(~primary_source_code) +
  theme(legend.position="none", panel.spacing = unit(0.1, "lines"), axis.ticks.x=element_blank())

#Wildfires
ggplot(data=subset(pws, !is.na(NOAA.Region)), aes(x = RC_avg_wildfire, y = fct_reorder(NOAA.Region, RC_avg_wildfire))) + 
  geom_boxplot() + theme_minimal() +
  labs(y= "NOAA Climate Region", x = "Relative Changes in Fire Weather Index") +
  theme(text=element_text(family="Open Sans", size=10))

ggplot(data=subset(pws,!is.na(NOAA.Region)), aes(x=RC_avg_wildfire, group=PopulationBins, fill=PopulationBins)) + geom_density(adjust=1.5) +
  scale_fill_brewer(palette = "Spectral") + theme_minimal() +
  labs(x = "Relative Changes in Fire Weather Index") + theme(text=element_text(family="Open Sans", size=10)) +
  facet_wrap(~PopulationBins) +
  theme(legend.position="none", panel.spacing = unit(0.1, "lines"), axis.ticks.x=element_blank())

ggplot(subset(pws, primary_source_code %in% c("GW", "SW", "GWP", "SWP")), aes(x=RC_avg_wildfire, group=primary_source_code, fill=primary_source_code)) + geom_density(adjust=1.5) +
  scale_fill_brewer(palette = "Spectral") + theme_minimal() +
  labs(x = "Relative Changes in Fire Weather Index") + theme(text=element_text(family="Open Sans", size=10)) + 
  facet_wrap(~primary_source_code) +
  theme(legend.position="none", panel.spacing = unit(0.1, "lines"), axis.ticks.x=element_blank())

#All hazards analysis
pws %>% ggplot() + geom_histogram(aes(x= Diff_maxtemp_5d), fill="#ef6f6a") +theme_minimal() + theme(axis.ticks=element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  theme(text=element_text(family="Open Sans")) +  labs( x = "∆ Max 5-day Temp (°C)" ) + geom_vline(xintercept = 2.890183, linetype="dashed",color = "black", size=0.5) +ylim(0,9000)

pws %>% ggplot() + geom_histogram(aes(x= RC_FT), fill="#bb7693") +theme_minimal() + theme(axis.ticks=element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  theme(text=element_text(family="Open Sans")) +  labs( x = "%∆ Freeze-Thaw Days" ) + geom_vline(xintercept = 0, linetype="dashed",color = "black", size=0.5) +ylim(0,9000)

pws %>% ggplot() + geom_histogram(aes(x= RC_highest_precip_5d), fill="#55ad89") +theme_minimal() + theme(axis.ticks=element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  theme(text=element_text(family="Open Sans")) +  labs( x = "%∆ 5-Day Precipitation" ) + geom_vline(xintercept = 10.23256, linetype="dashed",color = "black", size=0.5) +ylim(0,9000)

pws %>% ggplot() + geom_histogram(aes(x= energy_demand), fill="#c3bc3f") +theme_minimal() + theme(axis.ticks=element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  theme(text=element_text(family="Open Sans")) +  labs( x = "%∆ Annual Energy Demand" ) + geom_vline(xintercept = 7.993807, linetype="dashed",color = "black", size=0.5) +ylim(0,9000)

pws %>% ggplot() + geom_histogram(aes(x= water_stress), fill="#6388b4")+theme_minimal() + theme(axis.ticks=element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  theme(text=element_text(family="Open Sans")) +  labs( x = "Water Supply Stress Index" )  + geom_vline(xintercept = 10, linetype="dashed",color = "black", size=0.5) +ylim(0,9000)

pws %>% ggplot() + geom_histogram(aes(x= SLR_indicator), fill="#8cc2ca") +theme_minimal() + theme(axis.ticks=element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  theme(text=element_text(family="Open Sans")) +  labs( x = "Sea Level Rise Index" )  + geom_vline(xintercept = 1, linetype="dashed",color = "black", size=0.5) 

pws %>% ggplot() + geom_histogram(aes(x= RC_avg_wildfire), fill="#ffae34")+theme_minimal() + theme(axis.ticks=element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  theme(text=element_text(family="Open Sans")) +  labs( x = "%∆ Fire Weather Index" )  + geom_vline(xintercept = 10.76143, linetype="dashed",color = "black", size=0.5) +ylim(0,9000)

#Compounded Hazards - Thresholds
compounding_means <- pws
compounding_means <- compounding_means %>% mutate(heat_threshold = if_else(Diff_maxtemp_5d >= mean(Diff_maxtemp_5d), 1, 0))
compounding_means <- compounding_means %>% mutate(precip_threshold = if_else(RC_highest_precip_5d >= mean(RC_highest_precip_5d), 1, 0))
compounding_means <- compounding_means %>% mutate(SLR_threshold = if_else(SLR_indicator >= 1, 1, 0))
compounding_means <- compounding_means %>% mutate(wildfire_threshold = if_else(RC_avg_wildfire >= mean(RC_avg_wildfire), 1, 0))
compounding_means <- compounding_means %>% mutate(FT_threshold = if_else(RC_FT >= mean(RC_FT), 1, 0))
compounding_means <- compounding_means %>% mutate(waterstress_threshold = if_else(water_stress >= mean(water_stress), 1, 0))
compounding_means <- compounding_means %>% mutate(energydemand_threshold = if_else(energy_demand >= mean(energy_demand), 1, 0))

compounding$sum <- compounding$heat_threshold + compounding$precip_threshold + compounding$SLR_threshold + compounding$wildfire_threshold + compounding$FT_threshold + compounding$waterstress_threshold + compounding$energydemand_threshold

#Grouping Analysis - Min-Max Index
means <- c(mean(hazard_index_minmax$hazard_index),mean(hazard_index_minmax$heat_index), mean(hazard_index_minmax$FT_index), mean(hazard_index_minmax$extremeprecip_index), mean(hazard_index_minmax$waterrisk_index), mean(hazard_index_minmax$energydemand_index), mean(hazard_index_minmax$SLR_index), mean(hazard_index_minmax$wildfirerisk_index))
hazard_index_minmax_means_NOAARegion <- hazard_index_minmax %>%  group_by(NOAA.Region) %>% summarise(hazard_index = mean(hazard_index), heat_index = mean(heat_index), FT_index = mean(FT_index), extremeprecip_index = mean(extremeprecip_index), waterrisk_index = mean(waterrisk_index), energydemand_index = mean(energydemand_index), SLR_index = mean(SLR_index), wildfire_index = mean(wildfirerisk_index))
hazard_index_minmax_means_Population <- hazard_index_minmax %>%  group_by(PopulationBins) %>% summarise(hazard_index = mean(hazard_index), heat_index = mean(heat_index), FT_index = mean(FT_index), extremeprecip_index = mean(extremeprecip_index), waterrisk_index = mean(waterrisk_index), energydemand_index = mean(energydemand_index), SLR_index = mean(SLR_index), wildfire_index = mean(wildfirerisk_index))
hazard_index_minmax_means_source <- hazard_index_minmax %>%  group_by(primary_source_code) %>% summarise(hazard_index = mean(hazard_index), heat_index = mean(heat_index), FT_index = mean(FT_index), extremeprecip_index = mean(extremeprecip_index), waterrisk_index = mean(waterrisk_index), energydemand_index = mean(energydemand_index), SLR_index = mean(SLR_index), wildfire_index = mean(wildfirerisk_index))
hazard_index_minmax_means_States <- hazard_index_minmax %>%  group_by(State) %>% summarise(hazard_index = mean(hazard_index), heat_index = mean(heat_index), FT_index = mean(FT_index), extremeprecip_index = mean(extremeprecip_index), waterrisk_index = mean(waterrisk_index), energydemand_index = mean(energydemand_index), SLR_index = mean(SLR_index), wildfire_index = mean(wildfirerisk_index))
us_states_hazards <- merge(us_states_merged, hazard_index_minmax_means_States, by.x = "state", by.y = "State")

ggplot() + 
  geom_sf(data = us_states_hazards, aes(fill = hazard_index)) +
  scale_fill_continuous(low = "yellow", high = "darkred", name = "Average Hazard Index") +
  geom_sf(data = regions, fill = "transparent", color = "gray30", size = 0.5)  + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) + theme(text=element_text(family="Open Sans"))

#Regional Means
cols    <- c( "c1" = "#ef6f6a", "c2" = "#bb7693", "c3" = "#55ad89", "c4"="#c3bc3f","c5"= "#6388b4", "c6" = "#8cc2ca", "c7" = "#ffae34", "c8" = "black")
p1 <- ggplot(data=subset(hazard_index_minmax_means_NOAARegion,!is.na(NOAA.Region)), aes(x = NOAA.Region)) + theme_minimal() + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
p1 <- p1 +
  geom_point(aes(y = heat_index,  color = "c1", size = 0.1)) +
  geom_point(aes( y = FT_index, color = "c2",size = 0.1)) + 
  geom_point(aes( y = extremeprecip_index, color = "c3", size = 0.1)) + 
  geom_point(aes( y = energydemand_index, color = "c4", size = 0.1))+ 
  geom_point(aes( y = waterrisk_index, color = "c5", size = 0.1)) + 
  geom_point(aes( y = SLR_index, color = "c6",size = 0.1)) +
  geom_point(aes( y = wildfire_index, color = "c7",size = 0.1)) +
  geom_point(aes(x=fct_reorder(NOAA.Region, hazard_index), y = hazard_index, color = "c8",size = 0.1), shape = 18) +
  labs( x = "NOAA Region", y = "Min-Max Hazard Index Value" ) + 
  scale_color_manual(name = "Hazard Index Component", 
                     breaks = c("c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8"), 
                     values = cols,
                     labels = c("Excessive Heat", "Freeze-Thaw Cycles", "Extreme Precipitation", "Utility Energy Demand", "Water Supply Stress", "Sea Level Rise", "Wildfires", "Combined Hazard Index")) + coord_flip() +
  theme(text=element_text(family="Open Sans"))
p1

#Population Means
p2 <- ggplot(data=subset(hazard_index_minmax_means_Population,!is.na(PopulationBins)), aes(x = PopulationBins)) + theme_minimal() + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
p2 <- p2 + geom_point(aes(y = heat_index,  color = "c1", size = 0.1)) + 
  geom_point(aes( y = FT_index, color = "c2",size = 0.1)) + 
  geom_point(aes( y = extremeprecip_index, color = "c3", size = 0.1)) + 
  geom_point(aes( y = energydemand_index, color = "c4", size = 0.1))+ 
  geom_point(aes( y = waterrisk_index, color = "c5", size = 0.1)) + 
  geom_point(aes( y = SLR_index, color = "c6",size = 0.1)) +
  geom_point(aes( y = wildfire_index, color = "c7",size = 0.1)) +
  geom_point(aes(x=fct_reorder(PopulationBins, hazard_index), y = hazard_index, color = "c8",size = 0.1), shape = 18) +
  labs( x = "Population Served", y = "Min Max Hazard Index Value" )+ 
  scale_color_manual(name = "Hazard Index Component", 
                     breaks = c("c1", "c2", "c3", "c4", "c5", "c6", "c7"), 
                     values = cols,
                     labels = c("Excessive Heat", "Freeze-Thaw Cycles", "Extreme Precipitation", "Utility Energy Demand", "Water Supply Stress", "Sea Level Rise", "Wildfires")) + coord_flip() +
  theme(text=element_text(family="Open Sans"))
p2

#Source Water Means
p3 <- ggplot(data=subset(hazard_index_minmax_means_source,!is.na(primary_source_code)), aes(x = primary_source_code)) + theme_minimal() + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
p3 <- p3 + geom_point(aes(y = heat_index,  color = "c1", size = 0.1)) + 
  geom_point(aes( y = FT_index, color = "c2",size = 0.1)) + 
  geom_point(aes( y = extremeprecip_index, color = "c3", size = 0.1)) + 
  geom_point(aes( y = energydemand_index, color = "c4", size = 0.1))+ 
  geom_point(aes( y = waterrisk_index, color = "c5", size = 0.1)) + 
  geom_point(aes( y = SLR_index, color = "c6",size = 0.1)) +
  geom_point(aes( y = wildfire_index, color = "c7",size = 0.1)) +
  geom_point(aes(x=fct_reorder(primary_source_code, hazard_index), y = hazard_index, color = "c8",size = 0.1), shape = 18) +
  labs( x = "Population Served", y = "Index Value" )+ 
  scale_color_manual(name = "Hazard Component", 
                     breaks = c("c1", "c2", "c3", "c4", "c5", "c6", "c7"), 
                     values = cols,
                     labels = c("Excessive Heat", "Freeze-Thaw Cycles", "Extreme Precipitation", "Utility Energy Demand", "Water Supply Stress", "Sea Level Rise", "Wildfires")) + coord_flip() + 
  theme(text=element_text(family="Open Sans"))
p3

#Ridgeline Plots
ridgeline_minmax <- hazard_index_minmax %>% 
  drop_na(NOAA.Region) %>%
  mutate(State = "All")  %>% 
  ggplot() +
  geom_density_ridges(aes(x = hazard_index, y = fct_reorder(NOAA.Region, hazard_index), fill = NOAA.Region,  alpha = .5, na.rm = TRUE), scale = 1, rel_min_height = 0.005) +
  geom_density_ridges(aes(y = State, x= hazard_index, alpha = .5, na.rm = TRUE), scale = 0.25, rel_min_height = 0.0005) +
  geom_vline(xintercept = mean(hazard_index_minmax$hazard_index), linetype="dashed",color = "black", size=0.5) +
  theme_ridges() + 
  scale_fill_brewer(palette = "Spectral") +
  theme(legend.position = "none") + labs(y= "NOAA Climate Region", x = "Hazard Index - Min Max") +
  theme(text=element_text(family="Open Sans", size = 10)) +
  theme(axis.text = element_text(size = 8))
ridgeline_minmax

hazard_index_minmax %>% 
  mutate(State = "All")  %>% 
  ggplot() +
  geom_density_ridges(aes(x = hazard_index, y = PopulationBins, fill = PopulationBins,  alpha = .5, na.rm = TRUE), scale = 1, rel_min_height = 0.005) +
  geom_density_ridges(aes(y = State, x= hazard_index, alpha = .5, na.rm = TRUE), scale = 0.25, rel_min_height = 0.0005) +
  theme_ridges() + 
  scale_fill_brewer(palette = "Spectral") +
  theme(legend.position = "none") + labs(y= "NOAA Climate Region", x = "Hazard Index - Min Max") +
  theme(text=element_text(family="Open Sans"))

ggplot(data = subset(hazard_index_minmax, !is.na(primary_source_code))) +
  geom_density_ridges(aes(x = hazard_index, y = primary_source_code, fill = primary_source_code,  alpha = .5, na.rm = TRUE), scale = 1, rel_min_height = 0.005) +
  theme_ridges() + 
  scale_fill_brewer(palette = "Spectral") +
  theme(legend.position = "none") + labs(y= "NOAA Climate Region", x = "Hazard Index - Min Max") +
  theme(text=element_text(family="Open Sans"))

#Grouping Analysis - Min-Max Index, no Sea Level Rise
hazard_index_minmax$hazard_index_noSLR <- (hazard_index_minmax$heat_index + hazard_index_minmax$energydemand_index + hazard_index_minmax$FT_index+ hazard_index_minmax$extremeprecip_index+hazard_index_minmax$waterrisk_index + hazard_index_minmax$wildfirerisk_index)/6
hazard_index_minmax_means_noSLR_NOAARegion <- hazard_index_minmax %>%  group_by(NOAA.Region) %>% summarise(hazard_index_noSLR = mean(hazard_index_noSLR), heat_index = mean(heat_index), FT_index = mean(FT_index), extremeprecip_index = mean(extremeprecip_index), waterrisk_index = mean(waterrisk_index), energydemand_index = mean(energydemand_index), wildfire_index = mean(wildfirerisk_index))
p1_noSLR <- ggplot(data=subset(hazard_index_minmax_means_noSLR_NOAARegion,!is.na(NOAA.Region)), aes(x = NOAA.Region)) + theme_minimal() + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
p1_noSLR <- p1_noSLR + geom_point(aes(y = heat_index,  color = "c1", size = 0.1)) + 
  geom_point(aes( y = FT_index, color = "c2",size = 0.1)) + 
  geom_point(aes( y = extremeprecip_index, color = "c3", size = 0.1)) + 
  geom_point(aes( y = energydemand_index, color = "c4", size = 0.1))+ 
  geom_point(aes( y = waterrisk_index, color = "c5", size = 0.1)) + 
  geom_point(aes( y = wildfire_index, color = "c7",size = 0.1)) +
  geom_point(aes(x=fct_reorder(NOAA.Region, hazard_index_noSLR), y = hazard_index_noSLR, color = "c8",size = 0.1), shape = 18 ) +
  labs( x = "NOAA Region", y = "Min Max Index Value, no SLR" )+ 
  scale_color_manual(name = "Hazard Component", 
                     breaks = c("c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8"), 
                     values = cols,
                     labels = c("Excessive Heat", "Freeze-Thaw Cycles", "Extreme Precipitation", "Utility Energy Demand", "Water Supply Stress", "Sea Level Rise", "Wildfires", "Combined Hazard Index")) + coord_flip() +
  theme(text=element_text(family="Open Sans"))

p1_noSLR

hazard_index_minmax %>% 
  drop_na(NOAA.Region) %>%
  mutate(State = "All")  %>% 
  ggplot() +
  geom_density_ridges(aes(x = hazard_index_noSLR, y = fct_reorder(NOAA.Region, hazard_index_noSLR), fill = NOAA.Region,  alpha = .5, na.rm = TRUE), scale = 1, rel_min_height = 0.005) +
  geom_density_ridges(aes(y = State, x= hazard_index_noSLR, alpha = .5, na.rm = TRUE), scale = 0.25, rel_min_height = 0.0005) +
  theme_ridges() + 
  scale_fill_brewer(palette = "Spectral") +
  theme(legend.position = "none") + labs(y= "NOAA Climate Region", x = "Hazard Index - Min Max, No SLR") +
  theme(text=element_text(family="Open Sans"))

#Grouping Analysis - Min-Max Index, no Freeze-Thaw Cycles
hazard_index_minmax$hazard_index_noFT <- (hazard_index_minmax$heat_index + hazard_index_minmax$energydemand_index + hazard_index_minmax$SLR_index + hazard_index_minmax$extremeprecip_index+hazard_index_minmax$waterrisk_index + hazard_index_minmax$wildfirerisk_index)/6
hazard_index_minmax_means_noFT_NOAARegion <- hazard_index_minmax %>%  group_by(NOAA.Region) %>% summarise(hazard_index_noFT = mean(hazard_index_noFT), heat_index = mean(heat_index), SLR_index = mean(SLR_index), extremeprecip_index = mean(extremeprecip_index), waterrisk_index = mean(waterrisk_index), energydemand_index = mean(energydemand_index), wildfire_index = mean(wildfirerisk_index))

usmap + geom_point(data = hazard_index_minmax, aes(x = utm_E,y =utm_N, color = hazard_index_noFT), shape = 16) + 
  labs(title = "Drinking Water Utilities (n = 42,037)", color = "Combined Hazard Index") + 
  theme(legend.position = "right", axis.title.x=element_blank(), axis.title.y = element_blank(), axis.ticks = element_blank()) + 
  theme(text=element_text(family="Open Sans", size=10)) + 
  scale_color_gradient(low = "yellow", high = "darkred") 

p1_noFT <- ggplot(data=subset(hazard_index_minmax_means_noFT_NOAARegion,!is.na(NOAA.Region)), aes(x = NOAA.Region)) + theme_minimal() + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
p1_noFT <- p1_noFT + geom_point(aes(y = heat_index,  color = "c1", size = 0.1)) + 
  geom_point(aes( y = SLR_index, color = "c6",size = 0.1)) + 
  geom_point(aes( y = extremeprecip_index, color = "c3", size = 0.1)) + 
  geom_point(aes( y = energydemand_index, color = "c4", size = 0.1))+ 
  geom_point(aes( y = waterrisk_index, color = "c5", size = 0.1)) + 
  geom_point(aes( y = wildfire_index, color = "c7",size = 0.1)) +
  geom_point(aes(x=fct_reorder(NOAA.Region, hazard_index_noFT), y = hazard_index_noFT, color = "c8",size = 0.1), shape = 18 ) +
  labs( x = "NOAA Region", y = "Min Max Index Value, no Freeze-Thaw Days" )+ 
  scale_color_manual(name = "Hazard Component", 
                     breaks = c("c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8"), 
                     values = cols,
                     labels = c("Excessive Heat", "Freeze-Thaw Cycles", "Extreme Precipitation", "Utility Energy Demand", "Water Supply Stress", "Sea Level Rise", "Wildfires", "Combined Hazard Index")) + coord_flip() +
  theme(text=element_text(family="Open Sans"))

p1_noFT

hazard_index_minmax %>% 
  drop_na(NOAA.Region) %>%
  mutate(State = "All")  %>% 
  ggplot() +
  geom_density_ridges(aes(x = hazard_index_noFT, y = fct_reorder(NOAA.Region, hazard_index_noFT), fill = NOAA.Region,  alpha = .5, na.rm = TRUE), scale = 1, rel_min_height = 0.005) +
  geom_density_ridges(aes(y = State, x= hazard_index_noFT, alpha = .5, na.rm = TRUE), scale = 0.25, rel_min_height = 0.0005) +
  theme_ridges() + 
  scale_fill_brewer(palette = "Spectral") +
  theme(legend.position = "none") + labs(y= "NOAA Climate Region", x = "Hazard Index - Min Max, No Freeze-Thaw") +
  theme(text=element_text(family="Open Sans"))

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

hazard_index_zscore %>% 
  mutate(State = "All") %>% 
  ggplot() + theme_minimal() +
  geom_boxplot(aes(y = fct_reorder(NOAA.Region, hazard_index, .fun = "mean"), x = hazard_index)) + 
  geom_boxplot(aes(y = State, x= hazard_index)) + labs(y= "NOAA Climate Region", x = "Hazard Index - Z Score") +
  theme(text=element_text(family="Open Sans"))

usmap + geom_point(data = hazard_index_zscore, aes(x = utm_E,y =utm_N, color = hazard_index), shape = 16) + 
  labs(title = "Drinking Water Utilities (n = 42,786)", color = "Combined Hazard Index") + 
  theme(legend.position = "right", axis.title.x=element_blank(), axis.title.y = element_blank(), axis.ticks = element_blank()) + 
  theme(text=element_text(family="Open Sans", size=10)) + 
  scale_color_gradient(low = "yellow", high = "darkred") 

hazard_index_zscore_means_States <- hazard_index_zscore %>%  group_by(State) %>% summarise(hazard_index = mean(hazard_index), heat_index = mean(heat_index), FT_index = mean(FT_index), extremeprecip_index = mean(extremeprecip_index), waterrisk_index = mean(waterrisk_index), energydemand_index = mean(energydemand_index), SLR_index = mean(SLR_index), wildfire_index = mean(wildfirerisk_index))
us_states_hazards <- merge(us_states_merged, hazard_index_zscore_means_States, by.x = "state", by.y = "State")

ggplot() + 
  geom_sf(data = us_states_hazards, aes(fill = hazard_index)) +
  scale_fill_continuous(low = "yellow", high = "darkred", name = "Average Hazard Index") +
  geom_sf(data = regions, fill = "transparent", color = "gray30", size = 0.5)  + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) + theme(text=element_text(family="Open Sans"))

#Comparison of Min Max Aggregation and Z Score Aggregation Methods for Index
comparison <- hazard_index_minmax[, c("pwsid","hazard_index")]
names(comparison)[names(comparison) == "hazard_index"] <- "hazard_index_minmax"
comparison <- merge(comparison, hazard_index_zscore[, c("pwsid","hazard_index")], by = "pwsid")
names(comparison)[names(comparison) == "hazard_index"] <- "hazard_index_zscore"

ggplot(comparison, aes(x=hazard_index_minmax, y=hazard_index_zscore)) + 
  geom_point() + theme_minimal() +
  labs( x = "Min Max Index", y = "Z Score Index")  + 
  theme(text=element_text(family="Open Sans"))

#Climate Hazard Thresholds Exceeded - Min Max Linear Aggregation
hazard_index_minmax <- hazard_index_minmax %>% mutate(heat_threshold = if_else(Diff_maxtemp_5d >= 2.890183, 1, 0))
hazard_index_minmax <- hazard_index_minmax %>% mutate(precip_threshold = if_else(RC_highest_precip_5d >= 10.23256, 1, 0))
hazard_index_minmax <- hazard_index_minmax %>% mutate(SLR_threshold = if_else(SLR_indicator == 1, 1, 0))
hazard_index_minmax <- hazard_index_minmax %>% mutate(wildfire_threshold = if_else(RC_avg_wildfire >= 10.76143, 1, 0))
hazard_index_minmax <- hazard_index_minmax %>% mutate(FT_threshold = if_else(RC_FT > 0, 1, 0))
hazard_index_minmax <- hazard_index_minmax %>% mutate(waterstress_threshold = if_else(water_stress >= 10, 1, 0))
hazard_index_minmax <- hazard_index_minmax %>% mutate(energydemand_threshold = if_else(energy_demand >= 8, 1, 0))

hazard_index_minmax$sum <- hazard_index_minmax$heat_threshold + hazard_index_minmax$precip_threshold + hazard_index_minmax$SLR_threshold + hazard_index_minmax$wildfire_threshold + hazard_index_minmax$FT_threshold + hazard_index_minmax$waterstress_threshold + hazard_index_minmax$energydemand_threshold

ggplot(hazard_index_minmax, aes(x=sum, fill = hazard_index_group)) + geom_histogram() +
  scale_fill_manual(values = hazard_cols) +
  scale_alpha_manual(values=c(0.1, 0.5, .6, 0.5)) +
  labs(x = "Number of Climate Hazard Thresholds Exceeded", y = "Number of Utilities", fill = "Combined Hazard Index") +
  theme_minimal()+ theme(text=element_text(family="Open Sans", size=10))



