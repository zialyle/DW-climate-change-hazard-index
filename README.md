This code and database are associated with the manuscript, "Climate change hazard index reveals combined risks to United States drinking water utilities" by Lyle et al (2024).

The_ pws_withhazards.csv_ file contains climate change hazard projections linked to United States public water systems (pws). That file is the input for the _DW utility climate data analysis.R_ file, which contains the code to develop the combined climate hazard index for drinking water utilities. The output file, identified as _Lyle et al_2024.csv_, contains contains climate change hazard index values linked to United States public water systems (pws), and will be published open access along with the manuscript.

The columns in the database are as follows: 

pwsid: Public Water System Identification Number

primacy_agency_code: Two character postal code for the state or territory having regulatory oversight for the water system. 

pws_name: Name of the water system

State: State in which water system is located

city_served: City in which water system is located

County: County in which water system is located

population_served_count: Number of customers served by water system

service_connections_count: Number of service connections maintained by water system

service_area_type_code: Service area type code

owner_type_code: Code that dentifies the ownership category of the water system consisting of: F (Federal Government), L (Local Government), M (Public/Private), N (Native American), P (Private), or S (State Government)

is_wholesaler_ind: Indicates whether the system is a wholesaler of water

primacy_type: Code that indicates if the water system is regulated by a state, tribal, or territorial primacy program. Note that EPA direct implementation programs, except for Wyoming, are tribal primacy programs

primary_source_code: The code showing the differentiation between the sources of water: ground water (GW),groundwater purchased (GWP), surface water (SW), surface water purchased (SWP), groundwater under influence of surface water (GU), or purchased ground water under influence of surface water source (GUP)

tier:

centroid_lat: Latitude ocation of water system

centroid_lon: Longitude ocation of water system

NOAA.Region: NOAA Climate Region in which water system is located

heat_index: Extreme heat index value

energydemand_index: Energy demand index value, using regression model developed by Sowby & Burian, 2018

FT_index: Freeze-Thaw cycle index value

extremeprecip_index: Extreme precipitation index value

waterrisk_index: Water stress index value, using Dickson, K. E. & Dzombak, D. A. (2019)

SLR_index: Sea level rise index value

wildfirerisk_index: Wildfire index value

hazard_index: Combined climate change hazard index value, normalized from 0 to 1

hazard_index_group: Classification group for combined climate change hazard index value (minimal, low, moderate, high)

heat_threshold: Binary value indicating whether PWS exceeded risk threshold level for extreme heat (0 indicating no, 1 indicating yes)

precip_threshold: Binary value indicating whether PWS exceeded risk threshold level for extreme precipitation (0 indicating no, 1 indicating yes)

SLR_threshold: Binary value indicating whether PWS exceeded risk threshold level for sea level rise (0 indicating no, 1 indicating yes)

wildfire_threshold: Binary value indicating whether PWS exceeded risk threshold level for wildfires (0 indicating no, 1 indicating yes)

FT_threshold: Binary value indicating whether PWS exceeded risk threshold level for freeze-thaw cycles (0 indicating no, 1 indicating yes)

waterstress_threshold: Binary value indicating whether PWS exceeded risk threshold level for water stress (0 indicating no, 1 indicating yes)

energydemand_threshold: Binary value indicating whether PWS exceeded risk threshold level for enegery demand (0 indicating no, 1 indicating yes)

sum: Total number of climate hazard risk threshold values exceeded

exposure: Product of combined climate change hazard index value and population served
