# wp2 data prep script that sources the scripts specific to the SEAwise case study regions
# In the case of fish portions where the data is provided for all regions together, the data prep is handled together.
# Requires the following files:
# - template for deliverable 2.10.1_BoB_Demersal.xlsx
# - template for deliverable 2.10.1_Cmed_12m.xlsx
# - template for deliverable 2.10.1_HCMR.xlsx
# - template for deliverable 2.10.1_NorthSea.xlsx
# - Task2.10.1_Summary_output_small.vs.largeFleets_NorthSea.xlsx
# - WW_d2_10_fleet_info.xlsx
# - Fish_portions.xlsx
# - MONTHLY_MARINE_GASOIL_PRICE.xlsx
# - tool_social_input.txt

#

source("data-raw/wp2/prep_data_wp2_BoB.R")
source("data-raw/wp2/prep_data_wp2_CelticSea.R")
source("data-raw/wp2/prep_data_wp2_CentralMed.R")
source("data-raw/wp2/prep_data_wp2_EasternMed.R")
source("data-raw/wp2/prep_data_wp2_NorthSea.R")
source("data-raw/wp2/prep_data_wp2_fish_portions.R")
