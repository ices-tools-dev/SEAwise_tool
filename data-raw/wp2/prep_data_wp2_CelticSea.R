# Script to prepare the data for WP2 of the Celtic Sea case study.
# Requires the following files:
# - WW_d2_10_fleet_info.xlsx
# - Fish_portions.xlsx
# - tool_social_input.txt

# Generates the following data objects:
# CS_data.rds

# The script is organized into the following sections:
# 1. Fleet data
# 2. Socioeconomic data
# 3. Carbon data
# 4. Fuel use and cost
# 5. Adult portions
# 6. Climate and management scenario projections

rm(list=ls())
library(ggplot2)
library(dplyr)
library(tidyr)

# 1. Fleet data

fleet_data=data.frame(readxl::read_xlsx("data-raw/wp2/WW_d2_10_fleet_info.xlsx",sheet="WW_d2_10_fleet_info"))
fleet_data1=fleet_data[fleet_data$variable %in% c("vessels"),]
fleet_data1=fleet_data1[!is.na(fleet_data1$value),]
fleet_data1=fleet_data1[fleet_data1$small_large!="all",]
colnames(fleet_data1)[3]="Fleet"



output_data <- list(fleet_data = fleet_data1)

# 2. Socioeconomic data

fleet_data2=fleet_data[fleet_data$variable %in% c("land_val", "fuel_costs", "rep","variableCosts","fix costs","other_var_costs"),]
fleet_data2$id=paste(fleet_data2$year,fleet_data2$small_large,fleet_data2$country)
GVA_records=data.frame(matrix(nrow=0,ncol=8))

for (comb in unique(fleet_data2$id)){
  fleet_data2_temp=fleet_data2[fleet_data2$id==comb,]
  GVA = fleet_data2_temp[fleet_data2_temp$variable=="land_val","value"]-fleet_data2_temp[fleet_data2_temp$variable=="fuel_costs","value"]-fleet_data2_temp[fleet_data2_temp$variable=="rep","value"]- fleet_data2_temp[fleet_data2_temp$variable=="fix costs","value"]-fleet_data2_temp[fleet_data2_temp$variable=="variableCosts","value"]
    -fleet_data2_temp[fleet_data2_temp$variable=="other_var_costs","value"]   
  
  GVA_records2=fleet_data2_temp[1,]
  GVA_records2$variable="GVA"
  GVA_records2$value=GVA
  GVA_records=rbind(GVA_records,GVA_records2)  
}

fleet_data=rbind(fleet_data,GVA_records[,-ncol(GVA_records)])
fleet_data3=fleet_data[fleet_data$variable %in% c("land_val", "GVA"),]
fleet_data3=fleet_data3[!is.na(fleet_data3$value) & fleet_data3$value!=0,]
colnames(fleet_data3)[3]="Fleet"
fleet_data3=fleet_data3[fleet_data3$Fleet!="all",]

output_data$socioeco_data <- fleet_data3


# 3. Carbon data

carbon_data=fleet_data[!is.na(fleet_data$variable %in% c("CO2_emission")) & fleet_data$variable %in% c("CO2_emission") & fleet_data$unit=="kg.per.fishingDay",]
output_data$carbon_data <- carbon_data

# 4. Fuel use and cost

dati2=data.frame(readxl::read_xlsx("data-raw/wp2/WW_d2_10_fleet_info.xlsx",sheet="fish_price"))

fish_fuel_price <- data.frame() 
dati2<- dati2 %>%
   group_by(Year, Stock,Price.per.kg, Country, Fleet) %>%
  summarise(Price=mean(Price.per.kg))
dati2$Stock_Country=paste(dati2$Country,dati2$Stock,sep="_")
dati2$Variable="Price" 
dati2=dati2[dati2$Fleet!="all",]

for(i in unique(dati2$Country)) {
  dat=dati2[dati2$Country==i,]
  fuel=as.data.frame(fleet_data[fleet_data$variable=="fuel_price" & fleet_data$country==i,])
  dati3<- dat 
  colnames(dati3)[3]="variable"
  DF=merge(dati3,fuel,by.x=c("Year","Country"),by.y=c("year","country"))
  colnames(DF)[11]="fuel_price"
  fish_fuel_price <- bind_rows(fish_fuel_price, DF)
}

colnames(fish_fuel_price)[2]="country"

output_data$fish_fuel_data <- fish_fuel_price

# 5. Adult portions

dat=readxl::read_xlsx("data-raw/wp2/Fish_portions.xlsx",sheet="CS") 

output_data$adult_portions <- dat
saveRDS(output_data, file = "data/wp2/CS_data.rds")

# 6. Socio-eco projections

projections <- read.csv("data-raw/wp2/tool_social_input.txt", sep = '\t')
projections <- projections %>% filter(Area %in% c('WW Celtic'))
projections <- projections %>% select(active, everything()) %>% tidyr::pivot_longer(9:20) %>% na.omit() 
projections <- projections %>% group_by(Area, Model, SSF_LSF, 
                                        Mgt_scenario, Climate, year, Quantile, name) %>% summarise(value = mean(value))

projections$Quantile[projections$Quantile == '0.025'] <- 'lower'
projections$Quantile[projections$Quantile == '0.975'] <- 'higher'
projections$Quantile[projections$Quantile == '0.5'] <- 'median'

projections <- projections %>% tidyr::pivot_wider(values_from = value, names_from = Quantile)

output_data$projection_data <- projections
saveRDS(output_data, file = "data/wp2/CS_data.rds")
