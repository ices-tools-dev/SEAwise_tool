# Script to prepare the data for WP2 of the North Sea case study. 
# Deliverable 2.10
# Requires the following files:
# - MONTHLY_MARINE_GASOIL_PRICE.xlsx
# - Task2.10.1_Summary_output_small.vs.largeFleets_NorthSea.xlsx
# - Fish_portions_ns.txt
# - tool_social_input.txt

# Generates the following data objects:
# NS_data.rds

# The script is organized into the following sections:
# 1. Fleet data, 
# 2. Socioeconomic data
# 3. Carbon data
# 4. Fuel use and cost
# 5. Adult portions
# 6. Climate and management scenario projections

# Install and load required packages

rm(list=ls())
library(ggplot2)
library(dplyr)
library(reshape2)
library(ggpmisc)
library(broom)


# 1. Fleet data

fuel.price = readxl::read_xlsx("data-raw/wp2/MONTHLY_MARINE_GASOIL_PRICE.xlsx",
                               sheet = 4)
fuel.price = fuel.price[fuel.price$cod_country_iso2 %in% c("BE","DE","DK","FR","NL","SE"),]
fuel.price$country = ifelse(fuel.price$cod_country_iso2 == "DE","GE",
                            ifelse(fuel.price$cod_country_iso2 == "SE","SW",fuel.price$cod_country_iso2))

fuel.price.yr = plyr::ddply(fuel.price,c("country","Year"),summarise,
      fuel_price = mean(price))
fuel.price.yr_melt = melt(fuel.price.yr,id.vars = c("Year","country"))
fuel.price.yr_melt$unit = "euro"
names(fuel.price.yr_melt)[names(fuel.price.yr_melt) %in% c("Year")] <- "year"
fuel.price.yr_melt$size = "small"
fuel.price.yr_melt$Fleet = "Small_scale"
fuel.price.yr_melt = fuel.price.yr_melt[,c("size","year","variable","unit","country","value","Fleet")]

fuel.price.yr_melt_large = fuel.price.yr_melt
fuel.price.yr_melt_large$size = "large"
fuel.price.yr_melt_large$Fleet = "Large_scale"

fuel.price.yr_melt_all = rbind(fuel.price.yr_melt,fuel.price.yr_melt_large)


small_scale=data.frame(readxl::read_xlsx("data-raw/wp2/Task2.10.1_Summary_output_small.vs.largeFleets_NorthSea.xlsx",sheet="small fleets<24m"))
small_scale$Fleet="Small_scale"

large_scale=data.frame(readxl::read_xlsx("data-raw/wp2/Task2.10.1_Summary_output_small.vs.largeFleets_NorthSea.xlsx",sheet="large fleets>=24m"))
large_scale$Fleet="Large_scale"

fleet_data=rbind(small_scale,large_scale)
# add fuel price to the tables
fleet_data = rbind(fleet_data,fuel.price.yr_melt_all)

fleet_data1=fleet_data[fleet_data$variable %in% c("Number.of.vessels", "avg.KW","landings"),]
fleet_data1=fleet_data1[!is.na(fleet_data1$value),]


output_data <- list(fleet_data = fleet_data1)

# 2. Socioeconomic data

fleet_data2=fleet_data[fleet_data$variable %in% c("landings.value", "Energy_costs", "Repair_and_maintenance_costs","Other_variable_costs","fcosts","Other_non-variable_costs"),]
fleet_data2$id=paste(fleet_data2$year,fleet_data2$Fleet,fleet_data2$country)
GVA_records=data.frame(matrix(nrow=0,ncol=8))
colnames(GVA_records)=colnames(fleet_data2)  

for (comb in unique(fleet_data2$id)){
  fleet_data2_temp=fleet_data2[fleet_data2$id==comb,]
  GVA = fleet_data2_temp[fleet_data2_temp$variable=="landings.value","value"]-fleet_data2_temp[fleet_data2_temp$variable=="Energy_costs","value"]-fleet_data2_temp[fleet_data2_temp$variable=="Repair_and_maintenance_costs","value"]- fleet_data2_temp[fleet_data2_temp$variable=="Other_non-variable_costs","value"]-fleet_data2_temp[fleet_data2_temp$variable=="Other_variable_costs","value"]
  
  GVA_records2=fleet_data2_temp[1,]
  GVA_records2$variable="GVA"
  GVA_records2$value=GVA
  GVA_records=rbind(GVA_records,GVA_records2)  
}


fleet_data=rbind(fleet_data,GVA_records[,-ncol(GVA_records)])

fleet_data3=fleet_data[fleet_data$variable %in% c("landings.value",  "Employment(FTE)","GVA"),]
fleet_data3=fleet_data3[!is.na(fleet_data3$value),]

output_data$socioeco_data <- fleet_data3

# 3. Carbon data

carbon_data=fleet_data[!is.na(fleet_data$variable %in% c("CO2_emission")) & fleet_data$variable %in% c("CO2_emission") & fleet_data$unit=="kg.per.fishingDay",]
output_data$carbon_data <- carbon_data

# 4. Fuel use and cost

dati2=readxl::read_xlsx("data-raw/wp2/Task2.10.1_Summary_output_small.vs.largeFleets_NorthSea.xlsx",sheet="fish price")

# summarise by species and not by stock
dati2$spec = gsub("[0-9]+|-NS|-EC|OTH","",dati2$stock)

countries = unique(fuel.price$country)


fish_fuel_price <- data.frame()
for(ii in 1:length(countries)){
  
  dat <- dati2 %>%
    dplyr::group_by(year, spec, price_euro.per.kg, country, size) %>%
    dplyr::summarise(Price=mean(price_euro.per.kg))
  dat$Stock_Country=paste(dat$country,dat$spec,sep="_")
  dat$variable="Price"
  
  Country= countries[ii]
  Fleet="Small_scale"
  size="small"
  dat=dat[dat$size==size & dat$country==Country,]
  
  fuel=as.data.frame(fleet_data[fleet_data$variable=="fuel_price" & fleet_data$country==Country,])
  fuel = fuel[complete.cases(fuel),]
  # cast
  fuel_cast = dcast(fuel,year + size + Fleet + country ~ variable,value.var = "value")
  
  DF=merge(dat,fuel_cast,by.x=c("year","country","size"),by.y=c("year","country","size"))
  
  # Large scale
  
  dati2=readxl::read_xlsx("data-raw/wp2/Task2.10.1_Summary_output_small.vs.largeFleets_NorthSea.xlsx",sheet="fish price")
  
  # summarise by species and not by stock
  dati2$spec = gsub("[0-9]+|-NS|-EC|OTH","",dati2$stock)
  

  dat<- dati2 %>%
    dplyr::group_by(year, spec, price_euro.per.kg, country, size) %>%
    dplyr::summarise(Price=mean(price_euro.per.kg))
  dat$Stock_Country=paste(dat$country,dat$spec,sep="_")
  dat$variable="Price"
  Fleet="Large_scale"
  size="large"
  dat=dat[dat$size==size & dat$country==Country,]
  
  DF2=merge(dat,fuel_cast,by.x=c("year","country","size"),by.y=c("year","country","size"))
  DF=rbind(DF,DF2)

  fish_fuel_price <- bind_rows(fish_fuel_price, DF)
}
output_data$fish_fuel_data <- fish_fuel_price


# 5. Adult portions

dat=read.delim("data-raw/wp2/Fish_portions_ns.txt")
dat$adult_portions <- as.numeric(gsub(x = dat$adult_portions, pattern = ",", replacement = "."))

dat$spec = gsub("[0-9]+|-NS|-EC|OTH","",dat$Stock)
output_data$adult_portions <- dat


# 6. Socio-eco projections

projections <- read.csv("data-raw/wp2/tool_social_input.txt", sep = '\t')
projections <- projections %>% filter(Area %in% c('NS'))
projections <- projections %>% select(active, everything()) %>% tidyr::pivot_longer(9:20) #%>% na.exclude() 
projections <- projections %>% group_by(Area, Model, SSF_LSF, 
                                        Mgt_scenario, Climate, year, Quantile, name) %>% summarise(value = mean(value))

projections$Quantile[projections$Quantile == '0.025'] <- 'lower'
projections$Quantile[projections$Quantile == '0.975'] <- 'higher'
projections$Quantile[projections$Quantile == '0.5'] <- 'median'

projections <- projections %>% tidyr::pivot_wider(values_from = value, names_from = Quantile)
output_data$projection_data <- projections

saveRDS(output_data, file = "data/wp2/NS_data.rds")


