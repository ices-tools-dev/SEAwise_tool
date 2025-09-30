# Script to prepare the data for WP2 of the Eastern Mediterranean case study.
# Requires the following files:
# - template for deliverable 2.10.1_HCMR.xlsx
# - Fish_portions.xlsx
# - tool_social_input.txt

# Generates the following data objects:
# EMed_data.rds

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

fleet_data=data.frame(readxl::read_xlsx("data-raw/wp2/template for deliverable 2.10.1_HCMR.xlsx",sheet="small scale (fleet)"))[,1:6]
fleet_data$Fleet="Small scale"
fleet_data2=data.frame(readxl::read_xlsx("data-raw/wp2/template for deliverable 2.10.1_HCMR.xlsx",sheet="large scale (fleet)"))[,1:6]
fleet_data2$Fleet="Large scale"
fleet_data=rbind(fleet_data[,1:7],fleet_data2[,1:7])
fleet_data1=fleet_data[fleet_data$Variable %in% c("vessels","GT","KW","land"),]
fleet_data1$Value=round(as.numeric(fleet_data1$Value))


colnames(fleet_data1) <- tolower(colnames(fleet_data1))
output_data <- list(fleet_data = fleet_data1)

# 2. Socioeconomic data

fleet_data2=data.frame(fleet_data[fleet_data$Variable %in% c("land_val", "fuel_costs", "rep","other_var_costs","fix costs"),])
fleet_data2$id=paste(fleet_data2$Year,fleet_data2$Fleet)
GVA_records=data.frame(matrix(nrow=0,ncol=8))
colnames(GVA_records)=colnames(fleet_data2)  


for (comb in unique(fleet_data2$id)){
 
   fleet_data2_temp=data.frame(fleet_data2[fleet_data2$id==comb,])
  GVA = as.numeric(fleet_data2_temp[fleet_data2_temp$Variable=="land_val","Value"]) -as.numeric(fleet_data2_temp[fleet_data2_temp$Variable=="fuel_costs","Value"])-as.numeric(fleet_data2_temp[fleet_data2_temp$Variable=="rep","Value"])- as.numeric(fleet_data2_temp[fleet_data2_temp$Variable=="fix costs","Value"])-as.numeric(fleet_data2_temp[fleet_data2_temp$Variable=="other_var_costs","Value"])
  
  
  GVA_records2=fleet_data2_temp[1,]
  GVA_records2$Variable="GVA"
  GVA_records2$Value=GVA
  GVA_records=rbind(GVA_records,GVA_records2)  
}


fleet_data=rbind(fleet_data,GVA_records[,-ncol(GVA_records)])

fleet_data3=fleet_data[fleet_data$Variable %in% c("land_val", "GVA","jobs","unpaid"),]
fleet_data3=fleet_data3[!is.na(fleet_data3$Value) & fleet_data3$Value!=0,]
fleet_data3$Value=round(as.numeric(fleet_data3$Value,2))


output_data$socioeco_data <- fleet_data3

# 3. Carbon data

output_data$carbon_data <- NULL

# 4. Fuel use and cost


dati2=data.frame(readxl::read_xlsx("data-raw/wp2/template for deliverable 2.10.1_HCMR.xlsx",sheet="fish price"))[,1:6]
dati2=dati2[as.character(dati2$Price.per.kg)!="NA",c(1,2,4,5)]

dati2$Variable="Price"
colnames(dati2)[2]="Value"

fuel=as.data.frame(fleet_data[fleet_data$Variable=="fuel_price" & !is.na(fleet_data$Value) & as.character(fleet_data$Value)!="NA",])

dati3<- dati2 

DF=merge(dati3,fuel,by.x=c("Year","Fleet"),by.y=c("Year","Fleet"))
colnames(DF)[3]="Price"
colnames(DF)[10]="fuel_price"

DF$Price=as.numeric(DF$Price)
DF$fuel_price=as.numeric(DF$fuel_price)
colnames(DF)[colnames(DF) =="Country"] <- "country"

output_data$fish_fuel_data <- DF


# 5. Adult portions

dat=readxl::read_xlsx("data-raw/wp2/Fish_portions.xlsx",sheet="EMed")

output_data$adult_portions <- dat


# Socio-eco projections

projections <- read.csv("data-raw/wp2/tool_social_input.txt", sep = '\t')
projections <- projections %>% filter(Area %in% c('Med_GSA20'))

projections$Model[projections$Model == 'BEMTOO'] <- 'BEMTOOL'
projections$active[projections$active == ''] <- 'Passive/Active'
projections <- projections %>% select(active, everything()) %>% pivot_longer(9:20) %>% na.omit() 
projections <- projections %>% group_by(Area, Model, SSF_LSF, 
                                        Mgt_scenario, Climate, year, Quantile, name) %>% summarise(value = mean(value))

projections$Quantile[projections$Quantile == '0.025'] <- 'lower'
projections$Quantile[projections$Quantile == '0.975'] <- 'higher'
projections$Quantile[projections$Quantile == '0.5'] <- 'median'

projections <- projections %>% pivot_wider(values_from = value, names_from = Quantile)

output_data$projection_data <- projections

saveRDS(output_data, file = "data/wp2/EMed_data.rds")


