# Script to prepare the data for WP2 of the Central Mediterranean case study.
# Requires the following files:
# - template for deliverable 2.10.1_Cmed_12m.xlsx
# - Fish_portions.xlsx
# - tool_social_input.txt

# Generates the following data objects:
# CMed_data.rds

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

# 1. Fleet data

small_scale=data.frame(readxl::read_xlsx("data-raw/wp2/template for deliverable 2.10.1_Cmed_12m.xlsx",sheet="small scale (fleet)"))
small_scale$Fleet="Small_scale"

large_scale=data.frame(readxl::read_xlsx("data-raw/wp2/template for deliverable 2.10.1_Cmed_12m.xlsx",sheet="large scale (fleet)"))
large_scale$Fleet="Large_scale"


fleet_data=rbind(small_scale,large_scale)
fleet_data1=fleet_data[fleet_data$Variable %in% c("vessels", "KW","GT","land"),]
fleet_data1=fleet_data1[!is.na(fleet_data1$Value),]


colnames(fleet_data1) <- tolower(colnames(fleet_data1))
output_data <- list(fleet_data = fleet_data1)


# 2. Socioeconomic data

fleet_data2=fleet_data[fleet_data$Variable %in% c("land_val", "fuel_costs", "rep","other_var_costs","fix costs"),]
fleet_data2$id=paste(fleet_data2$Year,fleet_data2$Fleet,fleet_data2$Country)
GVA_records=data.frame(matrix(nrow=0,ncol=8))
colnames(GVA_records)=colnames(fleet_data2)  

for (comb in unique(fleet_data2$id)){
  fleet_data2_temp=fleet_data2[fleet_data2$id==comb,]
  GVA = fleet_data2_temp[fleet_data2_temp$Variable=="land_val","Value"]-fleet_data2_temp[fleet_data2_temp$Variable=="fuel_costs","Value"]-fleet_data2_temp[fleet_data2_temp$Variable=="rep","Value"]- fleet_data2_temp[fleet_data2_temp$Variable=="other_var_costs","Value"]-fleet_data2_temp[fleet_data2_temp$Variable=="fix costs","Value"]
  GVA_records2=fleet_data2_temp[1,]
  GVA_records2$Variable="GVA"
  GVA_records2$Value=GVA
  GVA_records=rbind(GVA_records,GVA_records2)  
}

fleet_data=rbind(fleet_data,GVA_records[,-ncol(GVA_records)])
fleet_data3=fleet_data[fleet_data$Variable %in% c("land_val",  "jobs","GVA"),]
fleet_data3=fleet_data3[!is.na(fleet_data3$Value),]

output_data$socioeco_data <- fleet_data3


# 3. Carbon data

carbon_data=fleet_data[fleet_data$Variable %in% c("carbon_emis"),]
output_data$carbon_data <- carbon_data

# 4. Fuel use and cost

dati2=readxl::read_xlsx("data-raw/wp2/template for deliverable 2.10.1_Cmed_12m.xlsx",sheet="fish price")

fish_fuel_price <- data.frame()

dati2$Stock_Country=paste(dati2$Country,dati2$Stock,sep="_")
dati2$Variable="Price"
Country="Croatia"
dati2=dati2[dati2$Country==Country,]

fuel=as.data.frame(small_scale[small_scale$Variable=="fuel_price" & small_scale$Country==Country,])
fuel2=as.data.frame(large_scale[large_scale$Variable=="fuel_price" & large_scale$Country==Country,])

fuel=rbind(fuel,fuel2)
dati3<-data.frame(dati2[,c(1,2,6,4,5,7)])
colnames(dati3)[2]="Variable"
DF=merge(dati3,fuel,by.x=c("Year","Country"),by.y=c("Year","Country"))
colnames(DF)[3]="Price"
colnames(DF)[5]="Fleet"

colnames(DF)[7]="fuel_price"


fish_fuel_price <- bind_rows(fish_fuel_price, DF)  

dati2=readxl::read_xlsx("data-raw/wp2/template for deliverable 2.10.1_Cmed_12m.xlsx",sheet="fish price")

dati2$Stock_Country=paste(dati2$Country,dati2$Stock,sep="_")
dati2$Variable="Price"


Country="Italy"
dati2=dati2[dati2$Country==Country,]

fuel=as.data.frame(small_scale[small_scale$Variable=="fuel_price" & small_scale$Country==Country,])
fuel2=as.data.frame(large_scale[large_scale$Variable=="fuel_price" & large_scale$Country==Country,])

fuel=rbind(fuel,fuel2)
dati3<-data.frame(dati2[,c(1,2,6,4,5,7)])
colnames(dati3)[2]="Variable"
DF=merge(dati3,fuel,by.x=c("Year","Country"),by.y=c("Year","Country"))
colnames(DF)[3]="Price"
colnames(DF)[5]="Fleet"

colnames(DF)[7]="fuel_price"

fish_fuel_price <- bind_rows(fish_fuel_price, DF)  

dati2=readxl::read_xlsx("data-raw/wp2/template for deliverable 2.10.1_Cmed_12m.xlsx",sheet="fish price")


dati2$Stock_Country=paste(dati2$Country,dati2$Stock,sep="_")
dati2$Variable="Price"


Country="Slovenia"
dati2=dati2[dati2$Country==Country,]

fuel=as.data.frame(small_scale[small_scale$Variable=="fuel_price" & small_scale$Country==Country,])
fuel2=as.data.frame(large_scale[large_scale$Variable=="fuel_price" & large_scale$Country==Country,])

fuel=rbind(fuel,fuel2)
dati3<-data.frame(dati2[,c(1,2,6,4,5,7)])
colnames(dati3)[2]="Variable"
DF=merge(dati3,fuel,by.x=c("Year","Country"),by.y=c("Year","Country"))


colnames(DF)[3]="Price"
colnames(DF)[5]="Fleet"
colnames(DF)[7]="fuel_price"

fish_fuel_price <- bind_rows(fish_fuel_price, DF)  
colnames(fish_fuel_price)[2]="country"

output_data$fish_fuel_data <- fish_fuel_price

# 5. Adult portions

dat=readxl::read_xlsx("data-raw/wp2/Fish_portions.xlsx",sheet="CMed")
output_data$adult_portions <- dat


# 6. Socio-eco projections

projections <- read.csv("data-raw/wp2/tool_social_input.txt", sep = '\t')

projections <- projections %>% filter(Area %in% c('Adriatic'))
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
saveRDS(output_data, file = "data/wp2/CMed_data.rds")

