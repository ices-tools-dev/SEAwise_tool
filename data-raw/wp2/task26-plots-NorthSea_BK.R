# =========================================== #
# script to derive plots for deliverable 2.10
# North Sea case study
# =========================================== #

rm(list=ls())
library(ggplot2)
library(dplyr)
library(plyr)
library(reshape2)

#setwd("C:\\Users\\Utente\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\H2020-SEAwise\\_____________WP2\\TASK 2.6\\templates")

# read in data

# get fuel prices
fuel.price = readxl::read_xlsx("data-raw/MONTHLY_MARINE_GASOIL_PRICE.xlsx",
                               sheet = 4)
#head(fuel.price)
# get countries
#unique(fuel.price$cod_country_iso2)
# UK is missing
fuel.price = fuel.price[fuel.price$cod_country_iso2 %in% c("BE","DE","DK","FR","NL","SE"),]
# fuel.price$country = countrycode::countrycode(fuel.price$cod_country_iso2,origin = "iso2c",
#                                               destination =  "country.name")
fuel.price$country = ifelse(fuel.price$cod_country_iso2 == "DE","GE",
                            ifelse(fuel.price$cod_country_iso2 == "SE","SW",fuel.price$cod_country_iso2))
#unique(fuel.price$country)


# calculate an average yearly price
fuel.price.yr = ddply(fuel.price,c("country","Year"),summarise,
      fuel_price = mean(price))
fuel.price.yr_melt = melt(fuel.price.yr,id.vars = c("Year","country"))
fuel.price.yr_melt$unit = "euro"
names(fuel.price.yr_melt)[names(fuel.price.yr_melt) %in% c("Year")] <- "year"
fuel.price.yr_melt$size = "small"
fuel.price.yr_melt$Fleet = "Small_scale"
# order columns
fuel.price.yr_melt = fuel.price.yr_melt[,c("size","year","variable","unit","country","value","Fleet")]

fuel.price.yr_melt_large = fuel.price.yr_melt
fuel.price.yr_melt_large$size = "large"
fuel.price.yr_melt_large$Fleet = "Large_scale"

fuel.price.yr_melt_all = rbind(fuel.price.yr_melt,fuel.price.yr_melt_large)


small_scale=data.frame(readxl::read_xlsx("data-raw/Task2.10.1_Summary_output_small.vs.largeFleets_NorthSea.xlsx",sheet="small fleets<24m"))
# head(small_scale)
small_scale$Fleet="Small_scale"

large_scale=data.frame(readxl::read_xlsx("data-raw/Task2.10.1_Summary_output_small.vs.largeFleets_NorthSea.xlsx",sheet="large fleets>=24m"))
# head(large_scale)
large_scale$Fleet="Large_scale"

fleet_data=rbind(small_scale,large_scale)
# add fuel price to the tables
fleet_data = rbind(fleet_data,fuel.price.yr_melt_all)

fleet_data1=fleet_data[fleet_data$variable %in% c("Number.of.vessels", "avg.KW","landings"),]
fleet_data1=fleet_data1[!is.na(fleet_data1$value),]

# ggplot(aes(x=year,y=value,colour=Fleet),data=fleet_data1)+
#   geom_line(aes(x=year,y=value,colour=Fleet, group=Fleet),data=fleet_data1,size=1) +
#   facet_wrap(country+variable~.,scale="free")

# ggsave("Fleet.jpg",width = 20,unit="cm")

# ggplot(data=fleet_data1, aes(x=year, y=value, fill=Fleet)) + 
#   geom_bar(stat="identity", position=position_dodge())+
#   facet_wrap(country + variable~.,scales="free_y",drop=FALSE,ncol=6)+ 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# ggsave("Fleet_histograms.jpg",width = 30,height=20,unit="cm")
# unique(fleet_data$variable)


output_data <- list(fleet_data = fleet_data1)

# GVA = Income from landings + other income - energy costs - repair and maintenance costs - other variable costs - non variable costs.


fleet_data2=fleet_data[fleet_data$variable %in% c("landings.value", "Energy_costs", "Repair_and_maintenance_costs","Other_variable_costs","fcosts","Other_non-variable_costs"),]
# head(fleet_data2)

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

# ggplot(aes(x=year,y=value,colour=Fleet),data=fleet_data3)+
#   geom_line(aes(x=year,y=value,colour=Fleet, group=Fleet),size=1)+
#   facet_wrap(country~variable,scale="free",ncol=6) + 
#   theme(axis.text.x = element_text(angle=45))
# 
# ggsave("Economic.jpg",width = 40,height=20,unit="cm")

output_data$socioeco_data <- fleet_data3


carbon_data=fleet_data[!is.na(fleet_data$variable %in% c("CO2_emission")) & fleet_data$variable %in% c("CO2_emission") & fleet_data$unit=="kg.per.fishingDay",]
output_data$carbon_data <- carbon_data

# ggplot(aes(x=year,y=value,colour=Fleet),data=fleet_data3)+
#   geom_line(aes(x=year,y=value,colour=Fleet,group=Fleet),size=1)+
#   facet_wrap(country+variable~.,scale="free")
# 
# 
# 
# ggplot(data=data.frame(fleet_data3), aes(x=year, y=value, fill=Fleet)) + 
#   geom_bar(stat="identity", position=position_dodge())+
#   facet_wrap(country~ variable,scales="free_y",drop=FALSE,ncol=3)+ 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#   labs(fill='Year')

# ggsave("Carbon.jpg",width = 20,height=15, unit="cm")

# --------------------------------- #
# Linear regressions fuel vs. price
# --------------------------------- # 

dati2=readxl::read_xlsx("data-raw/Task2.10.1_Summary_output_small.vs.largeFleets_NorthSea.xlsx",sheet="fish price")

# summarise by species and not by stock
dati2$spec = gsub("[0-9]+|-NS|-EC|OTH","",dati2$stock)

countries = unique(fuel.price$country)

library(ggpmisc)
library(broom)

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
  
  dati2=readxl::read_xlsx("data-raw/Task2.10.1_Summary_output_small.vs.largeFleets_NorthSea.xlsx",sheet="fish price")
  
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
  # 
  # formula<-y~x
  # ggplot(DF,aes(x=fuel_price, y=Price,colour=Fleet,group=Fleet)) +
  #   geom_point() +
  #   geom_smooth(method='lm',se=T)+facet_wrap(~spec,scale="free") +ggtitle(paste(Country,sep=" ")) +
  #   stat_fit_glance(method = 'lm',
  #                   method.args = list(formula = formula),
  #                   geom = 'text',
  #                   aes(label = paste("P = ", signif(..p.value.., digits = 2), sep = "")),
  #                   label.x = 'center', label.y = 0.35, size = 4, position = position_stack(vjust = 2))+
  #   expand_limits(y = 2)
  # 
  # ggsave(paste("Fish_fuel_price",Country,".jpg",sep=" "),width=20,height=20,units="cm")
  # 
  
}
output_data$fish_fuel_data <- fish_fuel_price

# ============== #
# Adult portions
# ============== #

dat=readxl::read_xlsx("data-raw/Fish_portions.xlsx",sheet="Foglio1")

dat$spec = gsub("[0-9]+|-NS|-EC|OTH","",dat$Stock)
output_data$adult_portions <- dat

saveRDS(output_data, file = "data/wp2/NS_data.rds")


# ggplot(data=dat, aes(y=Country, x=adult_portions, fill=Fleet)) + 
#   geom_bar(stat="identity", position=position_dodge())+
#   facet_wrap(spec~.,scales="free_x",drop=FALSE,ncol=3)+ 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#   labs(y="thousands")+
#   ggtitle ("Number of adult portions by Country and Species caught")
# 
# ggsave(paste("Adult_portions_NS.jpg",sep=" "),width=25,height=20,units="cm")
