rm(list=ls())
library(ggplot2)
library(dplyr)
#setwd("C:\\Users\\Utente\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\H2020-SEAwise\\_____________WP2\\TASK 2.6\\templates")



fleet_data=data.frame(readxl::read_xlsx("data-raw/wp2/template for deliverable 2.10.1_BoB_Demersal.xlsx",sheet="aggregated"))

fleet_data1=fleet_data[fleet_data$variable %in% c("vessels","land"),]
fleet_data1=fleet_data1[!is.na(fleet_data1$value),]

output_data <- list(fleet_data = fleet_data1)  
# ggplot(aes(x=year,y=value,colour=Fleet),data=fleet_data1)+geom_line(aes(x=year,y=value,colour=Fleet, group=Fleet),data=fleet_data1,size=1) +facet_wrap(country+variable~.,scale="free")
# 
# ggsave("Fleet.jpg",width = 20,unit="cm")
# 
# 
# ggplot(data=fleet_data1, aes(x=year, y=value, fill=Fleet)) + 
#   geom_bar(stat="identity", position=position_dodge())+
#   facet_wrap(country + variable~.,scales="free_y",drop=FALSE,ncol=6)+ 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# ggsave("Fleet_histograms.jpg",width = 30,height=15,unit="cm")


# unique(fleet_data$variable)

# GVA = Income from landings + other income - energy costs - repair and maintenance costs - other variable costs - non variable costs.


# fleet_data2=fleet_data[fleet_data$variable %in% c("land_val", "Energy_costs", "Repair_and_maintenance_costs","Other_variable_costs","fcosts","Other_non-variable_costs"),]
# head(fleet_data2)
# 
# fleet_data2$id=paste(fleet_data2$year,fleet_data2$Fleet,fleet_data2$country)
# GVA_records=data.frame(matrix(nrow=0,ncol=8))
# colnames(GVA_records)=colnames(fleet_data2)  
# 
# for (comb in unique(fleet_data2$id)){
#   fleet_data2_temp=fleet_data2[fleet_data2$id==comb,]
#   GVA = fleet_data2_temp[fleet_data2_temp$variable=="landings.value","value"]-fleet_data2_temp[fleet_data2_temp$variable=="Energy_costs","value"]-fleet_data2_temp[fleet_data2_temp$variable=="Repair_and_maintenance_costs","value"]- fleet_data2_temp[fleet_data2_temp$variable=="Other_non-variable_costs","value"]-fleet_data2_temp[fleet_data2_temp$variable=="Other_variable_costs","value"]
#   
#   GVA_records2=fleet_data2_temp[1,]
#   GVA_records2$variable="GVA"
#   GVA_records2$value=GVA
#   GVA_records=rbind(GVA_records,GVA_records2)  
# }


# fleet_data=rbind(fleet_data,GVA_records[,-ncol(GVA_records)])

fleet_data3=fleet_data[fleet_data$variable %in% c("land_val",  "jobs"),]
fleet_data3=fleet_data3[!is.na(fleet_data3$value),]

output_data$socioeco_data <- fleet_data3
# ggplot(aes(x=year,y=value,colour=Fleet),data=fleet_data3)+geom_line(aes(x=year,y=value,colour=Fleet, group=Fleet),size=1)+facet_wrap(country~variable,scale="free",ncol=6) + theme(axis.text.x = element_text(angle=45))
# 
# ggsave("Economic.jpg",width = 30,height=15,unit="cm")



carbon_data=fleet_data[fleet_data$variable %in% c("CO2 emissions"),]
output_data$carbon_data <- carbon_data
# ggplot(aes(x=year,y=value,colour=Fleet),data=fleet_data3)+geom_line(aes(x=year,y=value,colour=Fleet,group=Fleet),size=1)+facet_wrap(country+variable~.,scale="free")
# 
# 
# 
# ggplot(data=data.frame(fleet_data3), aes(x=year, y=value, fill=Fleet)) +
#   geom_bar(stat="identity", position=position_dodge())+
#   facet_wrap(country~ variable,scales="free_y",drop=FALSE,ncol=3)+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#   labs(fill='Year')
# 
# 
# 
 # ggsave("Carbon_dem.jpg",width = 20,height=10, unit="cm")
# 

# Linear regressions
# small_scale=data.frame(readxl::read_xlsx("data-raw/template for deliverable 2.10.1_BoB_Demersal.xlsx",sheet="fish price SP"))
# # head(small_scale)
# small_scale=small_scale[small_scale$Fleet=="<24",]
# small_scale$Fleet="Small_scale"
# 
# large_scale=data.frame(readxl::read_xlsx("data-raw/template for deliverable 2.10.1_BoB_Demersal.xlsx",sheet="fish price SP"))
# # head(large_scale)
# large_scale=large_scale[large_scale$Fleet!="<24",]
# large_scale$Fleet="Large_scale"
# 
# 
# fish_fuel_price <- data.frame()
# 
# fleet_data=rbind(small_scale,large_scale)
# dati2=fleet_data
# 
# dati2<- dati2 %>%
#    group_by(Year, Stock, Price.per.kg,  Fleet) %>%
#   summarise(Price=mean(Price.per.kg))
# dati2$Country="Spain"
# dati2$Stock_Country=paste(dati2$Country,dati2$Stock,sep="_")
# dati2$variable="Price"
# 
# ############
# 
# 
# countries = unique(fuel$Country)
# 
# library(ggpmisc)
# library(broom)
# 
# fish_fuel_price <- data.frame()
# for(ii in 1:length(countries)){
#   
#   dat <- dati2 %>%
#     dplyr::group_by(year, spec, price_euro.per.kg, country, size) %>%
#     dplyr::summarise(Price=mean(price_euro.per.kg))
#   dat$Stock_Country=paste(dat$country,dat$spec,sep="_")
#   dat$variable="Price"
#   
#   Country= countries[ii]
#   Fleet="Small_scale"
#   size="small"
#   dat=dat[dat$size==size & dat$country==Country,]
#   
#   fuel=as.data.frame(fleet_data[fleet_data$variable=="fuel_price" & fleet_data$country==Country,])
#   fuel = fuel[complete.cases(fuel),]
#   # cast
#   fuel_cast = dcast(fuel,year + size + Fleet + country ~ variable,value.var = "value")
#   
#   DF=merge(dat,fuel_cast,by.x=c("year","country","size"),by.y=c("year","country","size"))
#   
#   # Large scale
#   
#   dati2=readxl::read_xlsx("data-raw/Task2.10.1_Summary_output_small.vs.largeFleets_NorthSea.xlsx",sheet="fish price")
#   
#   # summarise by species and not by stock
#   dati2$spec = gsub("[0-9]+|-NS|-EC|OTH","",dati2$stock)
#   
#   
#   dat<- dati2 %>%
#     dplyr::group_by(year, spec, price_euro.per.kg, country, size) %>%
#     dplyr::summarise(Price=mean(price_euro.per.kg))
#   dat$Stock_Country=paste(dat$country,dat$spec,sep="_")
#   dat$variable="Price"
#   Fleet="Large_scale"
#   size="large"
#   dat=dat[dat$size==size & dat$country==Country,]
#   
#   DF2=merge(dat,fuel_cast,by.x=c("year","country","size"),by.y=c("year","country","size"))
#   DF=rbind(DF,DF2)
#   
#   fish_fuel_price <- bind_rows(fish_fuel_price, DF)
#   # 
#   # formula<-y~x
#   # ggplot(DF,aes(x=fuel_price, y=Price,colour=Fleet,group=Fleet)) +
#   #   geom_point() +
#   #   geom_smooth(method='lm',se=T)+facet_wrap(~spec,scale="free") +ggtitle(paste(Country,sep=" ")) +
#   #   stat_fit_glance(method = 'lm',
#   #                   method.args = list(formula = formula),
#   #                   geom = 'text',
#   #                   aes(label = paste("P = ", signif(..p.value.., digits = 2), sep = "")),
#   #                   label.x = 'center', label.y = 0.35, size = 4, position = position_stack(vjust = 2))+
#   #   expand_limits(y = 2)
#   # 
#   # ggsave(paste("Fish_fuel_price",Country,".jpg",sep=" "),width=20,height=20,units="cm")
#   # 
#   
# }
# 
# 
# 
# ###########
# 
# Country="Spain"
# #Fleet="Small_scale"
# dati2=dati2[dati2$Country==Country,]
# 
# fuel=readxl::read_xlsx("data-raw/template for deliverable 2.10.1_BoB_Demersal.xlsx",sheet="Indicators")
#   
# fuel=  as.data.frame(fuel[fuel$Variable=="fuel_price" & fuel$Country==Country,])
# 
# dati3<-data.frame(dati2[,c(1,2,6,4,5,7)])
# #colnames(dati3)[4]="Fleet"
# 
# DF=merge(dati3,fuel,by.x=c("Year","Country"),by.y=c("Year","Country"))
# colnames(DF)[4]="Fleet"
# 
# colnames(DF)[9]="fuel_price"
#   
# fish_fuel_price <- bind_rows(fish_fuel_price, DF)  
# # Large scale
# 
# #dati2=readxl::read_xlsx("Task2.10.1_Summary_output_small.vs.largeFleets-NorthSea.xlsx",sheet="fish price")
# 
# #dati2=dati2[,-3]
# #colnames(dati2)[2]="Value"
# #colnames(dati2)[3]="Stock_Country"
# 
# # dati2<- dati2 %>%
# #   group_by(year, stock, price_euro.per.kg, country, size) %>%
# #   summarise(Price=mean(price_euro.per.kg))
# # dati2$Stock_Country=paste(dati2$country,dati2$stock,sep="_")
# # dati2$variable="Price"
# # Fleet="Large_scale"
# # size="large"
# # dati2=dati2[dati2$size==size & dati2$country==Country,]
# # 
# # fuel=as.data.frame(large_scale[large_scale$variable=="fuel_price" & large_scale$country==Country,])
# # 
# # 
# # dati3<-data.frame(dati2[,c(1,2,6,4,5,7)])
# # colnames(dati3)[3]="variable"
# # 
# # 
# # DF2=merge(dati3,fuel,by.x=c("year","country","size"),by.y=c("year","country","size"))
# # colnames(DF2)[5]="Price"
# # 
# # colnames(DF2)[9]="fuel_price"
# # 
# # DF=rbind(DF,DF2)
# 
# # library(ggpmisc)
# # formula<-y~x
# # ggplot(DF,aes(x=fuel_price, y=Price,colour=Fleet,group=Fleet)) +
# #   geom_point() +
# #   geom_smooth(method='lm',se=T)+facet_wrap(~Stock,scale="free") +ggtitle(paste(Country,sep=" ")) +
# #   stat_fit_glance(method = 'lm',
# #                   method.args = list(formula = formula),
# #                   geom = 'text',
# #                   aes(label = paste("P = ", signif(..p.value.., digits = 2), sep = "")),
# #                   label.x = 'center', label.y = 0.35, size = 4, position = position_stack(vjust = 2))+
# #   expand_limits(y = 2)
# # 
# # ggsave(paste("Fish_fuel_price",Country,".jpg",sep=" "),width=20,height=20,units="cm")
# 
# 
# 
# 
# # 
# # ggplot(data=dat, aes(y=Stock, x=adult_portions, fill=Fleet)) +
# #   geom_bar(stat="identity", position=position_dodge())+
# #   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+labs(y="thousands")+ggtitle ("Numer of adult portions by Stock")
# # 
# # ggsave(paste("Adult_portions_BoB.jpg",sep=" "),width=25,height=20,units="cm")
# 
# 
# 
# # Pelagic fishery
# 
# rm(list=ls())
# library(ggplot2)
# library(dplyr)
# setwd("C:\\Users\\Utente\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\H2020-SEAwise\\_____________WP2\\TASK 2.6\\templates")
# 
# 
# small_scale=data.frame(readxl::read_xlsx("data-raw/template for deliverable 2.10.1_BoB_Pelagic.xlsx",sheet="<24 metres"))
# 
# head(small_scale)
# small_scale$Fleet="Small_scale"
# 
# large_scale=data.frame(readxl::read_xlsx("data-raw/template for deliverable 2.10.1_BoB_Pelagic.xlsx",sheet=">24metres"))
# head(large_scale)
# large_scale$Fleet="Large_scale"
# 
# fleet_data=rbind(small_scale,large_scale)
# fleet_data$Country="Basque country"
# fleet_data1=fleet_data[fleet_data$Variable %in% c("vessels", "KW","GT","land"),]
# fleet_data1=fleet_data1[!is.na(fleet_data1$Value),]
# 
# # ggplot(aes(x=year,y=Value,colour=Fleet),data=fleet_data1)+geom_line(aes(x=Year,y=Value,colour=Fleet, group=Fleet),data=fleet_data1,size=1) +facet_wrap(Country+Variable~.,scale="free")
# # 
# # ggsave("Fleet.jpg",width = 20,unit="cm")
# # 
# # 
# # ggplot(data=fleet_data1, aes(x=Year, y=Value, fill=Fleet)) + 
# #   geom_bar(stat="identity", position=position_dodge())+
# #   facet_wrap(Country + Variable~.,scales="free_y",drop=FALSE,ncol=6)+ 
# #   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# # 
# # ggsave("Fleet_histograms.jpg",width = 30,height=15,unit="cm")
# 
# 
# unique(fleet_data$Variable)
# 
# 
# # GVA = Income from landings + other income - energy costs - repair and maintenance costs - other variable costs - non variable costs.
# 
# 
# # fleet_data2=fleet_data[fleet_data$variable %in% c("land_val", "Energy_costs", "Repair_and_maintenance_costs","Other_variable_costs","fcosts","Other_non-variable_costs"),]
# # head(fleet_data2)
# # 
# # fleet_data2$id=paste(fleet_data2$year,fleet_data2$Fleet,fleet_data2$country)
# # GVA_records=data.frame(matrix(nrow=0,ncol=8))
# # colnames(GVA_records)=colnames(fleet_data2)  
# # 
# # for (comb in unique(fleet_data2$id)){
# #   fleet_data2_temp=fleet_data2[fleet_data2$id==comb,]
# #   GVA = fleet_data2_temp[fleet_data2_temp$variable=="landings.value","value"]-fleet_data2_temp[fleet_data2_temp$variable=="Energy_costs","value"]-fleet_data2_temp[fleet_data2_temp$variable=="Repair_and_maintenance_costs","value"]- fleet_data2_temp[fleet_data2_temp$variable=="Other_non-variable_costs","value"]-fleet_data2_temp[fleet_data2_temp$variable=="Other_variable_costs","value"]
# #   
# #   GVA_records2=fleet_data2_temp[1,]
# #   GVA_records2$variable="GVA"
# #   GVA_records2$value=GVA
# #   GVA_records=rbind(GVA_records,GVA_records2)  
# # }
# 
# 
# # fleet_data=rbind(fleet_data,GVA_records[,-ncol(GVA_records)])
# 
# fleet_data3=fleet_data[fleet_data$Variable %in% c("land_val",  "jobs"),]
# fleet_data3=fleet_data3[!is.na(fleet_data3$Value),]
# 
# # ggplot(aes(x=Year,y=Value,colour=Fleet),data=fleet_data3)+geom_line(aes(x=Year,y=Value,colour=Fleet, group=Fleet),size=1)+facet_wrap(Country~Variable,scale="free",ncol=6) + theme(axis.text.x = element_text(angle=45))
# # 
# # ggsave("Economic.jpg",width = 20,height=10,unit="cm")
# 
# 
# 
# fleet_data3=fleet_data[!is.na(fleet_data$Value) & fleet_data$Variable %in% c("carbon_emis") ,]
# 
# # ggplot(aes(x=Year,y=Value,colour=Fleet),data=fleet_data3)+geom_line(aes(x=Year,y=Value,colour=Fleet,group=Fleet),size=1)+facet_wrap(Country+Variable~.,scale="free")
# 
# 
# 
# # ggplot(data=data.frame(fleet_data3), aes(x=Year, y=Value, fill=Fleet)) + 
# #   geom_bar(stat="identity", position=position_dodge())+
# #   facet_wrap(Country~ Variable,scales="free_y",drop=FALSE,ncol=3)+ 
# #   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
# #   labs(fill='Year',y="kg CO2")
# # 
# # 
# # 
# # ggsave("Carbon.jpg",width = 10,height=10, unit="cm")
# 
# # 
# 
# # Linear regressions
# small_scale=data.frame(readxl::read_xlsx("data-raw/template for deliverable 2.10.1_BoB_Pelagic.xlsx",sheet="fish price"))
# # head(small_scale)
# small_scale=small_scale[small_scale$Fleet=="<24metres",]
# small_scale$Fleet="Small_scale"
# 
# large_scale=data.frame(readxl::read_xlsx("data-raw/template for deliverable 2.10.1_BoB_Pelagic.xlsx",sheet="fish price"))
# # head(large_scale)
# large_scale=large_scale[large_scale$Fleet!="<24metres",]
# large_scale$Fleet="Large_scale"
# 
# 
# fleet_data=rbind(small_scale,large_scale)
# dati2=data.frame(fleet_data[!is.na(fleet_data$Price.per.kg) & as.character(fleet_data$Price.per.kg)!="NA",])
# class(dati2$Price.per.kg)="numeric"
# 
# dati2<- dati2 %>%
#   group_by(Year, Stock, Price.per.kg,  Fleet) %>%
#   summarise(Price=mean(Price.per.kg))
# dati2$Country="Basque Country"
# dati2$Stock_Country=paste(dati2$Country,dati2$Stock,sep="_")
# dati2$variable="Price"
# 
# 
# #Fleet="Small_scale"
# #dati2=dati2[dati2$Country==Country,]
# 
# fuel=readxl::read_xlsx("template for deliverable 2.10.1_BoB_Pelagic.xlsx",sheet="<24 metres")
# 
# fuel=  as.data.frame(fuel[fuel$Variable=="fuel_price",])
# 
# dati3<-data.frame(dati2[,c(1,2,6,4,5,7)])
# #colnames(dati3)[4]="Fleet"
# 
# DF=merge(dati3,fuel,by.x=c("Year","Country"),by.y=c("Year","Country"))
# #colnames(DF)[4]="Fleet"
# 
# colnames(DF)[7]="fuel_price"
# 
# # Large scale
# 
# #dati2=readxl::read_xlsx("Task2.10.1_Summary_output_small.vs.largeFleets-NorthSea.xlsx",sheet="fish price")
# 
# #dati2=dati2[,-3]
# #colnames(dati2)[2]="Value"
# #colnames(dati2)[3]="Stock_Country"
# 
# # dati2<- dati2 %>%
# #   group_by(year, stock, price_euro.per.kg, country, size) %>%
# #   summarise(Price=mean(price_euro.per.kg))
# # dati2$Stock_Country=paste(dati2$country,dati2$stock,sep="_")
# # dati2$variable="Price"
# # Fleet="Large_scale"
# # size="large"
# # dati2=dati2[dati2$size==size & dati2$country==Country,]
# # 
# # fuel=as.data.frame(large_scale[large_scale$variable=="fuel_price" & large_scale$country==Country,])
# # 
# # 
# # dati3<-data.frame(dati2[,c(1,2,6,4,5,7)])
# # colnames(dati3)[3]="variable"
# # 
# # 
# # DF2=merge(dati3,fuel,by.x=c("year","country","size"),by.y=c("year","country","size"))
# # colnames(DF2)[5]="Price"
# # 
# # colnames(DF2)[9]="fuel_price"
# # 
# # DF=rbind(DF,DF2)
# Country="Basque Country"
# # library(ggpmisc)
# # formula<-y~x
# # ggplot(DF,aes(x=fuel_price, y=Price,colour=Fleet,group=Fleet)) +
# #   geom_point() +
# #   geom_smooth(method='lm',se=T)+facet_wrap(~Stock,scale="free") +ggtitle(paste(Country,sep=" ")) +
# #   stat_fit_glance(method = 'lm',
# #                   method.args = list(formula = formula),
# #                   geom = 'text',
# #                   aes(label = paste("P = ", signif(..p.value.., digits = 2), sep = "")),
# #                   label.x = 'center', label.y = 0.35, size = 4, position = position_stack(vjust = 2))+
# #   expand_limits(y = 2)
# # 
# # ggsave(paste("Fish_fuel_price",Country,".jpg",sep=" "),width=20,height=20,units="cm")

# Adult portions

dat=readxl::read_xlsx("data-raw/Fish_portions.xlsx",sheet="BoB") 


dat2=readxl::read_xlsx("data-raw/Fish_portions.xlsx",sheet="BoB_dem")

output_data$adult_portions <- rbind(dat,dat2)
saveRDS(output_data, file = "data/wp2/BoB_data.rds")

# ggplot(data=dat, aes(y=Country, x=adult_portions, fill=Fleet)) +
#   geom_bar(stat="identity", position=position_dodge())+
#   facet_wrap(Stock~.,scales="free_x",drop=FALSE,ncol=4)+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+labs(y="thousands")+ggtitle ("Numer of adult portions by Country and Stock")
# 
# ggsave(paste("Adult_portions_BoB_dem.jpg",sep=" "),width=30,height=20,units="cm")
