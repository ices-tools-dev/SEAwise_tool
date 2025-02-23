rm(list=ls())
library(ggplot2)
library(dplyr)

fleet_data=data.frame(readxl::read_xlsx("data-raw/WW_d2_10_fleet_info.xlsx",sheet="WW_d2_10_fleet_info"))
# head(fleet_data)

fleet_data1=fleet_data[fleet_data$variable %in% c("vessels"),]
fleet_data1=fleet_data1[!is.na(fleet_data1$value),]
#fleet_data1$Fleet=fleet_data1$small_large
fleet_data1=fleet_data1[fleet_data1$small_large!="all",]
colnames(fleet_data1)[3]="Fleet"



output_data <- list(fleet_data = fleet_data1)
# ggplot(aes(x=year,y=value,colour=Fleet),data=fleet_data1)+geom_line(aes(x=year,y=value,colour=Fleet, group=Fleet),data=fleet_data1,linewidth=1) +facet_wrap(country+variable~.,scale="free")

#ggsave("Fleet.jpg",width = 20,unit="cm")


# ggplot(data=fleet_data1, aes(x=year, y=value, fill=Fleet)) + 
#   geom_bar(stat="identity", position=position_dodge())+
#   facet_wrap(country + variable~.,scales="free_y",drop=FALSE,ncol=6)+ 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# ggsave("Fleet_histograms.jpg",width = 30,height=12,unit="cm")


unique(fleet_data$variable)

# GVA = Income from landings + other income - energy costs - repair and maintenance costs - other variable costs - non variable costs.


fleet_data2=fleet_data[fleet_data$variable %in% c("land_val", "fuel_costs", "rep","variableCosts","fix costs","other_var_costs"),]
head(fleet_data2)

fleet_data2$id=paste(fleet_data2$year,fleet_data2$small_large,fleet_data2$country)
GVA_records=data.frame(matrix(nrow=0,ncol=8))
colnames(GVA_records)=colnames(fleet_data2)  

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
# ggplot(aes(x=year,y=value,colour=Fleet),data=fleet_data3)+geom_line(aes(x=year,y=value,colour=Fleet, group=Fleet),size=1)+facet_wrap(country~variable,scale="free",ncol=6) + theme(axis.text.x = element_text(angle=45))
# 
# ggsave("Economic.jpg",width = 40,height=20,unit="cm")



carbon_data=fleet_data[!is.na(fleet_data$variable %in% c("CO2_emission")) & fleet_data$variable %in% c("CO2_emission") & fleet_data$unit=="kg.per.fishingDay",]
output_data$carbon_data <- carbon_data
# 
# ggplot(aes(x=year,y=value,colour=Fleet),data=fleet_data3)+geom_line(aes(x=year,y=value,colour=Fleet,group=Fleet),size=1)+facet_wrap(country+variable~.,scale="free")
# 
# 
# 
# ggplot(data=data.frame(fleet_data3), aes(x=year, y=value, fill=Fleet)) + 
  # geom_bar(stat="identity", position=position_dodge())+
  # facet_wrap(country~ variable,scales="free_y",drop=FALSE,ncol=3)+ 
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  # labs(fill='Year')



#ggsave("Carbon.jpg",width = 20,height=15, unit="cm")


# Linear regressions

# dati2=data.frame(readxl::read_xlsx("data-raw/WW_d2_10_fleet_info.xlsx",sheet="fish_price"))
# colnames(dati2)
# 
# #dati2=dati2[,-3]
# #colnames(dati2)[2]="Value"
# #colnames(dati2)[3]="Stock_Country"
# 
# fish_fuel_price <- data.frame()
# dati2<- dati2 %>%
#    group_by(Year, Stock,Price.per.kg, Country, Fleet) %>%
#   summarise(Price=mean(Price.per.kg))
# dati2$Stock_Country=paste(dati2$Country,dati2$Stock,sep="_")
# dati2$Variable="Price"
# 
# dati2=dati2[dati2$Fleet!="all",]
# 
# Country="BE"
# #Fleet="small"
# #size="small"
# dati2=dati2[dati2$Country==Country,]
# 
# fuel=as.data.frame(fleet_data[fleet_data$variable=="fuel_price" & fleet_data$country==Country,])
# 
# dati3<- dati2 #data.frame(dati2[,c(1,2,6,4,5,7)])
# colnames(dati3)[3]="variable"
# 
# DF=merge(dati3,fuel,by.x=c("Year","Country"),by.y=c("year","country"))
# #colnames(DF)[5]="Price"
# 
# colnames(DF)[11]="fuel_price"
#   
# fish_fuel_price <- bind_rows(fish_fuel_price, DF)
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
# # DE
# 
# dati2=data.frame(readxl::read_xlsx("data-raw/WW_d2_10_fleet_info.xlsx",sheet="fish_price"))
# colnames(dati2)
# 
# #dati2=dati2[,-3]
# #colnames(dati2)[2]="Value"
# #colnames(dati2)[3]="Stock_Country"
# 
# dati2<- dati2 %>%
#   group_by(Year, Stock,Price.per.kg, Country, Fleet) %>%
#   summarise(Price=mean(Price.per.kg))
# dati2$Stock_Country=paste(dati2$Country,dati2$Stock,sep="_")
# dati2$Variable="Price"
# 
# dati2=dati2[dati2$Fleet!="all",]
# 
# Country="DE"
# #Fleet="small"
# #size="small"
# dati2=dati2[dati2$Country==Country,]
# 
# fuel=as.data.frame(fleet_data[fleet_data$variable=="fuel_price" & fleet_data$country==Country,])
# 
# dati3<- dati2 #data.frame(dati2[,c(1,2,6,4,5,7)])
# colnames(dati3)[3]="variable"
# 
# DF=merge(dati3,fuel,by.x=c("Year","Country"),by.y=c("year","country"))
# #colnames(DF)[5]="Price"
# 
# colnames(DF)[11]="fuel_price"
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
# fish_fuel_price <- bind_rows(fish_fuel_price, DF)
# 
# # ES
# 
# dati2=data.frame(readxl::read_xlsx("WW_d2_10_fleet_info.xlsx",sheet="fish_price"))
# colnames(dati2)
# 
# #dati2=dati2[,-3]
# #colnames(dati2)[2]="Value"
# #colnames(dati2)[3]="Stock_Country"
# 
# dati2<- dati2 %>%
#   group_by(Year, Stock,Price.per.kg, Country, Fleet) %>%
#   summarise(Price=mean(Price.per.kg))
# dati2$Stock_Country=paste(dati2$Country,dati2$Stock,sep="_")
# dati2$Variable="Price"
# 
# dati2=dati2[dati2$Fleet!="all",]
# 
# Country="ES"
# #Fleet="small"
# #size="small"
# dati2=dati2[dati2$Country==Country,]
# 
# fuel=as.data.frame(fleet_data[fleet_data$variable=="fuel_price" & fleet_data$country==Country,])
# 
# dati3<- dati2 #data.frame(dati2[,c(1,2,6,4,5,7)])
# colnames(dati3)[3]="variable"
# 
# DF=merge(dati3,fuel,by.x=c("Year","Country"),by.y=c("year","country"))
# #colnames(DF)[5]="Price"
# 
# colnames(DF)[11]="fuel_price"
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
# fish_fuel_price <- bind_rows(fish_fuel_price, DF)
# 
# 
# # FRA
# 
# 
# dati2=data.frame(readxl::read_xlsx("WW_d2_10_fleet_info.xlsx",sheet="fish_price"))
# colnames(dati2)
# 
# #dati2=dati2[,-3]
# #colnames(dati2)[2]="Value"
# #colnames(dati2)[3]="Stock_Country"
# 
# dati2<- dati2 %>%
#   group_by(Year, Stock,Price.per.kg, Country, Fleet) %>%
#   summarise(Price=mean(Price.per.kg))
# dati2$Stock_Country=paste(dati2$Country,dati2$Stock,sep="_")
# dati2$Variable="Price"
# 
# dati2=dati2[dati2$Fleet!="all",]
# 
# Country="FRA"
# #Fleet="small"
# #size="small"
# dati2=dati2[dati2$Country==Country,]
# 
# fuel=as.data.frame(fleet_data[fleet_data$variable=="fuel_price" & fleet_data$country==Country,])
# 
# dati3<- dati2 #data.frame(dati2[,c(1,2,6,4,5,7)])
# colnames(dati3)[3]="variable"
# 
# DF=merge(dati3,fuel,by.x=c("Year","Country"),by.y=c("year","country"))
# #colnames(DF)[5]="Price"
# 
# colnames(DF)[11]="fuel_price"
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
# fish_fuel_price <- bind_rows(fish_fuel_price, DF)
# 
# # IE
# 
# dati2=data.frame(readxl::read_xlsx("WW_d2_10_fleet_info.xlsx",sheet="fish_price"))
# colnames(dati2)
# 
# #dati2=dati2[,-3]
# #colnames(dati2)[2]="Value"
# #colnames(dati2)[3]="Stock_Country"
# 
# dati2<- dati2 %>%
#   group_by(Year, Stock,Price.per.kg, Country, Fleet) %>%
#   summarise(Price=mean(Price.per.kg))
# dati2$Stock_Country=paste(dati2$Country,dati2$Stock,sep="_")
# dati2$Variable="Price"
# 
# dati2=dati2[dati2$Fleet!="all",]
# 
# Country="IE"
# #Fleet="small"
# #size="small"
# dati2=dati2[dati2$Country==Country,]
# 
# fuel=as.data.frame(fleet_data[fleet_data$variable=="fuel_price" & fleet_data$country==Country,])
# 
# dati3<- dati2 #data.frame(dati2[,c(1,2,6,4,5,7)])
# colnames(dati3)[3]="variable"
# 
# DF=merge(dati3,fuel,by.x=c("Year","Country"),by.y=c("year","country"))
# #colnames(DF)[5]="Price"
# 
# colnames(DF)[11]="fuel_price"
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
# fish_fuel_price <- bind_rows(fish_fuel_price, DF)
# output_data$fish_fuel_data <- fish_fuel_price

# Adult portions

dat=readxl::read_xlsx("data-raw/Fish_portions.xlsx",sheet="CS") 

output_data$adult_portions <- dat
saveRDS(output_data, file = "data/wp2/CS_data.rds")

# ggplot(data=dat, aes(y=Country, x=adult_portions, fill=Fleet)) + 
#   geom_bar(stat="identity", position=position_dodge())+
#   facet_wrap(Stock~.,scales="free_x",drop=FALSE,ncol=5)+ 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+labs(y="thousands")+ggtitle ("Numer of adult portions by Country and Stock")
# 
# ggsave(paste("Adult_portions_CS.jpg",sep=" "),width=25,height=20,units="cm")
