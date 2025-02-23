rm(list=ls())
library(ggplot2)
library(dplyr)
#setwd("C:\\Users\\Utente\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\H2020-SEAwise\\_____________WP2\\TASK 2.6\\templates")

small_scale=data.frame(readxl::read_xlsx("data-raw/wp2/template for deliverable 2.10.1_Cmed_12m.xlsx",sheet="small scale (fleet)"))
#head(small_scale)
small_scale$Fleet="Small_scale"

large_scale=data.frame(readxl::read_xlsx("data-raw/wp2/template for deliverable 2.10.1_Cmed_12m.xlsx",sheet="large scale (fleet)"))
#head(large_scale)
large_scale$Fleet="Large_scale"


fleet_data=rbind(small_scale,large_scale)
fleet_data1=fleet_data[fleet_data$Variable %in% c("vessels", "KW","GT","land"),]
fleet_data1=fleet_data1[!is.na(fleet_data1$Value),]

# ggplot(aes(x=year,y=value,colour=Fleet),data=fleet_data1)+geom_line(aes(x=Year,y=Value,colour=Fleet, group=Fleet),data=fleet_data1,linewidth=1) +facet_wrap(Country+Variable~.,scale="free")
# 
# ggsave("Fleet.jpg",width = 20,unit="cm")


# ggplot(data=fleet_data1, aes(x=Year, y=Value, fill=Fleet)) + 
#   geom_bar(stat="identity", position=position_dodge())+
#   facet_wrap(Country + Variable~.,scales="free_y",drop=FALSE,ncol=4)+ 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# ggsave("Fleet_histograms.jpg",width = 30,height=20,unit="cm")
colnames(fleet_data1) <- tolower(colnames(fleet_data1))
output_data <- list(fleet_data = fleet_data1)
# unique(fleet_data$Variable)

# GVA = Income from landings + other income - energy costs - repair and maintenance costs - other variable costs - non variable costs.


fleet_data2=fleet_data[fleet_data$Variable %in% c("land_val", "fuel_costs", "rep","other_var_costs","fix costs"),]
#head(fleet_data2)

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

# ggplot(aes(x=Year,y=Value,colour=Fleet),data=fleet_data3)+
# geom_line(aes(x=Year,y=Value,colour=Fleet, group=Fleet),size=1)+
# facet_wrap(Country~Variable,scale="free",ncol=3) + theme(axis.text.x = element_text(angle=45))
# 
# ggsave("Economic.jpg",width = 20,height=20,unit="cm")


output_data$socioeco_data <- fleet_data3

carbon_data=fleet_data[fleet_data$Variable %in% c("carbon_emis"),]
output_data$carbon_data <- carbon_data

# ggplot(aes(x=Year,y=Value,colour=Fleet),data=fleet_data3)+geom_line(aes(x=Year,y=Value,colour=Fleet,group=Fleet),size=1)+facet_wrap(Country+Variable~.,scale="free")



# ggplot(data=data.frame(fleet_data3), aes(x=Year, y=Value, fill=Fleet)) + 
#   geom_bar(stat="identity", position=position_dodge())+
#   facet_wrap(Country~ Variable,scales="free_y",drop=FALSE,ncol=3)+ 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#   labs(fill='Year')
# 
# 
# 
# ggsave("Carbon.jpg",width = 20,height=10, unit="cm")


# Linear regressions

dati2=readxl::read_xlsx("data-raw/wp2/template for deliverable 2.10.1_Cmed_12m.xlsx",sheet="fish price")

fish_fuel_price <- data.frame()
#dati2=dati2[,-3]
#colnames(dati2)[2]="Value"
#colnames(dati2)[3]="Stock_Country"

# dati2<- dati2 %>%
#    group_by(Year, Stock, Price.per.kg, country, size) %>%
#   summarise(Price=mean(price_euro.per.kg))
dati2$Stock_Country=paste(dati2$Country,dati2$Stock,sep="_")
dati2$Variable="Price"


Country="Croatia"
#Fleet="Small_scale"
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
# library(ggpmisc)
# formula<-y~x
# ggplot(DF,aes(x=fuel_price, y=Price,colour=Fleet,group=Fleet)) +
#   geom_point() +
#   geom_smooth(method='lm',se=T)+facet_wrap(~Stock,scale="free") +ggtitle(paste(Country,sep=" ")) +
#   stat_fit_glance(method = 'lm',
#                   method.args = list(formula = formula),
#                   geom = 'text',
#                   aes(label = paste("P = ", signif(..p.value.., digits = 2), sep = "")),
#                   label.x = 'center', label.y = 0.35, size = 4, position = position_stack(vjust = 2))+
#   expand_limits(y = 2)

# ggsave(paste("Fish_fuel_price",Country,".jpg",sep=" "),width=20,height=10,units="cm")



dati2=readxl::read_xlsx("data-raw/wp2/template for deliverable 2.10.1_Cmed_12m.xlsx",sheet="fish price")

#dati2=dati2[,-3]
#colnames(dati2)[2]="Value"
#colnames(dati2)[3]="Stock_Country"

# dati2<- dati2 %>%
#    group_by(Year, Stock, Price.per.kg, country, size) %>%
#   summarise(Price=mean(price_euro.per.kg))
dati2$Stock_Country=paste(dati2$Country,dati2$Stock,sep="_")
dati2$Variable="Price"


Country="Italy"
#Fleet="Small_scale"
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

# library(ggpmisc)
# formula<-y~x
# ggplot(DF,aes(x=fuel_price, y=Price,colour=Fleet,group=Fleet)) +
#   geom_point() +
#   geom_smooth(method='lm',se=T)+facet_wrap(~Stock,scale="free") +ggtitle(paste(Country,sep=" ")) +
#   stat_fit_glance(method = 'lm',
#                   method.args = list(formula = formula),
#                   geom = 'text',
#                   aes(label = paste("P = ", signif(..p.value.., digits = 2), sep = "")),
#                   label.x = 'center', label.y = 0.35, size = 4, position = position_stack(vjust = 2))+
#   expand_limits(y = 2)
# 
# ggsave(paste("Fish_fuel_price",Country,".jpg",sep=" "),width=20,height=17,units="cm")

dati2=readxl::read_xlsx("data-raw/wp2/template for deliverable 2.10.1_Cmed_12m.xlsx",sheet="fish price")

#dati2=dati2[,-3]
#colnames(dati2)[2]="Value"
#colnames(dati2)[3]="Stock_Country"

# dati2<- dati2 %>%
#    group_by(Year, Stock, Price.per.kg, country, size) %>%
#   summarise(Price=mean(price_euro.per.kg))
dati2$Stock_Country=paste(dati2$Country,dati2$Stock,sep="_")
dati2$Variable="Price"


Country="Slovenia"
#Fleet="Small_scale"
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
# library(ggpmisc)
# formula<-y~x
# ggplot(DF,aes(x=fuel_price, y=Price,colour=Fleet,group=Fleet)) +
#   geom_point() +
#   geom_smooth(method='lm',se=T)+facet_wrap(~Stock,scale="free") +ggtitle(paste(Country,sep=" ")) +
#   stat_fit_glance(method = 'lm',
#                   method.args = list(formula = formula),
#                   geom = 'text',
#                   aes(label = paste("P = ", signif(..p.value.., digits = 2), sep = "")),
#                   label.x = 'center', label.y = 0.35, size = 4, position = position_stack(vjust = 2))+
#   expand_limits(y = 2)
# 
# ggsave(paste("Fish_fuel_price",Country,".jpg",sep=" "),width=20,height=10,units="cm")

output_data$fish_fuel_data <- fish_fuel_price

# Adult portions

dat=readxl::read_xlsx("data-raw/wp2/Fish_portions.xlsx",sheet="CMed")
output_data$adult_portions <- dat


# Socio-eco projections

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

# ggplot(data=dat, aes(y=Country, x=adult_portions, fill=Fleet)) +
#   geom_bar(stat="identity", position=position_dodge())+
#   facet_wrap(Stock~.,scales="free_x",drop=FALSE,ncol=3)+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+labs(x="thousands")+ggtitle ("Numer of adult portions by Country and Stock")
# 
# ggsave(paste("Adult_portions_CMed.jpg",sep=" "),width=30,height=20,units="cm")
