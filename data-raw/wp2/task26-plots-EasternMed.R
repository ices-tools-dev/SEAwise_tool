rm(list=ls())
library(ggplot2)
library(dplyr)

fleet_data=data.frame(readxl::read_xlsx("data-raw/wp2/template for deliverable 2.10.1_HCMR.xlsx",sheet="small scale (fleet)"))[,1:6]
fleet_data$Fleet="Small scale"
fleet_data2=data.frame(readxl::read_xlsx("data-raw/wp2/template for deliverable 2.10.1_HCMR.xlsx",sheet="large scale (fleet)"))[,1:6]
fleet_data2$Fleet="Large scale"

fleet_data=rbind(fleet_data[,1:7],fleet_data2[,1:7])
fleet_data1=fleet_data[fleet_data$Variable %in% c("vessels","GT","KW","land"),]
#fleet_data1=fleet_data1[!is.na(fleet_data1$value),]
#head(fleet_data1)
#fleet_data1=fleet_data1[fleet_data1$small_large!="all",]
#colnames(fleet_data1)[3]="Fleet"

fleet_data1$Value=round(as.numeric(fleet_data1$Value))
# ggplot(aes(x=Year,y=Value,colour=Fleet),data=fleet_data1)+geom_line(aes(x=Year,y=Value,colour=Fleet, group=Fleet),data=fleet_data1,linewidth=1) +facet_wrap(Variable~.,scale="free")
#ggsave("Fleet.jpg",width = 20,unit="cm")
# ggplot(data=fleet_data1, aes(x=Year, y=Value, fill=Fleet)) + 
#   geom_bar(stat="identity", position=position_dodge())+
#   facet_wrap(Variable~.,scales="free_y",drop=FALSE,ncol=6)+ 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#ggsave("Fleet_histograms.jpg",width = 30,height=12,unit="cm")

colnames(fleet_data1) <- tolower(colnames(fleet_data1))
output_data <- list(fleet_data = fleet_data1)


# GVA = Income from landings + other income - energy costs - repair and maintenance costs - other variable costs - non variable costs.


fleet_data2=data.frame(fleet_data[fleet_data$Variable %in% c("land_val", "fuel_costs", "rep","other_var_costs","fix costs"),])
# head(fleet_data2)
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
#colnames(fleet_data3)[3]="Fleet"
#fleet_data3=fleet_data3[fleet_data3$Fleet!="all",]

#ggplot(aes(x=Year,y=Value,colour=Fleet),data=data.frame(fleet_data3))+geom_line(aes(x=Year,y=Value,colour=Fleet, group=Fleet),linewidth=1)+facet_wrap(Variable~.,scale="free",ncol=2) + theme(axis.text.x = element_text(angle=45))+ylim(0,NA)
#ggsave("Economic.jpg",width = 20,height=10,unit="cm")


output_data$socioeco_data <- fleet_data3
# carbon_data=fleet_data[fleet_data$Variable %in% c("carbon_emis"),]
output_data$carbon_data <- NULL
# # 
#  ggplot(aes(x=Year,y=value,colour=Fleet),data=fleet_data3)+geom_line(aes(x=year,y=value,colour=Fleet,group=Fleet),size=1)+facet_wrap(country+variable~.,scale="free")
# # 
# 
# 
# ggplot(data=data.frame(fleet_data3), aes(x=year, y=value, fill=Fleet)) + 
  # geom_bar(stat="identity", position=position_dodge())+
  # facet_wrap(country~ variable,scales="free_y",drop=FALSE,ncol=3)+ 
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  # labs(fill='Year')



#ggsave("Carbon.jpg",width = 20,height=15, unit="cm")


# Linear regressions

dati2=data.frame(readxl::read_xlsx("data-raw/wp2/template for deliverable 2.10.1_HCMR.xlsx",sheet="fish price"))[,1:6]
#colnames(dati2)
dati2=dati2[as.character(dati2$Price.per.kg)!="NA",c(1,2,4,5)]
#dati2=dati2[,-3]
#colnames(dati2)[2]="Value"
#colnames(dati2)[3]="Stock_Country"

# dati2<- dati2 %>%
#    group_by(Year, Stock,Price.per.kg, Country, Fleet) %>%
#   summarise(Price=mean(Price.per.kg))
# dati2$Stock_Country=paste(dati2$Country,dati2$Stock,sep="_")
dati2$Variable="Price"
colnames(dati2)[2]="Value"
#dati2=dati2[dati2$Fleet!="all",]

#Country="BE"
#Fleet="small"
#size="small"
#dati2=dati2[dati2$Country==Country,]

fuel=as.data.frame(fleet_data[fleet_data$Variable=="fuel_price" & !is.na(fleet_data$Value) & as.character(fleet_data$Value)!="NA",])

dati3<- dati2 #data.frame(dati2[,c(1,2,6,4,5,7)])
#colnames(dati3)[2]="Variable"

DF=merge(dati3,fuel,by.x=c("Year","Fleet"),by.y=c("Year","Fleet"))
colnames(DF)[3]="Price"

colnames(DF)[10]="fuel_price"
#colnames(DF)[13]="Fleet"
#DF=DF[as.character(DF$Price)!="NA", ]

#DF=data.frame(DF[, c(1,2,4,12,13)])
DF$Price=as.numeric(DF$Price)
DF$fuel_price=as.numeric(DF$fuel_price)
colnames(DF)[colnames(DF) =="Country"] <- "country"

output_data$fish_fuel_data <- DF
#library(ggpmisc)
#formula<-y~x
#ggplot(DF,aes(x=fuel_price, y=Price,colour=Fleet,group=Fleet)) +
  # geom_point() +
  # geom_smooth(method='lm',se=T)+facet_wrap(~Stock,scale="free")  +
  # stat_fit_glance(method = 'lm',
  #                 method.args = list(formula = formula),
  #                 geom = 'text',
  #                 aes(label = paste("P = ", signif(after_stat(p.value), digits = 2), sep = "")),
  #                 label.x = 'center', label.y = 0.35, size = 4, position = position_stack(vjust = 2))+
  # expand_limits(y = 2)

#ggsave(paste("Fish_fuel_price.jpg",sep=" "),width=20,height=15,units="cm")

# 
# # Adult portions
# 
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

# ggplot(data=dat, aes(y=Country, x=adult_portions, fill=Fleet)) +
#   geom_bar(stat="identity", position=position_dodge())+
#   facet_wrap(Stock~.,scales="free_x",drop=FALSE,ncol=5)+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+labs(x="thousands")+ggtitle ("Numer of adult portions by Country and Stock")
# 
# ggsave(paste("Adult_portions_EMed.jpg",sep=" "),width=15,height=5,units="cm")
