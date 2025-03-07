

rm(list=ls())

#library(sp)
#library(rgdal)
library(raster)
#library(rgeos)
library(dplyr)
library(sf)
library(ggmap, quietly = T)
library(ggnewscale)
library(ggtext)
library(stringr)


land <- read_sf("./data-raw/wp4/data/Europe_coastline_shapefile/Europe_coastline_poly.shp")
# ecoreg <- read_sf("./data-raw/data/ICES_ecoregions_20171207_erase_ESRI.shp")
#ecoreg_centroids <- st_centroid(ecoreg)
#head(ecoreg)

###set the projection for land!!
land <-sf::st_transform(land, crs =4326)

minlong <- -15
maxlong <- 0
minlat  <- 42
maxlat  <- 60

cutlat  <- 50


coordslim_ww <- c(minlong,maxlong,minlat,maxlat)
coordslim_bob <- c(-12,maxlong,minlat,52)
coordslim_cs <- c(minlong,2,48,maxlat)


coordxmap <- round(seq(minlong,maxlong,length.out = 4))
coordymap <- round(seq(minlat,maxlat,length.out = 4))
ext <- st_bbox(c(xmin = minlong, xmax = maxlong,
                 ymin = minlat, ymax = maxlat),
               crs =  4326)

cutext_bob <- st_bbox(c(xmin = minlong, xmax = maxlong,
                        ymin = minlat, ymax = cutlat),
                      crs =  4326)
cutext_igfs <- st_bbox(c(xmin = minlong, xmax = maxlong,
                         ymin = cutlat, ymax = cutlat),
                       crs =  4326)

load(file = "./data-raw/wp4/data/WW/rbs_bob.RData" ) # object is rbs_bob
load("./data-raw/wp4/data/WW/newrelbs.RDA") #object is depsar_dat and is celtic sea region

raster::crs(rbs_bob) <- "EPSG:3035"
rbs_bob <- projectRaster(rbs_bob, crs=4326)

# plot(rbs_bob)
rbs_bob <- crop(rbs_bob,cutext_bob)
rbs_cs <- depsar_dat[depsar_dat$y>50,]

rbs_bob <- as.data.frame(rbs_bob,xy=TRUE)

rbs_data <- list(rbs_cs = rbs_cs,
                 rbs_bob = rbs_bob)



###litter 

load("./data-raw/wp4/data/litter_casper_NE_atlantic.RData")
litter$noperkm <- litter$Fishing.related.2021*length(litter$lon) 
litter <- litter %>%
  filter(str_detect(as.character(litter$ICES_SUB), paste(c("\nVb\n","VI", "VII"),collapse = '|')))


####bycatch risk
PUFMAU_bob <- read_sf("./data-raw/wp4/data/WW/amaia/PUFMAU.shp/PUFMAU.shp",crs=4326)
table(PUFMAU_bob$z)
class(PUFMAU_bob$z)

factor_levels <- c("Low","Medium", "High")

PUFMAU_bob$z <- factor(PUFMAU_bob$z, levels = 1:3, labels = factor_levels)
table(PUFMAU_bob$z)

load("data-raw/wp4/data/WW/bigdat.rda") ## irish waters cetatcean risk
head(longdat)
pph <- longdat[longdat$species=="Pph" & longdat$gear=="nets" & longdat$season=="summer",]
dim(pph)
pph <- pph %>% group_by(grid_id) %>% summarise(mean = mean(R, na.rm=TRUE))
tmp_notsf <- tmp_notsf[,c("grid_id","x")]
tmp_notsf$R <- pph$mean[match(pph$grid_id,tmp_notsf$grid_id)]
#Categorize
tmp_notsf$R1.score<-NA
tmp_notsf$R1.score<-ifelse(tmp_notsf$R>3.18, 3,tmp_notsf$R1.score)
tmp_notsf$R1.score<-ifelse(tmp_notsf$R<=3.18&  tmp_notsf$R>2.64, 2,tmp_notsf$R1.score)
tmp_notsf$R1.score<-factor(ifelse(tmp_notsf$R<2.64,1,tmp_notsf$R1.score))
summary(tmp_notsf$R1.score)
levels(tmp_notsf$R1.score) <- c(1,2,3)
min(st_coordinates(tmp_notsf$x)[,1])

tmp <- sf::st_as_sf(tmp_notsf, crs =4326)

bycatch <- list(shearwater = PUFMAU_bob,
                cetacean = tmp)

#####ecosystem
SCENARIO<-"ALLGEARS"
subreg<-"CS"
sh <- "WW"

AGCSbiomass_output <- read.csv(file=paste("./data-raw/wp4/data/", sh,"/",subreg,"_",SCENARIO,"_BIOMASS_RESULTS.csv",sep=""),header=TRUE)
AGCSecosystem_CR_output <- read.csv(file=paste("./data-raw/wp4/data/", sh,"/",subreg,"_",SCENARIO,"_ECOSYSTEM_RISK_RESULTS.csv",sep=""),header=TRUE)

#Extract fish biomass

extract_af_ratio <- function(data){
  
  NFC<-ncol(data)
  
  subdata<-subset(data,units=="Tonnes_WW_in_the_whole_model_domain")
  
  fsub1 <- subdata[grep("Planktivorous_fish",subdata$variablename),]
  fsub2 <- subdata[grep("Demersal_fish",subdata$variablename),]
  fsub3 <- subdata[grep("Migratory_fish",subdata$variablename),]
  
  if(nrow(fsub1) == 1) PTY<-"nolarvae"
  if(nrow(fsub1) == 2) PTY<-"larvae"
  notlarvrow<-1
  if(nrow(fsub1) == 2){
    larvrow<-grep("larvae",fsub1$variablename)
    if(larvrow==1) notlarvrow<-2
    if(larvrow==2) notlarvrow<-1
  }
  notlarvae_biomass1<-(as.numeric(fsub1[notlarvrow,3:(NFC)]))/1000
  #if(PTY=="larvae") larvae_biomass1<-(as.numeric(fsub1[larvrow,3:(NFC)]))/1000
  
  if(nrow(fsub2) == 1) PTY<-"nolarvae"
  if(nrow(fsub2) == 2) PTY<-"larvae"
  notlarvrow<-1
  if(nrow(fsub2) == 2){
    larvrow<-grep("larvae",fsub2$variablename)
    if(larvrow==1) notlarvrow<-2
    if(larvrow==2) notlarvrow<-1
  }
  notlarvae_biomass2<-(as.numeric(fsub2[notlarvrow,3:(NFC)]))/1000
  #if(PTY=="larvae") larvae_biomass2<-(as.numeric(fsub2[larvrow,3:(NFC)]))/1000
  
  fishbiomass<-notlarvae_biomass1+notlarvae_biomass2 + as.numeric(fsub3[,3:(NFC)])/1000
  
  #Extract predator biomass
  
  psub1 <- subdata[grep("Bird",subdata$variablename),]
  psub2 <- subdata[grep("Pinniped",subdata$variablename),]
  psub3 <- subdata[grep("Cetacean",subdata$variablename),]
  
  apexbiomass <- as.numeric( (psub1[,3:(NFC)] + psub2[,3:(NFC)] + psub3[,3:(NFC)]))/1000
  
  af_ratio<- apexbiomass/fishbiomass
  
  return(af_ratio)
  
}

#.................
AGgmultvec<-c(0,0.2,0.4,0.7,1,1.5,2,2.5,3,4,6,8) # sequences of gear mults to use in the impact phase
AGNF<-length(AGgmultvec)


AGCSaf_ratio<-extract_af_ratio(data=AGCSbiomass_output)
AGCSrisk<-as.numeric((subset(AGCSecosystem_CR_output,variablename=="whole_ecosystem_CR"))[,3:(AGNF+2)])
ecosystem_data <- data.frame(
  ratio = AGCSaf_ratio,
  risk = AGCSrisk
)


WP4_WW <- list(rbs = rbs_data,
               bycatch = bycatch,
               litter = litter,
               ecosystem = ecosystem_data,
               map_parameters = list(coordslim = list(coordslim_ww = coordslim_ww,
                                                      coordslim_cs = coordslim_cs,
                                                      coordslim_bob = coordslim_bob),
                                     coordxmap = coordxmap,
                                     coordymap = coordymap))

usethis::use_data(WP4_WW, overwrite = T)