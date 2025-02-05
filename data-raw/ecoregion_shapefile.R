## code to prepare `ecoregion_shapefile` dataset goes here
library(sf)

eco_shape <- st_read(dsn = "data-raw/shape_eco_simplified/shape_eco_simplified.shp")

eco_shape <- dplyr::filter(eco_shape, !Ecoregion %in% c("Western Mediterranean Sea", "Black Sea", "Azores", "Icelandic Waters", "Greenland Sea", "Norwegian Sea", "Barents Sea", "Arctic Ocean", "Faroes", "Oceanic Northeast Atlantic"))
# Add an id to each ecoregion (this potentially can be eliminated because the ecoregions in the shape file have already an id)
eco_shape$uid <- paste0("P", 1:7)

med_features <- eco_shape$OBJECTID %in% c(5,7,8)
joined_regions <- st_union(eco_shape[med_features, ])
joined_sfc <- st_sf(OBJECTID = 3, Ecoregion = "Mediterranean", geometry = st_sfc(joined_regions))
eco_shape <- rbind(eco_shape[!med_features, c("OBJECTID", "Ecoregion")], joined_sfc)

ww_features <- eco_shape$OBJECTID %in% c(2,9)
joined_regions <- st_union(eco_shape[ww_features, ])
joined_sfc <- st_sf(OBJECTID = 4, Ecoregion = "Western Waters", geometry = st_sfc(joined_regions))
eco_shape <- rbind(eco_shape[!ww_features, c("OBJECTID", "Ecoregion")], joined_sfc)



usethis::use_data(eco_shape, overwrite = TRUE)

