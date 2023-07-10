library(tidyverse)
library(readxl)
library(GlobalArchive)

# read in data ----
caab.og <- read_excel("data/caab_dump_latest.xlsx") %>%
  ga.clean.names() %>%
  glimpse()

caab <- caab.og %>%
  dplyr::filter(class %in% c("Actinopterygii", "Elasmobranchii")) %>%
  dplyr::filter(!is.na(display_name)) %>%
  dplyr::filter(!is.na(parent_id)) %>%
  dplyr::filter(!stringr::str_detect(scientific_name, "non-current code")) %>%
  replace_na(list(genus = "Unknown", species = "spp")) %>%
  dplyr::select(spcode, kingdom, phylum, class, family, genus, species)
  
names(caab)

temp.list <- caab %>%
  filter(species %in% "spp")

list.to.download <- caab %>%
  filter(!species %in% "spp")

write.csv(temp.list, "output/fish/fish_caab_spps.csv", row.names = FALSE)

# ## Download distribution files - turn on to run again
# for (caab in unique(list.to.download$spcode)){
#   print(caab)
#   
#   # Set the URL of the shapefile
#   url <- paste0("https://www.cmar.csiro.au/geoserver/caab/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=caab%3ACAAB_FISHMAP&maxFeatures=200&outputFormat=SHAPE-ZIP&CQL_FILTER=SPCODE%3D%27", caab, "%27")
#   
#   # Set the destination file path where the shapefile will be saved
#   destination_file <- paste0("data/distributions/", caab, "_shapefile.zip")
#   
#   # Download the shapefile
#   download.file(url, destfile = destination_file, mode = "wb")
#   
#   # Check if the download was successful
#   if (file.exists(destination_file)) {
#     message("Shapefile downloaded successfully.")
#   } else {
#     message("Failed to download the shapefile.")
#   }
# }

# # Need to unzip the shapefiles and combine into one file ----
# caab <- 37076001
# 
# temp <- tempfile()
# 
# # Unzip the contents of the last caab from the loop above to get file format
# unzip(zipfile = paste0("data/distributions/", caab, "_shapefile.zip"), exdir = temp)
# 
# # Read the shapefile
# polygons <- sf::read_sf(temp) %>% glimpse()
# 
# for (caab in unique(list.to.download$spcode)){
#   # create a temp directory
#   temp <- tempfile()
#   
#   message(caab)
#   
#   # Unzip the contents of the temp and save unzipped content in 'temp2'
#   try(unzip(zipfile = paste0("data/distributions/", caab, "_shapefile.zip"), exdir = temp))
#   
#   # Read the shapefile
#   try(polygon <- sf::read_sf(temp) %>% glimpse())
#   
#   # Combine together
#   polygons <- bind_rows(polygons, polygon)
#   
# }
# print(Sys.time())
# 
# glimpse(polygons)
# 
# saveRDS(polygons, "data/distributions_polygons.RDS")

polygons <- readRDS("data/distributions_polygons.RDS")
