# Load the required libraries
library(httr)
library(jsonlite)
library(tidyverse)
library(GlobalArchive)
library(readxl)
library(rvest)
library(mregions)
library(worrms)
library(sf)
library(beepr)

categories <- read_csv("data/caab_categories.csv") %>%
  ga.clean.names() %>%
  dplyr::filter(include == "Yes")

# # Retrieve records for categories that have less than 1000 species ----
# less.1000 <- categories %>%
#   dplyr::filter(less.than.1000 == TRUE)
# 
# caab <- data.frame()
# 
# for (cat in unique(less.1000$category_code)) {
#   
#   print(cat)
#   
#   # URL to fetch JSON data
#   url <- paste0("https://www.cmar.csiro.au/data/services/caab/index.cfm?q=&cat=", cat, "&type=scientific&type=vernacular&type=synonym")
#   
#   # Fetch the data from the URL
#   response <- GET(url)
#   
#   # Check if the request was successful
#   if (http_status(response)$category == "Success") {
#     # Parse JSON content
#     data <- fromJSON(rawToChar(response$content))
#     
#     # Convert the list to a dataframe
#     df <- as.data.frame(data) %>%
#       mutate(across(everything(), as.character))
#     
#     caab <- bind_rows(caab, df)
#     
#     # Now, you have your dataframe (df) with the JSON data
#     # You can explore the dataframe using functions like head(), str(), etc.
#   } else {
#     cat("Failed to fetch data. Status code:", http_status(response)$status_code, "\n")
#   }
#   
# }
# 
# # Retrieve records for categories that have more than 1000 species ----
# more.1000 <- categories %>%
#   dplyr::filter(!less.than.1000 == TRUE)
# 
# # Need to get unique genus for those species ----
# caab.cat.with.genus <- read_excel("data/caab_dump_latest.xlsx") %>%
#   ga.clean.names() %>%
#   dplyr::mutate(cat = str_sub(spcode, 1, 2)) %>%
#   dplyr::distinct(cat, genus) %>%
#   dplyr::filter(!is.na(genus)) %>%
#   dplyr::filter(!is.na(cat)) %>%
#   dplyr::filter(cat %in% unique(more.1000$category_code)) %>%
#   dplyr::filter(!genus %in% c("Australoecia", "Brachycalanus", "Propontocypris")) # belong to two categories

# unique(caab.cat.with.genus$genus)
# 
# test <- caab.cat.with.genus %>%
#   dplyr::group_by(genus) %>%
#   summarise(n = n())
# 
# 
# Sys.time() # 30 seconds to do 100 8359/100*30/60 # should take ~ 42 minutes??
# # "2023-08-03 11:54:45 +08"
# # ETA = 12:30ish
# for (unique.genus in unique(caab.cat.with.genus$genus)) {
#   
#   # print(unique.genus)
#   
#   cat <- caab.cat.with.genus %>%
#     dplyr::filter(genus %in% unique.genus) %>%
#     distinct(cat)
#   
#   unique.cat <- unique(cat$cat) #%>%
#      #glimpse()
#   
#   # URL to fetch JSON data
#   url <- paste0("https://www.cmar.csiro.au/data/services/caab/index.cfm?q=", genus, "&cat=", unique.cat, "&type=scientific&type=vernacular&type=synonym")
#   
#   # Fetch the data from the URL
#   response <- GET(url)
#   
#   try(
#   # Check if the request was successful
#   if (http_status(response)$category == "Success") {
#     # Parse JSON content
#     data <- fromJSON(rawToChar(response$content))
#     
#     # Convert the list to a dataframe
#     df <- as.data.frame(data) %>%
#       mutate(across(everything(), as.character))
#     
#     caab <- bind_rows(caab, df)
#     
#     # Now, you have your dataframe (df) with the JSON data
#     # You can explore the dataframe using functions like head(), str(), etc.
#   } else {
#     cat("Failed to fetch data. Status code:", http_status(response)$status_code, "\n")
#   }
#   )
#   
# }
# 
# # Trying crustaceans again
# # URL to fetch JSON data
#   url <- paste0("https://www.cmar.csiro.au/data/services/caab/index.cfm?q=&cat=28&type=scientific&type=vernacular&type=synonym")
# 
#   # Fetch the data from the URL
#   response <- GET(url)
# 
#   # Check if the request was successful
#   if (http_status(response)$category == "Success") {
#     # Parse JSON content
#     data <- fromJSON(rawToChar(response$content))
# 
#     # Convert the list to a dataframe
#     df <- as.data.frame(data) %>%
#       mutate(across(everything(), as.character))
# 
#     caab <- bind_rows(caab, df)
# 
#     # Now, you have your dataframe (df) with the JSON data
#     # You can explore the dataframe using functions like head(), str(), etc.
#   } else {
#     cat("Failed to fetch data. Status code:", http_status(response)$status_code, "\n")
#   }
# 
# 
# 
# Sys.time()

# write_rds(caab, "data/caab_animals_scraped.RDS")
caab <- readRDS("data/caab_animals_scraped.RDS")

clean.caab <- caab %>%
  ga.clean.names() %>%
  dplyr::select(caab_code, habitat, parent_id, scientificname, kingdom, order, family, vernacularname, scientificnameid_worms, class, scientificnameid_afd, caab_status) %>%
  dplyr::filter(!parent_id %in% c("", NA, NULL)) %>%
  tidyr::separate(scientificname, into = c("genus", "species"), extra = "merge") %>%
  dplyr::mutate(species = str_replace_all(species, "spp.", "spp")) %>%
  dplyr::mutate(aphiaid = str_replace_all(scientificnameid_worms, "urn:lsid:marinespecies.org:taxname:", ""))

species.with.id <- clean.caab %>%
  filter(!aphiaid %in% c(NULL, "", NA)) # a couple thousand don't have an ID

ids <- unique(species.with.id$aphiaid)[1:100] # TODO run with all data

distributions <- wm_distribution_(id = c(as.numeric(ids))) %>%
  dplyr::filter(!is.na(higherGeographyID))

# Get shapefiles for the marine regions using the mregion package ----
eez <- mr_shp(key = "MarineRegions:eez") # slow to run
iho <- mr_shp(key = "MarineRegions:iho") # quick to run
iho_quad <- mr_shp(key = "MarineRegions:iho_quadrants_20150810") # quick to run
nations <- mr_shp(key = "MarineRegions:worldcountries_esri_2014") # quick to run

eez_df <- st_set_geometry(eez, NULL) %>%
  dplyr::rename(name = geoname) %>%
  dplyr::mutate(source = "EEZ") %>%
  dplyr::select(name, mrgid, source)

iho_df <- st_set_geometry(iho, NULL) %>%
  dplyr::mutate(source = "IHO") %>%
  dplyr::select(name, mrgid, source)

iho_quad_df <- st_set_geometry(iho_quad, NULL)

iho_q1 <- iho_quad_df %>%
  dplyr::select(name_1, mrgid_1) %>%
  dplyr::rename(name = name_1, mrgid = mrgid_1) %>%
  dplyr::mutate(source = "IHO Q1") %>%
  dplyr::filter(!mrgid == 0)

iho_q2 <- iho_quad_df %>%
  dplyr::select(name_2, mrgid_2) %>%
  dplyr::rename(name = name_2, mrgid = mrgid_2) %>%
  dplyr::mutate(source = "IHO Q2")%>%
  dplyr::filter(!mrgid == 0)

iho_q3 <- iho_quad_df %>%
  dplyr::select(name_3, mrgid_3) %>%
  dplyr::rename(name = name_3, mrgid = mrgid_3) %>%
  dplyr::mutate(source = "IHO Q1")%>%
  dplyr::filter(!mrgid == 0)

nations_df <- st_set_geometry(nations, NULL) %>%
  dplyr::mutate(source = "Nations") 

nat_q1 <- nations_df %>%
  dplyr::select(territory1, mrgid_ter1) %>%
  dplyr::rename(name = territory1, mrgid = mrgid_ter1) %>%
  dplyr::mutate(source = "Nations Q1") %>%
  dplyr::filter(!mrgid == 0)

nat_q2 <- nations_df %>%
  dplyr::select(territory2, mrgid_ter2) %>%
  dplyr::rename(name = territory2, mrgid = mrgid_ter2) %>%
  dplyr::mutate(source = "Nations Q2") %>%
  dplyr::filter(!mrgid == 0)

nat_q3 <- nations_df %>%
  dplyr::select(territory3, mrgid_ter3) %>%
  dplyr::rename(name = territory3, mrgid = mrgid_ter3) %>%
  dplyr::mutate(source = "Nations Q3") %>%
  dplyr::filter(!mrgid == 0)

codes <- bind_rows(eez_df, iho_df, iho_q1, iho_q2, iho_q3, nat_q1, nat_q2, nat_q3) %>%
  dplyr::mutate(mrgid = as.character(mrgid))

joined <- distributions %>%
  dplyr::mutate(mrgid = str_replace_all(locationID, "http://marineregions.org/mrgid/", "")) %>%
  left_join(codes)

missing <- joined %>%
  dplyr::filter(is.na(source))

# Save shapefiles as test ----
# st_write(eez, "data/eez.shp")
# st_write(iho, "data/iho.shp")
# st_write(iho_quad, "data/iho_quad.shp")
# st_write(nations, "data/nations.shp")