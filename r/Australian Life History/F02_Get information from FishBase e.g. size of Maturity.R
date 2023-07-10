library(tidyverse)
library(rfishbase)
library(openssl)
library(GlobalArchive)
library(taxize) # For IUCN

# Read in simple life history and synonyms ----

# Changing from life history to CAAB list 10/07/2023
# simple.lh <- readRDS("data/simple.life.history.RDS") %>%
#   ga.clean.names()

caab <- readRDS("data/caab.with.regions.RDS") %>%
  dplyr::mutate(scientific.name = paste(genus, species, sep = " ")) %>%
  dplyr::filter(!species == "spp")

synonyms <- readRDS("data/synonyms.RDS") %>%
  ga.clean.names() %>%
  distinct(genus, species, genus_correct, species_correct)

# Validate scientific names to use in fishbase package ----
validated <- rfishbase::validate_names(caab$scientific.name)

codes <- common_names(validated) %>%
  ga.clean.names() %>%
  distinct(species, speccode) %>%
  tidyr::separate(species, into = c("genus", "species"), sep = " ") %>%
  filter(!is.na(species)) %>%
  dplyr::left_join(synonyms) %>%
  dplyr::mutate(genus = if_else(is.na(genus_correct), genus, genus_correct)) %>%
  dplyr::mutate(species = if_else(is.na(species_correct), species, species_correct)) %>%
  dplyr::select(-c(genus_correct, species_correct))

fishbase.to.caab <- bind_cols(codes %>% dplyr::rename(fb.genus = genus, fb.species = species), caab) 
mismatches <- fishbase.to.caab %>%
  dplyr::filter(!fb.genus %in% genus | !fb.species %in% species)

# Download maturity data ----
# Filter to only Fork length measurements
# Remove any that are NA
# Average the length at maturity (cm)
# Then change synonyms

maturity <- maturity(validated) %>%
  ga.clean.names() %>%
  # filter(type1 %in% "FL") %>% # Would be good to turn this on but it gets rid of a lot of species.
  filter(!is.na(lm)) %>%
  dplyr::group_by(species, speccode) %>%
  dplyr::summarise(fb.length.at.maturity.cm = mean(lm)) %>%
  tidyr::separate(species, into = c("genus", "species"), sep = " ") %>%
  dplyr::left_join(synonyms) %>%
  dplyr::mutate(genus = if_else(is.na(genus_correct), genus, genus_correct)) %>%
  dplyr::mutate(species = if_else(is.na(species_correct), species, species_correct)) %>%
  full_join(codes) %>%
  dplyr::select(-c(genus_correct, species_correct)) %>%
  dplyr::rename(fb.code = speccode) %>%
  full_join(ga.clean.names(caab))

test <- anti_join(maturity, caab)
test <- anti_join(caab, maturity)

# Get FB.Vulnerability, FB.Length_MAX and FB.LTypeMaxM, information from FishBase ----
info <- species(validated) %>% 
  ga.clean.names() %>%
  dplyr::rename(FB.Length_MAX = length, 
                FB.LTypeMaxM = ltypemaxm,
                FB.Vulnerability = vulnerability) %>% # Length metrics are in cm
  dplyr::select(species, speccode, fbname, FB.Length_MAX, FB.LTypeMaxM, FB.Vulnerability) %>%
  dplyr::rename(scientific.name = species, 
                common.name = fbname) 

# TODO should demerspelag be added?

names(info) %>%sort()

# FB.countries and FB.Status ----
country.dat <- country(validated) %>%
  ga.clean.names()

status <- country.dat %>%
  dplyr::filter(.$country %in% "Australia") %>%
  dplyr::select(species, speccode, status) %>%
  dplyr::rename(scientific.name = species, 
                FB.Status = status) 

countries <- country.dat %>%
  dplyr::group_by(species, speccode) %>%
  dplyr::summarise(FB.countries = toString(country)) %>%
  dplyr::rename(scientific.name = species) 

# a and b values ----
lwr <- length_weight(validated) %>%
  ga.clean.names() %>%
  dplyr::mutate(speccode = as.character(speccode))

# bay_lwrs <- data.frame() # turned off for now So i don't loose the data we have already downloaded
bay_lwrs <- read.csv("data/bayesian_length-weights.csv") %>% distinct()


# TODO get IUCN status

specieslist.iucn.summary<-iucn_summary(validatedlist[1:1295],server = getOption("IUCN_REDLIST_KEY",IUCN_REDLIST_KEY))

# Use iucn_status to extract IUCN status----
specieslist.iucn.status<-iucn_status(specieslist.iucn.summary)

# Convert to data frame and rename columns ----
specieslist.iucn.status<-as.data.frame(specieslist.iucn.status)%>%
  tibble::rownames_to_column()%>%
  rename(Abbreviation=specieslist.iucn.status)%>%
  rename(Genus_species=rowname)

# Bring in categories ----
categories<-data.frame(Abbreviation=c("EX","EW","CR","EN","VU","NT","LC","DD"),IUCN.Ranking=c("Extinct","	Extinct in the Wild","	Critically Endangered","	Endangered","	Vulnerable","Near Threatened","	Least Concern","Data Deficient"))

# Join to species list ----
species.iucn.complete<-left_join(specieslist.iucn.status,categories,by="Abbreviation")

# TODO check if synonyms need updating


# Combine all data ----
data.to.read.in <- maturity %>%
  dplyr::left_join(countries) %>%
  dplyr::left_join(status) %>%
  dplyr::left_join(info) %>%
  dplyr::select(caab_code, family, genus, species, fb.code, fb.length.at.maturity.cm, FB.Length_MAX, FB.LTypeMaxM, FB.Vulnerability, 
                FB.countries, FB.Status) %>%
  dplyr::rename(CAAB_code = caab_code, 
                Family = family,
                Genus = genus,
                Species = species,
                FB.code = fb.code,
                FB.length.at.maturity.cm = fb.length.at.maturity.cm)

saveRDS(data.to.read.in, "data/maturity.RDS")
