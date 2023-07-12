library(tidyverse)
library(rfishbase)
library(openssl)
library(GlobalArchive)
library(taxize) # For IUCN
library(rredlist)

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

# fishbase.to.caab <- bind_cols(codes %>% dplyr::rename(fb.genus = genus, fb.species = species), caab) 
# 
# mismatches <- fishbase.to.caab %>%
#   dplyr::filter(!fb.genus %in% genus | !fb.species %in% species)

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
  # TODO investigate if i need to turn this back on - i think i might need to use the worms synyonms to find which "incorrect" names occur in caab
  # dplyr::left_join(synonyms) %>% 
  # dplyr::mutate(genus = if_else(is.na(genus_correct), genus, genus_correct)) %>%
  # dplyr::mutate(species = if_else(is.na(species_correct), species, species_correct)) %>%
  full_join(codes) %>%
  # dplyr::select(-c(genus_correct, species_correct)) %>%
  dplyr::rename(fb.code = speccode) %>%
  full_join(ga.clean.names(caab))

# TODO need some way to add these back in
missing.fishbase <- maturity %>%
  filter(is.na(fb.code)) # 197 species

missing.caab <- maturity %>%
  filter(is.na(caab_code)) # 146 species

# NOTE remember to remove all caab from the dataframe before changing the synonyms

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

names(info) %>% sort()

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

bay_lwrs <- read.csv("data/bayesian_length-weights.csv") %>% distinct()


# Get IUCN status - takes roughly 40 minutes
iucn <- data.frame()

for (species in validated) {
  message(paste("accessing IUCN data for:", species, "(species", nrow(iucn), "of", length(validated),")"))
  
  if(!species %in% c("NA", NA)){
  
  dat <- rl_search(species)
  temp.dat <- dat[[2]]
  
  if(!is.null(nrow(temp.dat))){
  iucn <- bind_rows(iucn, temp.dat)
  }
  }
}

# t <- rl_search("Pagrus auratus")
# iucn.dat <- t[[2]]

# TODO check if synonyms need updating

# Combine all data ----
all.fishbase.iucn <- maturity %>%
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

saveRDS(all.fishbase.iucn, "data/fishbase.and.iucn.RDS")
