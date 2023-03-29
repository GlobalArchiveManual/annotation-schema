library(tidyverse)
library(rfishbase)
library(openssl)
library(GlobalArchive)

# Read in simple life history and synonyms ----
simple.lh <- readRDS("data/simple.life.history.RDS") %>%
  ga.clean.names()

synonyms <- readRDS("data/synonyms.RDS") %>%
  ga.clean.names() %>%
  distinct(genus, species, genus_correct, species_correct)

# Validate scientific names to use in fishbase package ----
validated <- validate_names(simple.lh$scientific.name)

codes <- common_names(validated) %>%
  ga.clean.names() %>%
  distinct(species, speccode) %>%
  tidyr::separate(species, into = c("genus", "species"), sep = " ") %>%
  filter(!is.na(species)) %>%
  dplyr::left_join(synonyms) %>%
  dplyr::mutate(genus = if_else(is.na(genus_correct), genus, genus_correct)) %>%
  dplyr::mutate(species = if_else(is.na(species_correct), species, species_correct)) %>%
  dplyr::select(-c(genus_correct, species_correct))

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
  full_join(ga.clean.names(simple.lh))

test <- anti_join(maturity, simple.lh)
         
data.to.read.in <- maturity %>%
  select(family, genus, species, fb.code, fb.length.at.maturity.cm) %>%
  dplyr::rename(Family = family,
                Genus = genus,
                Species = species,
                FB.code = fb.code,
                FB.length.at.maturity.cm = fb.length.at.maturity.cm)

saveRDS(data.to.read.in, "data/maturity.RDS")
