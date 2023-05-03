library(rfishbase)
library(tidyverse)
library(GlobalArchive) # TODO add how to install 
library(worrms)
library(rredlist)

# Get a list of all fish species in the world from fishbase ----
all.species <- load_taxa() %>% 
  ga.clean.names() %>%
  dplyr::mutate(scientific = species) %>%
  tidyr::separate(species, into = c("genus", "species"), sep = " ") %>%
  dplyr::select(speccode, superclass, class, order, family, genus, species, scientific) %>%
  dplyr::mutate(speccode = as.character(speccode)) 

names(all.species)

# Format basic info ----
info <- species(all.species$scientific) %>% 
  ga.clean.names() %>%
  dplyr::rename(length.max = length, length.max.type = ltypemaxm) %>% # Length metrics are in cm
  dplyr::select(species, speccode, fbname, length.max, length.max.type, importance) %>%
  dplyr::mutate(speccode = as.character(speccode)) %>%
  dplyr::rename(scientific = species, 
                common.name = fbname) %>%
  dplyr::mutate(speccode = as.character(speccode)) 

# Validate species names ----
validated <- validate_names(all.species$scientific)

# Distribution (Which FAO major fishing areas are species present in) -----
distribution <- distribution(validated) %>%
  ga.clean.names() %>%
  dplyr::select(species, speccode, status, fao) %>% 
  # dpl
  dplyr::filter(!status %in% c("stray", "extirpated", "unclear", "questionable", "NA", NA)) %>%
  dplyr::mutate(fao = str_replace_all(.$fao, c(", " = "_", 
                                               " - " = "_",
                                               "-" = "_",
                                               " " = "."))) %>%
  nest(fao = c(fao)) %>%
  dplyr::mutate(fao = paste(fao)) %>%
  dplyr::mutate(fao = str_replace_all(.$fao, c("[()]" = "", 
                                               "listfao = c" = "", 
                                               "listfao = " = "", 
                                               '"' = ""))) %>%
  tidyr::pivot_wider(names_from = status, values_from = fao) %>%
  dplyr::rename(fao.fishing.area.endemic = endemic, 
                fao.fishing.area.native = native, 
                fao.fishing.area.introduced = introduced,
                scientific = species)%>%
  dplyr::mutate(speccode = as.character(speccode))

# Get size of maturity -----
maturity <- maturity(validated) %>%
  ga.clean.names() %>%
  # filter(type1 %in% "FL") %>% # Would be good to turn this on but it gets rid of a lot of species.
  filter(!is.na(lm)) %>%
  dplyr::group_by(species, speccode) %>%
  dplyr::summarise(length.at.maturity.cm = mean(lm)) %>%
  tidyr::separate(species, into = c("genus", "species"), sep = " ") %>%
  dplyr::mutate(speccode = as.character(speccode)) 
  

# Get length-weight relationship
# TODO come back and make this for only one species
# I don't think I can get bayesian for every species

lwr <- length_weight(validated) %>%
  ga.clean.names() %>%
  dplyr::mutate(speccode = as.character(speccode))

# Get list of synyonms
synonyms <- synonyms(all.species$scientific) %>% 
  ga.clean.names() %>%
  dplyr::filter(!status %in% "accepted name") %>% # Don't care about accepted names
  dplyr::filter(!synonym == species) # this is silly, but some lines had the same fish name as correct as a synonym. So had to remove

unique(synonyms$status)

# Get worms code ----
# The wm_records_names function can only handle 170 names at a time (have chopped in 150 chunks as it is an easier number)
# 150 names takes around ~ 6 seconds to run

# Create a list of all species
species.to.use <- unique(all.species$scientific)

# Break into chunks of 150
species.lists <- split(species.to.use, ceiling(seq_along(species.to.use)/150)) # 234 lists

worms <- data.frame()

# Time to run all = 11:35:07 - (i think it should take 25 minutes)
for(id in seq(1:length(species.lists))){
  
  dat <- species.lists[id][[1]] #%>% glimpse()
  temp <- wm_records_names(c(dat), marine_only = FALSE)
  
  temp.worms <- do.call("rbind", temp)
  
  worms <- bind_rows(worms, temp.worms)
  
}

worms.final <- worms %>%
  distinct() %>%
  ga.clean.names() %>%
  dplyr::select(aphiaid, scientificname, status, kingdom, phylum, class, order, family, genus, ismarine, isbrackish, isfreshwater) %>%
  dplyr::rename(scientific = scientificname)

# Get synonyms from worms using AphiaID ----
Sys.time()
syn.data <- wm_synonyms_(c(worms.final$aphiaid))
Sys.time()

syn.tidy <- syn.data %>%
  distinct() %>%
  ga.clean.names() %>%
  dplyr::select(scientificname, unacceptreason, valid_aphiaid, valid_name, match_type) %>%
  dplyr::filter(!scientificname == valid_name) %>% # a check to make sure the valid name isn't the same as the synonym
  dplyr::rename(scientific = valid_name, 
                aphiaid = valid_aphiaid,
                synonym = scientificname) # TODO Check if I need match_type with more data


# TODO need to check that the list of unaccepted names isn't a existing synonym for something else
# Could somehow flag this in CheckEM if there are some e.g. Pagrus auratus is an unaccepted for Sparus aurata Linnaeus, 1758
# Another example is Alectis indica (Rüppell, 1830) (accepted name), but Alectis indica is also a synonym for Alectis ciliaris (Bloch, 1787).
species.also.a.synonym <- syn.tidy %>%
  dplyr::rename(correct_name = scientific,
                scientific = synonym) %>%
  dplyr::select(correct_name, scientific) %>%
  dplyr::semi_join(info) %>%
  dplyr::rename(valid.name.too.but.also.a.synonym.for.correct.name = scientific)

# Get IUCN ranking ----
# This requires a IUCN API
# To register for a IUCN API RUN 'rl_use_iucn()' once you have applied and received your API TOKEN then run 'usethis::edit_r_environ()' and add "IUCN_REDLIST_KEY = XXXX" to the r environ
t <- rl_search("Pagrus auratus")


# get all results
out <- rl_sp(all = TRUE)
length(out)
vapply(out, "[[", 1, "count")
all_df <- do.call(rbind, lapply(out, "[[", "result"))
head(all_df)
NROW(all_df)

rl_sp_category('VU')
rl_sp_category('LRlc')
rl_sp_category('EN')
rl_sp_category('EX')
rl_sp_category('EX', parse = FALSE)
rl_sp_category_('EX')

# Final life history sheet should have ----
# - Y Scientific name
# - Y Class, Order, Family, Genus, Species
# - Y Common name
# - Y Regions were present
# - Length-weight 
# - Vulnerability
# - EPBC threat status
# - Y Fishing
# - Y Size at maturity

# And a Synonym list

fblh <- all.species %>%
  dplyr::select(-c(superclass, class, order)) %>%
  full_join(info) %>%
  full_join(distribution) %>%
  full_join(maturity) %>%
  full_join((worms.final))#%>%
  #full_join(lwr)

test <- fblh %>%
  filter(is.na(aphiaid))


