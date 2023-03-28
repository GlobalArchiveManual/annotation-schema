library(tidyverse)
library(GlobalArchive)
library(stringr)
library(googlesheets4)

# Read in extra caab codes ----
extra <- read.csv("data/extra-caab-codes.csv")

# Have not included codium or calerpa because they can differ in levels with the same CAAB code

# Read in catami codes
catami <- read.csv("data/catami-caab-codes_1.4.csv") %>%
  ga.clean.names() %>%
  dplyr::rename(CAAB_code = species_code,
                Parent_CAAB = catami_parent_id) %>%
  dplyr::select(CAAB_code, Parent_CAAB, catami_display_name) %>% 
  tidyr::separate(catami_display_name, into = c("level_2", "level_3", "level_4", "level_5", "level_6", "level_7", "level_8"), sep = ": ") %>%
  dplyr::mutate(level_1 = if_else(level_2 %in% c("Substrate", "Relief"), "Physical", "Biota")) %>%
  bind_rows(extra) %>%
  dplyr::mutate(qualifiers = NA) %>%
  dplyr::mutate(qualifiers = if_else(level_2 %in% c("Macroalgae") & (!level_3 == "Encrusting"), "Drift/No epiphytes/Epiphytes algae/Epiphytes other", as.character(qualifiers))) %>%
  dplyr::mutate(qualifiers = if_else(level_2 %in% c("Seagrasses"), "No epiphytes/Epiphytes algae/Epiphytes other", as.character(qualifiers))) %>%
  dplyr::mutate(qualifiers = if_else(level_3 %in% c("Corals"), "Alive/Recruit/Bleached/Dead/Recently dead", as.character(qualifiers))) %>%
  dplyr::select(CAAB_code, Parent_CAAB, level_1, everything()) %>% 
  dplyr::filter(!level_2 %in% c("Biota", "Physical")) %>%
  dplyr::arrange(level_1, level_2, level_3, level_4) %>%
  glimpse()

unique(catami$level_3)

# Write to google sheet
url <- "https://docs.google.com/spreadsheets/d/1tcvHnD8LtPmjro8eOMdOS6k6HKdND86mMAOk6AS_gfc/edit#gid=1972721984"

write_sheet(catami, ss = url, sheet = "code crosswalk")

time <- str_remove_all(Sys.time(), "[^[:alnum:] ]") # remove spaces for Nik
glimpse(time)

date <- str_sub(time, 1, 8)
hour <- str_sub(time, 10, 15)

# Save the csv
write.csv(catami, paste("output/habitat/benthic.annotation.schema.forward.facing", date, hour, "csv", sep = "."), row.names = FALSE, na = "")
