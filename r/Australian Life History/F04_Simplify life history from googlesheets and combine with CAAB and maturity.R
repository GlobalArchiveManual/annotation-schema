library(tidyverse)
library(googlesheets4)
library(openxlsx)

# designate project-specific cache
options(gargle_oauth_cache = ".secrets")
# check the value of the option, if you like
gargle::gargle_oauth_cache()
googlesheets4::gs4_auth()
2

# Read in sheet from googledrive ----
url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit#gid=825736197"
lh <- read_sheet(url)

saveRDS(lh, "data/life.history.RDS")

synonyms <- read_sheet(url, sheet = 2) %>% distinct()
saveRDS(synonyms, "data/synonyms.RDS")

fam.common.names <- read_sheet(url, sheet = 3)
lumped.common.names <- read_sheet(url, sheet = 4)

# Get columns to keep from original life history
keep <- lh %>%
  dplyr::select(Family, Genus, Species, 
                
                RLS.trophic.level,
                RLS.trophic.breadth,
                RLS.trophic.group,
                RLS.water.column,
                RLS.substrate.type,
                RLS.complexity,
                RLS.night.day,
                RLS.gregariousness,
                RLS.thermal.niche,
                RLS.vulnerability,
                
                Fishing.mortality,
                Fishing.type,
                Fishing.intensity,
                
                MinLegal.NT,
                MaxLegal.NT,
                Bag.NT,
                MinLegal.WA,
                MaxLegal.WA,
                Bag.WA,
                MinLegal.QLD,
                MaxLegal.QLD,
                Bag.QLD,
                MinLegal.NSW,
                MaxLegal.NSW,
                Bag.NSW,
                MinLegal.Vic,
                MaxLegal.Vic,
                Bag.Vic,
                MinLegal.SA,
                MaxLegal.SA,
                Bag.SA,
                MinLegal.Tas,
                MaxLegal.Tas,
                Bag.Tas,
                
                EPBC.Threat.Status)

# Other data to read in from script 1 & 
caab.regions <- readRDS("data/caab.with.regions.RDS") %>%
  dplyr::filter(!is.na(caab_code)) %>%
  dplyr::mutate(caab_code = as.character(caab_code))

fishbase <- readRDS("data/fishbase.and.iucn.RDS") %>%
  dplyr::filter(!(caab.scientific %in% "Epigonus macrops" & speccode %in% "14365"))%>%
  dplyr::mutate(caab_code = as.character(caab_code))

caab.scraped <- readRDS("data/caab_scraped_codes_common-names.RDS") %>%
  dplyr::mutate(family = str_replace_all(.$family, "[^[:alnum:]]", "")) %>%
  dplyr::mutate(species = str_replace_all(.$species, "cffilamentosa", "filamentosa")) %>%
  
  dplyr::mutate(scraped.name = paste(family, genus, species)) %>%
  dplyr::rename(scraped.family = family,
                scraped.genus = genus, 
                scraped.species = species,
                caab_code = caab.code) %>%
  dplyr::select(caab_code, scraped.name, scraped.family, scraped.genus, scraped.species, common.name) 

caab.combined <- full_join(caab.regions, caab.scraped) %>%
  dplyr::mutate(name = paste(family, genus, species)) %>%
  # dplyr::filter(!name %in% scraped.name) %>% # for testing
  # dplyr::filter(!is.na(scraped.name)) %>%# for testing 
  dplyr::mutate(scraped.family = if_else(is.na(scraped.family), family, scraped.family)) %>%
  dplyr::mutate(scraped.genus = if_else(is.na(scraped.genus), genus, scraped.genus)) %>%
  dplyr::mutate(scraped.species = if_else(is.na(scraped.species), species, scraped.species)) %>%
  
  dplyr::mutate(family = if_else(name %in% c(scraped.name), family, scraped.family)) %>%
  dplyr::mutate(genus = if_else(name %in% scraped.name, genus, scraped.genus)) %>%
  dplyr::mutate(species = if_else(name %in% scraped.name, species, scraped.species)) %>%
  dplyr::select(-c(scraped.name, scraped.family, scraped.genus, scraped.species, name)) %>%
  dplyr::mutate(common.name = str_replace_all(.$common.name, "\\[|\\]", "")) %>%
  dplyr::filter(!is.na(species))

# Make it simpler
simple.lh <- caab.combined %>%
  dplyr::left_join(fishbase) %>%
  dplyr::rename(Family = family,
                Genus = genus,
                Species = species,
                CAAB = caab_code,
                Marine.region = marine.region,
                Order = order,
                Class = class,
                Australian.common.name = common.name,
                FB.code = speccode,
                FB.length.at.maturity.cm = fb.length.at.maturity.cm,
                IUCN.Ranking = IUCN.ranking,
                Subfamily = subfamily) %>%
  
  dplyr::left_join(keep) %>%
  
  dplyr::mutate(Australian.source = "CAAB",
                Global.source = "FishBase",
                Local.source = "Harvey et al 2020") %>%
  
  dplyr::mutate(Scientific = paste(Genus, Species)) %>%

  dplyr::mutate(Global.Region = "Australia") %>%
  
  dplyr::select(c(Australian.source,
                CAAB,
                Class,
                Order,
                Family,
                Genus,
                Species,
                Scientific,
                Australian.common.name, # TODO need to add in 1st script or use the scraping
                Marine.region,
                
                Global.source,
                FB.code,
                FB.length.at.maturity.cm,
                # TODO need to add in 1st script
                Subfamily,
                Global.Region,
                Length.measure,
                a,
                b,
                aLL,
                bLL,
                Source_Level,
                FB.Vulnerability,
                FB.countries,
                FB.Status,
                FB.Length_MAX,
                FB.LTypeMaxM,
                RLS.trophic.group,
                RLS.water.column,
                RLS.substrate.type,
                RLS.thermal.niche,
                
                Local.source,
                EPBC.Threat.Status,
                IUCN.Ranking,
                Fishing.mortality,
                Fishing.type,
                MinLegal.NT,
                MaxLegal.NT,
                MinLegal.WA,
                MaxLegal.WA,
                MinLegal.QLD,
                MaxLegal.QLD,
                MinLegal.NSW,
                MaxLegal.NSW,
                MinLegal.Vic,
                MaxLegal.Vic,
                MinLegal.SA,
                MaxLegal.SA,
                MinLegal.Tas,
                MaxLegal.Tas
                )) %>%
  
  dplyr::rename('Scientific name' = Scientific)
  
  
  
test <- simple.lh %>% 
  dplyr::group_by(CAAB) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1)

test <- simple.lh %>%
  filter(is.na(CAAB))
  

saveRDS(simple.lh, "data/simple.life.history.RDS")

# Define styles for excel
aus = createStyle(fontColour = "black", bgFill = "#D9EAD3")
global = createStyle(fontColour = "black", bgFill = "#FCE5CD")
local = createStyle(fontColour = "black", bgFill = "#FFF2CC")

# Create new workbook
wb = createWorkbook(title = "fish.life.history")

# Add Sheet 1 - Information -----
addWorksheet(wb, "information")

info <- data.frame(Source = c("Australian.source", "Global.source", "Local.source"),
                 From = c("CAAB", "FishBase", "Harvey et al. 2020"),
                 Description = c("Species list", "Life history information", "Fisheries and TEPS information"),
                 Use = c("Region and ID QAQC", "life history and body size QAQC", "Metric calculations"),
                 Comments = c("","includes L/W formula", "Need to add in maturity etc metrics"))

# Add the data 
header = createStyle(textDecoration = "Bold")
writeData(wb, "information", info, headerStyle = header)

# Set the width of cells to auto
# setColWidths(wb, "information", cols = 1:ncol(info), widths = "auto")

# Colour the cells
conditionalFormatting(wb, "information", 
                      cols = 1, 
                      rows = 1:(nrow(info)+1), 
                      rule = "Australian.source", 
                      type = "contains",
                      style = aus)

conditionalFormatting(wb, "information", 
                      cols = 1, 
                      rows = 1:(nrow(info)+1), 
                      rule = "Global.source", 
                      type = "contains",
                      style = global)

conditionalFormatting(wb, "information", 
                      cols = 1, 
                      rows = 1:(nrow(info)+1), 
                      rule = "Local.source", 
                      type = "contains",
                      style = local)

# Add Sheet 2 - fish.life.history ----
addWorksheet(wb, "fish.life.history")

# Add the data
writeData(wb, "fish.life.history", simple.lh, headerStyle = header)

# Set the width of cells to auto
# setColWidths(wb, "fish.life.history", cols = 1:ncol(simple.lh), widths = "auto")

# Colour the cells
conditionalFormatting(wb, "fish.life.history", 
                      cols = 1:10, 
                      rows = 1:(nrow(simple.lh)+1), 
                      rule= "!=0", 
                      style = aus)

conditionalFormatting(wb, "fish.life.history", 
                      cols = 1:10, 
                      rows = 1:(nrow(simple.lh)+1), 
                      rule= "=0", 
                      style = aus)


conditionalFormatting(wb, "fish.life.history", 
                      cols = 11:30, 
                      rows = 1:(nrow(simple.lh)+1), 
                      rule = "!=0", 
                      style = global)


conditionalFormatting(wb, "fish.life.history", 
                      cols = 11:30, 
                      rows = 1:(nrow(simple.lh)+1), 
                      rule = "=0", 
                      style = global)

conditionalFormatting(wb, "fish.life.history", 
                      cols = 31:49, 
                      rows = 1:(nrow(simple.lh)+1), 
                      rule = "!=0", 
                      style = local)


conditionalFormatting(wb, "fish.life.history", 
                      cols = 31:49, 
                      rows = 1:(nrow(simple.lh)+1), 
                      rule = "=0", 
                      style = local)

# Add Sheet 3 - Synonyms ----
addWorksheet(wb, "synonyms")

# Add the data 
writeData(wb, "synonyms", synonyms, headerStyle = header)

# Set the width of cells to auto
# setColWidths(wb, "synonyms", cols = 1:ncol(synonyms), widths = "auto")

# Add Sheet 4 - Family common names ----
addWorksheet(wb, "family.common.names")

# Add the data 
writeData(wb, "family.common.names", fam.common.names, headerStyle = header)

# Set the width of cells to auto
# setColWidths(wb, "family.common.names", cols = 1:ncol(fam.common.names), widths = "auto")

# Add Sheet 5 - Lumped common names ----
addWorksheet(wb, "lumped.common.names")

# Add the data 
writeData(wb, "lumped.common.names", lumped.common.names, headerStyle = header)

# Set the width of cells to auto
# setColWidths(wb, "lumped.common.names", cols = 1:ncol(lumped.common.names), widths = "auto")

# Show the workbook
openXL(wb)

time <- str_remove_all(Sys.time(), "[^[:alnum:] ]") # remove spaces for Nik
glimpse(time)

date <- str_sub(time, 1, 8)
hour <- str_sub(time, 10, 15)

# Save the workbook
saveWorkbook(wb, paste("output/fish/fish.life.history", date, hour, "xlsx", sep = "."), overwrite = TRUE)
