library(tidyverse)
library(googlesheets4)
library(openxlsx)

# Read in sheet from googledrive ----
url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit#gid=825736197"

lh <- read_sheet(url)
1

saveRDS(lh, "data/life.history.RDS")

synonyms <- read_sheet(url, sheet = 2)
saveRDS(synonyms, "data/synonyms.RDS")

fam.common.names <- read_sheet(url, sheet = 3)
lumped.common.names <- read_sheet(url, sheet = 4)

names(lh)

# Read in maturity from other script
maturity <- readRDS("data/maturity.RDS")

# Make it simpler
simple.lh <- lh %>%
  dplyr::mutate(Australian.source = "CAAB",
                Gloabal.source = "FishBase",
                Local.source = "Harvey et al 2020") %>%
  
  dplyr::filter(!is.na(CAAB)) %>%
  
  full_join(maturity) %>%
  
  dplyr::select(c(Australian.source,
                CAAB,
                Class,
                Order,
                Family,
                Genus,
                Species,
                Scientific,
                Australian.common.name,
                Marine.region,
                
                Gloabal.source,
                FB.code,
                FB.length.at.maturity.cm,
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
writeData(wb, "information", info, headerStyle = header)

# Set the width of cells to auto
setColWidths(wb, "information", cols = 1:ncol(info), widths = "auto")

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
header = createStyle(textDecoration = "Bold")
writeData(wb, "fish.life.history", simple.lh, headerStyle = header)

# Set the width of cells to auto
setColWidths(wb, "fish.life.history", cols = 1:ncol(simple.lh), widths = "auto")

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
                      cols = 31:47, 
                      rows = 1:(nrow(simple.lh)+1), 
                      rule = "!=0", 
                      style = local)


conditionalFormatting(wb, "fish.life.history", 
                      cols = 31:47, 
                      rows = 1:(nrow(simple.lh)+1), 
                      rule = "=0", 
                      style = local)

# Add Sheet 3 - Synonyms ----
addWorksheet(wb, "synonyms")

# Add the data 
writeData(wb, "synonyms", synonyms, headerStyle = header)

# Set the width of cells to auto
setColWidths(wb, "synonyms", cols = 1:ncol(synonyms), widths = "auto")

# Add Sheet 4 - Family common names ----
addWorksheet(wb, "family.common.names")

# Add the data 
writeData(wb, "family.common.names", fam.common.names, headerStyle = header)

# Set the width of cells to auto
setColWidths(wb, "family.common.names", cols = 1:ncol(fam.common.names), widths = "auto")

# Add Sheet 5 - Lumped common names ----
addWorksheet(wb, "lumped.common.names")

# Add the data 
writeData(wb, "lumped.common.names", lumped.common.names, headerStyle = header)

# Set the width of cells to auto
setColWidths(wb, "lumped.common.names", cols = 1:ncol(lumped.common.names), widths = "auto")

# Show the workbook
openXL(wb)

# Save the workbook
saveWorkbook(wb, "fish.life.history.xlsx", overwrite = TRUE)
