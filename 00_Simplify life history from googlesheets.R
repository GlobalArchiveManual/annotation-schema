library(tidyverse)
library(googlesheets4)
library(openxlsx)

# Read in sheet from googledrive ----
url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit#gid=825736197"

lh <- read_sheet(url)
1

names(lh)

# Make it simpler
simple.lh <- lh %>%
  dplyr::mutate(Australian.source = "CAAB",
                Gloabal.source = "FishBase",
                Local.source = "Harvey et al 2020") %>%
  
  dplyr::filter(!is.na(CAAB)) %>%
  
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

# Define styles for excel
aus = createStyle(fontColour = "black", bgFill = "#D9EAD3")
global = createStyle(fontColour = "black", bgFill = "#FCE5CD")
local = createStyle(fontColour = "black", bgFill = "#FFF2CC")

# Create new workbook
wb = createWorkbook(title = "fish.life.history")

# Add Sheet 1
addWorksheet(wb, "fish.life.history")

# Add the data
header = createStyle(textDecoration = "Bold")
writeData(wb, "fish.life.history", simple.lh, headerStyle = header)



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
                      cols = 11:28, 
                      rows = 1:(nrow(simple.lh)+1), 
                      rule = "!=0", 
                      style = global)


conditionalFormatting(wb, "fish.life.history", 
                      cols = 11:28, 
                      rows = 1:(nrow(simple.lh)+1), 
                      rule = "=0", 
                      style = global)

conditionalFormatting(wb, "fish.life.history", 
                      cols = 29:47, 
                      rows = 1:(nrow(simple.lh)+1), 
                      rule = "!=0", 
                      style = local)


conditionalFormatting(wb, "fish.life.history", 
                      cols = 29:47, 
                      rows = 1:(nrow(simple.lh)+1), 
                      rule = "=0", 
                      style = local)



# Show the workbook
openXL(wb)

# Save the workbook
saveWorkbook(wb, "fish.life.history.xlsx", overwrite = TRUE)


