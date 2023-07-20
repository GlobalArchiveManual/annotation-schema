library(tidyverse)

extras <- data.frame(FAMILY = c("Unknown", "Larval", "SUS", "Baitfish"),
                     GENUS = c("Unknown", "Larval", "SUS", "Baitfish"),
                     SPECIES = c("Unknown", "Larval", "SUS", "Baitfish"),
                     'CAAB CODE' = c("0", "1", "2", "3")) %>%
  dplyr::rename("CAAB CODE" = CAAB.CODE)

lh <- readRDS("data/simple.life.history.RDS") 
  
all <- lh %>%  
  dplyr::rename("CAAB CODE" = CAAB, 
                FAMILY = Family,
                GENUS = Genus, 
                SPECIES = Species) %>%
  dplyr::select(FAMILY, GENUS, SPECIES, "CAAB CODE") %>%
  dplyr::filter(!SPECIES %in% "spp") %>%
  dplyr::bind_rows(., extras)

date <- str_sub(str_remove_all(Sys.time(), "[^[:alnum:] ]"), 1, 8)

write_tsv(all, paste0("output/fish/CAAB codes for EM/CAAB ", date, ".txt"))