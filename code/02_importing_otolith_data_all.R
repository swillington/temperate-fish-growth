
library(tidyverse)
library(here)

rm(list = ls()) # clear memory

project_dir <- here::here()

# amys samples incs ----------------------------------------------------------------------------------------------

file_list <- list.files("0_data/01_raw_data/sarah otolith incs", full.names = TRUE)
read_name <- function(i, ...)read_csv(i, ...) |> mutate(id = i)

oto_incs <- map(file_list, read_name) |> 
  bind_rows() |> 
  mutate(id = str_remove(id, "0_data/01_raw_data/sarah otolith incs/"), 
         id = str_remove(id, "_raw_incs.csv"),
         species_id = str_extract(id, "^[A-Za-z]*")) |> 
  select(-c(id, X, Y, Quality, Core))

oto_incs <- oto_incs |> 
  filter(species_id!="PLA" ) |> 
  mutate(Sample = str_remove(Sample, "_.*$")) |> 
  rename(FishID = Sample, 
         inccount = Age, 
         cohort = Cohort, 
         age = I,
         fishyear = Year, 
         increment = Increment) |> 
  filter(age!="core"& age!="edge" & age!="1") |> 
  mutate(age = as.numeric(age))


info <- read_csv("0_data/01_raw_data/otolith_incs_species_info.csv")

oto_incs <- oto_incs |> left_join(info, by = "FishID") |> 
  mutate( site = case_when(region == "South NSW" ~ "narooma", 
                           region == "Mid NSW" ~ "sydney", 
                           region == "North NSW" ~ "NSI", 
                           region == "North Tassie" ~ "EC", 
                           region == "Mid Tassie" ~ "EC"))

#adjust fishyear and cohort

#by season
oto_incs <- oto_incs |> 
  mutate(fishyear = case_when(season == "Spring" ~ fishyear-1, 
                              season == "Autumn" ~ fishyear-2), 
         cohort = case_when(season == "Spring" ~ cohort, 
                              season == "Autumn" ~ cohort-1))

#adjust for fish with and inc on the edge
oto_incs <- oto_incs |> 
  mutate(fishyear = case_when(
    FishID %in% c("AAU05", "SAE01", "SAE03", "SAE04", "SAE05", "SAE06", "SAE07", "SAE08", "SAE09", "TLU01", "TLU02", "TLU05", "TLU06", "TLU08", "TLU09", "TLU010" ) ~ fishyear-1,
    TRUE ~ fishyear), 
    cohort = case_when(
      FishID %in% c("AAU05", "SAE01", "SAE03", "SAE04", "SAE05", "SAE06", "SAE07", "SAE08", "SAE09", "TLU01", "TLU02", "TLU05", "TLU06", "TLU08", "TLU09", "TLU010") ~ cohort-1,
      TRUE ~ cohort))

# choose length
oto_incs <- oto_incs |> 
  mutate(length = case_when(
    species_id %in% c("APU", "CFU", "SLI") ~ FL_mm, 
    species_id %in% c("GEL", "GTR", "SAE") ~ TL_mm, 
    TRUE ~ SL_mm
  ))



#write_csv(oto_incs, "0_data/02_cleaned_data/oto_incs.csv")

# sand flathead --------------------------------------------------------------------------------------------------
file_list <- list.files("0_data/01_raw_data/SFH data", full.names = TRUE)
read_name <- function(i, ...)read_csv(i, ...) |> mutate(id = i)

sfh <- map(file_list, read_name, col_types = cols(Sample = col_character())) |> 
  bind_rows() |> 
  mutate(id = str_remove(id, "0_data/01_raw_data/SFH data/"), 
         id = str_remove(id, "_incrememt.csv"), 
         site = str_extract(id, "^[A-Za-z]*"), 
         yearcap = as.numeric(str_extract(id, "(\\d+)"))) |> 
  select(-c(id, X, Y, Quality, Core))

sfh <- sfh |> 
  rename(FishID = Sample, 
         inccount = Age, 
         cohort = Cohort, 
         age = I,
         fishyear = Year, 
         increment = Increment) |> 
  filter(age!="core"& age!="edge"& age!=1) |> 
  mutate(species = "Platycephalus bassensis", 
         fishyear = fishyear-2,
         cohort = cohort-1,
         age = as.numeric(age), 
         FishID = as.character(FishID), 
         site = "FI", 
         species_id = "PBA" )

sfh <- sfh |> filter(FishID!="217006" 
                       & FishID!="217007"
                     &FishID!="217006a"
                     &FishID!="216959")
#write_csv(sfh, "0_data/02_cleaned_data/sfh.csv") 

# Purple wrasse --------------------------------------------------------------------------------------------------
wrasse_path <- file.path(project_dir, "0_data", "01_raw_data", "final wrasse increment.csv")
wrasse <- read_csv(wrasse_path,
                   col_select = -c(siteyear, marginwidth, Jmedge)) %>% 
  mutate(species = "Notolabrus fucicola")

wrasse <- wrasse %>% 
  rename(yearcap = capyear, 
         monthcap = capmonth, 
         cohort = yearclass) %>% 
  mutate(FishID = as.character(FishID))%>% 
  filter(age>1) |> 
  mutate(increment = increment*1000, 
    species_id = "NFU")

wrasse <- wrasse |> 
  mutate( site = case_when(site == "pb" ~ "EC", 
                           site == "ehn" ~ "SEC", 
                           TRUE ~ site))

#write_csv(wrasse, "0_data/02_cleaned_data/pw_wrasse.csv")

# Banded morwong ----------------------------------------------------------------------------------------------------------
morwong_path <- file.path(project_dir, "0_data", "01_raw_data", "banded morwong raw long.csv")
morwong <- read_csv(morwong_path) %>% 
  mutate(species = "Cheilodactylus spectabilis") %>% 
  filter(site!="NZ") |> 
  filter(age!=1) |> 
  select(-c(day, edge, radius))

morwong <- morwong %>% 
  rename(monthcap = month, 
         cohort = yearclass)%>% 
  mutate(FishID = as.character(FishID), 
         sex = as.character(sex), 
         species_id = "CSP", 
         inccount = adjage)%>% 
  filter(FishID!= "723817" & FishID!="103" & FishID!="481" & FishID!="50121057" & FishID!="720542" ) |> 
  filter(yearcap>1996)
#str(morwong)

#write_csv(morwong, "0_data/02_cleaned_data/bmw.csv")
  
  
  
  
  
  
  