# PROJECT: brining
# PURPOSE: Munge and Analysis of UNAIDS data
# AUTHOR:  Tim Esssam | SI
# REF ID:  d35cabe4
# LICENSE: MIT
# DATE:   2023-08-03
# NOTES:   

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(glue)
    library(gt)
    library(readxl)
    
    
  # SI specific paths/functions  
    load_secrets()

  
  # REF ID for plots
    ref_id <- "d35cabe4"
    
  # Functions  
  

# LOAD DATA ============================================================================  

  unaids_list <- list.files("Data_public/", full.names = T)
  unaids_indics <- list.files("Data_public/")  %>% str_remove_all(.x, "Elimination of vertical transmission_")
  
  new_hiv <- "Data/New infant HIV infections Male+Female.csv"

# MUNGE ============================================================================
  
  df <- purrr::map_dfr(unaids_list, ~read.csv(.x) %>% 
                   mutate(indic = str_remove_all(.x, "Data_public/Elimination of vertical transmission_") %>% 
                            str_remove_all(., ".csv"))) %>% 
    mutate(X2022 = str_remove_all(X2022, "<|>|\\.| ") %>% 
             as.numeric(),
           year = 2022) %>% 
    filter(Country != "Global") %>% 
    rename(value = X2022)
  
  df_wide <- 
    df %>% select(Country, value, indic, year) %>% 
    pivot_wider(names_from = indic, 
                values_from = value)
  
  
  new_hiv_inf <- read_csv(new_hiv) %>% 
    rename(Country = `...4`) 
  
  df_wide_all <- df_wide %>% left_join(new_hiv_inf) %>% 
    select(ISO2, ISO3, Country, everything())
  
  
  write_csv(df_wide_all, "Dataout/2022_UNAIDS_Vertical_transmission_indic_new_infant_hiv.csv", na = "")
  write_csv(df, "Dataout/2022_UNAIDS_vt_indic_long.csv", na = "")    
  
  write_csv(pepfar_country_list, "Dataout/PEPFAR_country_list.csv")
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

