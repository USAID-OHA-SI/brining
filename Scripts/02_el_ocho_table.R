# PROJECT: brining
# PURPOSE: Table request of things
# AUTHOR:  Tim Esssam | SI
# REF ID:  3e4dcbf7
# LICENSE: MIT
# DATE:   2023-08-08
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
     library(gtExtras)
     library(googlesheets4)

    
  # SI specific paths/functions  
    load_secrets()
   
   gs_id <- "1YRNVFW6h2akhYXfgeTK_tuMHIeeRk0OYeWT9ppaZVdw"    
   

  # REF ID for plots
    ref_id <- "3e4dcbf7"
    
  # Functions  
    drkn_clmn_hdr <- function(.data){
      .data %>%  
        tab_style(
          style = list(
            cell_text(color = grey90k)
          ),
          locations = cells_column_labels(
          )
        )
    }
  

# LOAD DATA ============================================================================  

  el_ocho <- pepfar_country_list %>% 
      filter(pepfar_accel == TRUE)  %>% 
      select(country, country_iso, pepfar_accel) %>% 
      mutate(across())
    
  df_enhcd <- read_sheet(gs_id, sheet = "Enhanced_list", col_types = "c")  
    
  df <- read_sheet(gs_id) %>% select(country_iso, country, primary_impact, unicef_ga)
    
  names(df)  
  names(df_enhcd)
  
# MUNGE ============================================================================
  
  df_combo <- df_enhcd %>% 
    left_join(df) %>% 
    left_join(el_ocho) %>% 
    select(-country_iso) %>% 
    mutate(across(primary_impact:pepfar_accel, ~ifelse(.x == TRUE, "YES", NA_character_))) %>% 
    arrange(desc(`USAID’s share of the PEPFAR PMTCT_ART target for 2023`)) %>% 
    relocate(9, .after = 1) %>% 
    relocate(12, .after = 2) %>% 
    relocate(11, .after = 3) %>% 
    relocate(12, .after = 6) %>% 
    relocate(11, .after = 8)
  
# VIZ ============================================================================

  # Write a version to google drive for folks to play with
  write_sheet(df_combo, ss = gs_id, sheet = "custom_table_revised")
  
  
  nms_ <- names(df_combo)
  nms_ <- nms_[3:12]
  
  df_combo %>% 
    gt()  %>% 
    tab_style(
      style = list(
        cell_fill(color = scooter_med),
        cell_text(color = scooter_med)),
      locations = map( nms_ ,
                       \(x){cells_body(
                         columns = x,
                         rows = str_detect(!!sym(x),"YES")
                       )})
    ) %>% 
    sub_missing(columns = everything(), 
                missing_text = "") %>% 
    cols_align(
      align = "center",
      columns = everything()
    ) %>% 
    gt_theme_nytimes() %>% 
    tab_options(
      data_row.padding = px(3),
      table.font.size = px(10),
      column_labels.font.size = px(10)) %>% 
    gt_add_divider(
      2:12,
      sides = "right",
      color = "white",
      style = "solid",
      weight = px(2),
      include_labels = TRUE
    ) %>% 
    cols_width(
      2:12 ~ px(60),
    ) %>% 
    drkn_clmn_hdr()

    
  # Subset of this in table
  # 8+1+1, MCH, PI, AP3 UNICEF
    
    
    
    
# SPINDOWN ============================================================================

