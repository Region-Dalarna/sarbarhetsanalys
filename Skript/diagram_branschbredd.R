#test_list <- diag_branschbredd(region_vekt = "0022",spara_figur = FALSE,returnera_data = TRUE)
diag_branschbredd <-function(region_vekt = hamtakommuner("20",tamedlan = TRUE,tamedriket = TRUE), # Val av region.
                            output_mapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                            output_mapp_data = NA,
                            filnamn_data = "branschbredd.xlsx",
                            valda_farger = diagramfarger("rus_sex"),
                            spara_figur = TRUE, # Om true sparas figuren till output_mapp
                            returnera_data = FALSE){ # Om TRUE, returneras data till R-studios globala miljö 
  
  # ========================================== Allmän info ============================================
  
  # Skapar diagram för branschbredd på kommunnivå
  
  # ========================================== Inställningar ============================================
  # Nödvändiga bibliotek och funktioner
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(openxlsx,
                 here,
                 tidyverse)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  
  # Det som står under diagrammet
  diagram_capt <- "Källa: Kolada (RKA)\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Unika branscher i området dividerat med totalt antal branscher enligt SNI2007 på 5-siffernivå"
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)

  # =============================================== API-uttag ===============================================
  
  # Hämtar data
  source(here("Skript","hamta_data_branschbredd_kolada.R"), encoding="UTF-8")
  df <- hamta_data_branschbredd (region = region_vekt,
                                 output_mapp = output_mapp_data,
                                 filnamn = filnamn_data, 
                                 tid = 1900:2100, 
                                 returnera_data = TRUE)
  

  if(returnera_data == TRUE){
    assign("branschbredd", df, envir = .GlobalEnv)
  }
  
  diagram_titel <- paste0("Branschbredd")
  diagram_typ <- "branschbredd"
  diagramfil <- "branschbredd.png"
  
  gg_obj <- SkapaStapelDiagram(skickad_df = df %>% 
                                 filter(year%in%c(min(year),max(year)),
                                        municipality != "Riket") %>% 
                                  mutate(municipality = skapa_kortnamn_lan(municipality)), 
                               skickad_x_var = "municipality", 
                               skickad_y_var = "Branschbredd", 
                               skickad_x_grupp = "year",
                               manual_x_axis_text_vjust=1,
                               manual_x_axis_text_hjust=1,
                               manual_color = valda_farger,
                               x_axis_sort_value = TRUE,
                               x_axis_sort_grp = 2,
                               vand_sortering = TRUE,
                               manual_y_axis_title="procent",
                               diagram_titel = diagram_titel,
                               diagram_capt = diagram_capt,
                               output_mapp = output_mapp_figur,
                               filnamn_diagram = diagramfil,
                               skriv_till_diagramfil = spara_figur)
    
  gg_list <- c(gg_list, list(gg_obj))
  
  names(gg_list) <- diagram_typ
  return(gg_list)
  
}
