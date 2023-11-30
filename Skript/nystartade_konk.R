hamta_data_nystartade_konk = function(region = c("0020", "0000"),
                                       outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/",
                                       filnamn = "konk_nystartade.xlsx", 
                                       senaste_ar = FALSE, # Om man enbart vill ha senaste år
                                       tid = 1900:2100){ # Välj ett högt värde som sista värde om alla år skall vara med.

##### Hämtar data för att beräkna branschbredd #######
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 rKolada,
                 readxl)

  source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)
  options(dplyr.summarise.inform = FALSE)
  
    
    diagram_capt <- "Källa: SCB (via Kolada/RKA)\nBearbetning: Samhällsanalys, Region Dalarna" 
  
    
    # =============================================== API-uttag ===============================================
    
    if (!require("pacman")) install.packages("pacman")
    pacman::p_load(tidyverse,
                   rKolada,
                   readxl)
    
    source("https://raw.githubusercontent.com/FaluPeppe/func/main/func_API.R")
    
    if(senaste_ar == TRUE) tid <- max(unique(hamta_kolada_giltiga_ar("N00999",vald_region = region)))
    
    #### Dra hem variablerna från Kolada
    Nystartade_df <- get_values(
      kpi = c("N00999"),
      municipality = c("0000","0020",hamtakommuner(BaraEttLän)),
      period = tid
    )
    
    if(senaste_ar == TRUE) tid <- max(unique(hamta_kolada_giltiga_ar("N00926",vald_region = region)))
    
    Konkurser_df <- get_values(
      kpi = c("N00926"),
      municipality = c("0000","0020",hamtakommuner(BaraEttLän)),
      period = tid
    )
    
    # Nystartade företag
    
    Nystartade_df<-Nystartade_df %>% 
      select(year,gender,value,municipality) %>%
      rename("nystartade_ftg"=value)
    
    # Gör om år till en character
    Nystartade_df$year<-as.character(Nystartade_df$year)
    
    # Konkurser
    Konkurser_df<-Konkurser_df %>% 
      select(year,gender,value,municipality) %>%
      rename("konkurser"=value)
    
    # Gör om år till en character
    Konkurser_df$year<-as.character(Konkurser_df$year)
    
}
