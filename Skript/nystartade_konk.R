hamta_data_nystartade_konk = function(region = c("0020"),
                                      alla_regioner = FALSE,
                                      alla_kommuner = TRUE, # Enbart om alla_regioner är false och man enbart har valt en region
                                      ta_med_riket = TRUE,
                                      outputmapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                      filnamn = c("nystartade.xlsx","konk.xlsx"), 
                                      cont_cod = c("N00999","N00926"),
                                      senaste_ar = FALSE, # Om man enbart vill ha senaste år
                                      tid = 1900:2100,
                                      spara_data = TRUE){ # Välj ett högt värde som sista värde om alla år skall vara med.

  # ===========================================================================================================
  # 
  # Skript som hämtar data för nystartade företag och/eller konkurser
  # Parametrar som skickas med (= variabler i Kolada-tabellen) är:
  # - region: Vald region
  # - alla_regioner: Välj om man vill ha alla regioner. Om den är satt till True så skriver den över region ovan.
  # - alla_kommuner: Om man vill ta med alla kommuner
  # - ta_med_riket: TRUE om man vill ta med riket också
  # - kpi:  
  # - N00999: Antal företagskonkurser per tusen invånare, 16-64 år.
  # - N00926: Antal nystartade företag delat med tusen invånare 16-64 år år T-1
  # - filnamn : Vad skall filerna heta. Bör sättas i samma ordning som cont_cod
  # - senaste_ar: Sätts till TRUE om man bara vill ha data för senaste år
  # - tid: Vilka år vill man ha? Välj ett högt senaste år om man vill ha alla
  # - spara_data: TRUE om data skall sparas till CSV
  # ===========================================================================================================

    if (!require("pacman")) install.packages("pacman")
    pacman::p_load(tidyverse,
                   rKolada,
                   readxl)
    
    source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
    
    if(alla_regioner == TRUE){
      region = hamtaAllaLan(tamedriket = FALSE) 
      region = paste0("00",region)
    }
    
    if(alla_kommuner == TRUE){
      region_kommun = hamtakommuner(substr(region,3,4),tamedlan = FALSE,tamedriket = FALSE)
      region = c(region,region_kommun)
    }
    
    if(ta_med_riket == TRUE){
      region = c("0000",region)
    } 
    
    if("N00999" %in% cont_cod ){
    
      if(senaste_ar == TRUE) tid <- max(unique(hamta_kolada_giltiga_ar("N00999",vald_region = region)))
    
      cont = "N00999"
      
      Nystartade_df <- get_values(
        kpi = cont,
        municipality = region,
        period = tid
      )
      
      Nystartade_df <- Nystartade_df %>% 
        select(year,value,municipality) %>%
          rename("nystartade_ftg" = value)
      
      fil = paste0(outputmapp,filnamn[1])
      if (spara_data == TRUE) write.xlsx(Nystartade_df, fil)
      
    }
    
    if("N00926" %in% cont_cod ){
        
      if(senaste_ar == TRUE) tid <- max(unique(hamta_kolada_giltiga_ar("N00926",vald_region = region)))
      
      cont = "N00926"
      
      Konkurser_df <- get_values(
        kpi = cont,
        municipality = region,
        period = tid
      )
      
      # Konkurser
      Konkurser_df<-Konkurser_df %>% 
        select(year,value,municipality) %>%
         rename("konkurser" = value) 
      
      fil = paste0(outputmapp,filnamn[2])
      
      if (spara_data == TRUE) write.xlsx(Konkurser_df, fil)
    }
}
