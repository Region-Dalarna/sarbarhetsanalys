#test = diagram_nystartade_konkurser(spara_figur = FALSE,returnera_data = TRUE)

diagram_nystartade_konkurser <- function(region_vekt = "0020", # Val av region. Börjar med 00 för regioner (och Sverige) i Kolada
                                         alla_regioner = FALSE, # TRUE om man vill ha alla regioner. Övertrumfar region
                                         alla_kommuner = TRUE, # TRUE om man vill ha alla kommuner för valda kommuner.
                                         ta_med_riket = TRUE, # TRUE om man vill ha med riket.
                                         output_mapp_figur= "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                         vald_farg = "rus_sex", # Vilken färgvektor vill man ha. Blir alltid "kon" när man väljer det diagrammet
                                         spara_figur = TRUE, # Sparar figuren till output_mapp_figur
                                         returnera_figur = TRUE, # Om man vill att figuren skall returneras från funktionen
                                         returnera_data = FALSE, # Data läggs upp i R-Studios globala miljö (för användning i exempelvis R-markdown)
                                         diag_nystartade = TRUE, # Skriver ut diagram för nystartade företag
                                         diag_konkurser = TRUE # Skriver ut ett diagram för konkurser
){
  
  # ===========================================================================================================
  #
  # Diagram som visar antal nystartade förertag och antalet konkurser (per 1000 invånare) för de två senaste åren samt 5-årsperioden
  # dessförinnan
  # ===========================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         tidyverse,
         openxlsx)
  
  gg_list <- list() # Skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  #i <- 1 # Räknare
  objektnamn <- c() # Används för att namnge
  
  # Data som sourcas från Region Dalarna
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_nystart_konkurser_kolada.R")
  options(dplyr.summarise.inform = FALSE)
  
  ny_konk_list <- hamta_data_nystartade_konkurser(region = region_vekt, # Val av region. Börjar med 00 för regioner (och Sverige) i Kolada
                                              alla_regioner = FALSE, # TRUE om man vill ha alla regioner. Övertrumfar region
                                              alla_kommuner = TRUE, # TRUE om man vill ha alla kommuner för valda kommuner.
                                              ta_med_riket = TRUE, # TRUE om man vill ha med riket.
                                              outputmapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                              filnamn = c("nystartade.xlsx","konkurser.xlsx"), # Filnamn. Bör inte ändras.
                                              cont_cod = c("N00999","N00926"), # "N00999" om man vill ha nystartade, "N00926" om man vill ha konkurser. Ordning bör vara samma som filnamn
                                              tid = 1900:2100, # "Om man enbart vill ha senaste år"99" om man enbart vill ha senaste år. Välj ett högt värde som sista värde om alla år skall vara med.
                                              spara_data = FALSE, # Om man vill spara data
                                              returnera_data = TRUE)
  
  
  
  if(diag_nystartade == TRUE){
    
    senaste_ar_nystartade = max(ny_konk_list$Nystartade_df$year)
    
    valda_år_nystartade = paste0("medelvärde ",senaste_ar_nystartade-7,"-",senaste_ar_nystartade-2)
    
    # Beräknar medelvärde för de fem år som föranleder näst senaste år
    Nystartade_medel <- ny_konk_list$Nystartade_df %>%
      filter(year %in% c((senaste_ar_nystartade-7):(senaste_ar_nystartade-2))) %>% 
        group_by(municipality) %>% 
          summarize(nystartade_ftg = mean(nystartade_ftg)) %>% 
            mutate(year = valda_år_nystartade)
    
    Nystartade_utskrift <- rbind(ny_konk_list$Nystartade_df %>% filter(year%in%c(max(year),max(year-1))),Nystartade_medel)    
    
    if(returnera_data == TRUE){
      assign("Nystartade", Nystartade_utskrift, envir = .GlobalEnv)
    }
    
    diagram_capt <- "Källa: SCB (via Kolada/RKA)\nBearbetning: Samhällsanalys, Region Dalarna"
    diagram_titel <- paste0("Antal nystartade företag per 1000 invånare (16-64 år)")
    diagramfil <- "nystartade.png"
    objektnamn <- c(objektnamn,"nystartade")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = Nystartade_utskrift %>%
                                   mutate(municipality=ifelse(municipality=="Region Dalarna","Dalarna",municipality)) %>% 
                                   mutate(municipality=ifelse(municipality=="Riket", "Sverige",municipality)) %>% 
                                   mutate(year = factor(year, levels = c(valda_år_nystartade,as.character(senaste_ar_nystartade-1), senaste_ar_nystartade))), 
                                 skickad_x_var = "municipality", 
                                 skickad_y_var = "nystartade_ftg", 
                                 skickad_x_grupp = "year",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = diagramfarger(vald_farg),
                                 x_axis_sort_value = TRUE,
                                 x_axis_sort_grp = 3,
                                 vand_sortering=TRUE,
                                 dataetiketter = FALSE,
                                 stodlinjer_avrunda_fem = TRUE,
                                 dataetiketter_antal_dec = 0,
                                 manual_y_axis_title="Nystartade företag/1000 invånare",
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diagram_capt,
                                 output_mapp = output_mapp_figur,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = spara_figur)
    
 
    gg_list <- c(gg_list, list(gg_obj))
    
  }
  
  if(diag_konkurser == TRUE){
    
    senaste_ar_konk = max(ny_konk_list$Konkurser_df$year)
    valda_år_konk = paste0("medelvärde ",(senaste_ar_konk-7),"-",(senaste_ar_konk-2))
    
    konkurser_medel <- ny_konk_list$Konkurser_df %>%
      filter(year %in% c((senaste_ar_konk-7):(senaste_ar_konk-2))) %>% 
        group_by(municipality) %>% 
          summarize(konkurser = mean(konkurser)) %>% 
            mutate(year = valda_år_konk)
    
    konkurser_utskrift <- rbind(ny_konk_list$Konkurser_df %>% filter(year%in%c(max(year),max(year-1))),konkurser_medel)
    
    if(returnera_data == TRUE){
      assign("Konkurser", konkurser_utskrift, envir = .GlobalEnv)
    }
    
    diagram_titel <- paste0("Antal konkurser per 1000 invånare (16-64 år)")
    diagramfil <- "konkurser.png"
    objektnamn <- c(objektnamn,"konkurser")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = konkurser_utskrift %>%
                                          mutate(municipality=ifelse(municipality=="Region Dalarna","Dalarna",municipality)) %>% 
                                          mutate(municipality=ifelse(municipality=="Riket", "Sverige",municipality)) %>% 
                                          mutate(year = factor(year, levels = c(valda_år_konk,as.character(senaste_ar_konk-1), senaste_ar_konk))), 
                                        skickad_x_var = "municipality", 
                                        skickad_y_var = "konkurser", 
                                        skickad_x_grupp = "year",
                                        manual_x_axis_text_vjust=1,
                                        manual_x_axis_text_hjust=1,
                                        manual_color = diagramfarger(vald_farg),
                                        x_axis_sort_value = TRUE,
                                        x_axis_sort_grp = 3,
                                        vand_sortering=TRUE,
                                        dataetiketter = FALSE,
                                        stodlinjer_avrunda_fem = TRUE,
                                        dataetiketter_antal_dec = 0,
                                        manual_y_axis_title="Konkurser/1000 invånare",
                                        diagram_titel = diagram_titel,
                                        diagram_capt = diagram_capt,
                                        output_mapp = output_mapp_figur,
                                        filnamn_diagram = diagramfil,
                                        skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
    
  }
  
  names(gg_list) <- c(objektnamn)
  if(returnera_figur == TRUE) return(gg_list)
  
}


