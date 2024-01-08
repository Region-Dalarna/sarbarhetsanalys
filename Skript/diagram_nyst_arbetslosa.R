#test = diagram_nystartade_konkurser(spara_figur = FALSE)

diagram_nystartade_konkurser <- function(region_vekt = hamtakommuner("20",tamedlan = TRUE,tamedriket = TRUE), # Val av region. 
                                         output_mapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/", # Här hamnar figur
                                         output_mapp_data = NA, # Här hamnar data
                                         filnamn_data = "nystartade_konkurser.xlsx", # Filnamn för datafil.
                                         vald_farg = diagramfarger("rus_sex"), # Vilken färgvektor vill man ha. Blir alltid "kon" när man väljer det diagrammet
                                         spara_figur = TRUE, # Sparar figuren till output_mapp_figur
                                         returnera_figur = TRUE, # Om man vill att figuren skall returneras från funktionen
                                         returnera_data = TRUE, # Om man vill att data skall returneras
                                         cont_cod = c("N00999","N00926") # "N00999" om man vill ha nystartade, "N00926" om man vill ha konkurser
){
  
  # ===========================================================================================================
  #
  # Diagram som visar antal nystartade förertag och antalet konkurser (per 1000 invånare) för de två senaste åren samt 5-årsperioden
  # dessförrinnan
  # ===========================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         tidyverse,
         openxlsx)
  
  gg_list <- list() # Skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  lista_data = lst() # Används för att spara till Excel
  objektnamn <- c() # Används för att namnge
  
  # Data som sourcas från Region Dalarna
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_nystart_konkurser_kolada.R")
  options(dplyr.summarise.inform = FALSE)
  
  ny_konk_list <- hamta_data_nystartade_konkurser(region = region_vekt, # Val av region.
                                                  output_mapp = NA, # Data sparas i ett senare skede
                                                  cont_cod = cont_cod, # "N00999" om man vill ha nystartade, "N00926" om man vill ha konkurser. 
                                                  tid = 1900:2100, 
                                                  returnera_data = TRUE)
  
  
  
  if("N00999" %in% cont_cod){
    
    senaste_ar_nystartade = max(ny_konk_list[["Nystartade"]]$year)
    
    valda_år_nystartade = paste0("medelvärde ",senaste_ar_nystartade-7,"-",senaste_ar_nystartade-2)
    
    # Beräknar medelvärde för de fem år som föranleder näst senaste år
    Nystartade_medel <- ny_konk_list[["Nystartade"]] %>%
      filter(year %in% c((senaste_ar_nystartade-7):(senaste_ar_nystartade-2))) %>% 
        group_by(municipality) %>% 
          summarize(nystartade_ftg = mean(nystartade_ftg)) %>% 
            mutate(year = valda_år_nystartade)
    
    Nystartade_utskrift <- rbind(ny_konk_list[["Nystartade"]] %>% filter(year%in%c(max(year),max(year-1))),Nystartade_medel)    
    
    if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
      lista_data = c(lista_data,lst("Nystartade" = Nystartade_utskrift))
    }
    
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
                                 manual_color = vald_farg,
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
  
  if("N00926" %in% cont_cod){
    
    senaste_ar_konk = max(ny_konk_list[["Konkurser"]]$year)
    valda_år_konk = paste0("medelvärde ",(senaste_ar_konk-7),"-",(senaste_ar_konk-2))
    
    konkurser_medel <- ny_konk_list[["Konkurser"]] %>%
      filter(year %in% c((senaste_ar_konk-7):(senaste_ar_konk-2))) %>% 
        group_by(municipality) %>% 
          summarize(konkurser = mean(konkurser)) %>% 
            mutate(year = valda_år_konk)
    
    konkurser_utskrift <- rbind(ny_konk_list[["Konkurser"]] %>% filter(year%in%c(max(year),max(year-1))),konkurser_medel)
    
    if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
      lista_data = c(lista_data,lst("Konkurser" = konkurser_utskrift))
    }
    
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
                                        manual_color = vald_farg,
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
  
  if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
    write.xlsx(lista_data,paste0(output_mapp_data,filnamn_data))
  }
  
  names(gg_list) <- c(objektnamn)
  if(returnera_figur == TRUE) return(gg_list)
  
}


