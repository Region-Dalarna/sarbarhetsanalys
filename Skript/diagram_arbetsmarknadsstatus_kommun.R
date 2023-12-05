
diagram_arbetsmarknadsstatus_kommun <-function(region_vekt = "20",
                                              output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/", # Output-mapp för Excel
                                              output_mapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/", # Output-mapp för figur
                                              filnamn = "arbetsmarknadsstatus_kommun.xlsx",
                                              spara_data = TRUE,
                                              spara_figur = TRUE, # Sparar figuren till output_mapp_figur
                                              returnera_figur = FALSE, # Returnerar en figur
                                              diag_farger = "rus_tre_fokus",
                                              ta_med_lan = TRUE,
                                              ta_med_riket = TRUE,
                                              diag_arbetslosthet = TRUE,
                                              diag_arbetskraftsdeltagande = TRUE,
                                              diag_sysselsattningsgrad = TRUE
){
  
  ## =================================================================================================================
  # Skript som laddar hem data för arbetsmarknadsstatus från SCB på kommunnivå (månadsdata) och skriver ut diagram. Beroende på val tas data för arbetslöshet, arbetskraftsdeltagande och sysselsättningsgrad med i uttaget.
  # Källa  https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0210__AM0210A/ArbStatusM/
  # För förklaringar av parameteralternativ (alder och födelseregion) använd pxvardelist("https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0210/AM0210A/ArbStatusM", "Kon"), 
  # där "Alder" byts mot vald variabel (från varlista nedan)
  # diag_arbetsloshet, diag_arbetskraftsdeltagande och diag_sysselsattningsgrad sätts till TRUE baserat på vilka variabler man vill ha
  # =================================================================================================================
  if (!require("pacman")) install.packages("pacman")
  p_load(openxlsx)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/sarbarhetsanalys/main/Skript/arbetsmarknadsstatus_kommun.R")
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  objektnamn<-c()
  i=1 # Räknare som används för att lägga till objekt i listan
  
  arbetsmarknadsstatus_df = hamta_data_arbetsmarknadsstatus_kommun(region_vekt = region_vekt,
                                                                   output_mapp = Output_mapp,
                                                                   spara_data = spara_data,
                                                                   ta_med_lan = ta_med_lan,
                                                                   ta_med_riket = ta_med_riket,
                                                                   diag_arbetslosthet = diag_arbetslosthet,
                                                                   diag_arbetskraftsdeltagande = diag_arbetskraftsdeltagande,
                                                                   diag_sysselsattningsgrad = diag_sysselsattningsgrad)
  

  
  valt_lan = skapa_kortnamn_lan(hamtaregion_kod_namn(region_vekt)$region)
  
  
  if(diag_sysselsattningsgrad==TRUE){
    
    diagram_capt = "Källa: SCB:s öppna statistikdatabas, befolkningens arbetsmarknadsstatus (BAS).\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Andelen av befolkningen som är sysselsatt (sysselsättningsgrad)."
    diagramtitel <- paste0("Sysselsättningsgrad i åldersgruppen ",unique(arbetsmarknadsstatus_df$ålder), " i ",last(arbetsmarknadsstatus_df$manad_long)," ",max(arbetsmarknadsstatus_df$ar))
    objektnamn <-c(objektnamn,paste0("Sysselsättningsgrad","_kommun"))
    
    # Skapar diagram 
    gg_obj <- SkapaStapelDiagram(skickad_df = arbetsmarknadsstatus_df %>% 
                                   mutate(region=ifelse(region=="Riket", "Sverige",region),
                                          fokus = ifelse(region == valt_lan,1,
                                                         ifelse(region == "Sverige",2,0))), 
                                 skickad_x_var = "region", 
                                 skickad_y_var = "sysselsättningsgrad", 
                                 #skickad_x_grupp = "kön",
                                 manual_y_axis_title = "procent",
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 manual_color = diagramfarger(diag_farger),
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 x_var_fokus = "fokus",
                                 x_axis_sort_value = TRUE,
                                 x_axis_lutning = 45,
                                 stodlinjer_avrunda_fem = TRUE,
                                 output_mapp = output_mapp_figur,
                                 filnamn_diagram = "sysselsattningsgrad_kommun.png",
                                 skriv_till_diagramfil = spara_figur)
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  if(diag_arbetslosthet==TRUE){
    
    diagram_capt = "Källa: SCB:s öppna statistikdatabas, befolkningens arbetsmarknadsstatus (BAS).\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Andelen av personer i arbetskraften som är arbetslösa."
    diagramtitel <- paste0("Arbetslöshet i åldersgruppen ",unique(arbetsmarknadsstatus_df$ålder), " i ",last(arbetsmarknadsstatus_df$manad_long)," ",max(arbetsmarknadsstatus_df$ar))
    objektnamn <-c(objektnamn,paste0("Arbetslöshet","_kommun"))
    
    # Skapar diagram 
    gg_obj <- SkapaStapelDiagram(skickad_df = arbetsmarknadsstatus_df %>% 
                                   mutate(region=ifelse(region=="Riket", "Sverige",region),
                                          fokus = ifelse(region == valt_lan,1,
                                                         ifelse(region == "Sverige",2,0))), 
                                 skickad_x_var = "region", 
                                 skickad_y_var = "arbetslöshet", 
                                 #skickad_x_grupp = "kön",
                                 manual_y_axis_title = "procent",
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 manual_color = diagramfarger(diag_farger),
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 x_var_fokus = "fokus",
                                 x_axis_sort_value = TRUE,
                                 x_axis_lutning = 45,
                                 stodlinjer_avrunda_fem = TRUE,
                                 output_mapp = output_mapp_figur,
                                 filnamn_diagram = "arbetslöshet_kommun.png",
                                 skriv_till_diagramfil = spara_figur)
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  if(diag_arbetskraftsdeltagande == TRUE){
    
    diagram_capt = "Källa: SCB:s öppna statistikdatabas, befolkningens arbetsmarknadsstatus (BAS).\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Andelen av personer i arbetskraften som är arbetslösa."
    diagramtitel <- paste0("Arbetslöshet i åldersgruppen",unique(arbetsmarknadsstatus_df$ålder), " i ",last(arbetsmarknadsstatus_df$manad_long)," ",max(arbetsmarknadsstatus_df$ar))
    objektnamn <-c(objektnamn,paste0("Arbetslöshet","_kommun"))
    
    # Skapar diagram 
    gg_obj <- SkapaStapelDiagram(skickad_df = arbetsmarknadsstatus_df %>% 
                                   mutate(region=ifelse(region=="Riket", "Sverige",region),
                                          fokus = ifelse(region == valt_lan,1,
                                                         ifelse(region == "Sverige",2,0))), 
                                 skickad_x_var = "region", 
                                 skickad_y_var = "arbetskraftsdeltagande", 
                                 #skickad_x_grupp = "kön",
                                 manual_y_axis_title = "procent",
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 manual_color = diagramfarger(diag_farger),
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 x_var_fokus = "fokus",
                                 x_axis_sort_value = TRUE,
                                 x_axis_lutning = 45,
                                 stodlinjer_avrunda_fem = TRUE,
                                 output_mapp = output_mapp_figur,
                                 filnamn_diagram = "arbetskraftsdeltagande_kommun.png",
                                 skriv_till_diagramfil = spara_figur)
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  if(returnera_figur == TRUE){
    names(gg_list) <- objektnamn
    return(gg_list)
  }
  
}

