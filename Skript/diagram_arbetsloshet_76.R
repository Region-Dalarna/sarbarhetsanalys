diagram_data_arbetsloshet_76 <- function(region_vekt = c("00","20"), # Vilka regioner vill man ha. Jämförs (ej kommuner). Om man vill jämföra kön får bara 1 väljas
                                         #output_mapp_data = NA, # Om man vill spara data. Används primärt i Rmarkdown-rapporter.
                                         output_mapp_figur= "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                         #filnamn_data = "arbetsloshet_76.xlsx", # Filnamn på sparad data
                                         vald_farg = diagramfarger("rus_sex"), # Val av diagramfärger
                                         filnamn_figur = "arbetsloshet_76.png", # Filnamn
                                         spara_figur = TRUE, # Sparar figuren till output_mapp_figur
                                         returnera_figur = TRUE, # Skall figuren returneras som ett ggplot-objekt
                                         diag_region = TRUE, # Tidsserie för totalt (dvs. både kvinnor och män)
                                         diag_kon = FALSE,
                                         returnera_data = TRUE){ # Tidsserie där kön jämförs. Går bara om en region valts i region_vekt
  
  
  # =================================================================================================================
  # Diagram för arbetslöshet från 1974 till senaste år (AKU - SCB). 
  # Finns för tillfället i två varianter, det ena visar en jämförelse mellan län (alternativt län och riket)
  # Det andra visar en jämförelse mellan kön inom ett län (eller riket).
  # Källor för data: 
  # https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0401__AM0401L/NAKUBefolkningLAr/
  # https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0402__AM0402F/AKUABefolkningL/
  #
  # Uppdaterad med nya PXweb - Jon 2026-07-02  #
  # =================================================================================================================

  # Skript som behövs
  if (!require("pacman")) install.packages("pacman")
  p_load(here,
         tidyverse)
  
  gg_list <- list() # Skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  i<-1 # Räknare
  objektnamn <- c() # Används för att namnge
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  #source(here("Skript","arbetsloshet_76.R"), encoding="UTF-8")
  #source("https://raw.githubusercontent.com/Region-Dalarna/sarbarhetsanalys/refs/heads/main/Skript/arbetsloshet_76_ny.R")
  
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         tidyverse,
         openxlsx)
  
  # Funktioner som behövs
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_pxweb2.R")
  
  ###############
  # Hämtar data #
  ###############
  
  # Till och med 04
  df_76_04 <- pxweb2_hamta_data(
    tabell = "TAB88",
    query = list(
      Region = region_vekt,
      Kon = c("män","kvinnor"),
      Arbetskraftstillh = c("SYS","ALÖS"),
      ContentsCode = "*",
      Tid = "*")) %>% 
    rename(Antal = value) %>% 
    pivot_wider(names_from = arbetskraftstillhörighet,values_from=Antal ) %>% 
    mutate(arbetsloshet = (arbetslösa/(arbetslösa+sysselsatta))*100) %>% 
    select(-tabellinnehåll,-region_kod)
  
  # Från 05
  df_05_ <- pxweb2_hamta_data(
    tabell = "TAB2922",
    query = list(
      Region = region_vekt,
      Arbetskraftstillh = c("ALÖS","SYS"),
      Kon = c("män","kvinnor"),
      ContentsCode = "1000-tal",
      Tid = "*"
    )) %>% 
    rename(Antal = value) %>% 
    pivot_wider(names_from = arbetskraftstillhörighet,values_from=Antal ) %>% 
    mutate(arbetsloshet = (arbetslösa/(arbetslösa+sysselsatta))*100) %>% 
    select(-tabellinnehåll,-region_kod)
  
  df <- rbind(df_76_04,df_05_) %>% 
    mutate(region = skapa_kortnamn_lan(region,byt_ut_riket_mot_sverige = TRUE))
  
  
  # Sourcar data för arbetslöshet
  # df <-  hamta_data_arbetsloshet_76(region_vekt = region_vekt,
  #                                                 output_mapp = output_mapp_data,
  #                                                 filnamn = filnamn_data,
  #                                                 returnera_data = TRUE) 
  
  if(returnera_data == TRUE){
    assign("arbetsloshet_76", df, envir = .GlobalEnv)
  }
  
  if(diag_region == TRUE){
    
    df_utskrift <- df %>% 
        group_by(region,år) %>% 
          summarize("sysselsatta" = sum(sysselsatta),
                    "arbetslösa" = sum(arbetslösa)) %>% 
      mutate(arbetsloshet = (arbetslösa/(sysselsatta+arbetslösa))*100)
    
    diagram_capt <- "Källa: SCB, arbetskraftsundersökningarna (AKU).\nBearbetning: Samhällsanalys, Region Dalarna.\nFrån och med oktober 2007 räknas även studenter som aktivt söker ett arbete och är villiga att ta jobb som arbetslösa.\nFram till 2004, arbetslöshet i åldersgruppen 16-64 år, därefter 15-74 år."
    diagramtitel <- paste0("Arbetslöshet i ",glue_collapse(unique(unique(df_utskrift$region)),sep = ", ", last = " och ")," ",min(df_utskrift$år),"-",max(df_utskrift$år))
    objektnamn <- c(objektnamn,"arbetsloshet_region")
    
    gg_obj <- SkapaLinjeDiagram(skickad_df = df_utskrift,
                                skickad_x_var = "år",
                                skickad_y_var = "arbetsloshet",
                                skickad_x_grupp = "region",
                                manual_color = vald_farg,
                                diagram_titel = diagramtitel,
                                diagram_capt =  diagram_capt,
                                manual_y_axis_title = "procent",
                                x_axis_lutning = 45,
                                x_axis_visa_var_xe_etikett = 4,
                                x_axis_var_xe_etikett_ta_bort_nast_sista_vardet = TRUE,
                                stodlinjer_avrunda_fem = TRUE,
                                output_mapp = output_mapp_figur,
                                filnamn_diagram = filnamn_figur,
                                skriv_till_diagramfil = spara_figur)
    
    gg_list[[i]] <-gg_obj
    i=i+1
    
  }
  
  if(diag_kon == TRUE && length(region_vekt)== 1){  
    # Fungerar enbart om man bara har valt ett län.
    diagram_capt <- "Källa: SCB, arbetskraftsundersökningarna (AKU).\nBearbetning: Samhällsanalys, Region Dalarna.\nFrån och med oktober 2007 räknas även studenter som aktivt söker ett arbete och är villiga att ta jobb som arbetslösa.\nFram till 2004, arbetslöshet i åldersgruppen 16-64 år, därefter 15-74 år."
    diagramtitel <- paste0("Arbetslöshet i ",hamtaregion_kod_namn(region_vekt)$region)
    objektnamn <- c(objektnamn,"arbetsloshet_kon")
    
    gg_obj <- SkapaLinjeDiagram(skickad_df = df ,
                                skickad_x_var = "år",
                                skickad_y_var = "arbetsloshet",
                                skickad_x_grupp = "kön",
                                manual_color = diagramfarger("kon"),
                                diagram_titel = diagramtitel,
                                diagram_capt =  diagram_capt,
                                manual_y_axis_title = "procent",
                                x_axis_lutning = 45,
                                x_axis_visa_var_xe_etikett = 4,
                                stodlinjer_avrunda_fem = TRUE,
                                output_mapp = output_mapp_figur,
                                filnamn_diagram = filnamn_figur,
                                skriv_till_diagramfil = spara_figur)
    
    gg_list[[i]] <- gg_obj
    i=i+1
  }
  
  names(gg_list) <- c(objektnamn)
  if(returnera_figur == TRUE) return(gg_list)
  
}
