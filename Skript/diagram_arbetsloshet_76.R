
#test = diagram_data_arbetsloshet_76(region_vekt = "20",spara_figur = FALSE,diag_kon = TRUE)
diagram_data_arbetsloshet_76 <- function(region_vekt = c("00","20"),
                                         output_mapp_excel = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                         output_mapp_figur= "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                         filnamn_excel = "arbetsloshet_76.xlsx",
                                         vald_farg = "rus_sex",
                                         filnamn_figur = "arbetsloshet_76.png",
                                         spara_data = TRUE,
                                         spara_figur = TRUE, # Sparar figuren till output_mapp_figur
                                         returnera_figur = TRUE,
                                         diag_region = TRUE,
                                         diag_kon = FALSE){
  
  
  # =================================================================================================================
  # Diagram för arbetslöshet från 1974 till senaste år (AKU - SCB). 
  # Finns för tillfället i två varianter, det ena visar en jämförelse mellan län (alternativt län och riket)
  # Det andra visar en jämförelse mellan kön inom ett län (eller riket).
  # Källa  https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0210__AM0210A/ArbStatusM/
  # =================================================================================================================

  # Skript som behövs
  if (!require("pacman")) install.packages("pacman")
  p_load(here,
         tidyverse)
  
  gg_list <- list() # Skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  i<-1 # Räknare
  objektnamn <- c() # Används för att namnge
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source(here("Skript","arbetsloshet_76.R"), encoding="UTF-8")
  
  # Sourcar data för arbetslöshet
  df <-  hamta_data_arbetsloshet_76(region_vekt = region_vekt,
                                                  output_mapp = output_mapp_excel,
                                                  filnamn_excel = filnamn_excel,
                                                  spara_data = spara_data,
                                                  returnera_data = TRUE) 
  
  if(diag_region == TRUE){
    
    df_utskrift <- df %>% 
        group_by(region,år) %>% 
          summarize("sysselsatta" = sum(sysselsatta),
                    "arbetslösa" = sum(arbetslösa)) %>% 
      mutate(arbetsloshet = (arbetslösa/(sysselsatta+arbetslösa))*100)
    
    diagram_capt <- "Källa: SCB, arbetskraftsundersökningarna (AKU).\nBearbetning: Samhällsanalys, Region Dalarna.\nFrån och med oktober 2007 räknas även studenter som aktivt söker ett arbete och är villiga att ta jobb som arbetslösa"
    diagramtitel <- paste0("Arbetslöshet")
    objektnamn <- c(objektnamn,"arbetsloshet_region")
    
    gg_obj <- SkapaLinjeDiagram(skickad_df = df_utskrift,
                                skickad_x_var = "år",
                                skickad_y_var = "arbetsloshet",
                                skickad_x_grupp = "region",
                                manual_color = diagramfarger(vald_farg),
                                diagram_titel = diagramtitel,
                                diagram_capt =  diagram_capt,
                                manual_y_axis_title = "procent",
                                x_axis_lutning = 45,
                                visa_var_x_xlabel = 4,
                                stodlinjer_avrunda_fem = TRUE,
                                output_mapp = "output_mapp_figur",
                                filnamn_diagram = "filnamn_figur",
                                skriv_till_diagramfil = FALSE)
    
    gg_list[[i]] <-gg_obj
    i=i+1
    
  }
  
  if(diag_kon == TRUE && length(region_vekt)== 1){  
    # Fungerar enbart om man bara har valt ett län.
    diagram_capt <- "Källa: SCB, arbetskraftsundersökningarna (AKU).\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Från och med oktober 2007 räknas även studenter som aktivt söker ett arbete och är villiga att ta jobb som arbetslösa"
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
                                visa_var_x_xlabel = 4,
                                stodlinjer_avrunda_fem = TRUE,
                                output_mapp = "output_mapp_figur",
                                filnamn_diagram = "filnamn_figur",
                                skriv_till_diagramfil = FALSE)
    
    gg_list[[i]] <- gg_obj
    i=i+1
  }
  
  names(gg_list) <- c(objektnamn)
  if(returnera_figur == TRUE) return(gg_list)
  
}
