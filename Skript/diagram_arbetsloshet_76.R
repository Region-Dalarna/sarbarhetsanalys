diagram_data_arbetsloshet_76 <- function(region_vekt = "20",
                                         output_mapp_excel = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                         output_mapp_figur= "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                         filnamn_excel = "/arbetsloshet_76.xlsx",
                                         diag_farger = "rus_sex",
                                         filnamn_figur = "arbetsloshet_76.png",
                                         spara_data = TRUE,
                                         spara_figur = TRUE, # Sparar figuren till output_mapp_figur
                                         returnera_figur = TRUE){
  
  
  # =================================================================================================================
  # Diagram för arbetslöshet från 1974 till senaste år (AKU - SCB) 
  # Går i praktiken bara att ändra region för tillfället. Nästa steg, anpassa koden för kön
  # Källa  https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0210__AM0210A/ArbStatusM/
  # =================================================================================================================

  # Skript som behövs
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source(here("Skript","arbetsloshet_76.R"), encoding="UTF-8")
  
  # Sourcar data för arbetslöshet
  df_bef_slutgiltig <-  hamta_data_arbetsloshet_76(region_vekt = region_vekt,
                                                   output_mapp = output_mapp_excel,
                                                   filnamn_excel = filnamn_excel,
                                                   spara_data = spara_data,
                                                   returnera_data = TRUE)
  
    
  diagram_capt <- "Källa: SCB, arbetskraftsundersökningarna (AKU).\nBearbetning: Samhällsanalys, Region Dalarna.\nFrån och med oktober 2007 räknas även studenter som aktivt söker ett arbete och är villiga att ta jobb som arbetslösa"
  diagramtitel <- paste0("Arbetslöshet")
  
  gg_obj <- SkapaLinjeDiagram(skickad_df = df_bef_slutgiltig ,
                              skickad_x_var = "år",
                              skickad_y_var = "arbetsloshet",
                              skickad_x_grupp = "region",
                              manual_color = diagramfarger(diag_farger),
                              diagram_titel = diagramtitel,
                              diagram_capt =  diagram_capt,
                              manual_y_axis_title = "procent",
                              x_axis_lutning = 45,
                              visa_var_x_xlabel = 4,
                              stodlinjer_avrunda_fem = TRUE,
                              output_mapp = output_mapp_figur,
                              filnamn_diagram = filnamn_figur,
                              skriv_till_diagramfil = spara_figur)
  
  
   if(returnera_figur == TRUE) return(gg_obj)
  
}
