diagram_andel_offentligt <- function(region = "20",
                                     ta_med_riket = TRUE,
                                     Alder = "*",
                                     output_mapp_data = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                     output_mapp_figur= "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                     vald_farg = "rus_sex",
                                     spara_data = TRUE,
                                     spara_figur = TRUE, # Sparar figuren till output_mapp_figur
                                     senaste_ar = TRUE, # True om man enbart vill ha senaste år
                                     tid = c("*"), # 
                                     returnera_figur = FALSE # Om man vill att figuren skall returneras från funktionen
){
  
  # ===========================================================================================================
  #
  # Skript för att skapa diagram för andelen offentligt anställda 
  # 
  # Generellt gäller c("*) om man vill ha alla variabler
  # Parametrar som skickas med (= variabler i SCB-tabellen) är:
  # - region: Vald region
  # - alla_regioner: Välj om man vill ha alla regioner. Om den är satt till TRUE så skriver den över region ovan.
  # - alla_kommuner: Om man vill ha alla kommuner
  # - Alder: Se ovan (pxvardelist)
  # - outputmapp: Vart skall data sparas
  # - outputmapp_figur: Vart skall figuren sparas
  # - filnamn : Vad skall filen heta
  # - senaste_ar: Sätts till TRUE om man bara vill ha data för senaste år
  # - tid: Vilka år vill man ha? Normalt c("*"), men går även att sätta ett intervall.
  # - returnera_data: True om data skall returneras som en df
  # - spara_till_excel: True om data skall sparas till Excel  
  # ===========================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         tidyverse,
         openxlsx)
  
  # Data som sourcas från Region Dalarna
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/sarbarhetsanalys/main/Skript/andel_offentligt.R")
  
  andel_df <- hamta_data_andel_offentligt(region = "20",
                                          alla_regioner = FALSE,
                                          alla_kommuner = TRUE, # Enbart om alla_regioner är false och man enbart har valt en region
                                          ta_med_riket = ta_med_riket,
                                          Alder = Alder,
                                          output_mapp = output_mapp_data,
                                          filnamn = "andel_offentligt.xlsx",
                                          spara_data = spara_data,
                                          returnera_data = TRUE,
                                          senaste_ar = senaste_ar)
  
  
  
  mutate(`arbetsställets sektortillhörighet` = 
           ifelse(`arbetsställets sektortillhörighet` %in% 
                    c("statlig förvaltning","statliga affärsverk","primärkommunal förvaltning","regioner","övriga offentliga institutioner"),"Offentlig sektor","Övriga")) %>% 
    group_by(regionkod, region,`arbetsställets sektortillhörighet`,år) %>% 
    summarize("Förvärvsarbetande" = sum(`Förvärvsarbetande 16-74 år med arbetsplats i regionen (dagbefolkning) (RAMS)`)) %>% 
    ungroup()
  
  # Beräknar andelar      
  antal_sektor_df_utskrift <- antal_sektor_df %>%
    group_by(region,år) %>% 
    mutate(Andel_forv = (Förvärvsarbetande/sum(Förvärvsarbetande)*100)-0.01,
           region = region %>% skapa_kortnamn_lan()) %>% 
    rename(sektor = `arbetsställets sektortillhörighet`) %>% 
    ungroup()
    
  diagram_titel <- paste0("Andel offentligt anställda år ",unique(andel_df$år))
  diagramfilnamn <- "andel_offentligt.png"
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."
  
  gg_obj <- SkapaStapelDiagram(skickad_df = andel_df  %>% 
                                 mutate("sektor"= factor(`sektor`, levels = c("Offentlig sektor","Övriga")[2:1])) ,
                               skickad_x_var = "region", 
                               skickad_y_var = "Andel_forv", 
                               skickad_x_grupp = "sektor",
                               manual_x_axis_text_vjust=1,
                               manual_x_axis_text_hjust=1,
                               manual_color = diagramfarger(vald_farg),
                               stodlinjer_avrunda_fem = TRUE,
                               geom_position_stack = TRUE,
                               legend_vand_ordning = TRUE,
                               x_axis_sort_value = TRUE,
                               x_axis_sort_grp = 1,
                               manual_y_axis_title="procent",
                               diagram_titel = diagram_titel,
                               diagram_capt = diagram_capt,
                               output_mapp = output_mapp_figur,
                               filnamn_diagram = diagramfilnamn,
                               skriv_till_diagramfil = spara_figur)
  
  if(returnera_figur == TRUE){
    return(gg_obj)
  }
 
}


