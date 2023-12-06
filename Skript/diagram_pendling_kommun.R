diagram_andel_pendling <- function(region = "20",
                                   ta_med_riket = TRUE,
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
  # Skript för att skapa diagram för pendling 
  # 
  # Generellt gäller c("*) om man vill ha alla variabler
  # Parametrar som skickas med (= variabler i SCB-tabellen) är:
  # - region: Vald region
  # - alla_regioner: Välj om man vill ha alla regioner. Om den är satt till TRUE så skriver den över region ovan.
  # - alla_kommuner: Om man vill ha alla kommuner
  # - outputmapp_data: Vart skall data sparas
  # - outputmapp_figur: Vart skall figuren sparas
  # - filnamn : Vad skall filen heta
  # - senaste_ar: Sätts till TRUE om man bara vill ha data för senaste år
  # - tid: Vilka år vill man ha? Normalt c("*"), men går även att sätta ett intervall.
  # - returnera_data: True om data skall returneras som en df
  # - spara_till_excel: True om data skall sparas till Excel  
  # ===========================================================================================================

  # Data som sourcas från Region Dalarna
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/kompetensforsorjning_i_Dalarna/main/Skript/pendling_kommun.R")
  pendling_df = hamta_data_pendling_kommun(region_vekt = region,
                                           output_mapp = output_mapp_data,
                                           ta_med_riket = ta_med_riket,
                                           senaste_ar = senaste_ar,
                                           tid = tid,
                                           returnera_data = TRUE)

  diagram_capt <- "Källa: SCB (via Kolada)\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Antal inpendlare (personer som förvärvsarbetar i kommunen/regionen och bor i en annan kommun/region)\ndividerat med antal förvärvsarbetande som arbetar i kommunen/regionen (dagbefolkning)\nAntal utpendlare (personer som bor i kommunen/regionen och förvärvsarbetar i en annan kommun/region)\ndividerat med antal förvärvsarbetande som bor i kommunen/regionen (nattbefolkning)" 
  
  diagram_titel <- paste0("Andel av förvärvsarbetande som pendlar över kommungräns år ",max(pendling_df$year))
  diagram_titel<-str_wrap(diagram_titel,50)
  diagramfil <- "andel_pendling_kommun.png"
  
  gg_obj <- SkapaStapelDiagram(skickad_df = pendling_df %>% 
                                       filter(year%in%c(max(pendling_df$year))) %>% 
                                       filter(gender=="T") %>% 
                                       mutate(municipality = ifelse(municipality == "Riket", "Sverige",municipality)),
                                     skickad_x_var = "municipality", 
                                     skickad_y_var = "Pendling_andel", 
                                     skickad_x_grupp = "pendling_typ",
                                     manual_x_axis_text_vjust=1,
                                     manual_x_axis_text_hjust=1,
                                     manual_color = diagramfarger(vald_farg),
                                     x_axis_sort_value = TRUE,
                                     x_axis_sort_grp = 2,
                                     vand_sortering=TRUE,
                                     dataetiketter = FALSE,
                                     dataetiketter_antal_dec = 0,
                                     manual_y_axis_title="procent",
                                     diagram_titel = diagram_titel,
                                     diagram_capt = diagram_capt,
                                     output_mapp = output_mapp_figur,
                                     filnamn_diagram = diagramfil,
                                     skriv_till_diagramfil = spara_figur)
  
  if(returnera_figur == TRUE){
    return(gg_obj)
  }
  
}


