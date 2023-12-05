diagram_langtidsarb = function(region = c("0020"),
                               ta_med_riket = FALSE,
                               outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/", 
                               outputmapp_data = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/",
                               vald_farg = "rus_sex",
                               spara_data = TRUE,
                               spara_figur = TRUE, # Sparar figuren till output_mapp_figur
                               returnera_figur = TRUE,
                               senaste_ar = FALSE, # om man enbart vill ha senaste år
                               tid = 2011:2100){ 
  
  # =================================================================================================================
  # Diagram för långtidsarbetlöshet på kommunal nivå.
  # För tillfället inte möjligt att dela upp efter kön
  # ta_med_riket: TRUE om man vill ta med riket i jämförelsen, annars tar enbart län med
  # Outputmapp: Mapp där figur hamnar
  # Outputmapp_data: Mapp där Exceldokument hamnar (om vi väljer att spara)
  # =================================================================================================================
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/uppfoljning_dalastrategin/main/Skript/l%C3%A5ngtidsarbetsl%C3%B6shet.R")
  
  langtidsarbetsloshet_df = hamta_data_langtidsarb(region = region,
                                                   alla_regioner = FALSE,
                                                   alla_kommuner = TRUE,
                                                   ta_med_riket = ta_med_riket,
                                                   cond_code = c("N03923"),
                                                   outputmapp = outputmapp_data,
                                                   returnera_data = TRUE,
                                                   spara_data = spara_data,
                                                   senaste_ar = senaste_ar,
                                                   filnamn = "langtidsarbetsloshet.csv",
                                                   tid = tid)

  langtidsarbetsloshet_df$municipality = skapa_kortnamn_lan(hamtaregion_kod_namn(langtidsarbetsloshet_df$municipality)$region,byt_ut_riket_mot_sverige = TRUE)

  valt_lan = skapa_kortnamn_lan(hamtaregion_kod_namn(region)$region)
  
  diagram_capt <- "Källa: Arbetsförmedlingen (via Kolada).\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Antal invånare 15-74 år som varit öppet arbetslösa eller i program med aktivitetsstöd i minst sex månader,\ndividerat med totalt antal invånare 15-74 år som är öppet arbetslösa eller i program med aktivitetsstöd." 
  
  diagram_titel <- paste0("Långtidsarbetslöshet (15-74 år)")
  diagramfil <- "langtidsarbetsloshet.png"
  
  langtid_fig <- SkapaStapelDiagram(skickad_df = langtidsarbetsloshet_df %>% 
                                      filter(gender == "T",year%in%c(min(langtidsarbetsloshet_df$year),max(langtidsarbetsloshet_df$year))),
                                    skickad_x_var = "municipality",
                                    skickad_y_var = "value",
                                    skickad_x_grupp = "year",
                                    manual_x_axis_text_vjust=1,
                                    manual_x_axis_text_hjust=1,
                                    manual_color = diagramfarger(vald_farg),
                                    x_axis_sort_value = TRUE,
                                    x_axis_sort_grp = 4,
                                    vand_sortering = TRUE,
                                    dataetiketter = FALSE,
                                    manual_y_axis_title="procent",
                                    diagram_titel = diagram_titel,
                                    diagram_capt = diagram_capt,
                                    stodlinjer_avrunda_fem = TRUE,
                                    output_mapp = outputmapp,
                                    filnamn_diagram = "langtidsarbetsloshet.png",
                                    skriv_till_diagramfil = spara_figur)
  
  if(returnera_figur == TRUE) return(langtid_fig)

}