diagram_langtidsarb = function(region = c("0020"),
                               alla_regioner = FALSE,
                               alla_kommuner = FALSE, 
                               ta_med_riket = FALSE,
                               outputmapp = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/", 
                               cond_code = c("N03926"),
                               returnera_data = FALSE,
                               spara_data = TRUE,
                               senaste_ar = FALSE, # om man enbart vill ha senaste år
                               tid = 2011:2100){ 
  
  source("https://raw.githubusercontent.com/Region-Dalarna/uppfoljning_dalastrategin/main/Skript/l%C3%A5ngtidsarbetsl%C3%B6shet.R")
  langtidsarbetsloshet_df = hamta_data_langtidsarb(region = region,
                                                   alla_regioner = FALSE,
                                                   alla_kommuner = TRUE,
                                                   ta_med_riket = ta_med_riket,
                                                   cond_code = c("N03923"),
                                                   outputmapp = Output_mapp,
                                                   returnera_data = TRUE,
                                                   filnamn = "langtidsarbetsloshet.csv",
                                                   tid = tid)


  diagram_capt <- "Källa: Arbetsförmedlingen (via Kolada).\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Antal invånare 15-74 år som varit öppet arbetslösa eller i program med aktivitetsstöd i minst sex månader,\ndividerat med totalt antal invånare 15-74 år som är öppet arbetslösa eller i program med aktivitetsstöd." 
  
  diagram_titel <- paste0("Långtidsarbetslöshet (15-74 år)")
  diagramfil <- "langtidsarbetsloshet.png"
  
  langtid_fig <- SkapaStapelDiagram(skickad_df = langtidsarbetsloshet_df %>% 
                                      filter(gender == "T",year%in%c(min(langtidsarbetsloshet_df$year),max(langtidsarbetsloshet_df$year))) %>% 
                                      mutate(region = ifelse(municipality == "Riket", "Sverige",
                                                             ifelse(municipality == "Region Dalarna", "Dalarna", municipality))),
                                    skickad_x_var = "municipality",
                                    skickad_y_var = "value",
                                    skickad_x_grupp = "year",
                                    manual_x_axis_text_vjust=1,
                                    manual_x_axis_text_hjust=1,
                                    manual_color = diagramfarger("rus_sex"),
                                    x_axis_sort_value = TRUE,
                                    x_axis_sort_grp = 4,
                                    vand_sortering = TRUE,
                                    dataetiketter = FALSE,
                                    manual_y_axis_title="procent",
                                    diagram_titel = diagram_titel,
                                    diagram_capt = diagram_capt,
                                    stodlinjer_avrunda_fem = TRUE,
                                    output_mapp = output_mapp,
                                    filnamn_diagram = "langtidsarbetsloshet.png",
                                    skriv_till_diagramfil = spara_figur)

}