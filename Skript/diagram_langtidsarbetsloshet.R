
diagram_langtidsarb = function(region = hamtakommuner("20",tamedlan = TRUE,tamedriket = FALSE),
                               outputmapp_figur = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/", # Här hamnar sparad figur
                               outputmapp_data = NA, # Här hamnar fil med data
                               vald_farg = diagramfarger("rus_sex"), #Vilken färg vill vi använda
                               jmf_ar = TRUE, # Om true, jämförs långtidsarbetslöshet för första och sista år i vald tid
                               spara_figur = TRUE, # Vill man spara figur
                               returnera_figur = TRUE, # Skall figuren returneras som ett ggplot-objekt
                               returnera_data = FALSE,
                               tid = 2011:2100){# Vilken tidsperiod vill vi fokusera på. 
  
  # =================================================================================================================
  # Diagram för långtidsarbetlöshet (15-74 år) på kommunal nivå. Går att jämföra första och sista år eller bara ta med sista år
    # =================================================================================================================
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  objektnamn <-c()
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/uppfoljning_dalastrategin/main/Skript/l%C3%A5ngtidsarbetsl%C3%B6shet.R")
  
  langtidsarbetsloshet_df = hamta_data_langtidsarb(region = region,
                                                   cond_code = c("N03923"),
                                                   returnera_data = TRUE,
                                                   filnamn = "langtidsarbetsloshet.csv",
                                                   output_mapp = outputmapp_data,
                                                   tid = tid)

  if(returnera_data == TRUE){
    assign("langtidsarbetsloshet", langtidsarbetsloshet_df, envir = .GlobalEnv)
  }
  
  langtidsarbetsloshet_df$municipality = skapa_kortnamn_lan(hamtaregion_kod_namn(langtidsarbetsloshet_df$municipality)$region,byt_ut_riket_mot_sverige = TRUE)

  valt_lan = skapa_kortnamn_lan(hamtaregion_kod_namn(region)$region)
  
  if(jmf_ar == TRUE){
    utskrift_df = langtidsarbetsloshet_df %>% 
      filter(gender == "T",year%in%c(min(langtidsarbetsloshet_df$year),max(langtidsarbetsloshet_df$year)-1,max(langtidsarbetsloshet_df$year)))
    vand_sortering = TRUE
    diagram_titel <- paste0("Långtidsarbetslöshet (15-74 år)")
  } else{
    utskrift_df = langtidsarbetsloshet_df %>% 
      filter(gender == "T",year == max(year))
    diagram_titel <- paste0("Långtidsarbetslöshet (15-74 år) år ",max(langtidsarbetsloshet_df$year))
    vand_sortering = FALSE
  } 
  
  diagram_capt <- "Källa: Arbetsförmedlingen (via Kolada).\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Antal invånare 15-74 år som varit öppet arbetslösa eller i program med aktivitetsstöd i minst sex månader,\ndividerat med totalt antal invånare 15-74 år som är öppet arbetslösa eller i program med aktivitetsstöd." 
  
  diagramfil <- "langtidsarbetsloshet.png"
  objektnamn <- c(objektnamn,"langtidsarbetsloshet")
  
  gg_obj <- SkapaStapelDiagram(skickad_df = utskrift_df,
                                    skickad_x_var = "municipality",
                                    skickad_y_var = "value",
                                    skickad_x_grupp = ifelse(jmf_ar == TRUE,"year",NA),
                                    manual_x_axis_text_vjust=1,
                                    manual_x_axis_text_hjust=1,
                                    manual_color = vald_farg,
                                    x_axis_sort_value = TRUE,
                                    x_axis_sort_grp = ifelse(jmf_ar == TRUE,3,NA),
                                    vand_sortering = vand_sortering,
                                    dataetiketter = FALSE,
                                    manual_y_axis_title="procent",
                                    diagram_titel = diagram_titel,
                                    diagram_capt = diagram_capt,
                                    procent_0_100_10intervaller = TRUE,
                                    stodlinjer_avrunda_fem = TRUE,
                                    output_mapp = outputmapp_figur,
                                    filnamn_diagram = "langtidsarbetsloshet.png",
                                    skriv_till_diagramfil = spara_figur)
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list) <- c(objektnamn)
  
  
  if(returnera_figur == TRUE) return(gg_list)

}
