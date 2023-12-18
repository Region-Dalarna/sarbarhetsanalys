
# test = diagram_langtidsarb(region = c("0020"),
#                            ta_med_riket = TRUE,
#                            outputmapp = Output_mapp_figur,
#                            jmf_ar = FALSE,
#                            returnera_figur = TRUE,
#                            spara_figur = FALSE,
#                           vald_farg = "rus_sex")

diagram_langtidsarb = function(region = c("0020"),
                               ta_med_riket = FALSE,
                               outputmapp_figur = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/", # Här hamnar sparad figur
                               outputmapp_data = "G:/skript/projekt/data/uppfoljning_dalastrategin/Data/", # Här hamnar fil med data
                               vald_farg = diagramfarger("rus_sex"), #Vilken färg vill vi använda
                               jmf_ar = TRUE, # Om true, jämförs långtidsarbetslöshet för första och sista år i vald tid
                               spara_figur = TRUE, # Vill man spara figur
                               spara_data = FALSE, # Vill man spara data
                               returnera_figur = TRUE, # Skall figuren returneras som ett ggplot-objekt
                               tid = 2011:2100){# Vilken tidsperiod vill vi fokusera på
  
  # =================================================================================================================
  # Diagram för långtidsarbetlöshet på kommunal nivå. Jämför första år i dataset med sista år i dataset
  # För tillfället inte möjligt att dela upp efter kön
  # ta_med_riket: TRUE om man vill ta med riket i jämförelsen, annars tar enbart län med
  # Outputmapp: Mapp där figur hamnar
  # Outputmapp_data: Mapp där Exceldokument hamnar (om vi väljer att spara)
  # =================================================================================================================
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  objektnamn <-c()
  i=1 # Räknare som används för att lägga till objekt i listan
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/uppfoljning_dalastrategin/main/Skript/l%C3%A5ngtidsarbetsl%C3%B6shet.R")
  
  langtidsarbetsloshet_df = hamta_data_langtidsarb(region = region,
                                                   alla_regioner = FALSE,
                                                   alla_kommuner = TRUE,
                                                   ta_med_riket = ta_med_riket,
                                                   cond_code = c("N03923"),
                                                   returnera_data = TRUE,
                                                   spara_data = spara_data,
                                                   outputmapp = outputmapp_data,
                                                   senaste_ar = FALSE,
                                                   tid = tid)

  langtidsarbetsloshet_df$municipality = skapa_kortnamn_lan(hamtaregion_kod_namn(langtidsarbetsloshet_df$municipality)$region,byt_ut_riket_mot_sverige = TRUE)

  valt_lan = skapa_kortnamn_lan(hamtaregion_kod_namn(region)$region)
  
  # if(returnera_data == TRUE){
  #   assign("langtidsarbetsloshet", langtidsarbetsloshet_df %>% filter(gender == "T"), envir = .GlobalEnv)
  # }
  
  if(jmf_ar == TRUE){
    utskrift_df = langtidsarbetsloshet_df %>% 
      filter(gender == "T",year%in%c(min(langtidsarbetsloshet_df$year),max(langtidsarbetsloshet_df$year)))
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
                                    x_axis_sort_grp = ifelse(jmf_ar == TRUE,4,NA),
                                    vand_sortering = vand_sortering,
                                    dataetiketter = FALSE,
                                    manual_y_axis_title="procent",
                                    diagram_titel = diagram_titel,
                                    diagram_capt = diagram_capt,
                                    procent_0_100_10intervaller = TRUE,
                                    stodlinjer_avrunda_fem = TRUE,
                                    output_mapp = outputmapp,
                                    filnamn_diagram = "langtidsarbetsloshet.png",
                                    skriv_till_diagramfil = spara_figur)
  gg_list[[i]] <- gg_obj
  i=i+1
  names(gg_list) <- c(objektnamn)
  
  
  if(returnera_figur == TRUE) return(gg_list)

}
