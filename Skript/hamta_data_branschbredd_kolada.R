#test_list <- hamta_data_branschbredd(spara_data = FALSE,ta_med_riket = FALSE)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
source("https://raw.githubusercontent.com/JonFrank81/funktioner/main/func_API_alternativ.R")

hamta_data_branschbredd <-function(region = hamtakommuner("20",tamedlan = TRUE,tamedriket = TRUE), # Val av region.
                                   ta_med_riket = TRUE, # TRUE om man vill ha med riket.
                                   outputmapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                   filnamn = c("branschbredd.xlsx"), # Filnamn.
                                   tid = 1900:2100, # "Om man enbart vill ha senaste år"9999" om man enbart vill ha senaste år. Välj ett högt värde som sista värde om alla år skall vara med.
                                   spara_data = FALSE, # Om man vill spara data
                                   returnera_data = TRUE){
  
  # ===========================================================================================================
  # 
  # Skript som hämtar data för branschbredd från Kolada. Tabellen finns inte uppdelad på kön
  # Skapad av Jon Frank
  # Senast ändrad: 2023-12-13
  # ===========================================================================================================
  
  # Paket som används
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 rKolada,
                 readxl)
  options(dplyr.summarise.inform = FALSE)
  
  # I Kolada har alla regioner fyra siffror varför region och riket behöver justeras
  region = ifelse(nchar(region) == 2,paste0("00",region),region)
  
  if("9999" %in% tid) tid <- max(unique(hamta_kolada_giltiga_ar("N45702",vald_region = region)))
  
  #### Ta hem data från Kolada
  branschbredd <- get_values(
    kpi = c("N45702"),
    municipality = region,
    period = tid)
  
  # Väljer ut relevanta variabler och döper om value till Branschbredd
  
  branschbredd <- branschbredd %>% 
    select(year,value,municipality,municipality_id) %>% 
      rename("Branschbredd" = value) %>% 
        mutate(municipality = byt_namn_lan_kolada(municipality))
  
  if(returnera_data == TRUE) return(branschbredd)
  
  if (spara_data==TRUE){
    write.xlsx(antal_sektor_df,paste0(output_mapp,filnamn))
  }
  
  # hej<-branschbredd_utskift %>% 
  #   filter(year%in%c(min(branschbredd_utskift$year),max(branschbredd_utskift$year)))
  
  
  # diagram_titel <- paste0("Branschbredd")
  # diagram_typ <- "branschbredd"
  # diagramfil <- "branschbredd.png"
  # objektnamn <- diagram_typ
  # 
  # gg_obj <- SkapaStapelDiagram(skickad_df = branschbredd_utskift %>% 
  #                                filter(year%in%c(min(branschbredd_utskift$year),max(branschbredd_utskift$year))), 
  #                              skickad_x_var = "municipality", 
  #                              skickad_y_var = "Branschbredd", 
  #                              skickad_x_grupp = "year",
  #                              manual_x_axis_text_vjust=1,
  #                              manual_x_axis_text_hjust=1,
  #                              manual_color = diagramfarger("rus_sex"),
  #                              x_axis_sort_value = TRUE,
  #                              manual_y_axis_title="procent",
  #                              diagram_titel = diagram_titel,
  #                              diagram_capt = diagram_capt,
  #                              output_mapp = output_mapp,
  #                              filnamn_diagram = diagramfil,
  #                              skriv_till_diagramfil = skapa_fil)
  # 
  # gg_list[[i]] <-gg_obj
  # i=i+1
  # names(gg_list) <-c(objektnamn)
  # return(gg_list)
  
}
