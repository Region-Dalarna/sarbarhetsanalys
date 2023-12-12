

# # Laddar in de funktioner som behövs för att skapa diagram mm
# source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
# source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)
# source("G:/skript/func/func_filer.R", encoding = "utf-8", echo = FALSE)

#test_list <- diag_sysselsatta_andel(skapa_fil = FALSE)

diag_sysselsatta_andel <-function(region_vekt = "20", 
                                  output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                  skapa_fil = TRUE,
                                  diag_lan = TRUE, #Skapar ett diagram där län jämförs med riket
                                  diag_kommun = TRUE){ # Skapar diagram för samtliga kommuner 
  
  # ========================================== Allmän info ============================================
  
  # Diagram för antal företagssamma, antal arbetsställen och andel företagssamma. Data hämtas från företagarna:
  # https://www.foretagsklimat.se/downloads
  # Enbart senaste år
  # Data uppdaterades senast 2023-11-16
  
  # ========================================== Inställningar ============================================
  # Nödvändiga bibliotek och funktioner
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(openxlsx,
                 here,
                 tidyverse)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  # Det som står under diagrammet
  diagram_capt <- "Källa: RAMS i SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Branschens andel av totalt antal förvärvsarbetande"
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  # Skapar en tom vektor som skall innehålla objektnamn
  objektnamn <- c() 

  vald_region = skapa_kortnamn_lan(hamtaregion_kod_namn("20")$region)

# =============================================== API-uttag ===============================================

  # Hämtar data
  source("G:/skript/hamta_data/hamta_syss_bransch_kon_manad_bas.R")
  df = hamta_syss_branscher_kon_manad_bas(region_vekt =hamtakommuner(region_vekt,tamedlan = TRUE,tamedriket = TRUE),
                                            ar_vekt = "9999")
  
  # Summerar på region och sektor
  df_sum <- df %>%
    group_by(år, månad_år, region, bransch) %>% 
        summarize("Antal" = sum(`sysselsatta efter arbetsställets belägenhet`)) %>% 
      mutate(andel = (Antal/sum(Antal))*100,
             region = skapa_kortnamn_lan(region,byt_ut_riket_mot_sverige = TRUE))
  
  if(diag_lan==TRUE){
    
    diagram_titel <- paste0("Andel förvärvsarbetande (16-74) år per bransch ",unique(df_sum$månad_år))
    diagramfil <- "andel_per_bransch.png"
    objektnamn <- c(objektnamn,"andel_per_bransch")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = df_sum %>% 
                                   filter(region%in%c("Sverige",vald_region),bransch != "Okänt"),
                                 skickad_x_var = "bransch", 
                                 skickad_y_var = "andel", 
                                 skickad_x_grupp = "region",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = diagramfarger("rus_sex"),
                                 x_axis_sort_value = TRUE,
                                 manual_y_axis_title="procent",
                                 stodlinjer_avrunda_fem = TRUE,
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diagram_capt,
                                 diagram_facet = FALSE,
                                 facet_grp="år",
                                 berakna_index = FALSE,
                                 output_mapp = "output_mapp",
                                 filnamn_diagram = "diagramfil",
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list <- c(gg_list, list(gg_obj))
    
  }
  
  # Vektor som används för att skapa figurer för samtliga kommuner (mha en loop)
  kommun_vektor=hamtaregion_kod_namn(hamtakommuner(region_vekt,tamedlan=FALSE,tamedriket=FALSE))[2]
  
  # Loop som skapar diagram för samtliga Dalarnas kommuner
  j=1
  if(diag_kommun==TRUE){
    while(j <= length(kommun_vektor$region)){
      diagram_titel <- paste0("Andel förvärvsarbetande (16-74) år per bransch ",unique(df_sum$månad_år))
      diagram_typ <- paste0("andel_per_bransch","_",kommun_vektor$region[j])
      diagramfil <- paste0(diagram_typ,".png")
      objektnamn <- c(objektnamn,diagram_typ)
      
      # För att ort respektive län skall skrivas i rätt ordning (Dalarna först) så skapas en faktorvariabel med mutate nedan
      gg_obj <- SkapaStapelDiagram(skickad_df = df_sum %>% 
                                     filter(region %in% c(vald_region,kommun_vektor$region[j])) %>% 
                                     mutate(region = factor(region, levels = c(vald_region,kommun_vektor$region[j]))), 
                                   skickad_x_var = "bransch", 
                                   skickad_y_var = "andel", 
                                   skickad_x_grupp = "region",
                                   manual_x_axis_text_vjust = 1,
                                   manual_x_axis_text_hjust = 1,
                                   manual_color = diagramfarger("rus_sex"),
                                   x_axis_sort_value = TRUE,
                                   x_axis_sort_grp = 2,
                                   vand_sortering = TRUE,
                                   manual_y_axis_title = "procent",
                                   diagram_titel = diagram_titel,
                                   diagram_capt = diagram_capt,
                                   diagram_facet = FALSE,
                                   facet_grp = "år",
                                   berakna_index = FALSE,
                                   output_mapp = "output_mapp",
                                   filnamn_diagram = "diagramfil",
                                   skriv_till_diagramfil = skapa_fil)
      
      gg_list <- c(gg_list, list(gg_obj))
      j=j+1
    }
    
  }

  names(gg_list) <- c(objektnamn)
  return(gg_list)
  
}
