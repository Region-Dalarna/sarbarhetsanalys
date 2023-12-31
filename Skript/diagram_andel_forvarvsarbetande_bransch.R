#test_list <- diag_sysselsatta_andel(region_vekt = "25",spara_figur = FALSE)

diag_sysselsatta_andel <-function(region_vekt = "20", # Region vi är intresserade av. 
                                  output_mapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/", # Här hamnar sparad figur
                                  output_mapp_data = NA, # Här hamnar sparad data
                                  filnamn_data = "andel_forvarvsarbetande.xlsx",
                                  valda_farger = diagramfarger("rus_sex"), # Vilka färger skall användas i diagram
                                  spara_figur = TRUE, # Om true sparas figuren till output_mapp
                                  diag_lan = TRUE, # Skapar ett diagram där län jämförs med riket
                                  diag_kommun = TRUE, # Motsvarande diagram där kommuner jämförs med länet
                                  returnera_figur = TRUE,
                                  returnera_data = FALSE){ 
  
  # ========================================== Allmän info ============================================
  
  # Skapar diagram för andelen förvärvsarbetande inom olika branscher, dels på länsnivå, dels på kommunnivå
  # Enbart senaste år och ingen uppdelning på kön
  
  # ========================================== Inställningar ============================================
  # Nödvändiga bibliotek och funktioner
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(openxlsx,
                 here,
                 tidyverse)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  # Det som står under diagrammet
  diagram_capt <- "Källa: BAS i SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Branschens andel av totalt antal förvärvsarbetande"
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  # Skapar en tom vektor som skall innehålla objektnamn
  objektnamn <- c() 

  vald_region = skapa_kortnamn_lan(hamtaregion_kod_namn(region_vekt)$region)

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
  
  if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
    write.xlsx(df_sum,paste0(output_mapp_data,filnamn_data))
  }
  
  if(returnera_data == TRUE){
    assign("andel_forvarvsarbetande_bransch", df_sum, envir = .GlobalEnv)
  }
  
  if(diag_lan==TRUE){
    
    diagram_titel <- paste0("Andel förvärvsarbetande (16-74) år per bransch ",unique(df_sum$månad_år))
    diagramfil <- "andel_per_bransch.png"
    objektnamn <- c(objektnamn,"andel_per_bransch")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = df_sum %>% 
                                   filter(region%in%c("Sverige",vald_region),bransch != "Okänt") %>% 
                                    mutate(bransch = str_wrap(bransch,20)),
                                 skickad_x_var = "bransch", 
                                 skickad_y_var = "andel", 
                                 skickad_x_grupp = "region",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = valda_farger,
                                 x_axis_sort_value = TRUE,
                                 manual_y_axis_title = "procent",
                                 stodlinjer_avrunda_fem = TRUE,
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diagram_capt,
                                 output_mapp = output_mapp_figur,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = spara_figur)
    
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
                                     filter(region %in% c(vald_region,kommun_vektor$region[j]),bransch != "Okänt") %>% 
                                     mutate(region = factor(region, levels = c(vald_region,kommun_vektor$region[j])),
                                            bransch = str_wrap(bransch,20)), 
                                   skickad_x_var = "bransch", 
                                   skickad_y_var = "andel", 
                                   skickad_x_grupp = "region",
                                   manual_x_axis_text_vjust = 1,
                                   manual_x_axis_text_hjust = 1,
                                   manual_color = valda_farger,
                                   x_axis_sort_value = TRUE,
                                   x_axis_sort_grp = 2,
                                   vand_sortering = TRUE,
                                   manual_y_axis_title = "procent",
                                   stodlinjer_avrunda_fem = TRUE,
                                   diagram_titel = diagram_titel,
                                   diagram_capt = diagram_capt,
                                   output_mapp = output_mapp_figur,
                                   filnamn_diagram = diagramfil,
                                   skriv_till_diagramfil = spara_figur)
      
      gg_list <- c(gg_list, list(gg_obj))
      j=j+1
    }
    
  }

  names(gg_list) <- c(objektnamn)
  if(returnera_figur == TRUE) return(gg_list)
  
}
