# Hur många företag krävs för att lönesumman i en kommun skall uppgå till 50 % (kumulativt)
# R-skript som skapar data finns på MONA under P1079_Gem/Jon/Sårbarhetsanalys/Ftg_50procent_lonesumma_ny_variant

pacman::p_load(openxlsx,here,tidyverse)

# Skript som behövs
source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)

#test_list=diag_50proc_lonesumma(skapa_fil=TRUE,diag_LA=FALSE)
diag_50proc_lonesumma <- function(region_vekt="20",
                                  output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                  skapa_fil=TRUE,
                                  diag_kommun=TRUE,
                                  diag_LA=TRUE){
  
  # ========================================== Inställningar ============================================
  # Text till diagram
  diagram_capt_kommun <- c("Källa: SCB.\nBearbetning: Samhällsanalys, Region Dalarna.")
  diagram_capt_LA <- c("Källa: SCB.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: LA-region: Geografiska områden som är relativt oberoende av omvärlden\nmed avseende på utbud och efterfrågan av arbetskraft.")
  ValdGeografi <- hamtaregion_kod_namn(region_vekt)$region
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  objektnamn<-c()
  i=1 # Räknare som används för att lägga till objekt i listan
  
  # ========================================== Läser in data ============================================
  # Läser in data från Excel (ursprung arbetsförmedlingen)
  kommun_df <- read.xlsx("G:/skript/projekt/data/sarbarhetsanalys/1_nov_23_50_procent_lonesumma.xlsx",sheet=1)
  LA_df <- read.xlsx("G:/skript/projekt/data/sarbarhetsanalys/17_mar_23_50_procent_lonesumma.xlsx",sheet=2)

  if(diag_kommun==TRUE){
    diagramtitel <- paste0("Summan av antalet företag i respektive kommun som utgör 50 procent av lönesumman ",max(kommun_df$Ar))
    diagramtitel <- str_wrap(diagramtitel,55)
    diagramfilnamn <- paste0("50proc_lonesumma.png")
    objektnamn <- c(objektnamn,diagramtitel)
    
    gg_obj <- SkapaStapelDiagram(skickad_df =kommun_df,
                                 skickad_x_var = "LAKommun_namn", 
                                 skickad_y_var = "cnum", 
                                 #skickad_x_grupp = "utb_niva",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = diagramfarger("rus_sex")[1],
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt_kommun,
                                 manual_y_axis_title = "Antal företag",
                                 diagram_facet = FALSE,
                                 #x_var_fokus = "fokus",
                                 dataetiketter=TRUE,
                                 x_axis_lutning = 45,
                                 x_axis_sort_value = TRUE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  if(diag_LA==TRUE){
    diagramtitel <- paste0("Summan av antalet företag i respektive LA-region som utgör 50 procent av lönesumman ",max(LA_df$Ar))
    diagramtitel <- str_wrap(diagramtitel,55)
    diagramfilnamn <- paste0("50proc_lonesumma_LA.png")
    objektnamn <- c(objektnamn,diagramtitel)
    
    gg_obj <- SkapaStapelDiagram(skickad_df =LA_df,
                                 skickad_x_var = "LA_namn", 
                                 skickad_y_var = "cnum", 
                                 #skickad_x_grupp = "utb_niva",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = diagramfarger("rus_sex")[1],
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt_LA,
                                 manual_y_axis_title = "Antal företag",
                                 diagram_facet = FALSE,
                                 #x_var_fokus = "fokus",
                                 dataetiketter=FALSE,
                                 x_axis_lutning = 45,
                                 x_axis_sort_value = TRUE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  names(gg_list)<-objektnamn
  return(gg_list)
}
