# Tar fram data för bredbandsstatistik
# Excelfil som jag har fått från Anders Oksvold

pacman::p_load(openxlsx,here,tidyverse)

# Skript som behövs
source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)

#test_list=diag_bredband(skapa_fil=FALSE)
diag_bredband_PTS <- function(region_vekt="20",
                              output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                              skapa_fil=TRUE,
                              diag_bredband_facet=TRUE){
  
  # ========================================== Inställningar ============================================
  # Text till diagram
  diagram_capt <- "Källa: Post- och telestyrelsen (PTS).\nBearbetning: Samhällsanalys, Region Dalarna."
  ValdGeografi <- hamtaregion_kod_namn(region_vekt)$region
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  objektnamn<-c()
  i=1 # Räknare som används för att lägga till objekt i listan
  
  # ========================================== Läser in data ============================================
  # Läser in data från Excel (ursprung arbetsförmedlingen)
  bredband_tatbyggt_df <- read.xlsx("G:/skript/projekt/Sårbarhetsanalys/Indata/Bredband_inlas.xlsx",startRow=2,sheet=1)
  bredband_glesbyggt_df <- read.xlsx("G:/skript/projekt/Sårbarhetsanalys/Indata/Bredband_inlas.xlsx",startRow=2,sheet=2)
  
  # Tätbebyggt område
  # Pivoterar data
  bredband_tat_pivot<-bredband_tatbyggt_df %>%
    pivot_longer(2:11,names_to="variabel",values_to="Andel")
  
  # Separerar två variabler
  bredband_tat_pivot<-bredband_tat_pivot %>% 
    separate(variabel,c("Variabel","Ar"),sep="_")
  
  # Glesbebyggt område
  # Pivoterar data
  bredband_glesbyggt_pivot<-bredband_glesbyggt_df %>% 
    pivot_longer(2:11,names_to="variabel",values_to="Andel")
  
  # Separerar två variabler
  bredband_glesbyggt_pivot<-bredband_glesbyggt_pivot %>% 
    separate(variabel,c("Variabel","Ar"),sep="_")
  
  # Lägger till en variabel område, baserat på om det är tätort eller glesbygd
  bredband_tat_pivot$omrade<-"Tätort"
  bredband_glesbyggt_pivot$omrade<-"Glesbygd"
  
  # Slår ihop de två datasetten
  
  bredband_utskrift<-rbind(bredband_tat_pivot,bredband_glesbyggt_pivot)
  
  # Gör om andelar till procent
  bredband_utskrift$Andel<-bredband_utskrift$Andel*100
  
  # skapa fokusvariabel för att fokusera på valt län och riket
  bredband_utskrift$fokus <- NA                      # en metod för att få bort warning messages för "Unknown or uninitialised column: `fokus`."
  bredband_utskrift$fokus <- 0
  bredband_utskrift$fokus[bredband_utskrift$Kommun =="Dalarnas län"] <- 1
  bredband_utskrift$fokus[bredband_utskrift$Kommun =="Riket"] <- 2
  
  if(diag_bredband_facet==TRUE){
    diagramtitel <- paste0("Andel hushåll med tillgång till 1 Gbit/s bredband i absolut närhet (Homes Passed) oktober 2021")
    diagramtitel<-str_wrap(diagramtitel,50)
    diagramfilnamn <- paste0("bredband_homespassed.png")
    objektnamn <- c(objektnamn,diagramtitel)
    
    gg_obj <- SkapaStapelDiagram(skickad_df =bredband_utskrift %>% 
                                   filter(Ar==max(bredband_utskrift$Ar),Variabel=="Hushåll"),
                                 skickad_x_var = "Kommun", 
                                 skickad_y_var = "Andel", 
                                 #skickad_x_grupp = "Variabel",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = diagramfarger("gron_tre_fokus"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 manual_y_axis_title = "procent",
                                 diagram_facet = TRUE,
                                 facet_scale = "fixed",
                                 facet_grp = "omrade",
                                 facet_legend_bottom = TRUE,
                                 x_axis_lutning = 45,
                                 x_axis_sort_value = TRUE,
                                 x_var_fokus = "fokus",
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  names(gg_list)<-objektnamn
  return(gg_list)
}
