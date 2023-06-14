# Tar fram diverse olika diagram kopplat till företagande
# https://www.foretagsklimat.se/downloads

pacman::p_load(openxlsx,here,tidyverse)

# Skript som behövs
source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)

#test_list=diag_ranking(skapa_fil=FALSE)
diag_ranking <- function(region_vekt="20",
                         output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                         skapa_fil=TRUE,
                         diag_arbetsstallen=TRUE,
                         diag_nyforetagsamma=TRUE,
                         diag_foretagsamma=TRUE){
  
  # ========================================== Inställningar ============================================
  # Text till diagram
  diagram_capt <- c("Källa: SCB.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Gäller arbetsställen med minst 1 anställd",
                    "Källa: UC AB.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Nyföretagsamma personer är sådana som inte klassades som företagsamma förra året, men som gör det under innevarande år.",
                    "Källa: UC, Kreicbergs Utredning & Opinion AB\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: De invånare som innehar F-skattesedel, är delägare i ett aktivt handelsbolag\neller är vd eller styrelseordförande i ett aktivt aktiebolag räknas som företagsamma.\n")
  ValdGeografi <- hamtaregion_kod_namn(region_vekt)$region
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  objektnamn<-c()
  i=1 # Räknare som används för att lägga till objekt i listan
  
  # ========================================== Läser in data ============================================
  # Läser in data från Excel (ursprung arbetsförmedlingen)
  # arbetsstallen_df <- read.xlsx("G:/skript/projekt/Sårbarhetsanalys/Indata/Arbetsställen_2002-2023.xlsx",startRow=5)
  # nyforetagsamma_df <- read.xlsx("G:/skript/projekt/Sårbarhetsanalys/Indata/Nyföretagsamhet_2002-2023.xlsx",startRow=5)
  # foretagsamma_df <- read.xlsx("G:/skript/projekt/Sårbarhetsanalys/Indata/Företagsamhet_2002-2023.xlsx",startRow=5)
  
  # Läser in data från Excel (ursprung arbetsförmedlingen)
  arbetsstallen_df <- read.xlsx("G:/skript/data/sarbarhetsanalys/Indata/Arbetsställen_2002-2023.xlsx",startRow=5)
  nyforetagsamma_df <- read.xlsx("G:/skript/data/sarbarhetsanalys/Indata/Nyföretagsamhet_2002-2023.xlsx",startRow=5)
  foretagsamma_df <- read.xlsx("G:/skript/data/sarbarhetsanalys/Indata/Företagsamhet_2002-2023.xlsx",startRow=5)
  
  

  # ARBETSSTÄLLEN
  
  # Pivoterar data och för vissa justeringar
  arbetsstallen_df<-arbetsstallen_df %>%
    select(-3) %>% 
      pivot_longer(3:(ncol(arbetsstallen_df)-1),names_to="year",values_to = "Arb_stallen") %>% 
       rename("Kategorier"=Delserie.1)
  
  # Tar ut Dalarnas kommuner
  Dalarnas_kommuner=hamtaregion_kod_namn(hamtakommuner(tamedlan = FALSE,tamedriket = FALSE))[2]
  # Filtrerar ut Dalarnas kommuner
  
  arbetsstallen_df_Dalarna<-arbetsstallen_df %>% 
    filter(Kommun%in%Dalarnas_kommuner$region,Kategorier=="Minst en anställd",year%in%max(year))
  
  # Beräknar genomsnittet för Dalarna
  arbetsstallen_df_Dalarna_sum<-arbetsstallen_df_Dalarna %>%
      group_by(year,Kategorier) %>%
        summarize(Arb_stallen=mean(Arb_stallen)) %>% 
          mutate("Kommun"="Dalarna")
  
  # Beräknar genomsnittet för riket
  arbetsstallen_df_riket<-arbetsstallen_df %>%
    filter(Kategorier=="Minst en anställd",year==max(year)) %>%
      group_by(year,Kategorier) %>%
        summarize(Arb_stallen=mean(Arb_stallen)) %>% 
          mutate("Kommun"="Riket")
  
  arbetsstallen_df_utskrift<-rbind(arbetsstallen_df_Dalarna,arbetsstallen_df_Dalarna_sum,arbetsstallen_df_riket)
  
  # NYFÖRETAGSAMMA
  
  # Pivoterar data och gör vissa justeringar
  nyforetagsamma_df<-nyforetagsamma_df %>%
    select(-3) %>% 
      pivot_longer(3:(ncol(nyforetagsamma_df)-1),names_to="year",values_to = "Antal_nyforetagsamma") %>% 
        rename("Kategorier"=Delserie.1)
  
  # Filtrerar ut Dalarnas kommuner
  nyforetagsamma_df_Dalarna<-nyforetagsamma_df %>% 
    filter(Kommun%in%Dalarnas_kommuner$region,Kategorier== "Antal nyföretagsamma individer",year%in%max(year))
  
  # Beräknar genomsnittet för Dalarna
  nyforetagsamma_df_Dalarna_sum<-nyforetagsamma_df_Dalarna %>%
    group_by(year,Kategorier) %>%
      summarize(Antal_nyforetagsamma=mean(Antal_nyforetagsamma)) %>% 
        mutate("Kommun"="Dalarna")
  
  # Beräknar genomsnittet för riket
  nyforetagsamma_df_riket<-nyforetagsamma_df %>%
    filter(Kategorier== "Antal nyföretagsamma individer",year%in%max(year)) %>% 
      group_by(year,Kategorier) %>%
        summarize(Antal_nyforetagsamma=mean(Antal_nyforetagsamma)) %>% 
          mutate("Kommun"="Riket")
  
  nyforetagsamma_df_utskrift<-rbind(nyforetagsamma_df_Dalarna,nyforetagsamma_df_Dalarna_sum,nyforetagsamma_df_riket)
  
  # Företagssamma individer
  
  # Pivoterar data och gör vissa justeringar
  foretagsamma_df<-foretagsamma_df %>%
    select(-3) %>% 
      pivot_longer(3:(ncol(foretagsamma_df)-1),names_to="year",values_to = "Andel_foretagsamma") %>% 
        rename("Kategorier"=Delserie.1)
  
  # Filtrerar ut Dalarnas kommuner
  foretagsamma_df_Dalarna<-foretagsamma_df %>% 
    filter(Kommun%in%Dalarnas_kommuner$region,Kategorier== "Andel företagsamma individer",year%in%max(year))
  
  # Beräknar inte genomsnitt för Dalarna eller riket då det inte är rimligt vid andelar.
  # Beräknar genomsnittet för Dalarna
  # foretagsamma_df_Dalarna_sum<-foretagsamma_df_Dalarna %>%
  #   group_by(year,Kategorier) %>%
  #     summarize(Antal_foretagsamma=mean(Andel_foretagsamma)) %>% 
  #       mutate("Kommun"="Dalarna")
  # 
  # # Beräknar genomsnittet för riket
  # foretagsamma_df_riket<-foretagsamma_df %>%
  #   filter(Kategorier== "Antal företagsamma individer",year%in%max(year)) %>% 
  #     group_by(year,Kategorier) %>%
  #       summarize(Antal_foretagsamma=mean(Antal_foretagsamma)) %>% 
  #         mutate("Kommun"="Riket")
  # 
  # foretagsamma_df_utskrift<-rbind(foretagsamma_df_Dalarna,foretagsamma_df_Dalarna_sum,foretagsamma_df_riket)
  
  # # Ranking företagsklimat
  # # Gör vissa justeringar av dataset (se förklaringar under tidigare variabler)
  # foretagsklimat_df<-foretagsklimat_df %>% 
  #   select(1,4) %>%
  #     filter(Kommun%in%Dalarnas_kommuner$region) %>% 
  #       rename("Ranking"="2022")
  
  
  # skapa fokusvariabel för att fokusera på valt län och riket
  arbetsstallen_df_utskrift$fokus <- NA                      # en metod för att få bort warning messages för "Unknown or uninitialised column: `fokus`."
  arbetsstallen_df_utskrift$fokus <- 0
  arbetsstallen_df_utskrift$fokus[arbetsstallen_df_utskrift$Kommun =="Dalarna"] <- 1
  arbetsstallen_df_utskrift$fokus[arbetsstallen_df_utskrift$Kommun =="Riket"] <- 2
  
  if(diag_arbetsstallen==TRUE){
    diagramtitel <- paste0("Antal privata arbetsställen per 1000 invånare år ",max(arbetsstallen_df_utskrift$year))
    diagramfilnamn <- paste0("arbetsstallen_per_1000.png")
    objektnamn <- c(objektnamn,diagramtitel)
    
    gg_obj <- SkapaStapelDiagram(skickad_df =arbetsstallen_df_utskrift %>% 
                                   mutate(Kommun=ifelse(Kommun=="Riket", "Sverige",Kommun)),
                                 skickad_x_var = "Kommun", 
                                 skickad_y_var = "Arb_stallen", 
                                 #skickad_x_grupp = "utb_niva",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = diagramfarger("rus_tre_fokus"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt[1],
                                 manual_y_axis_title = "Antal arbetsställen per 1000 invånare",
                                 diagram_facet = FALSE,
                                 x_var_fokus = "fokus",
                                 x_axis_lutning = 45,
                                 x_axis_sort_value = TRUE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  # skapa fokusvariabel för att fokusera på valt län och riket
  nyforetagsamma_df_utskrift$fokus <- NA                      # en metod för att få bort warning messages för "Unknown or uninitialised column: `fokus`."
  nyforetagsamma_df_utskrift$fokus <- 0
  nyforetagsamma_df_utskrift$fokus[nyforetagsamma_df_utskrift$Kommun =="Dalarna"] <- 1
  nyforetagsamma_df_utskrift$fokus[nyforetagsamma_df_utskrift$Kommun =="Riket"] <- 2
  
  if(diag_nyforetagsamma==TRUE){
    diagramtitel <- paste0("Antal nyföretagsamma per 1000 invånare i Dalarnas län ",max(nyforetagsamma_df_utskrift$year))
    diagramfilnamn <- paste0("nyforetagsamma_per_1000.png")
    objektnamn <- c(objektnamn,diagramtitel)
    
    gg_obj <- SkapaStapelDiagram(skickad_df =nyforetagsamma_df_utskrift %>% 
                                   mutate(Kommun=ifelse(Kommun=="Riket", "Sverige",Kommun)),
                                 skickad_x_var = "Kommun", 
                                 skickad_y_var = "Antal_nyforetagsamma", 
                                 #skickad_x_grupp = "utb_niva",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = diagramfarger("rus_tre_fokus"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt[2],
                                 manual_y_axis_title = "Antal nyföretagsamma per 1000 invånare",
                                 diagram_facet = FALSE,
                                 x_axis_lutning = 45,
                                 x_var_fokus = "fokus",
                                 x_axis_sort_value = TRUE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  # # skapa fokusvariabel för att fokusera på valt län och riket
  # foretagsamma_df_utskrift$fokus <- NA                      # en metod för att få bort warning messages för "Unknown or uninitialised column: `fokus`."
  # foretagsamma_df_utskrift$fokus <- 0
  # foretagsamma_df_utskrift$fokus[foretagsamma_df_utskrift$Kommun =="Dalarna"] <- 1
  # foretagsamma_df_utskrift$fokus[foretagsamma_df_utskrift$Kommun =="Riket"] <- 2
  
  if(diag_foretagsamma==TRUE){
    diagramtitel <- paste0("Andel företagsamma ",max(foretagsamma_df_Dalarna$year))
    diagramfilnamn <- paste0("foretagsamma_andel.png")
    objektnamn <- c(objektnamn,diagramtitel)
    
    gg_obj <- SkapaStapelDiagram(skickad_df =foretagsamma_df_Dalarna %>% 
                                   mutate(Kommun=ifelse(Kommun=="Riket", "Sverige",Kommun)),
                                 skickad_x_var = "Kommun", 
                                 skickad_y_var = "Andel_foretagsamma", 
                                 #skickad_x_grupp = "utb_niva",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = diagramfarger("rus_sex")[1],
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt[3],
                                 manual_y_axis_title = "procent",
                                 diagram_facet = FALSE,
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
