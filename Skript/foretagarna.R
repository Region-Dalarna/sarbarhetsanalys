hamta_data_foretagarna <- function(region_vekt="20",
                                   output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                   filnamn = "foretagarna.xlsx",
                                   spara_data = TRUE){
  
  
  # Tar fram diverse olika diagram kopplat till företagande
  # https://www.foretagsklimat.se/downloads
  # Välj variabel av intresse under "gör dina urval per faktor"
  # Senast uppdaterad (data) - 20250113
  # Data för företagssamma, fortfarande 2022 som max
  
  
  pacman::p_load(openxlsx,here,tidyverse)
  
  # Skript som behövs
  source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
  source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)
  
  # ========================================== Läser in data ============================================
  # Läser in data från Excel (ursprung Företagarna (se ovan för länk till nedladdning))
  
  arbetsstallen_df <- read.xlsx("G:/skript/projekt/data/sarbarhetsanalys/Arbetsställen_2002-2025.xlsx",startRow=5)
  nyforetagsamma_df <- read.xlsx("G:/skript/projekt/data/sarbarhetsanalys/Nyföretagsamhet_2002-2025.xlsx",startRow=5)
  foretagsamma_df <- read.xlsx("G:/skript/projekt/data/sarbarhetsanalys/Företagsamhet_2002-2025.xlsx",startRow=5)
  
  # ARBETSSTÄLLEN
  
  # Pivoterar data och för vissa justeringar
  arbetsstallen_df<-arbetsstallen_df %>%
    select(-3) %>% 
      pivot_longer(3:(ncol(arbetsstallen_df)-1),names_to="year",values_to = "Arb_stallen") %>% 
        rename("Kategorier"=Delserie.1)
  
  # Tar ut Dalarnas kommuner
  Dalarnas_kommuner=hamtaregion_kod_namn(hamtakommuner(lan =region_vekt, tamedlan = FALSE,tamedriket = FALSE))[2]
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
  
  flik_lista = lst("arbetsställen" = arbetsstallen_df_utskrift,
                   "nyföretagssamma" = nyforetagsamma_df_utskrift,
                   "företagssamma" = foretagsamma_df_Dalarna)
  
  if (spara_data==TRUE){
    write.xlsx(flik_lista,paste0(output_mapp,filnamn))
  }

}
