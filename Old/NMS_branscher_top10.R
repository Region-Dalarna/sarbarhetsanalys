# Data från Mona med bland annat bakgrund och utbildning på branschnivå

pacman::p_load(openxlsx,here,tidyverse)

# Skript som behövs
source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)

#test_list=diag_ranking(skapa_fil=FALSE)
diag_ranking <- function(region_vekt="20",
                         output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                         skapa_fil=TRUE,
                         diag_rank_syss_lan=TRUE,
                         diag_rank_andel_lan=TRUE){
  
  # ========================================== Inställningar ============================================
  # Text till diagram
  diagram_capt <- c("Källa: SCB.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Branscher enligt SNI2007 på 2-siffernivå" )
  ValdGeografi <- hamtaregion_kod_namn(region_vekt)$region
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  objektnamn<-c()
  i=1 # Räknare som används för att lägga till objekt i listan
  
  # ========================================== Läser in data ============================================
  # Läser in data från Excel (ursprung arbetsförmedlingen)
  inlas_df <- read.xlsx("G:/skript/projekt/Sårbarhetsanalys/Indata/11_nov_22_ranking_branscher.xlsx")

  # Vissa justeringar av data
  inlas_df<-inlas_df %>% 
    rename(Branschgrupp=AstSNI2007_namn.y)
  
  #############################################
  ################# LÄN #######################
  #############################################
  
  # Ranking och därefter summering på länsnivå bland utvalda branscher
  # Väljer de 10 största branscherna baserat på länsnivå (och beräknar hur stor andel av sysselsatta dessa har)
  inlas_df_lan_10<-inlas_df %>% 
    group_by(Ar,Branschgrupp) %>%
      summarize(Antal_syss=sum(Antal_syss)) %>%
        mutate(Andel_syss=Antal_syss/sum(Antal_syss)) %>% 
          slice_max(Antal_syss,n=10)
  
  # Lägger dessa 10 branscher i en vektor
  valda_branscher<-unique(inlas_df_lan_10$Branschgrupp)
  
  # Beräknar andelar för ett antal variabler och slår sedan ihop dessa
  inlas_df_lan_10_Pivot<-inlas_df_lan_10 %>%
    select(1:2,4) %>% 
      pivot_longer(3,names_to="Variabel",values_to="andel")
  
  inlas_df_lan_10_Pivot$andel<-inlas_df_lan_10_Pivot$andel*100
  
  # Kön
  lan_kon_df<-inlas_df %>% 
    group_by(Ar,Branschgrupp,Kon_namn) %>% 
      summarize(Antal_syss=sum(Antal_syss)) %>% 
        mutate(andel=Antal_syss/sum(Antal_syss)*100) %>% 
          filter(Branschgrupp%in%valda_branscher,Kon_namn=="kvinna") %>% 
            rename("Variabel"=Kon_namn) %>% 
              select(1:3,5)
  
  lan_kon_df$Variabel<-as.character(lan_kon_df$Variabel)
  
  # Utbildning
  lan_utb_df<-inlas_df %>% 
    group_by(Ar,Branschgrupp,utbildning) %>% 
      summarize(Antal_syss=sum(Antal_syss)) %>% 
        mutate(andel=Antal_syss/sum(Antal_syss)*100) %>% 
          filter(Branschgrupp%in%valda_branscher,utbildning=="Högskoleutbildade") %>% 
            rename("Variabel"=utbildning) %>% 
              select(1:3,5)
  
  # Bakgrund
  lan_bakgrund_df<-inlas_df %>% 
    group_by(Ar,Branschgrupp,bakgrund) %>% 
      summarize(Antal_syss=sum(Antal_syss)) %>% 
        mutate(andel=Antal_syss/sum(Antal_syss)*100) %>% 
          filter(Branschgrupp%in%valda_branscher,bakgrund=="Utrikes födda") %>% 
            rename("Variabel"=bakgrund)%>% 
              select(1:3,5)
  
  # Slår ihop de tre
  
  lan_utskrift<-rbind(inlas_df_lan_10_Pivot,lan_kon_df,lan_utb_df,lan_bakgrund_df)
  
  lan_utskrift[lan_utskrift=="Andel_syss"]="Andel av förvärvsarbetande"
  lan_utskrift[lan_utskrift=="kvinna"]="Andel kvinnor"
  lan_utskrift[lan_utskrift=="Utrikes födda"]="Andel utrikes födda"
  lan_utskrift[lan_utskrift=="Högskoleutbildade"]="Andel högskoleutbildade"
  
  lan_utskrift$Variabel <- factor( lan_utskrift$Variabel, levels = c("Andel av förvärvsarbetande","Andel kvinnor","Andel utrikes födda","Andel högskoleutbildade")[4:1])
  
  if(diag_rank_andel_lan==TRUE){
    
    diagramtitel <- paste0("De 10 största branscherna i Dalarnas län inom det privata näringslivet, per antal förvärvsarbetande")
    diagramfilnamn <- paste0("rank_10_sysselsatt.png")
    objektnamn <- c(objektnamn,diagramtitel)
    
    gg_obj <- SkapaStapelDiagram(skickad_df =inlas_df_lan_10,
                                 skickad_x_var = "Branschgrupp", 
                                 skickad_y_var = "Antal_syss",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = diagramfarger("gron_sex")[6],
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt[1],
                                 manual_y_axis_title = "Antal förvärvsarbetande",
                                 diagram_facet = FALSE,
                                 diagram_liggande = TRUE,
                                 x_axis_lutning = 0,
                                 x_axis_sort_value = TRUE,
                                 vand_sortering = FALSE,
                                 legend_vand_ordning = TRUE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  
  if(diag_rank_andel_lan==TRUE){
    
    diagramtitel <- paste0("De 10 största branscherna i Dalarnas län inom det privata näringslivet")
    diagramfilnamn <- paste0("rank_10_andelar.png")
    objektnamn <- c(objektnamn,diagramtitel)
    
    gg_obj <- SkapaStapelDiagram(skickad_df =lan_utskrift,
                                 skickad_x_var = "Branschgrupp", 
                                 skickad_y_var = "andel", 
                                 skickad_x_grupp = "Variabel",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = diagramfarger("gron_sex")[3:6],
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt[1],
                                 manual_y_axis_title = "Procent",
                                 diagram_facet = FALSE,
                                 diagram_liggande = TRUE,
                                 x_axis_lutning = 0,
                                 x_axis_sort_value = TRUE,
                                 x_axis_sort_grp =4,
                                 vand_sortering = TRUE,
                                 legend_vand_ordning = TRUE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  names(gg_list)<-objektnamn
  return(gg_list)
}
