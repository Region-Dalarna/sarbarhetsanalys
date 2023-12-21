# Beräknar andelen med eftergymnasial utbildning för senaste år. Användaren kan välja om man vill eller inte vill dela upp på kön
# Källa https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__UF__UF0506__UF0506B/UtbSUNBef/
pacman::p_load(tidyverse,httr,pxweb)

# Skript som behövs
source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)

#test_list=diag_utbniva(skapa_fil=FALSE)
diag_utbniva <- function(region_vekt="20",
                         output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                         valda_farger =diagramfarger("rus_tre_fokus"), 
                         skapa_fil=TRUE,
                         diag_utb_kommun=TRUE,
                         diag_utb_kommun_kon=TRUE){
  
  # ========================================== Inställningar ============================================
  # Text till diagram
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."
  
  ValdGeografi <- hamtaregion_kod_namn(region_vekt)$region
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  i=1 # Räknare som används för att lägga till objekt i listan
  objektnamn <- c()

    # ========================================== Läser in data ============================================
  # Skapa en lista med information som vi vill ha hem -- skapas lättast via pxweb_interactive()
  
  url="https://api.scb.se/OV0104/v1/doris/sv/ssd/UF/UF0506/UF0506B/Utbildning"
  pxweb_query_list <- 
    list("Region"=hamtakommuner(region_vekt,tamedlan=TRUE,tamedriket = TRUE),
         "Kon"=c("*"),
         "Alder"=c(as.character(25:64)),
         "UtbildningsNiva"=c("*"),
         "ContentsCode"=c("UF0506A1"),
         "Tid"=max(hamta_giltiga_varden_fran_tabell(url, "tid")))
  
  # Download data 
  px_data <- 
    pxweb_get(url=url,
              query = pxweb_query_list)
  
  # Convert to data.frame 
  px_df <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
  
  px_df$utb_niva<- case_when(
    px_df$utbildningsnivå == "förgymnasial utbildning kortare än 9 år" ~ "Förgymnasial utbildning",
    px_df$utbildningsnivå == "förgymnasial utbildning, 9 (10) år" ~ "Förgymnasial utbildning",
    px_df$utbildningsnivå == "gymnasial utbildning, högst 2 år" ~ "Gymnasial utbildning",
    px_df$utbildningsnivå == "gymnasial utbildning, 3 år" ~ "Gymnasial utbildning",
    px_df$utbildningsnivå == "eftergymnasial utbildning, mindre än 3 år" ~ "Eftergymnasial utbildning, mindre än 3 år",
    px_df$utbildningsnivå == "eftergymnasial utbildning, 3 år eller mer"~ "Eftergymnasial utbildning, 3 år eller mer",
    px_df$utbildningsnivå == "forskarutbildning" ~ "Eftergymnasial utbildning, 3 år eller mer",
    px_df$utbildningsnivå == "uppgift om utbildningsnivå saknas" ~ "Uppgift saknas")
  
  # Tar bort uppgift saknas och beräknar hur stor andel som har en viss utbildning - ej uppdelat på kön
  px_df_utskrift<-px_df %>%
    filter(utb_niva!="Uppgift saknas") %>% 
      group_by(år,region,utb_niva) %>% 
        summarize(antal=sum(Befolkning)) %>% 
          mutate(andel=(antal/sum(antal))*100)
  
  # Tar bort uppgift saknas och beräknar hur stor andel som har en viss utbildning - uppdelat på kön
  px_df_utskrift_kon<-px_df %>%
    filter(utb_niva!="Uppgift saknas") %>% 
      group_by(år,region,kön,utb_niva) %>% 
        summarize(antal=sum(Befolkning)) %>% 
          mutate(andel=(antal/sum(antal))*100)
  
  # Gör om till en faktorvariabel för att styra vilken ordning utbildningsnivåer kommer i.
  px_df_utskrift$utb_niva <- factor(px_df_utskrift$utb_niva, levels = c("Eftergymnasial utbildning, 3 år eller mer","Eftergymnasial utbildning, mindre än 3 år","Gymnasial utbildning","Förgymnasial utbildning"))

  # Ta bort s och län i länsnamn
  px_df_utskrift$region<-skapa_kortnamn_lan(px_df_utskrift$region)
  px_df_utskrift_kon$region<-skapa_kortnamn_lan(px_df_utskrift_kon$region)
  
  px_df_utskrift_fokus <- px_df_utskrift
  # skapa fokusvariabel för att fokusera på valt län och riket
  px_df_utskrift_fokus$fokus <- NA                      # en metod för att få bort warning messages för "Unknown or uninitialised column: `fokus`."
  px_df_utskrift_fokus$fokus <- 0
  px_df_utskrift_fokus$fokus[px_df_utskrift_fokus$region =="Dalarna"] <- 1
  px_df_utskrift_fokus$fokus[px_df_utskrift_fokus$region =="Riket"] <- 2
  
  if(diag_utb_kommun==TRUE){
    diagramtitel <- paste0("Andel av befolkningen (25-64 år) med minst 3 års eftergymnasial utbildning ",unique(px_df_utskrift$år))
    diagramtitel <- str_wrap(diagramtitel)
    diagramfilnamn <- paste0("utbildningsniva_jmf_lan.png")
    objektnamn <- c(objektnamn,diagramtitel)
    
    gg_obj <- SkapaStapelDiagram(skickad_df =px_df_utskrift_fokus %>%
                                   filter(utb_niva=="Eftergymnasial utbildning, 3 år eller mer") %>% 
                                    mutate(region=ifelse(region=="Riket", "Sverige",region)),
                                 skickad_x_var = "region", 
                                 skickad_y_var = "andel", 
                                 #skickad_x_grupp = "utb_niva",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = valda_farger,
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 diagram_facet = FALSE,
                                 x_axis_sort_value = TRUE,
                                 x_axis_lutning = 45,
                                 x_var_fokus = "fokus",
                                 diagram_liggande = FALSE,
                                 legend_vand_ordning=FALSE,
                                 geom_position_stack = FALSE,
                                 manual_y_axis_title="procent",
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  if(diag_utb_kommun_kon==TRUE){
    diagramtitel <- paste0("Andel av befolkningen (25-64 år) med minst 3 års eftergymnasial utbildning ",unique(px_df_utskrift$år))
    diagramtitel <- str_wrap(diagramtitel)
    diagramfilnamn <- paste0("utbildningsniva_jmf_lan_kon.png")
    objektnamn <- c(objektnamn,diagramtitel)
    
    gg_obj <- SkapaStapelDiagram(skickad_df =px_df_utskrift_kon %>%
                                   filter(utb_niva=="Eftergymnasial utbildning, 3 år eller mer")%>% 
                                    mutate(region=ifelse(region=="Riket", "Sverige",region)), 
                                 skickad_x_var = "region", 
                                 skickad_y_var = "andel", 
                                 skickad_x_grupp = "kön",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = diagramfarger("kon"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 diagram_facet = FALSE,
                                 x_axis_sort_value = TRUE,
                                 x_axis_lutning = 45,
                                 #x_var_fokus = "fokus",
                                 diagram_liggande = FALSE,
                                 legend_vand_ordning=FALSE,
                                 geom_position_stack = FALSE,
                                 manual_y_axis_title="procent",
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  names(gg_list)<-c(objektnamn)
  return(gg_list)
  
}
