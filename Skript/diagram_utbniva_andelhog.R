
# Funktioner som sourcas från Region Dalarna
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

#test_list=diag_utbniva(skapa_fil=FALSE)
diag_utbniva <- function(region = hamtakommuner("20",tamedlan = TRUE,tamedriket = TRUE), # Använd förslagsvis hamtakommuner eller hamtaallalan
                         alder = c(as.character(25:64)), # antingen "tot16-74" eller annat intervall, exempelvis c(as.character(25:64)), "*" ger alla år
                         utbildningsniva_klartext = "*", # För alternativ, se text nedan
                         kon_klartext = c("män","kvinnor"), # c("män","kvinnor") eller var och en uppdelad. "*" funkar inte
                         output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/", # Outputmapp
                         returnera_data = TRUE, # Om man vill returnera data
                         tid ="9999" # Sätts till "9999" om man enbart vill ha senaste år,"*" för alla alternativt intervall
){
  
  # ===========================================================================================================
  #
  # Skript för att hämta data för utbildningsnivå 
  # Källa https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__UF__UF0506__UF0506B/UtbSUNBef/
  # Förklaring av vissa variabler
  # utbildningsniva_klartext:
  #   förgymnasial utbildning kortare än 9 år
  #   förgymnasial utbildning, 9 (10) år
  #   gymnasial utbildning, högst 2 år
  #   gymnasial utbildning, 3 år
  #   eftergymnasial utbildning, mindre än 3 år
  #   eftergymnasial utbildning, 3 år eller mer
  #   forskarutbildning
  #   uppgift om utbildningsnivå saknas
  # Generellt gäller "*" om man vill ha alla variabler
  # Skapad av Jon Frank
  # Uppdaterad senast 2023-12-14
  # ===========================================================================================================
  

  # ========================================== Läser in data ============================================
  # Skapa en lista med information som vi vill ha hem -- skapas lättast via pxweb_interactive()
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_utbniva_SCB.R")
  df <- hamta_data_utbniva (region = region,
                            tid = tid, # "Om man enbart vill ha senaste år"9999" om man enbart vill ha senaste år. Välj ett högt värde som sista värde om alla år skall vara med.
                            spara_data = FALSE, # Om man vill spara data
                            returnera_data = TRUE)
  
  df <- df %>% 
    mutate(utbildningsnivå = case_when(
      utbildningsnivå == "förgymnasial utbildning kortare än 9 år" ~ "Förgymnasial utbildning",
      utbildningsnivå == "förgymnasial utbildning, 9 (10) år" ~ "Förgymnasial utbildning",
      utbildningsnivå == "gymnasial utbildning, högst 2 år" ~ "Gymnasial utbildning",
      utbildningsnivå == "gymnasial utbildning, 3 år" ~ "Gymnasial utbildning",
      utbildningsnivå == "eftergymnasial utbildning, mindre än 3 år" ~ "Eftergymnasial utbildning, mindre än 3 år",
      utbildningsnivå == "eftergymnasial utbildning, 3 år eller mer"~ "Eftergymnasial utbildning, 3 år eller mer",
      utbildningsnivå == "forskarutbildning" ~ "Eftergymnasial utbildning, 3 år eller mer",
      utbildningsnivå == "uppgift om utbildningsnivå saknas" ~ "Uppgift saknas"))

  # Tar bort uppgift saknas och beräknar hur stor andel som har en viss utbildning - ej uppdelat på kön
  df_utskrift <- df %>%
    filter(utbildningsnivå != "Uppgift saknas") %>%
      group_by(år,region,utbildningsnivå) %>%
        summarize(antal = sum(Befolkning)) %>%
          mutate(andel = (antal/sum(antal))*100)

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
