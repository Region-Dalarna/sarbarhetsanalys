
# Funktioner som sourcas från Region Dalarna
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

#test_list=diag_utbniva(spara_figur = FALSE,uppdelning_kon = FALSE)
diag_utbniva <- function(region_vekt = hamtakommuner("20",tamedlan = TRUE,tamedriket = TRUE), # Använd förslagsvis hamtakommuner eller hamtaallalan
                         lan_fokus = "20", # Vilket län vill man fokusera på (i diagrammet)
                         alder = c(as.character(25:64)), # Välj ett intervall (inte "*"). 16-74 är möjligt
                         uppdelning_kon = FALSE, 
                         valda_farger = diagramfarger("rus_tre_fokus"),
                         valda_farger_kon = diagramfarger("kon"),
                         output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/", # Outputmapp
                         spara_figur = TRUE,
                         returnera_data = TRUE){
  
  # ===========================================================================================================
  #
  # Diagram som tar fram andelen av befolkningen med minst 3 års eftergymnasial utbildning
  # Finns uppdelat på kön men bara senaste år
  # Skapad av Jon Frank
  # Uppdaterad senast 2023-12-15
  # ===========================================================================================================
  
  gg_list = list()
  objektnamn = c()

  # ========================================== Läser in data ============================================
  # Skapa en lista med information som vi vill ha hem -- skapas lättast via pxweb_interactive()
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_utbniva_SCB.R")
  df <- hamta_data_utbniva (region = region_vekt,
                            alder = alder,
                            utbildningsniva_klartext = "*",
                            tid = "9999", # "Om man enbart vill ha senaste år"9999" om man enbart vill ha senaste år. Välj ett högt värde som sista värde om alla år skall vara med.
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
  
  assign("df_ut", df, envir = .GlobalEnv)

  if(uppdelning_kon == TRUE){
    variabler = c("år","kön","region","utbildningsnivå")
    valda_farger = valda_farger_kon
    
  } else{
    variabler = c("år","region","utbildningsnivå")
    valda_farger = valda_farger
  } 
  
  df_utskrift <- df %>%
    filter(utbildningsnivå != "Uppgift saknas") %>% 
      group_by(across(all_of(variabler))) %>%
        summarize(antal = sum(Befolkning)) %>%
          mutate(andel = (antal/sum(antal))*100) %>% 
            mutate(region = skapa_kortnamn_lan(region,byt_ut_riket_mot_sverige = TRUE)) %>% 
              ungroup()
 
  if(returnera_data == TRUE){
    assign("andel_hogutbildade", df_utskrift, envir = .GlobalEnv)
  }

  diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."
  diagramtitel <- paste0("Andel av befolkningen (",min(df$ålder), " - ", max(df$ålder),") med minst 3 års eftergymnasial utbildning ",unique(df_utskrift$år))
  diagramtitel <- str_wrap(diagramtitel,50)
  diagramfilnamn <- paste0("andel_hogutbildade.png")
  objektnamn <- c(objektnamn, "andel_hogutbildade")

  gg_obj <- SkapaStapelDiagram(skickad_df = df_utskrift %>%
                                 filter(utbildningsnivå == "Eftergymnasial utbildning, 3 år eller mer") %>% 
                                   mutate(fokus = ifelse(region == "Sverige", 2,
                                                         ifelse(region == skapa_kortnamn_lan(hamtaregion_kod_namn(lan_fokus)$region),1,0))), 
                               skickad_x_var = "region",
                               skickad_y_var = "andel",
                               skickad_x_grupp = ifelse(uppdelning_kon == TRUE,"kön",NA),
                               manual_x_axis_text_vjust = 1,
                               manual_x_axis_text_hjust = 1,
                               manual_color = valda_farger,
                               diagram_titel = diagramtitel,
                               diagram_capt =  diagram_capt,
                               diagram_facet = FALSE,
                               x_axis_sort_value = TRUE,
                               x_axis_lutning = 45,
                               x_var_fokus = NA,
                               legend_vand_ordning = FALSE,
                               manual_y_axis_title = "procent",
                               output_mapp = output_mapp,
                               filnamn_diagram = diagramfilnamn,
                               skriv_till_diagramfil = spara_figur)

  gg_list <- c(gg_list, list(gg_obj))
  
  names(gg_list) <- c(objektnamn)
  return(gg_list)
  
  
  
}
