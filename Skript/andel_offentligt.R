hamta_data_andel_offentligt <- function(region = "20",
                                        alla_regioner = FALSE,
                                        alla_kommuner = TRUE, # Enbart om alla_regioner är false och man enbart har valt en region
                                        ta_med_riket = TRUE,
                                        Alder = "*",
                                        output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                        output_mapp_figur= "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                        filnamn = "andel_offentligt.xlsx",
                                        diag_farger = "rus_sex",
                                        spara_data = TRUE,
                                        returnera_data = FALSE, # Om man vill att data skall göras till en global variabel (och hamna i R-Studios enviroment)
                                        diag_linje = FALSE, # Vill man skapa en figur
                                        spara_figur = TRUE, # Sparar figuren till output_mapp_figur
                                        senaste_ar = TRUE, # True om man enbart vill ha senaste år
                                        artal = c("*"), # Välj årtal. Senaste år ger just det.
                                        returnera_figur = FALSE # Om man vill att figuren skall returneras från funktionen
){
  
  # ===========================================================================================================
  #
  # Skript för att hämta data och skapa figurer från SCB för andel offentligt anställda. 
  # 
  # För att få en djupare förklaring av vad som de olika kategorierna under varje variabel betyder, använd: 
  # pxvardelist("https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0207/AM0207Z/DagSektAldKN", "Kon"), 
  # där man byter mot den variabel man är intresserad av.
  # 
  # Generellt gäller c("*) om man vill ha alla variabler
  # Parametrar som skickas med (= variabler i SCB-tabellen) är:
  # - region: Vald region
  # - alla_regioner: Välj om man vill ha alla regioner. Om den är satt till True så skriver den över region ovan.
  # - alla_kommuner: Om man vill ha alla kommuner
  # - Alder: Se ovan (pxvardelist)
  # - outputmapp: Vart skall data sparas
  # - filnamn : Vad skall filen heta
  # - senaste_ar: Sätts till TRUE om man bara vill ha data för senaste år
  # - artal: Vilka år vill man ha? Normalt c("*"), men går även att sätta ett intervall.
  # - returnera_data: True om data skall returneras som en df
  # - spara_till_excel: True om data skall sparas till Excel  
  # ===========================================================================================================

  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         tidyverse,
         openxlsx)
  
  # Data som sourcas från Region Dalarna
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  if(alla_regioner == TRUE){
    region = hamtaAllaLan(tamedriket = FALSE) 
  }
  
  if(alla_kommuner == TRUE){
    region_kommun = hamtakommuner(region,tamedlan = FALSE,tamedriket = FALSE)
    region = c(region,region_kommun)
  }
  
  if(ta_med_riket == TRUE){
    region = c("00",region)
  } 

  # "Adresser" till SCBs databas
  url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0207/AM0207Z/DagSektAldKN"
  
  if(senaste_ar == TRUE) artal = max(hamta_giltiga_varden_fran_tabell(url_uttag, "tid"))
  
  varlista <- list(Region= region,
                   ArbetsSektor = "*",
                   Alder = Alder,
                   Kon= "*",
                   ContentsCode = "00000545",
                   Tid = artal)
  
  px_uttag <- pxweb_get(url = url_uttag,query = varlista)
  
  # Gör uttaget samt diverse justeringar och grupperingar av data.
  antal_sektor_df <- as.data.frame(px_uttag) %>% 
    cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>%
            select(regionkod = Region)) %>%
    relocate(regionkod, .before = region) %>% 
      mutate(`arbetsställets sektortillhörighet` = 
               ifelse(`arbetsställets sektortillhörighet` %in% 
                        c("statlig förvaltning","statliga affärsverk","primärkommunal förvaltning","regioner","övriga offentliga institutioner"),"Offentlig sektor","Övriga")) %>% 
        group_by(regionkod, region,`arbetsställets sektortillhörighet`,år) %>% 
          summarize("Förvärvsarbetande" = sum(`Förvärvsarbetande 16-74 år med arbetsplats i regionen (dagbefolkning) (RAMS)`)) %>% 
            ungroup()
  
  # Beräknar andelar      
  antal_sektor_df_utskrift <- antal_sektor_df %>%
    group_by(region,år) %>% 
      mutate(Andel_forv = (Förvärvsarbetande/sum(Förvärvsarbetande)*100)-0.01,
             region = region %>% skapa_kortnamn_lan()) %>% 
        rename(sektor = `arbetsställets sektortillhörighet`) %>% 
          ungroup()
  
  if (spara_data==TRUE){
    write.xlsx(antal_sektor_df_utskrift,paste0(output_mapp,filnamn))
  }
  
  # Data returneras som en DF om användaren vill det
  if(returnera_data == TRUE) assign("andel_offentligt_df", antal_sektor_df_utskrift, envir = .GlobalEnv)
  
  if(diag_linje==TRUE){
    
    diagram_titel <- paste0("Andel offentligt anställda år ",unique(antal_sektor_df_utskrift$år))
    diagramfilnamn <- "andel_offentligt.png"
    diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."
    
    gg_obj <- SkapaStapelDiagram(skickad_df = antal_sektor_df_utskrift %>% 
                                   mutate("sektor"= factor(`sektor`, levels = c("Offentlig sektor","Övriga")[2:1])) ,
                                 skickad_x_var = "region", 
                                 skickad_y_var = "Andel_forv", 
                                 skickad_x_grupp = "sektor",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = diagramfarger(diag_farger),
                                 stodlinjer_avrunda_fem = TRUE,
                                 geom_position_stack = TRUE,
                                 legend_vand_ordning = TRUE,
                                 x_axis_sort_value = TRUE,
                                 x_axis_sort_grp = 1,
                                 manual_y_axis_title="procent",
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diagram_capt,
                                 output_mapp = output_mapp_figur,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = spara_figur)
    
    if(returnera_figur == TRUE){
      return(gg_obj)
    }
  }
}


