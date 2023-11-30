# Skript som beräknar andelen inom offentlig sektor för senaste år
# https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0207__AM0207Z/DagSektAldKN/
pacman::p_load(pxweb,httr,tidyverse)

# Laddar in de funktioner som används för att skapa diagram
source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)

#test_list=diag_andel_offentligt(skapa_fil=FALSE)
diag_andel_offentligt <- function(region_vekt="20",
                                  output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                  skapa_fil=TRUE){
  
  # ========================================== Inställningar ============================================
  # Text till diagram
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."

  ValdGeografi <- hamtaregion_kod_namn(region_vekt)$region
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  i=1 # Räknare som används för att lägga till objekt i listan
  objektnamn <- c()
  
  #==========================================================================================================  
  # För att komma förbi proxyn
  set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090, username = Sys.getenv("userid"), password = Sys.getenv("pwd")))
  set_config(config(ssl_verifypeer = 0L))
  # =============================================== API-uttag ===============================================
  # "Adresser" till SCBs databas
  url1 <- "https://api.scb.se"
  url2 <- c("/OV0104/v1/doris/sv/ssd/AM/AM0207/AM0207Z/DagSektAldKN")
  url3 <- paste0(url1,url2)
  
  varlista <- list("Region"=hamtakommuner(lan=region_vekt,tamedriket=TRUE),
                   "ArbetsSektor"=c("*"),
                   "Alder"=c("*"),
                   "Kon"=c("*"),
                   "ContentsCode"=c("00000545"),
                   "Tid"=max(hamta_giltiga_varden_fran_tabell(url3, "tid")))
  
  px_data <- pxweb_get(url = url3,query = varlista)
  
  antal_sektor_df <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
  
  # Klassificerar som Offentlig sektor respektive övriga
  antal_sektor_df$`arbetsställets sektortillhörighet`<-ifelse(antal_sektor_df$`arbetsställets sektortillhörighet`%in%c("statlig förvaltning","statliga affärsverk","primärkommunal förvaltning","regioner","övriga offentliga institutioner"),"Offentlig sektor","Övriga")
  
  # Summerar på region och sektor
  antal_sektor_df_utskrift <- antal_sektor_df %>%
    group_by(region,`arbetsställets sektortillhörighet`,år) %>% 
      summarize("Förvärvsarbetande"=sum(`Förvärvsarbetande 16-74 år med arbetsplats i regionen (dagbefolkning) (RAMS)`))
  
  # Beräknar andelar      
  antal_sektor_df_utskrift<-antal_sektor_df_utskrift %>%
    group_by(region,år) %>% 
      mutate(Andel_forv=(Förvärvsarbetande/sum(Förvärvsarbetande)*100)-0.01)
  
  # Tar bort eventuellt s och län i länsnamn
  antal_sektor_df_utskrift$region<-skapa_kortnamn_lan(antal_sektor_df_utskrift$region)
  
  # Skapar ett diagram för andelen inom offentlig sektor
  diagramtitel <- paste0("Andel offentligt anställda år ",unique(antal_sektor_df_utskrift$år))
  diagramfilnamn <- paste0("andel_offentligt.png")
  objektnamn <- paste0("andel_offentligt")
  
  gg_obj <- SkapaStapelDiagram(skickad_df =antal_sektor_df_utskrift %>% 
                                 mutate("arbetsställets sektortillhörighet"= factor(`arbetsställets sektortillhörighet`, levels = c("Offentlig sektor","Övriga")[2:1]))  %>% 
                                  mutate(region=ifelse(region=="Riket", "Sverige",region)),
                               skickad_x_var = "region", 
                               skickad_y_var = "Andel_forv", 
                               skickad_x_grupp = "arbetsställets sektortillhörighet",
                               manual_x_axis_text_vjust=1,
                               manual_x_axis_text_hjust=1,
                               manual_color = diagramfarger("rus_sex"),
                               diagram_titel = diagramtitel,
                               diagram_capt =  diagram_capt,
                               diagram_facet = FALSE,
                               geom_position_stack = TRUE,
                               legend_vand_ordning = TRUE,
                               x_axis_sort_value = TRUE,
                               x_axis_sort_grp = 1,
                               manual_y_axis_title="procent",
                               berakna_index = FALSE,
                               output_mapp = output_mapp,
                               filnamn_diagram = diagramfilnamn,
                               skriv_till_diagramfil = skapa_fil)
  
  gg_list[[i]] <-gg_obj
  i=i+1
  
  names(gg_list)<-c(objektnamn)
  return(gg_list)
}


