# Skript som beräknar andelen inom offentlig sektor för senaste år
# https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0207__AM0207Z/DagSektAldKN/
pacman::p_load(pxweb,tidyverse)

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
  
  # =============================================== API-uttag ===============================================
  # "Adresser" till SCBs databas
  url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0207/AM0207Z/DagSektAldKN"
   
  varlista <- list(Region=hamtakommuner(lan = region_vekt),
                   ArbetsSektor = "*",
                   Alder = "*",
                   Kon= "*",
                   ContentsCode = "00000545",
                   Tid = max(hamta_giltiga_varden_fran_tabell(url_uttag, "tid")))
  
  px_uttag <- pxweb_get(url = url_uttag,query = varlista)
  
  antal_sektor_df <- as.data.frame(px_uttag, column.name.type = "text", variable.value.type = "text")
  
  antal_sektor_df <- as.data.frame(px_uttag) %>% 
    cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>%
            select(regionkod = Region)) %>%
    relocate(regionkod, .before = region) %>% 
    mutate(`arbetsställets sektortillhörighet` = 
             ifelse(`arbetsställets sektortillhörighet` %in% 
                      c("statlig förvaltning","statliga affärsverk","primärkommunal förvaltning","regioner","övriga offentliga institutioner"),"Offentlig sektor","Övriga")) %>% 
    group_by(regionkod, region,`arbetsställets sektortillhörighet`,år) %>% 
      summarize("Förvärvsarbetande"=sum(`Förvärvsarbetande 16-74 år med arbetsplats i regionen (dagbefolkning) (RAMS)`)) %>% 
    ungroup()
  
  # Beräknar andelar      
  antal_sektor_df_utskrift <- antal_sektor_df %>%
    group_by(region,år) %>% 
      mutate(Andel_forv=(Förvärvsarbetande/sum(Förvärvsarbetande)*100)-0.01,
             region = region %>% skapa_kortnamn_lan()) %>% 
    ungroup() %>% 
    filter(`arbetsställets sektortillhörighet` != "Övriga")
  
  # Skapar ett diagram för andelen inom offentlig sektor
  diagramtitel <- paste0("Andel offentligt anställda år ",unique(antal_sektor_df_utskrift$år))
  diagramfilnamn <- paste0("andel_offentligt.png")
  objektnamn <- paste0("andel_offentligt")
  
  # lägg värde för Sverige i sv_varde, används för att rita en linje över alla staplar istället för stapel
  sv_varde <- antal_sektor_df_utskrift %>% filter(regionkod == "00") %>% select(Andel_forv) %>% pull()
  # lägg värde för valt län i valt_lan_varde, används för att rita en linje över alla staplar istället för stapel
  valt_lan_varde <- antal_sektor_df_utskrift %>% filter(regionkod == region_vekt) %>% select(Andel_forv) %>% pull()
  
  # lägg textbenämning för län och Sverige i sv_txt och valt_lan_txt i kortversioner att använda ovanför deras linjer 
  sv_txt <- antal_sektor_df_utskrift %>% filter(regionkod == "00") %>% select(region) %>% pull() %>% skapa_kortnamn_lan()
  valt_lan_txt <- antal_sektor_df_utskrift %>% filter(regionkod == region_vekt) %>% select(region) %>% pull() %>% skapa_kortnamn_lan()
  
  linjebredd <- 0.1                                                           # tjocklek för linjer
  text_bredd_just <- 2.5
  text_hojd_just <- 0.5
  antal_regioner <- length(unique(antal_sektor_df_utskrift$regionkod))        # används för att lägga texten rätt, ovanför linjerna för riks- och valt läns genomsnitt
  
  sv_valt_lan_list <- list(list(geom = "rect", ymin=valt_lan_varde-linjebredd, ymax=valt_lan_varde+linjebredd, xmin=0, xmax=Inf, alpha=1, fill=diagramfarger("rus_tre_fokus")[2]),
                           list(geom = "text", x = antal_regioner-text_bredd_just, y = valt_lan_varde+(linjebredd+text_hojd_just), size = 2.5, fontface = "bold", angle = 0,label = paste0("Genomsnitt ", valt_lan_txt), color =diagramfarger("rus_tre_fokus")[2]),
                           list(geom = "rect", ymin=sv_varde-linjebredd, ymax=sv_varde+linjebredd, xmin=0, xmax=Inf, alpha=1, fill=diagramfarger("rus_tre_fokus")[3]),
                           list(geom = "text", x = antal_regioner-text_bredd_just, y = sv_varde+(linjebredd+text_hojd_just), size = 2.5, fontface = "bold", angle = 0,label = paste0("Genomsnitt ", sv_txt), color =diagramfarger("rus_tre_fokus")[3]))
  
  gg_obj <- SkapaStapelDiagram(skickad_df =antal_sektor_df_utskrift %>% 
                                 mutate("arbetsställets sektortillhörighet"= factor(`arbetsställets sektortillhörighet`, levels = c("Offentlig sektor","Övriga")[2:1])) %>% 
                                 filter(`arbetsställets sektortillhörighet` != "Övriga",
                                        !regionkod %in% c("00", region_vekt)),
                               skickad_x_var = "region", 
                               skickad_y_var = "Andel_forv", 
                               #skickad_x_grupp = "arbetsställets sektortillhörighet",
                               manual_x_axis_text_vjust=1,
                               manual_x_axis_text_hjust=1,
                               manual_color = diagramfarger("rus_sex"),
                               diagram_titel = diagramtitel,
                               diagram_capt =  diagram_capt,
                               diagram_facet = FALSE,
                               stodlinjer_avrunda_fem = TRUE,
                               #procent_0_100_10intervaller = TRUE,
                               geom_position_stack = TRUE,
                               legend_vand_ordning = TRUE,
                               x_axis_sort_value = TRUE,
                               #x_axis_sort_grp = 1,
                               fokusera_varden = sv_valt_lan_list,
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


