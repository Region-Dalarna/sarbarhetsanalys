hamta_data_andel_offentligt <- function(region_vekt = "20",
                                        output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                        filnamn = "andel_offentligt.xlsx",
                                        spara_data = TRUE,
                                        artal = "senaste år", # Välj årtal. Senaste år ger just det.
                                        ta_med_lan = TRUE,
                                        ta_med_riket = TRUE){
  
  # Skript som beräknar andelen av de förvärvsarbetande som arbetar inom offentlig sektor.
  
  # https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0207__AM0207Z/DagSektAldKN/

  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         tidyverse,
         openxlsx)
  
  source("https://raw.githubusercontent.com/FaluPeppe/func/main/func_API.R")
  
  # =============================================== API-uttag ===============================================
  # "Adresser" till SCBs databas
  url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0207/AM0207Z/DagSektAldKN"
  
  if(artal == "senaste år") artal = max(hamta_giltiga_varden_fran_tabell(url_uttag, "tid"))
  
  varlista <- list(Region=hamtakommuner(lan = region_vekt,tamedlan = ta_med_lan,tamedriket = ta_med_riket),
                   ArbetsSektor = "*",
                   Alder = "*",
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
          summarize("Förvärvsarbetande"=sum(`Förvärvsarbetande 16-74 år med arbetsplats i regionen (dagbefolkning) (RAMS)`)) %>% 
            ungroup()
  
  # Beräknar andelar      
  antal_sektor_df_utskrift <- antal_sektor_df %>%
    group_by(region,år) %>% 
      mutate(Andel_forv=(Förvärvsarbetande/sum(Förvärvsarbetande)*100)-0.01,
             region = region %>% skapa_kortnamn_lan()) %>% 
        rename(sektor = `arbetsställets sektortillhörighet`) %>% 
          ungroup()
  
  if (spara_data==TRUE){
    write.xlsx(antal_sektor_df_utskrift,paste0(output_mapp,filnamn))
  }
}


