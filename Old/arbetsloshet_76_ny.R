hamta_data_arbetsloshet_76 <- function(region_vekt = c("00","20"), # Enbart för regioner och riket ("00")
                                       output_mapp = NA,
                                       filnamn = "arbetsloshet_76.xlsx",
                                       Kon_klartext = c("män","kvinnor"), # Alternativ är män och/eller kvinnor (totalt saknas)
                                       returnera_data = TRUE # Om man vill att data skall returneras
){
  
  
  # =================================================================================================================
  # Tar fram antal arbetslösa, antal sysselsatta och arbetslöshet från 1976 till senaste år (AKU - SCB). Enbart på länsnivå
  # Notera även att det inte går att välja arbetskraftstillhörighet. Detta eftersom kategorierna varierar över tiden
  #
  # Källor: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0401__AM0401L/NAKUBefolkningLAr/
  #  https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0402__AM0402F/AKUABefolkningL/
  #
  # Uppdaterad med nya PXweb - Jon 2026-07-02
  # =================================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         tidyverse,
         openxlsx)
  
  # Funktioner som behövs
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_pxweb2.R")
  
  # Till och med 04
  df_76_04 <- pxweb2_hamta_data(
    tabell = "TAB88",
    query = list(
      Region = region_vekt,
      Kon = Kon_klartext,
      Arbetskraftstillh = c("SYS","ALÖS"),
      ContentsCode = "*",
      Tid = "*")) %>% 
        rename(Antal = value) %>% 
          pivot_wider(names_from = arbetskraftstillhörighet,values_from=Antal ) %>% 
            mutate(arbetsloshet = (arbetslösa/(arbetslösa+sysselsatta))*100) %>% 
              select(-tabellinnehåll,-region_kod)

  # Från 05
  df_05_ <- pxweb2_hamta_data(
    tabell = "TAB2922",
    query = list(
      Region = region_vekt,
      Arbetskraftstillh = c("ALÖS","SYS"),
      Kon = Kon_klartext,
      ContentsCode = "1000-tal",
      Tid = "*"
    )) %>% 
        rename(Antal = value) %>% 
          pivot_wider(names_from = arbetskraftstillhörighet,values_from=Antal ) %>% 
            mutate(arbetsloshet = (arbetslösa/(arbetslösa+sysselsatta))*100) %>% 
              select(-tabellinnehåll,-region_kod)
  
  df_bef_slutgiltig <- rbind(df_76_04,df_05_) %>% 
    mutate(region = skapa_kortnamn_lan(region,byt_ut_riket_mot_sverige = TRUE))
  
  
  # Sparar till Excel
  if (!is.na(output_mapp) & !is.na(filnamn)){
    flik_lista=lst("Arbetslöshet" = df_bef_slutgiltig)
    write.xlsx(flik_lista,paste0(output_mapp,filnamn))
  }
  
  if(returnera_data == TRUE) return(df_bef_slutgiltig)
  
}
