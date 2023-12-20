hamta_data_arbetsloshet_76 <- function(region_vekt = c("00","20"), # Enbart för regioner och riket ("00")
                                       output_mapp = NA,
                                       filnamn = "arbetsloshet_76.xlsx",
                                       Kon_klartext = c("män","kvinnor"), # Alternativ är män och/eller kvinnor (totalt saknas)
                                       returnera_data = TRUE # Om man vill att data skall returneras
                                       ){
  
  
  # =================================================================================================================
  # Tar fram antal arbetslösa, antal sysselsatta och arbetslöshet från 1976 till senaste år (AKU - SCB). Enbart på länsnivå
  # Notera även att det inte går att välja arbetskraftstillhörighet. Detta eftersom kategorierna varierar över tiden
  # Källa  https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0210__AM0210A/ArbStatusM/
  # pxvardelist("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0402/AM0402F/AKUABefolkningL", "Kon")
  # =================================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         tidyverse,
         openxlsx)

  # Funktioner som behövs
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

  # Adresser" till SCBs databas
  url_76_04 <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0402/AM0402F/AKUABefolkningL"
  url_05_ <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0401/AM0401N/NAKUBefolkningLAr"
  
  kon_vekt <- hamta_kod_med_klartext(url_76_04, Kon_klartext, skickad_fran_variabel = "kon")
  
  url <- c(url_76_04,url_05_)
  
  varlista_76_04 <- list("Region" = region_vekt,
                         "Arbetskraftstillh" = c("SYS","ALÖS"),
                         "Kon" = kon_vekt,
                         "ContentsCode" = c("*"),
                         "Tid" = c("*"))
  
  varlista_05_ <- list("Region"= region_vekt,
                         "Arbetskraftstillh"=c("ALÖS","SYS"),
                         "Kon"= kon_vekt,
                         "ContentsCode"=c("*"),
                         "Tid"=c("*"))

  
  varlista_lista = list(varlista_76_04,varlista_05_)
  
  # Loopar över lista med url:er och hämtar data för de olika variablerna. Dessa läggs i en lista
  lista=list()
  j=1
  
  while(j<=(length(url))){
    px_uttag <- pxweb_get(url = url[j],
                          query = varlista_lista[[j]]
    ) 
    
    # Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
    # välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
    
    lista[[j]] <- as.data.frame(px_uttag) %>% 
      cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>%
              select(Region)) %>% 
      rename(regionkod = Region) %>% relocate(regionkod, .before = region)
    
    j=j+1
  }
  
  # Namnger lista
  names(lista) <- c("76_04","05_")
  
  # Beräknar arbetslöshet i två steg för de olika perioderna

  df_76_04 <- lista$`76_04` %>% 
    filter(arbetskraftstillhörighet%in%c("arbetslösa","sysselsatta")) %>% 
      group_by(region,år,kön,arbetskraftstillhörighet) %>% 
        summarize("Antal" = sum(`Befolkningen 16-64 år (AKU), 100-tal`)) %>% 
          pivot_wider(names_from = arbetskraftstillhörighet,values_from=Antal ) %>% 
            ungroup()
  
  df_76_04$arbetsloshet=(df_76_04$arbetslösa/(df_76_04$arbetslösa+df_76_04$sysselsatta))*100
  
  df_05_ <- lista$`05_` %>% 
    filter(arbetskraftstillhörighet%in%c("arbetslösa","sysselsatta")) %>% 
      group_by(region,år,kön,arbetskraftstillhörighet) %>% 
        summarize("Antal" = sum(`1000-tal`))%>% 
          pivot_wider(names_from = arbetskraftstillhörighet,values_from = Antal) %>% 
           ungroup()
  
  df_05_$arbetsloshet=(df_05_$arbetslösa/(df_05_$arbetslösa+df_05_$sysselsatta))*100
  
  #Slår ihop dataset och grupperar på regionkod och år
  df_bef_slutgiltig <- rbind(df_76_04,df_05_) %>% 
    mutate(region = skapa_kortnamn_lan(region,byt_ut_riket_mot_sverige = TRUE))

  # Sparar till Excel
  if (!is.na(output_mapp) & !is.na(filnamn)){
    flik_lista=lst("Arbetslöshet" = df_bef_slutgiltig)
    write.xlsx(flik_lista,paste0(output_mapp,filnamn))
  }
  
  if(returnera_data == TRUE) return(df_bef_slutgiltig)
 
}
