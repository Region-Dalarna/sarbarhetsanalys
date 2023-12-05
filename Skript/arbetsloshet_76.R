hamta_data_arbetsloshet_76 <- function(region_vekt = "20",
                                       output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                       filnamn_excel = "/arbetsloshet_76.xlsx",
                                       spara_data = TRUE,
                                       returnera_data = FALSE # Om man vill att data skall göras till en global variabel (och hamna i R-Studios enviroment)
                                       ){
  
  
  # =================================================================================================================
  # Tar fram arbetslöshet från 1974 till senaste år (AKU - SCB) 
  # Går i praktiken bara att ändra region för tillfället. Nästa steg, anpassa koden för kön
  # Källa  https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0210__AM0210A/ArbStatusM/
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
  
  url <- c(url_76_04,url_05_)
  
  varlista_76_04 <- list("Region" = c("00",region_vekt),
                         "Arbetskraftstillh" = c("IARB","SYS20-34","SYS35+","SYS","ALÖS","EIAKR"),
                         "Kon" = c("1","2"),
                         "ContentsCode" = c("*"),
                         "Tid" = c("*"))
  
  varlista_05_ <- list("Region"=c("00",region_vekt),
                         "Arbetskraftstillh"=c("TOTALT","ALÖS","EIAKR","SYS"),
                         "Kon"= c("1","2"),
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
      group_by(region,år,arbetskraftstillhörighet) %>% 
        summarize("Antal" = sum(`Befolkningen 16-64 år (AKU), 100-tal`)) %>% 
          pivot_wider(names_from = arbetskraftstillhörighet,values_from=Antal )
  
  df_76_04$arbetsloshet=(df_76_04$arbetslösa/(df_76_04$arbetslösa+df_76_04$sysselsatta))*100
  
  df_05_ <- lista$`05_` %>% 
    filter(arbetskraftstillhörighet%in%c("arbetslösa","sysselsatta")) %>% 
      group_by(region,år,arbetskraftstillhörighet) %>% 
        summarize("Antal" = sum(`1000-tal`))%>% 
          pivot_wider(names_from=arbetskraftstillhörighet,values_from=Antal )
  
  df_05_$arbetsloshet=(df_05_$arbetslösa/(df_05_$arbetslösa+df_05_$sysselsatta))*100
  
  #Slår ihop dataset och grupperar på regionkod och år
  df_bef_slutgiltig <- rbind(df_76_04,df_05_)

  # Tar bort län i länsnamn och väljer ut de variabler vi är intresseade av (dvs. inte arbetslösa och sysselsatta (i antal))
  df_bef_slutgiltig <- df_bef_slutgiltig %>% 
    mutate(region = skapa_kortnamn_lan(region,byt_ut_riket_mot_sverige = TRUE)) %>% 
      select(region,år,arbetsloshet)
  
  # Sparar till Excel
  if (spara_data==TRUE){
    flik_lista=lst("Arbetslöshet" = df_bef_slutgiltig)
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp,filnamn_excel))
  }
  
  if(returnera_data == TRUE) return(df_bef_slutgiltig)
 
}
