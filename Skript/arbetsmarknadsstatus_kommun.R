

#test_list <- diag_arbetsmarknadsstatus(skapa_fil = FALSE)

hamta_data_arbetsmarknadsstatus_kommun <-function(region_vekt = "20",
                                                  output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                                  artal = "senaste år", # Välj årtal. Senaste år ger just det.
                                                  skapa_fil = TRUE,
                                                  ta_med_lan = TRUE,
                                                  ta_med_riket = TRUE,
                                                  diag_arbetslosthet = TRUE,
                                                  diag_arbetskraftsdeltagande = TRUE,
                                                  diag_sysselsattningsgrad = TRUE){
  
  ## =================================================================================================================
  # Skript som laddar hem data för arbetsmarknadsstatus från SCB på kommunnivå (månadsdata). Beroende på val tas data för arbetslöshet, arbetskraftsdeltagande och sysselsättningsgrad med i uttaget.
  # Källa  https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0210__AM0210A/ArbStatusM/
  # =================================================================================================================
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(pxweb,
                 tidyverse,
                 openxlsx)
  
  source("https://raw.githubusercontent.com/FaluPeppe/func/main/func_API.R")
  
  
  # =============================================== API-uttag ===============================================
  # "Adresser" till SCBs databas
  url1 <- "https://api.scb.se"
  url2 <- "/OV0104/v1/doris/sv/ssd/AM/AM0210/AM0210A/ArbStatusM"
  url3 <- paste0(url1, url2)
  
  # Om man vill ta reda vilka variabler som finns i tabellen. 
  # pxvarlist(url3)                        # för att se alla variabler för en tabell
  # pxvardelist(url3, "contentscode")      # för att se alla värden för vald variabel
  
  # Tar bara ut data för de variabler vi vill ha:
  cont_code=c()
  if(diag_arbetslosthet==TRUE) cont_code=c(cont_code, "000006II")
  if(diag_arbetskraftsdeltagande == TRUE) cont_code=c(cont_code,"000006IJ")
  if(diag_sysselsattningsgrad == TRUE) cont_code=c(cont_code,"000006IK")
  
  if(artal == "senaste år") ar = max(hamta_giltiga_varden_fran_tabell(url3, "tid"))
  
  # Variabler som skall tas ut
  varlista <-  list("Region"=hamtakommuner(lan = region_vekt,tamedlan = ta_med_lan,tamedriket = ta_med_riket),
                    "Kon"=c("1+2"),
                    "Alder"=c("20-64"),
                    "Fodelseregion"=c("tot"),
                    "ContentsCode"=cont_code,
                    "Tid"=ar)
  
  # Uttag av data
  px_uttag <- pxweb_get(url = url3,query = varlista)
  
  # Konverterar data till en Data Frame och tar med regionkod
  arbetsmarknadsstatus_df <- as.data.frame(px_uttag) %>% 
    cbind(regionkod = as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>% 
            select(Region))
  
  
  # Döper om vissa variabler och gör om tidsvariabeln till år, månad och period (vilket används i markdown-skriptet så småningom)
  arbetsmarknadsstatus_df <- arbetsmarknadsstatus_df %>% 
    rename(regionkod = Region)%>%
      relocate(regionkod, .before = region) %>% 
        mutate(ar=substr(månad,1,4),
               manad_long=format(as.Date(paste(ar, str_sub(månad, 6,7),"1", sep = "-")), "%B"),
               Period=paste(ar, str_sub(månad, 6,7),sep = "-")) %>% 
          select(-månad) 
  
  # Tar bort län i länsnamnet
  arbetsmarknadsstatus_df$region <-skapa_kortnamn_lan(arbetsmarknadsstatus_df$region)
  
  openxlsx::write.xlsx(arbetsmarknadsstatus_df,paste0(output_mapp,"/arbetsmarknadsstatus_kommun.xlsx"))
  
}

