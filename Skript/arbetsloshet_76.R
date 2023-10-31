# Tar fram arbetslöshet från 1974 till 2022 (AKU) 
# https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0402__AM0402F/AKUABefolkningL/
# https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0403__AM0403N/NAKUBefolkningLArTD/
# https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0401__AM0401N/NAKUBefolkningLAr/

pacman::p_load(pxweb,here,tidyverse)

# Skript som behövs
source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)

#test_list=diag_arbetsloshet_76(region_vekt="20",skapa_fil=FALSE,output_mapp=here("Diagram","/"))
hamta_data_arbetsloshet_76 <- function(region_vekt="20",
                                       output_mapp_excel = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                       output_mapp_figur= "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                       filnamn_excel = "/arbetsloshet_76.xlsx",
                                       spara_data = TRUE,
                                       diag_stapel = FALSE,
                                       diag_linje = FALSE){
  

  # "Adresser" till SCBs databas
  url_76_04 <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0402/AM0402F/AKUABefolkningL"
  #url_05_ <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0403/AM0403N/NAKUBefolkningLArTD"
  url_05_ <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0401/AM0401N/NAKUBefolkningLAr"
  
  url <- c(url_76_04,url_05_)
  
  varlista_76_04 <- list("Region"=c("00",region_vekt),
                         "Arbetskraftstillh"=c("IARB","SYS20-34","SYS35+","SYS","ALÖS","EIAKR"),
                         "Kon"=c("1","2"),
                         "ContentsCode"=c("*"),
                         "Tid"=c("*"))
  
  varlista_05_ <- list("Region"=c("00",region_vekt),
                         "Arbetskraftstillh"=c("TOTALT","ALÖS","EIAKR","SYS"),
                         "Kon"=c("1","2"),
                         "ContentsCode"=c("*"),
                         "Tid"=c("*"))
  
  # varlista_21_ <- list("Region"=c("00",region_vekt),
  #                      "Arbetskraftstillh"=c("TOTALT","ALÖS","EIAKR","SYS"),
  #                      "Kon"=c("1","2"),
  #                      "ContentsCode"=c("*"),
  #                      "Tid"=c("*")) 
  
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
        summarize("Antal"=sum(`Befolkningen 16-64 år (AKU), 100-tal`)) %>% 
          pivot_wider(names_from=arbetskraftstillhörighet,values_from=Antal )
  
  df_76_04$arbetsloshet=(df_76_04$arbetslösa/(df_76_04$arbetslösa+df_76_04$sysselsatta))*100
  
  df_05_ <- lista$`05_` %>% 
    filter(arbetskraftstillhörighet%in%c("arbetslösa","sysselsatta")) %>% 
      group_by(region,år,arbetskraftstillhörighet) %>% 
        summarize("Antal"=sum(`1000-tal`))%>% 
          pivot_wider(names_from=arbetskraftstillhörighet,values_from=Antal )
  
  df_05_$arbetsloshet=(df_05_$arbetslösa/(df_05_$arbetslösa+df_05_$sysselsatta))*100
  
  # df_21_<- lista$`21_` %>% 
  #   filter(arbetskraftstillhörighet%in%c("arbetslösa","sysselsatta")) %>% 
  #     group_by(region,år,arbetskraftstillhörighet) %>% 
  #       summarize("Antal"=sum(`1000-tal`))%>% 
  #         pivot_wider(names_from=arbetskraftstillhörighet,values_from=Antal)
  # 
  # df_21_$arbetsloshet=(df_21_$arbetslösa/(df_21_$arbetslösa+df_21_$sysselsatta))*100
  
  #Slår ihop dataset och grupperar på regionkod och år
  df_bef_slutgiltig <- rbind(df_76_04,df_05_)

  # Tar bort län i länsnamn och väljer ut de variabler vi är intresseade av (dvs. inte arbetslösa och sysselsatta (i antal))
  df_bef_slutgiltig <- df_bef_slutgiltig %>% 
    mutate(region = skapa_kortnamn_lan(region,byt_ut_riket_mot_sverige = TRUE)) %>% 
      select(region,år,arbetsloshet)
  
  # Sparar till Excel
  if (spara_data==TRUE){
    flik_lista=lst("Arbetslöshet"=df_bef_slutgiltig)
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp_excel,filnamn_excel))
  }
  
  # Om användaren vill skriva ut figurer så sker detta i nästa steg. Annars returneras inget från funktionen (utan det enda som händer är att ett dokument sparas i Excel)
  if(diag_stapel==TRUE){

    diagramtitel <- paste0("Arbetslöshet")
    diagramfilnamn <- paste0("arbetsloshet76_stapel.png")
    objektnamn <- c(objektnamn,"arbetsloshet_stapel")

    gg_obj <- SkapaStapelDiagram(skickad_df =df_bef_slutgiltig %>%
                                   filter(region%in%c("Riket",ValdGeografi)) %>%
                                   mutate(region=ifelse(region=="Riket", "Sverige",region)),
                                 skickad_x_var = "år",
                                 skickad_y_var = "arbetsloshet",
                                 skickad_x_grupp = "region",
                                 # manual_x_axis_text_vjust=1,
                                 # manual_x_axis_text_hjust=1,
                                 manual_color = diagramfarger("rus_sex"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 manual_y_axis_title = "procent",
                                 diagram_facet = FALSE,
                                 x_axis_lutning = 90,
                                 diagram_liggande = FALSE,
                                 legend_vand_ordning=FALSE,
                                 geom_position_stack = FALSE,
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp_figur,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skapa_fil)

    gg_list[[i]] <-gg_obj
    i=i+1

  }

  if(diag_linje==TRUE){
    diagramtitel <- paste0("Arbetslöshet")
    diagramfilnamn <- paste0("arbetsloshet76_linje.png")
    objektnamn <- c(objektnamn,"arbetsloshet_linje")

    gg_obj <- SkapaLinjeDiagram(skickad_df =df_bef_slutgiltig %>%
                                  filter(region%in%c("Riket",ValdGeografi)) %>%
                                  mutate(region=ifelse(region=="Riket", "Sverige",region)),
                                skickad_x_var = "år",
                                skickad_y_var = "arbetsloshet",
                                skickad_x_grupp = "region",
                                manual_color = diagramfarger("rus_sex"),
                                diagram_titel = diagramtitel,
                                diagram_capt =  diagram_capt,
                                manual_y_axis_title = "procent",
                                x_axis_lutning = 45,
                                visa_var_x_xlabel = 4,
                                output_mapp = output_mapp_figur,
                                filnamn_diagram = diagramfilnamn,
                                skriv_till_diagramfil = skapa_fil)

    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  if(diag_stapel == TRUE||diag_linje == TRUE){
    names(gg_list) <- objektnamn
    return(gg_list)
  }
}
