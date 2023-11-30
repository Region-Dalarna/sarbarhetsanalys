# Tar fram arbetslöshet från 1974 till 2020 
# https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0402__AM0402F/AKUABefolkningL/
# https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0403__AM0403N/NAKUBefolkningLArTD/
pacman::p_load(pxweb,httr,openxlsx,here,tidyverse)

# Skript som behövs
source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)

#test_list=diag_antal_varslade(skapa_fil=TRUE,output_mapp=here("Diagram","/"))
diag_antal_varslade <- function(region_vekt="20",
                                output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                skapa_fil=TRUE,
                                diag_arbetsloshet=TRUE){
  
  # ========================================== Inställningar ============================================
  # Text till diagram
  diagram_capt <- "Källa: Arbetsförmedlingen.\nBearbetning: Samhällsanalys, Region Dalarna."
  
  ValdGeografi <- hamtaregion_kod_namn(region_vekt)$region
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  objektnamn<-c()
  i=1 # Räknare som används för att lägga till objekt i listan
  lista=list()
  #==========================================================================================================  
  # För att komma förbi proxyn
  set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090, username = Sys.getenv("userid"), password = Sys.getenv("pwd")))
  set_config(config(ssl_verifypeer = 0L))
  # ========================================== Läser in data ============================================
  ##############################
  # Antal sysselsatta (PX-web) #
  ##############################
  
  # "Adresser" till SCBs databas
  url_76_04 <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0402/AM0402F/AKUABefolkningL"
  url_05_20 <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0403/AM0403N/NAKUBefolkningLArTD"

  url<-c(url_76_04,url_05_20)
  
  varlista_76_04 <- list("Region"=hamtaAllaLan(tamedriket=TRUE),
                         "Arbetskraftstillh"=c("IARB","SYS20-34","SYS35+","SYS","ALÖS","EIAKR"),
                         "Kon"=c("1","2"),
                         "ContentsCode"=c("*"),
                         "Tid"=c("*"))
  
  varlista_05_20 <- list("Region"=hamtaAllaLan(tamedriket=TRUE),
                         "Arbetskraftstillh"=c("TOTALT","ALÖS","EIAKR","SYS"),
                         "Kon"=c("1","2","1+2"),
                         "ContentsCode"=c("*"),
                         "Tid"=c("*"))
  
  varlista_lista=list(varlista_76_04,varlista_05_20)
  
  # Loopar över lista med url:er och hämtar data för de olika variablerna. Dessa läggs i en lista
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
  names(lista) <- c("76_04","05_20")

  # Beräknar arbetslöshet i två steg
  df_76_04 <- lista$`76_04` %>% 
    filter(arbetskraftstillhörighet%in%c("arbetslösa","sysselsatta")) %>% 
      group_by(regionkod,region,år,arbetskraftstillhörighet) %>% 
        summarize("Antal"=sum(`Befolkningen 16-64 år (AKU), 100-tal`)) %>% 
          pivot_wider(names_from=arbetskraftstillhörighet,values_from=Antal )
  
  df_76_04$arbetsloshet=(df_76_04$arbetslösa/(df_76_04$arbetslösa+df_76_04$sysselsatta))*100
  
  df_05_20 <- lista$`05_20` %>% 
    filter(arbetskraftstillhörighet%in%c("arbetslösa","sysselsatta")) %>% 
      group_by(regionkod,region,år,arbetskraftstillhörighet) %>% 
        summarize("Antal"=sum(`1000-tal`))%>% 
          pivot_wider(names_from=arbetskraftstillhörighet,values_from=Antal )
  
  df_05_20$arbetsloshet=(df_05_20$arbetslösa/(df_05_20$arbetslösa+df_05_20$sysselsatta))*100

  #Slår ihop dataset och grupperar på regionkod och år
  df_bef_slutgiltig <- rbind(df_85_93,df_94_03,df_04_18,df_19_21)
  
  # Läser in "moderna" namn på län
  lansnamn <- hamtaregion_kod_namn(hamtaAllaLan(tamedriket = TRUE))
  
  df_bef_slutgiltig<-left_join(df_bef_slutgiltig,lansnamn,by="regionkod")
  
  df_bef_slutgiltig$region<-skapa_kortnamn_lan(df_bef_slutgiltig$region)
  
  #####################
  # Varsel Excel (AF) #
  ####################
  
  # Läser in data från Excel (ursprung arbetsförmedlingen)
  varsel_lan_df <- read.xlsx(here("Indata","/","Varsel_lan_1992_2022.xlsx"),sheet="Månad 1992-01--2022-09",startRow=5)
  
  # =========== Årsdata =======================
  # Döper om variabel utan namn till år
  varsel_lan_df<-varsel_lan_df %>% 
    rename("Ar"="X1")
  
  # Separerar års och månadsdata (att använda senare)
  varsel_lan_df=varsel_lan_df %>% 
    separate(Ar,c("Ar","Manad"),"-")
  
  # Tar bort allt utom årtal
  varsel_lan_df$Ar<-substr(varsel_lan_df$Ar,1,4)
  
  # Pivoterar så att länsnamnen hamnar som grupper snarare än kolumner
  varsel_lan_df<-pivot_longer(varsel_lan_df,3:24,names_to = "Region",values_to = "Personer_varsel")
  
  # Tar bort s i länsnamn
  varsel_lan_df$Region<-skapa_kortnamn_lan(varsel_lan_df$Region)
  
  varsel_lan_df[varsel_lan_df=="Västra.Götaland"] <- "Västra Götaland"
  
  # Grupperar på år och län och summerar antalet varsel (vi går från månadsdata till årsdata)
  varsel_lan_df_utskrift_ar<-varsel_lan_df %>% 
    group_by(Ar,Region) %>% 
    summarize(Varsel=sum(Personer_varsel))
  
  
  ###############################################################################################
  # Slår ihop dataset med varsel och befolkning och beräknar därefter andel som blivit varslade #
  ###############################################################################################
  df_slutgiltig<-left_join(df_bef_slutgiltig %>% filter(år>"1991"),varsel_lan_df_utskrift_ar,by=c("region"="Region","år"="Ar"))
  
  df_slutgiltig$andel_varsel<-(df_slutgiltig$Varsel/df_slutgiltig$Förvärvsarbetande)*100
  
  # ========= Månadsdata =================== 
  # Namnger månader - Problematiskt då jag inte vet hur man skall hantera månader som saknas (för senaste år)
  varsel_lan_df[varsel_lan_df=="01"]<-"Januari"
  varsel_lan_df[varsel_lan_df=="02"]<-"Februari"
  varsel_lan_df[varsel_lan_df=="03"]<-"Mars"
  varsel_lan_df[varsel_lan_df=="04"]<-"April"
  varsel_lan_df[varsel_lan_df=="05"]<-"Maj"
  varsel_lan_df[varsel_lan_df=="06"]<-"Juni"
  varsel_lan_df[varsel_lan_df=="07"]<-"Juli"
  varsel_lan_df[varsel_lan_df=="08"]<-"Augusti"
  varsel_lan_df[varsel_lan_df=="09"]<-"September"
  # Dessa tre manader saknas 2022, vilket skapar problem. Jag låter dom vara tillsvidare då vi inte använder dem för tillfället
  
  # varsel_lan_df[varsel_lan_df=="10"]<-"Oktober"
  # varsel_lan_df[varsel_lan_df=="11"]<-"November"
  # varsel_lan_df[varsel_lan_df=="12"]<-"December"
  
  if(diag_varsel_ar==TRUE){
    diagramtitel <- paste0("Antal personer berörda av varsel i Dalarnas län per år ")
    diagramfilnamn <- paste0("varsel_lan_tidsserie_ar.png")
    objektnamn <- c(objektnamn,diagramtitel)
    
    gg_obj <- SkapaStapelDiagram(skickad_df =varsel_lan_df_utskrift_ar %>% 
                                   filter(Region=="Dalarna",Ar<max(varsel_lan_df_utskrift_ar$Ar)),
                                 skickad_x_var = "Ar", 
                                 skickad_y_var = "Varsel", 
                                 #skickad_x_grupp = "utb_niva",
                                 # manual_x_axis_text_vjust=1,
                                 # manual_x_axis_text_hjust=1,
                                 manual_color = diagramfarger("gron_sex")[6],
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 manual_y_axis_title = "Antal varslade",
                                 diagram_facet = FALSE,
                                 x_axis_lutning = 90,
                                 diagram_liggande = FALSE,
                                 legend_vand_ordning=FALSE,
                                 geom_position_stack = FALSE,
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  if(diag_andel_varsel==TRUE){
    diagramtitel <- paste0("Andel av förvärvsarbetande berörda av varsel under utvalda år ")
    diagramfilnamn <- paste0("varsel_andel_kris.png")
    objektnamn <- c(objektnamn,diagramtitel)
    
    gg_obj <- SkapaStapelDiagram(skickad_df =df_slutgiltig %>% 
                                   filter(år%in%c("1992","1993","2008","2009","2020")),
                                 skickad_x_var = "region", 
                                 skickad_y_var = "andel_varsel", 
                                 skickad_x_grupp = "år",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = diagramfarger("gron_sex")[2:6],
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 manual_y_axis_title = "procent",
                                 diagram_facet = FALSE,
                                 x_axis_lutning = 90,
                                 diagram_liggande = FALSE,
                                 legend_vand_ordning=FALSE,
                                 geom_position_stack = FALSE,
                                 berakna_index = FALSE,
                                 x_axis_sort_value = TRUE,
                                 x_axis_sort_grp = 1,
                                 vand_sortering=TRUE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  if(diag_varsel_manad==TRUE){
    # Väljer fem år 
    valda_ar<-c("2018","2019","2020","2021","2022")
    # Väljer de sex första månaderna varje år
    valda_manader<-c("Januari","Februari","Mars","April","Maj","Juni","Juli","Augusti","September")
    # Skapar ett dataset för att skriva ut. Går inte i gg_obj eftersom variabeln månad ändras till en faktorvariabel
    varsel_lan_df_utskrift_manad <- varsel_lan_df %>% 
      filter(Region=="Dalarna",Ar%in%valda_ar,Manad%in%valda_manader)
    
    # Använder faktorvariabler för att byta ordning på månaderna
    varsel_lan_df_utskrift_manad$Manad <- factor(varsel_lan_df_utskrift_manad$Manad, levels = c("Januari","Februari","Mars","April","Maj","Juni","Juli","Augusti","September"))
    
    diagramtitel <- paste0("Antal personer berörda av varsel i Dalarnas län (Januari-September) ")
    diagramfilnamn <- paste0("varsel_lan_manad.png")
    objektnamn <- c(objektnamn,diagramtitel)
    
    gg_obj <- SkapaStapelDiagram(skickad_df =varsel_lan_df_utskrift_manad,
                                 skickad_x_var = "Ar", 
                                 skickad_y_var = "Personer_varsel", 
                                 skickad_x_grupp = "Manad",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = varlda_farger,
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 manual_y_axis_title = "Antal varslade",
                                 diagram_facet = FALSE,
                                 x_axis_lutning = 90,
                                 diagram_liggande = FALSE,
                                 legend_vand_ordning=FALSE,
                                 geom_position_stack = FALSE,
                                 dataetiketter = TRUE,
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  names(gg_list)<-objektnamn
  return(gg_list)
}
