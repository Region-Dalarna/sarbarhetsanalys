# Skriptet tar först fram antalet sysselsatta tillbaka till tidigt 80-tal (från SCBs databas)
# Därefter tas antalet personer berörda av varsel baserat på Arbetsförmedlingens data fram (via webscraping). 
# Slutligen relateras dessa till varandra
pacman::p_load(openxlsx,here,tidyverse)

# Skript som behövs
source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)

#test_list=diag_antal_varslade(skapa_fil=FALSE,output_mapp=here("Diagram","/"))
diag_antal_varslade <- function(region_vekt="20",
                                output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                skapa_fil=TRUE,
                                diag_varsel_ar=TRUE,
                                diag_andel_varsel=TRUE){
  
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
  # set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090, username = Sys.getenv("userid"), password = Sys.getenv("pwd")))
  # set_config(config(ssl_verifypeer = 0L))
  # ========================================== Läser in data ============================================
  ##############################
  # Antal sysselsatta (PX-web) #
  ##############################
  
  # "Adresser" till SCBs databas
  url_85_93 <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0207/AM0207E/AMPAK1"
  url_93_03 <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0207/AM0207C/AMPAK3"
  url_03_18 <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0207/AM0207K/DagSektAldK"
  url_19_21 <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0207/AM0207Z/DagSni07KonKN"
    
  url<-c(url_85_93,url_93_03,url_03_18,url_19_21)
  
  varlista_83_93 <- list("Region"=hamtaAllaLan(tamedriket=TRUE),
                   "Sysselsattning"=c("FÖRV"),
                   "Alder"=c("*"),
                   "Kon"=c("*"),
                   "ContentsCode"=c("AM0207A1"),
                   "Tid"=c("*"))
  
  varlista_93_03 <- list("Region"=hamtaAllaLan(tamedriket=TRUE),
                         "SNI92"=c("*"),
                        "Kon"=c("*"),
                        "ContentsCode"=c("AM0207A3"),
                        "Tid"=as.character(1994:2003))
  
  varlista_04_18 <- list("Region"=hamtaAllaLan(tamedriket=TRUE),
                         "ArbetsSektor"=c("*"),
                         "Alder"=c("*"),
                         "Kon"=c("*"),
                         "ContentsCode"=c("AM0207F9"),
                         "Tid"=c("*"))
  
  varlista_19_21 <- list("Region"=hamtaAllaLan(tamedriket=TRUE),
                         "SNI2007"=c("*"),
                         "Kon"=c("*"),
                         "ContentsCode"=c("00000544"),
                         "Tid"=c("*"))
  
  varlista_lista=list(varlista_83_93,varlista_93_03,varlista_04_18,varlista_19_21)
  
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
  names(lista) <- c("85_93","94_03","04_18","19_21")
  
  # Grupperar data på relevanta 
  
  df_85_93 <- lista$`85_93` %>% 
    group_by(regionkod,år) %>% 
      summarize("Förvärvsarbetande"=sum(`Befolkningen 16+ år (ÅRSYS)`))
  
  df_94_03 <- lista$`94_03` %>% 
    group_by(regionkod,år) %>% 
      summarize("Förvärvsarbetande"=sum(`Förvärvsarbetande 16+ år med arbetsplats i regionen (dagbefolkning) (RAMS)`))
  
  df_04_18 <-lista$`04_18` %>% 
    group_by(regionkod,år) %>% 
      summarize("Förvärvsarbetande"=sum(`Förvärvsarbetande 16+ år med arbetsplats i regionen (dagbefolkning) (RAMS)`))
  
  df_19_21 <-lista$`19_21` %>% 
    group_by(regionkod,år) %>% 
    summarize("Förvärvsarbetande"=sum(`Förvärvsarbetande 16-74 år med arbetsplats i regionen (dagbefolkning) (RAMS)`))
  
  #Slår ihop dataset och grupperar på regionkod och år
  df_bef_slutgiltig <- rbind(df_85_93,df_94_03,df_04_18,df_19_21)
  
  # Läser in "moderna" namn på län
  lansnamn <- hamtaregion_kod_namn(hamtaAllaLan(tamedriket = TRUE))
  
  df_bef_slutgiltig<-left_join(df_bef_slutgiltig,lansnamn,by="regionkod")
  
  df_bef_slutgiltig$region<-skapa_kortnamn_lan(df_bef_slutgiltig$region)
  
  # #####################
  # # Varsel Excel (AF) #
  # ####################
  # 
  # # Läser in data från Excel (ursprung arbetsförmedlingen)
  # varsel_lan_df <- read.xlsx(here("Indata","/","Varsel_lan_1992_2022.xlsx"),sheet="Månad 1992-01--2022-09",startRow=5)
  
  # ================================== nedladdning av fil ============================================
  
  # hämta webbsidan med tidigare statistik på Arbetsförmedlingen och spara som en vektor
  webbsida <- suppressWarnings(readLines("https://arbetsformedlingen.se/statistik/sok-statistik/tidigare-statistik"))
  
  varsel_index <- which(str_detect(webbsida, "varsel"))   # sök alla rader där "varsel" finns med
  xlsx_index <- which(str_detect(webbsida, ".xlsx"))      # sök alla rader där ".xlsx" finns med
  
  fil_index <- varsel_index[varsel_index %in% xlsx_index]    # ta index där båda är med
  fil_strang <- webbsida[fil_index]                          # skapa sträng med det element där båda är med
  
  # i den strängen, ta ut startposition för alla "/download/" som hittar i strängen (det är sökvägar)
  start_sokvagar <- str_locate_all(fil_strang, "/download/")[[1]][,1]  
  
  # funktion för att ta ut fullständig url från de startpositioner vi hittade i raden ovan
  extrahera_sokvag <- function(strang, startpos) {
    
    nystrang <- str_sub(strang, startpos, nchar(strang))
    slutpos <- str_locate(nystrang, '\"')[[1]]-1
    
    retur_strang <- str_sub(nystrang, 1, slutpos)
    retur_strang <- paste0("https://arbetsformedlingen.se", retur_strang)
    return(retur_strang)
  }       
  
  # vi skapar en vektor med fullständiga sökvägar för samtliga excelfiler som finns på webbsidan
  af_urler <- start_sokvagar %>% map_chr(~ extrahera_sokvag(fil_strang, .x))
  
  # ta ut sökväg för varsel per län, dvs en sökväg som innehåller "varsel" och "lan" men inte "bransch"
  varsel_lan_url <- af_urler[str_detect(af_urler, "varsel") & str_detect(af_urler, "lan") & !str_detect(af_urler, "bransch")]   
  
  # spara filen temporärt för att kunna extrahera fliknamn och kolla startrad
  
  td = tempdir()              # skapa temporär mapp
  varsel_fil <- tempfile(tmpdir=td, fileext = ".xlsx")
  
  download.file(varsel_lan_url, destfile = varsel_fil, mode = "wb")       # ladda hem hela filen, mode = "wb" viktigt, annars blir det fel
  fliknamn <- getSheetNames(varsel_fil)               # hämta alla fliknamn ur Excelfilen
  flik_alla_ar <- fliknamn[str_detect(fliknamn, "Månad")]
  
  startrads_test_df <- read.xlsx(varsel_lan_url, sheet = flik_alla_ar, skipEmptyRows = FALSE)
  startrad <- which(!is.na(startrads_test_df[[1]]))[1]-1 
  
  # läs in Excelfilen till en df direkt från url:en
  varsel_lan_df <-read.xlsx(varsel_lan_url, sheet = flik_alla_ar, startRow = startrad)
  
  # ===================================== slut på hämtning av fil =====================================
  
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
  
  varsel_antal_manader <- varsel_lan_df %>% 
    filter(Region==skapa_kortnamn_lan(ValdGeografi),Ar==max(Ar)) %>% 
      count(Region)
  
  caption=paste0("OBS: År ",max(varsel_lan_df$Ar)," består av ",varsel_antal_manader[2]," månader")
  
  diagram_capt_alt<-paste0(diagram_capt,"\nDiagramförklaring: ",caption)
    
  
  ###############################################################################################
  # Slår ihop dataset med varsel och befolkning och beräknar därefter andel som blivit varslade #
  ###############################################################################################
  df_slutgiltig<-left_join(df_bef_slutgiltig %>% filter(år>"1991"),varsel_lan_df_utskrift_ar,by=c("region"="Region","år"="Ar"))
  
  df_slutgiltig$andel_varsel<-(df_slutgiltig$Varsel/df_slutgiltig$Förvärvsarbetande)*100


  if(diag_varsel_ar==TRUE){
      diagramtitel <- paste0("Antal personer berörda av varsel i ",ValdGeografi)
      diagramfilnamn <- paste0("varsel_lan_tidsserie_ar.png")
      objektnamn <- c(objektnamn,diagramtitel)
      
      gg_obj <- SkapaStapelDiagram(skickad_df =varsel_lan_df_utskrift_ar %>% 
                                     filter(Region==skapa_kortnamn_lan(ValdGeografi),Ar<=max(varsel_lan_df_utskrift_ar$Ar)),
                                   skickad_x_var = "Ar", 
                                   skickad_y_var = "Varsel", 
                                   #skickad_x_grupp = "utb_niva",
                                   manual_x_axis_text_vjust=1,
                                   manual_x_axis_text_hjust=1,
                                   manual_color = diagramfarger("rus_sex")[1],
                                   diagram_titel = diagramtitel,
                                   diagram_capt =  diagram_capt_alt,
                                   manual_y_axis_title = "Antal varslade",
                                   diagram_facet = FALSE,
                                   x_axis_lutning = 45,
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
                                 manual_color = diagramfarger("rus_sex"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 manual_y_axis_title = "procent",
                                 diagram_facet = FALSE,
                                 x_axis_lutning = 45,
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
 
  names(gg_list)<-objektnamn
  return(gg_list)
}
