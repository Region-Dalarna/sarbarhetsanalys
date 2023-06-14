# Skapar en figur med antal anställda i olika branscher.
# Från RAMS i SCBs öppna statistikdatabas (val 25 under arbetsmarknad)

# Laddar in paket
pacman::p_load(pxweb,httr,askpass,tidyverse)

# Laddar in de funktioner som behövs för att skapa diagram mm
source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_filer.R", encoding = "utf-8", echo = FALSE)

#test_list <- diag_sysselsatta_andel(skapa_fil = FALSE)

diag_sysselsatta_andel <-function(region_vekt = "20", 
                                  output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                  skapa_fil = TRUE,
                                  lan=TRUE,
                                  kommun=TRUE){
  
  # ========================================== Inställningar ============================================
  # Adress till statistikdatabasen
  url_list <- "/OV0104/v1/doris/sv/ssd/AM/AM0207/AM0207Z/DagSni07KonKN"
  
  # Variabel vi är intresserade av
  SummeringsVar <- c("Förvärvsarbetande 16+ år (dagbef)")
  # Variabellista
  varlista <- list(
    Region = c(hamtakommuner(region_vekt)),
    SNI2007 = '*',
    Kon = '*',
    ContentsCode = "*",
    Tid = c('*')
  )
  
  # Det som står under diagrammet
  diagram_capt <- "Källa: RAMS i SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Branschens andel av totalt antal förvärvsarbetande"
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  # Skapar en tom vektor som skall innehålla objektnamn
  objektnamn<-c() 
  # Räknare
  i<-1
  #==========================================================================================================
  # För att komma förbi proxyn
  set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090, username = Sys.getenv("userid"), password = Sys.getenv("pwd")))
  set_config(config(ssl_verifypeer = 0L))
  
  # =============================================== API-uttag ===============================================
  
  px_df <- NULL
  url1 <- "https://api.scb.se"
  
  for (url_tab in 1:length(url_list)){
    
    url3 <- paste0(url1, url_list[url_tab])
    px_uttag <- pxweb_get(url = url3,
                          query = varlista
    ) 
    
    filnamn <- paste0(unlist(strsplit(px_uttag$metadata[[1]][[3]], " "))[1], "_API.xlsx")
    
    # Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
    # välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
    px_df_temp <- as.data.frame(px_uttag) %>% 
      cbind(regionkod = as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>% 
              select(Region))  
    px_df_temp <- px_df_temp %>% rename(regionkod = Region) %>% relocate(regionkod, .before = region)
    names(px_df_temp)[ncol(px_df_temp)] <- "Förvärvsarbetande 16+ år (dagbef)"
    
    px_df <- rbind(px_df, px_df_temp)
    
  }
  
  # Ändra namn på vissa branscher
  px_df$Branschgrupp <- case_when(
    px_df$`näringsgren SNI 2007` == "jordbruk, skogsbruk och fiske" ~ "Jordbruk och skogsbruk",
    px_df$`näringsgren SNI 2007` == "tillverkning och utvinning" ~ "Tillverkning och utvinning",
    px_df$`näringsgren SNI 2007` == "energiförsörjning; miljöverksamhet" ~ "Energi och miljö",
    px_df$`näringsgren SNI 2007` == "byggverksamhet" ~ "Bygg",
    px_df$`näringsgren SNI 2007` == "handel" ~ "Handel",
    px_df$`näringsgren SNI 2007` == "transport och magasinering"~ "Transport",
    px_df$`näringsgren SNI 2007` == "hotell- och restaurangverksamhet" ~ "Hotell och restaurang",
    px_df$`näringsgren SNI 2007` == "information och kommunikation" ~ "IT och kommunikation",
    px_df$`näringsgren SNI 2007` == "finans- och försäkringsverksamhet" ~ "Finans och försäkring",
    px_df$`näringsgren SNI 2007` == "fastighetsverksamhet" ~ "Fastighet",
    px_df$`näringsgren SNI 2007` == "företagstjänster" ~ "Företagstjänster",
    px_df$`näringsgren SNI 2007` == "offentlig förvaltning och försvar"~ "Offentlig förvaltning",
    px_df$`näringsgren SNI 2007` == "utbildning " ~ "Utbildning",
    px_df$`näringsgren SNI 2007` == "vård och omsorg; sociala tjänster" ~ "Vård och omsorg",
    px_df$`näringsgren SNI 2007` == "kulturella och personliga tjänster m.m." ~ "Kultur m.m.",
    px_df$`näringsgren SNI 2007` == "okänd verksamhet"~ "Okänd verksamhet")
  
  pre_titel <- gsub(",.*", "", SummeringsVar)
  
  # Summerar på region och sektor
  px_df <- px_df %>%
    filter(år==max(px_df$år)) %>% 
      group_by(år,region,`Branschgrupp`) %>% 
        summarize("Förvärvsarbetande"=sum(`Förvärvsarbetande 16+ år (dagbef)`))
  
  # Beräknar andelar      
  px_df<-px_df %>%
      group_by(år,region) %>% 
        mutate(Andel_forv=(Förvärvsarbetande/sum(Förvärvsarbetande)*100)-0.01)
  
  px_df$region<-skapa_kortnamn_lan(px_df$region)
  
  if(lan==TRUE){
  
    diagram_titel <- paste0("Andel förvärvsarbetande (16-74) år per bransch ",max(px_df$år))
    diagram_typ <- "andel_per_bransch"
    diagramfil <- paste0(pre_titel, diagram_typ, ".png")
    objektnamn <- c(objektnamn,paste0(pre_titel, diagram_typ))
    
    gg_obj <- SkapaStapelDiagram(skickad_df = px_df %>% 
                                   filter(region%in%c("Riket","Dalarna")) %>% 
                                   mutate(region=ifelse(region=="Riket", "Sverige",region)),
                                 skickad_x_var = "Branschgrupp", 
                                 skickad_y_var = "Andel_forv", 
                                 skickad_x_grupp = "region",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = diagramfarger("rus_sex"),
                                 x_axis_sort_value = TRUE,
                                 manual_y_axis_title="procent",
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diagram_capt,
                                 diagram_facet = FALSE,
                                 facet_grp="år",
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  # Vektor som används för att skapa figurer för samtliga kommuner (mha en loop)
  kommun_vektor=hamtaregion_kod_namn(hamtakommuner(region_vekt,tamedlan=FALSE,tamedriket=FALSE))[2]
  
  # Loop som skapar diagram för samtliga Dalarnas kommuner
  j=1
  if(kommun==TRUE){
    while(j<=length(kommun_vektor$region)){
      diagram_titel <- paste0("Andel förvärvsarbetande (16-74) år per bransch ",max(px_df$år))
      diagram_typ <- paste0("andel_per_bransch ","_",kommun_vektor$region[j])
      diagramfil <- paste0(pre_titel, diagram_typ, ".png")
      objektnamn <- c(objektnamn,paste0(pre_titel, diagram_typ))
      
      # För att ort respektive län skall skrivas i rätt ordning (Dalarna först) så skapas en faktorvariabel med mutate nedan
      gg_obj <- SkapaStapelDiagram(skickad_df = px_df %>% 
                                     filter(region%in%c("Dalarna",kommun_vektor$region[j])) %>% 
                                      mutate(region=factor(region, levels = c("Dalarna",kommun_vektor$region[j]))), 
                                   skickad_x_var = "Branschgrupp", 
                                   skickad_y_var = "Andel_forv", 
                                   skickad_x_grupp = "region",
                                   manual_x_axis_text_vjust=1,
                                   manual_x_axis_text_hjust=1,
                                   manual_color = diagramfarger("rus_sex"),
                                   x_axis_sort_value = TRUE,
                                   manual_y_axis_title="procent",
                                   diagram_titel = diagram_titel,
                                   diagram_capt = diagram_capt,
                                   diagram_facet = FALSE,
                                   facet_grp="år",
                                   berakna_index = FALSE,
                                   output_mapp = output_mapp,
                                   filnamn_diagram = diagramfil,
                                   skriv_till_diagramfil = skapa_fil)
      
      gg_list[[i]] <-gg_obj
      i=i+1
      j=j+1
    }
    
  }
  
  names(gg_list) <-c(objektnamn)
  return(gg_list)
  
}
