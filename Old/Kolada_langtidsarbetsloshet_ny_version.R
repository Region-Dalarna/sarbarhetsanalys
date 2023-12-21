##### Andel som är långtidsarbetslösa från Kolada #######
# https://www.kolada.se/verktyg/fri-sokning/?kpis=167067&years=30199,30198,30197&municipals=16765&rows=municipal,kpi&visualization=bar-chart
pacman::p_load(dplyr,tidyverse,rKolada)

source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_filer.R", encoding = "utf-8", echo = FALSE)

#test_list <- diag_langtidsarbetsloshet(skapa_fil = TRUE,region_vekt = "20")

diag_langtidsarbetsloshet <-function(region_vekt = "20", 
                                     output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                     skapa_fil = TRUE){

  # ========================================== Inställningar ============================================
  
  nyckel_mapp <- "G:/Samhällsanalys/Automatisering och R/nycklar/"
  
  BaraEttLän <- region_vekt
  
  diagram_capt <- "Källa: Arbetsförmedlingen (via Kolada).\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Antal invånare 15-74 år som varit öppet arbetslösa eller i program med aktivitetsstöd i minst sex månader,\ndividerat med totalt antal invånare 15-74 år som är öppet arbetslösa eller i program med aktivitetsstöd." 
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  i<-1

  # =============================================== API-uttag ===============================================
  # Tyvärr saknas data för Dalarna och Riket de senste två åren, varför jag bara fokuserar på Dalarnas kommuner
  #### Ta hem data från Kolada
  langtidsarbetsloshet <- get_values(
    kpi = c("N03923"),
    municipality = c("0000","0020",hamtakommuner(BaraEttLän)),
    period = 2011:2100
  )
  
  # Väljer ut relevanta variabler och byter namn
  langtidsarbetsloshet<-langtidsarbetsloshet %>% 
    select(kpi,year,value,gender,municipality) %>% 
      rename("andel"=value,"variabel"=kpi,"region"=municipality)
  
  # Döper om Region Dalarna till Dalarna
  langtidsarbetsloshet$region=ifelse(langtidsarbetsloshet$region=="Region Dalarna","Dalarna",langtidsarbetsloshet$region)
  
  
  ## Döp om variabler till mer begripliga namn
  langtidsarbetsloshet[langtidsarbetsloshet=="N00930"] <- "Långtidsarbetslöshet 15-74 år, årsmedelvärde, andel (%) av arbetslösa"
  
  #langtidsarbetsloshet[langtidsarbetsloshet=="Region Dalarna"] <- "Dalarna"

  diagram_titel <- paste0("Långtidsarbetslöshet 15-74 år, årsmedelvärde, andel (%) av arbetslösa")
  diagram_titel<-str_wrap(diagram_titel)
  diagram_typ <- "langtidsarbetsloshet"
  diagramfil <- "langtidsarbetsloshet.png"
  objektnamn <- diagram_typ
  
  #mutate( fokus = ifelse( Region == "Dalarnas län", "1", "0" )
  gg_obj <- SkapaStapelDiagram(skickad_df = langtidsarbetsloshet %>% 
                                 filter(gender=="T",year%in%c(min(langtidsarbetsloshet$year),max(langtidsarbetsloshet$year))) %>% 
                                  mutate(region=ifelse(region=="Riket", "Sverige",region)),
                               skickad_x_var = "region",
                               skickad_y_var = "andel",
                               skickad_x_grupp = "year",
                               manual_x_axis_text_vjust=1,
                               manual_x_axis_text_hjust=1,
                               manual_color = diagramfarger("rus_sex"),
                               x_axis_sort_value = TRUE,
                               x_axis_sort_grp = 4,
                               vand_sortering = TRUE,
                               dataetiketter = FALSE,
                               dataetiketter_antal_dec = 0,
                               manual_y_axis_title="procent",
                               #x_var_fokus="fokus",
                               diagram_titel = diagram_titel,
                               diagram_capt = diagram_capt,
                               output_mapp = output_mapp,
                               filnamn_diagram = diagramfil,
                               skriv_till_diagramfil = skapa_fil)

  gg_list[[i]] <-gg_obj
  i=i+1
  
  # gg_obj <- SkapaStapelDiagram(skickad_df = langtidsarbetsloshet %>% 
  #                                filter(year%in%c(max(langtidsarbetsloshet$year)-1),gender=="T") %>% 
  #                                 mutate(fokus = ifelse( region == "Dalarna", "1", ifelse( region == "Riket", "2", "0" ))), 
  #                              skickad_x_var = "region", 
  #                              skickad_y_var = "andel", 
  #                              #skickad_x_grupp = "omrade",
  #                              manual_x_axis_text_vjust=1,
  #                              manual_x_axis_text_hjust=1,
  #                              manual_color = diagramfarger("gron_tre_fokus"),
  #                              x_axis_sort_value = TRUE,
  #                              dataetiketter = FALSE,
  #                              dataetiketter_antal_dec = 0,
  #                              manual_y_axis_title="procent",
  #                              x_var_fokus="fokus",
  #                              diagram_titel = diagram_titel,
  #                              diagram_capt = diagram_capt,
  #                              output_mapp = output_mapp,
  #                              filnamn_diagram = diagramfil,
  #                              skriv_till_diagramfil = skapa_fil)
  # 
  # gg_list[[i]] <-gg_obj
  # i=i+1
  names(gg_list) <-c(objektnamn)
  return(gg_list)
  
}
