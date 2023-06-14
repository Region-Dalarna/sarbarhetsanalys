##### Andel som har bredband från Kolada #######
# https://www.kolada.se/verktyg/fri-sokning/?kpis=167067&years=30199,30198,30197&municipals=16765&rows=municipal,kpi&visualization=bar-chart
pacman::p_load(tidyverse,rKolada)

source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_filer.R", encoding = "utf-8", echo = FALSE)

#test_list <- diag_bredband(skapa_fil = FALSE,region_vekt = "20")

diag_bredband <-function(region_vekt = "20", 
                         output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                         skapa_fil = TRUE){
  
  # ========================================== Inställningar ============================================
  
  nyckel_mapp <- "G:/Samhällsanalys/Automatisering och R/nycklar/"
  
  BaraEttLän <- region_vekt
  
  diagram_capt <- "Källa: PTS (Post- och telestyrelsen).\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Andel av hushåll i tätbebyggt område som har tillgång till bredband om minst 1 gigabit per sekund, eller fiber i absolut närhet (homes passed)" 
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  i<-1
  # =============================================== API-uttag ===============================================
  
  #### Dra hem variablerna från Kolada
  bredband_gles <- get_values(
    kpi = c("N07920"),
    municipality = c("0000",hamtakommuner(BaraEttLän)),
    period = 2000:2100
  )
  
  bredband_tat  <- get_values(
    kpi = c(" N07919"),
    municipality = c("0000",hamtakommuner(BaraEttLän)),
    period = 2000:2100
  )
  
  # Väljer ut relevanta variabler och döper om value till bredband
  bredband_gles<-bredband_gles %>% 
    select(year,value,municipality) %>% 
      rename("Bredband_andel"=value)
  
  bredband_gles$omrade<-"Glesbygd"
  
  # Väljer ut relevanta variabler och döper om value till bredband
  bredband_tat<-bredband_tat %>% 
    select(year,value,municipality) %>% 
      rename("Bredband_andel"=value)
  
  bredband_tat$omrade<-"Tätort"
  
  bredband_utskrift<-rbind(bredband_gles,bredband_tat)
  
  # Lägger till en variabel som särskiljer mellan glesbygd och tätort

  # Gör om år till en character
  bredband_utskrift$year<-as.character(bredband_utskrift$year)

  diagram_titel <- paste0("Andel av hushåll som har tillgång till bredband om minst 1 gigabit per sekund i ",hamtaregion_kod_namn(BaraEttLän)[[2]]," och Riket")
  diagram_titel<-str_wrap(diagram_titel)
  diagram_typ <- "bredband"
  diagramfil <- "bredband.png"
  objektnamn <- diagram_typ
  
  gg_obj <- SkapaStapelDiagram(skickad_df = bredband_utskrift %>% 
                                 filter(year%in%c(max(bredband_utskrift$year))), 
                               skickad_x_var = "municipality", 
                               skickad_y_var = "Bredband_andel", 
                               skickad_x_grupp = "omrade",
                               manual_x_axis_text_vjust=1,
                               manual_x_axis_text_hjust=1,
                               manual_color = diagramfarger("gron_sex")[5:6],
                               x_axis_sort_value = TRUE,
                               vand_sortering = TRUE,
                               x_axis_sort_grp = 2,
                               dataetiketter = FALSE,
                               dataetiketter_antal_dec = 0,
                               manual_y_axis_title="procent",
                               diagram_titel = diagram_titel,
                               diagram_capt = diagram_capt,
                               output_mapp = output_mapp,
                               filnamn_diagram = diagramfil,
                               skriv_till_diagramfil = skapa_fil)
  
  gg_list[[i]] <-gg_obj
  i=i+1
  names(gg_list) <-c(objektnamn)
  return(gg_list)
  
}
