##### Hämtar data för att beräkna branschbredd #######
pacman::p_load(tidyverse,rKolada)

source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_filer.R", encoding = "utf-8", echo = FALSE)
options(dplyr.summarise.inform = FALSE)

#test_list <- diag_branschbredd(skapa_fil = FALSE)

diag_branschbredd <-function(region_vekt = "20", 
                             output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                             skapa_fil = TRUE){
  
  # ========================================== Inställningar ============================================
  
  nyckel_mapp <- "G:/Samhällsanalys/Automatisering och R/nycklar/"
  
  BaraEttLän <- region_vekt

  diagram_capt <- "Källa: Kolada (RKA)\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Unika branscher i området dividerat med totalt antal branscher enligt SNI2007 på 5-siffernivå"
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  i<-1
  # =============================================== API-uttag ===============================================
  
  #### Ta hem data från Kolada
  branschbredd <- get_values(
    kpi = c("N45702"),
    municipality = c("0020",hamtakommuner(BaraEttLän)),
    period = 2000:2100)
  
  # Väljer ut relevanta variabler och döper om value till Branschbredd
  
  branschbredd_utskift<-branschbredd %>% 
    select(year,value,municipality) %>% 
      rename("Branschbredd"=value)
  
  # Ändrar Region Dalarna till Dalarna
  branschbredd_utskift[branschbredd_utskift=="Region Dalarna"]<-"Dalarna"
  
  # Gör om år till en character
  branschbredd_utskift$year<-as.character(branschbredd_utskift$year)
  
  # hej<-branschbredd_utskift %>% 
  #   filter(year%in%c(min(branschbredd_utskift$year),max(branschbredd_utskift$year)))
  
  
  diagram_titel <- paste0("Branschbredd")
  diagram_typ <- "branschbredd"
  diagramfil <- "branschbredd.png"
  objektnamn <- diagram_typ
  
  gg_obj <- SkapaStapelDiagram(skickad_df = branschbredd_utskift %>% 
                                 filter(year%in%c(min(branschbredd_utskift$year),max(branschbredd_utskift$year))), 
                               skickad_x_var = "municipality", 
                               skickad_y_var = "Branschbredd", 
                               skickad_x_grupp = "year",
                               manual_x_axis_text_vjust=1,
                               manual_x_axis_text_hjust=1,
                               manual_color = diagramfarger("rus_sex"),
                               x_axis_sort_value = TRUE,
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
