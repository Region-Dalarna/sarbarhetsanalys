##### Hämtar data för att beräkna arbetslöshet #######
pacman::p_load(tidyverse,rKolada)

source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_filer.R", encoding = "utf-8", echo = FALSE)

#test_list <- diag_arbetsloshet(skapa_fil = FALSE)

diag_arbetsloshet <-function(region_vekt = "20", 
                             output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                             skapa_fil = TRUE){
  
  # ========================================== Inställningar ============================================
  BaraEttLän <- region_vekt
  
  diagram_capt <- "Källa: SCB via Kolada\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Antal invånare 16-64 år som är öppet arbetslösa eller i program med aktivitetsstöd,\ndividerat med antal i arbetskraften och öppet arbetslösa eller i program med aktivitetsstöd 16-64 år"
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  i<-1
  # =============================================== API-uttag ===============================================
  
  #### Ta hem data från Kolada
  arbetsloshet <- get_values(
    kpi = c("N03932"),
    municipality = c("0000","0020",hamtakommuner(BaraEttLän)),
    period = 2000:2100)
  
  # Väljer ut relevanta variabler och döper om value till Branschbredd
  
  arbetsloshet_utskift<-arbetsloshet %>% 
    select(year,gender,value,municipality) %>% 
      rename("Arbetsloshet"=value)
  
  # Ändrar Region Dalarna till Dalarna
  arbetsloshet_utskift[arbetsloshet_utskift=="Region Dalarna"]<-"Dalarna"
  
  # Gör om år till en character
  arbetsloshet_utskift$year<-as.character(arbetsloshet_utskift$year)
  
  # hej<-branschbredd_utskift %>% 
  #   filter(year%in%c(min(branschbredd_utskift$year),max(branschbredd_utskift$year)))
  
  
  diagram_titel <- paste0("Arbetslöshet i ",hamtaregion_kod_namn(BaraEttLän)[[2]]," och Riket")
  diagram_typ <- "arbetsloshet"
  diagramfil <- "arbetsloshet.png"
  objektnamn <- diagram_typ
  
  gg_obj <- SkapaStapelDiagram(skickad_df = arbetsloshet_utskift %>% 
                                 filter(gender=="T",year%in%c(min(arbetsloshet_utskift$year),max(arbetsloshet_utskift$year))), 
                               skickad_x_var = "municipality", 
                               skickad_y_var = "Arbetsloshet", 
                               skickad_x_grupp = "year",
                               manual_x_axis_text_vjust=1,
                               manual_x_axis_text_hjust=1,
                               manual_color = diagramfarger("gron_sex")[5:6],
                               x_axis_sort_value = TRUE,
                               x_axis_sort_grp = 2,
                               vand_sortering = TRUE,
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
