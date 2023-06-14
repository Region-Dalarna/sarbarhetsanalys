##### Andel som fått ekonomiskt bistånd (från Kolada) #######
# https://www.kolada.se/verktyg/fri-sokning/?kpis=56362&years=30199,30198,30197&municipals=16756,16757,16758,16759,16760,16761,16762,16763,16764,16765,16766,16767,16768,16769,16770&rows=municipal,kpi&visualization=bar-chart&focus=16765
pacman::p_load(tidyverse,rKolada)

source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_filer.R", encoding = "utf-8", echo = FALSE)

#test_list <- diag_ekonomiskt_bistand(skapa_fil = FALSE,region_vekt = "20",)

diag_ekonomiskt_bistand <-function(region_vekt = "20", 
                                   output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                   skapa_fil = TRUE){
  
  # ========================================== Inställningar ============================================

  BaraEttLän <- region_vekt
  
  diagram_capt <- "Källa: Socialstyrelsen (statistikdatabasen) & SCB (via Koalada).\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Antal invånare (inklusive barn) som någon gång under året erhållit ekonomiskt bistånd,\ndividerat med antalet invånare den 31/12 multiplicerat med 100" 
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  i<-1
  
  # =============================================== API-uttag ===============================================
  
  #### Ta hem data från Kolada
  ekonomiskt_bistand <- get_values(
    kpi = c("N31807"),
    municipality = c("0000","0020",hamtakommuner(BaraEttLän)),
    period = 2011:2100
  )
  
  # Väljer ut relevanta variabler och byter namn
  ekonomiskt_bistand<-ekonomiskt_bistand %>% 
    select(kpi,year,value,gender,municipality) %>% 
      rename("andel"=value,"variabel"=kpi,"region"=municipality)
  
  ## Döp om variabler till mer begripliga namn
  ekonomiskt_bistand[ekonomiskt_bistand=="N00930"] <- "Invånare som någon gång under året erhållit ekonomiskt bistånd, andel (%) av bef."
  
  ekonomiskt_bistand[ekonomiskt_bistand=="Region Dalarna"] <- "Dalarna"
  
  diagram_titel <- paste0("Invånare som någon gång under året erhållit ekonomiskt bistånd, andel (%) av bef. i ",hamtaregion_kod_namn(BaraEttLän)[[2]]," och Riket ",
                          max(ekonomiskt_bistand$year))
  diagram_titel<-str_wrap(diagram_titel)
  diagram_typ <- "ekonomiskt_bistand"
  diagramfil <- "ekonomiskt_bistand.png"
  objektnamn <- diagram_typ
  
  # I anropet till funktionen skapas en variabel som sätter fokus på Dalarna och riket.
  
  gg_obj <- SkapaStapelDiagram(skickad_df = ekonomiskt_bistand %>% 
                                 filter(year%in%c(max(ekonomiskt_bistand$year)),gender=="T") %>% 
                                 mutate(fokus = ifelse( region == "Dalarna", "1", ifelse( region == "Riket", "2", "0" ))), 
                               skickad_x_var = "region", 
                               skickad_y_var = "andel", 
                               #skickad_x_grupp = "omrade",
                               manual_x_axis_text_vjust=1,
                               manual_x_axis_text_hjust=1,
                               manual_color = diagramfarger("gron_tre_fokus"),
                               x_axis_sort_value = TRUE,
                               dataetiketter = FALSE,
                               dataetiketter_antal_dec = 0,
                               manual_y_axis_title="procent",
                               x_var_fokus="fokus",
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
