##### Hämtar data för att beräkna branschbredd #######
pacman::p_load(httr,tidyverse,rKolada,askpass)

source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_filer.R", encoding = "utf-8", echo = FALSE)
options(dplyr.summarise.inform = FALSE)

#test_list <- diag_nystartade_konk(skapa_fil = FALSE)

diag_nystartade_konk <-function(region_vekt = "20", 
                           output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                           skapa_fil = TRUE,
                           diag_nystartade=TRUE,
                           diag_konkurser=TRUE){
  
  # ========================================== Inställningar ============================================
  
  nyckel_mapp <- "G:/Samhällsanalys/Automatisering och R/nycklar/"
  
  BaraEttLän <- region_vekt
  
  diagram_capt <- "Källa: SCB (via Kolada/RKA)\nBearbetning: Samhällsanalys, Region Dalarna" 
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  objektnamn=c()
  i<-1

  # =============================================== API-uttag ===============================================
  
  #### Dra hem variablerna från Kolada
  Nystartade_df <- get_values(
    kpi = c("N00999"),
    municipality = c("0000","0020",hamtakommuner(BaraEttLän)),
    period = 1900:2100
  )
  
  Konkurser_df <- get_values(
    kpi = c("N00926"),
    municipality = c("0000","0020",hamtakommuner(BaraEttLän)),
    period = 1900:2100
  )
  
  # Nystartade företag
  
  Nystartade_df<-Nystartade_df %>% 
    select(year,gender,value,municipality) %>%
        rename("nystartade_ftg"=value)
  
  # Gör om år till en character
  Nystartade_df$year<-as.character(Nystartade_df$year)

  # Konkurser
  Konkurser_df<-Konkurser_df %>% 
    select(year,gender,value,municipality) %>%
    rename("konkurser"=value)
  
  # Gör om år till en character
  Konkurser_df$year<-as.character(Konkurser_df$year)
  
  if(diag_nystartade==TRUE){
    diagram_titel <- paste0("Antal nystartade företag per 1000 invånare (16-64 år)")
    diagram_typ <- "nystartade"
    diagramfil <- "nystartade.png"
    objektnamn <- c(objektnamn,diagram_typ)
    
    gg_obj <- SkapaStapelDiagram(skickad_df = Nystartade_df %>% 
                                   filter(year%in%c(min(Nystartade_df$year),"2015",max(Nystartade_df$year))) %>% 
                                    filter(gender=="T") %>% 
                                      mutate(municipality=ifelse(municipality=="Region Dalarna","Dalarna",municipality)) %>% 
                                        mutate(municipality=ifelse(municipality=="Riket", "Sverige",municipality)), 
                                 skickad_x_var = "municipality", 
                                 skickad_y_var = "nystartade_ftg", 
                                 skickad_x_grupp = "year",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = diagramfarger("rus_sex"),
                                 x_axis_sort_value = TRUE,
                                 x_axis_sort_grp = 3,
                                 vand_sortering=TRUE,
                                 dataetiketter = FALSE,
                                 dataetiketter_antal_dec = 0,
                                 manual_y_axis_title="Nystartade företag/1000 invånare",
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diagram_capt,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
 
  if(diag_konkurser==TRUE){
    diagram_titel <- paste0("Antal konkurser per 1000 invånare (16-64 år)")
    diagram_typ <- "konkurser"
    diagramfil <- "konkurser.png"
    objektnamn <- c(objektnamn,diagram_typ)
    
    gg_obj <- SkapaStapelDiagram(skickad_df = Konkurser_df %>% 
                                   filter(year%in%c(min(Konkurser_df$year),max(Konkurser_df$year))) %>% 
                                    filter(gender=="T") %>% 
                                      mutate(municipality=ifelse(municipality=="Region Dalarna","Dalarna",municipality)) %>% 
                                        mutate(municipality=ifelse(municipality=="Riket", "Sverige",municipality)), 
                                 skickad_x_var = "municipality", 
                                 skickad_y_var = "konkurser", 
                                 skickad_x_grupp = "year",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = diagramfarger("rus_sex"),
                                 x_axis_sort_value = TRUE,
                                 x_axis_sort_grp = 2,
                                 vand_sortering=TRUE,
                                 dataetiketter = FALSE,
                                 dataetiketter_antal_dec = 0,
                                 manual_y_axis_title="Konkurser/1000 invånare",
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diagram_capt,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  names(gg_list) <-c(objektnamn)
  return(gg_list)
  
}
