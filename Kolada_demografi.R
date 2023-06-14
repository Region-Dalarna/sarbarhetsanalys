##### Hämtar data för att beräkna branschbredd #######
pacman::p_load(tidyverse,rKolada)

source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_filer.R", encoding = "utf-8", echo = FALSE)


#test_list <- diag_demografi(skapa_fil = FALSE,region_vekt ="20")

diag_demografi <-function(region_vekt = "20", 
                          output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                          skapa_fil = TRUE,
                          diag_demo_forsorjning=TRUE,
                          diag_demo_medelalder=TRUE){
  
  # ========================================== Inställningar ============================================
  
  nyckel_mapp <- "G:/Samhällsanalys/Automatisering och R/nycklar/"
  
  BaraEttLän <- region_vekt
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  objektnamn=c()
  i<-1

  # =============================================== API-uttag ===============================================
  
  #### Dra hem variablerna från Kolada
  demo_df <- get_values(
    kpi = c("N00927"),
    municipality = c("0000",hamtakommuner(BaraEttLän)),
    period = 1900:2100
  )
  
  #### Dra hem variablerna från Kolada
  medelalder_df <- get_values(
    kpi = c("N00959"),
    municipality = c("0000",hamtakommuner(BaraEttLän)),
    period = 1900:2100
  )
  
  # Demografisk försörjningskvot
  # Väljer ut relevanta variabler och döper om value
  
  demo_df<-demo_df %>% 
    select(year,gender,value,municipality) %>%
      mutate("value"=value*100) %>% 
        rename("Demografi_andel"=value)
  
  # Gör om år till en character
  demo_df$year<-as.character(demo_df$year)
  
  # Medelålder
  # Väljer ut relevanta variabler och döper om value
  
  medelalder_df<-medelalder_df %>% 
    select(year,gender,value,municipality) %>%
      mutate("value"=value) %>% 
        rename("Medelalder"=value)
  
  # Gör om år till en character
  medelalder_df$year<-as.character(medelalder_df$year)
  
  if(diag_demo_forsorjning==TRUE){
    diagram_titel <- paste0("Demografisk försörjningskvot")
    diagram_typ <- "demo_fors"
    diagramfil <- "demo_fors.png"
    objektnamn <- c(objektnamn,diagram_typ)
    diagram_capt <- "Källa: SCB (via RKA/Kolada)\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Den demografiska försörjningskvoten beräknas som summan av antal personer 0-19 år och antal personer 65 år och äldre dividerat med antal personer 20-64 år.\nEtt värde över 100 innebär att gruppen äldre och yngre är större än den i arbetsför ålder." 
    
    gg_obj <- SkapaStapelDiagram(skickad_df = demo_df %>% 
                                   filter(year%in%c(min(demo_df$year),max(demo_df$year))) %>% 
                                    filter(gender=="T") %>% 
                                      mutate(municipality=ifelse(municipality=="Riket", "Sverige",municipality)),
                                 skickad_x_var = "municipality", 
                                 skickad_y_var = "Demografi_andel", 
                                 skickad_x_grupp = "year",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = diagramfarger("rus_sex"),
                                 x_axis_sort_value = TRUE,
                                 x_axis_sort_grp = 2,
                                 vand_sortering=TRUE,
                                 dataetiketter = FALSE,
                                 dataetiketter_antal_dec = 0,
                                 manual_y_axis_title="Demografisk försörjningskvot",
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diagram_capt,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  medelalder_df$fokus <- NA                      # en metod för att få bort warning messages för "Unknown or uninitialised column: `fokus`."
  medelalder_df$fokus <- 0
  medelalder_df$fokus[medelalder_df$municipality =="Riket"] <- 1
  
  if(diag_demo_medelalder==TRUE){
  
    diagram_titel <- paste0("Medelålder år ",max(medelalder_df$year))
    diagram_typ <- "medelalder"
    diagramfil <- "medelalder.png"
    objektnamn <-  c(objektnamn,diagram_typ)
    diagram_capt <- "Källa: SCB (via RKA/Kolada)\nBearbetning: Samhällsanalys, Region Dalarna\n" 
    
    gg_obj <- SkapaStapelDiagram(skickad_df = medelalder_df %>% 
                                   filter(year%in%c(max(medelalder_df$year))) %>% 
                                    filter(gender=="T")%>% 
                                      mutate(municipality=ifelse(municipality=="Riket", "Sverige",municipality)),
                                 skickad_x_var = "municipality", 
                                 skickad_y_var = "Medelalder", 
                                 #skickad_x_grupp = "year",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = diagramfarger("rus_tva_fokus"),
                                 x_axis_sort_value = TRUE,
                                 dataetiketter = FALSE,
                                 dataetiketter_antal_dec = 1,
                                 manual_y_axis_title="Medelålder",
                                 x_var_fokus = "fokus",
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
