
hamta_data_langtidsarbetsloshet <-function(region_vekt = "20", 
                                           output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                           filnamn = "langtidsarbesloshet_kommun.xlsx",
                                           spara_data = TRUE,
                                           cont_code = c("N03923")){
  
  ##### Andel som är långtidsarbetslösa från Kolada #######
  # https://www.kolada.se/verktyg/fri-sokning/?kpis=167067&years=30199,30198,30197&municipals=16765&rows=municipal,kpi&visualization=bar-chart
 
  if (!require("pacman")) install.packages("pacman")
  p_load(rKolada,
         tidyverse,
         openxlsx)
  
  source("https://raw.githubusercontent.com/FaluPeppe/func/main/func_API.R")
 
  # =============================================== API-uttag ===============================================
  # Tyvärr saknas data för Dalarna och Riket de senste två åren, varför jag bara fokuserar på Dalarnas kommuner
  #### Ta hem data från Kolada
  langtidsarbetsloshet <- get_values(
    kpi = cont_code,
    municipality = c("0000","0020",hamtakommuner(region_vekt)),
    period = 2011:2100
  )
  
  # Väljer ut relevanta variabler och byter namn
  langtidsarbetsloshet<-langtidsarbetsloshet %>% 
    select(kpi,year,value,gender,municipality) %>% 
      rename("andel" = value,"variabel" = kpi,"region" = municipality)
  
  # Döper om vissa variabler
  langtidsarbetsloshet <- langtidsarbetsloshet %>% 
    mutate(region = ifelse(region=="Region Dalarna","Dalarna",region),
           variabel = case_when(variabel == "N03923" ~ "Langtidsarbetsloshet"))
  
  if (spara_data==TRUE){
    write.xlsx(langtidsarbetsloshet,paste0(output_mapp,filnamn))
  }
}
