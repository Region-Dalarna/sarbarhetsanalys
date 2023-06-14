# Masterskript som uppdaterar figurerna i projektet sårbarhetsanalys
# Måste köras lokalt (blir felmeddelande annars), men delen som skapar en tabell med största branscher och företag kan enbart köras på regionytan.
pacman::p_load(here,httr,keyring)

# Skapar listan

source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)

#test=hamta_figurer(skapa_fil=TRUE,skapa_ppt=FALSE)
hamta_figurer <- function(skapa_ppt=FALSE,skapa_fil=TRUE,Output_mapp = here("Diagram","/")){
  master_lista <-list()
  vald_region="20"
  
  # Tar oss förbi gateway. Behövs för skriptet som skriver ut varsel på månadsbasis.
  set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090, username = Sys.info()[["user"]], password = key_get("rd")))
  set_config(config(ssl_verifypeer = 0L))

  #=========================================================
  # Historiska kriser i Dalarna
  #=========================================================
  

  # Arbetslöshet från 1976 till senaste data
  # Uppdateras automatiskt förutsatt att SCB inte byter tabell (gäller framförallt mer sentida data)
  source(here("PXweb_arbetsloshet_76_korrekt.R"), encoding = "utf-8", echo = FALSE)
  master_lista <- c(master_lista,diag_arbetsloshet_76(region_vekt=vald_region,
                                                      output_mapp = Output_mapp,
                                                      skapa_fil=skapa_fil,
                                                      diag_stapel=TRUE,
                                                      diag_linje=TRUE))

  
  #==========================================
  # Socioekonomi 
  #==========================================

  # Demografi - Kolada - 2 figurer demografisk försörjningskvot och medelålder
  source(here("Kolada_demografi.R"), encoding = "utf-8", echo = FALSE)
  master_lista <- c(master_lista,diag_demografi(skapa_fil = skapa_fil,
                                                region_vekt=vald_region,
                                                output_mapp = Output_mapp,
                                                diag_demo_forsorjning=TRUE,
                                                diag_demo_medelalder=TRUE))
  
  # Arbetsmarknadsstatus (arbetslöshet mm) på kommunnivå (inkl riket och länet) - Pxweb, SCB 2 figur (sysselsättningsgrad och arbetslöshet enbart)
  # Ingen uppdelning på kön eller bakgrund
  # Uppdateras automatiskt
  source(here("PXweb_arbetsmarknadsstatus.R"), encoding = "utf-8", echo = FALSE)
  master_lista <-c(master_lista,diag_arbetsmarknadsstatus(region_vekt =  vald_region,
                                                          skapa_fil = skapa_fil,
                                                          output_mapp = Output_mapp,
                                                          diag_arbetslosthet = TRUE,
                                                          diag_arbetskraftsdeltagande = FALSE,
                                                          diag_sysselsattningsgrad = TRUE))
  
  # Långtidsarbetslöshet - Kolada 1 figur
  source(here("Kolada_langtidsarbetsloshet.R"), encoding = "utf-8", echo = FALSE)
  master_lista <-c(master_lista,diag_langtidsarbetsloshet(region_vekt =  vald_region,
                                                          skapa_fil = skapa_fil,
                                                          output_mapp = Output_mapp))
  
  # Andel med högre utbildning (minst 3 år) - pxweb: 2 figurer (kommunal nivå)
  # Ingen uppdelning på kön eller bakgrund
  source(here("pxweb_utbniva_eftergym.R"), encoding = "utf-8", echo = FALSE)
  master_lista <- c(master_lista,diag_utbniva(skapa_fil = skapa_fil,
                                              output_mapp = Output_mapp,
                                              region_vekt=vald_region,
                                              diag_utb_kommun=TRUE,
                                              diag_utb_kommun_kon=FALSE))

  

  #########################
  ##### Arbetsmarknad #####
  #########################

  # Andelen anställda inom offentlig verksamhet på kommunnivå - pxweb 1 figur
  source(here("PXweb_andel_offentligt.R"), encoding = "utf-8", echo = FALSE)
  master_lista <- c(master_lista,diag_andel_offentligt(skapa_fil = skapa_fil,
                                                       output_mapp = Output_mapp,
                                                       region_vekt=vald_region))

  
  # # Pendlingsintensitet på kommunnivå - Kolada 1 figur - visar ut och inpendling i Dalarnas kommuner
  source(here("Kolada_pendling.R"), encoding = "utf-8", echo = FALSE)
  master_lista <- c(master_lista,diag_pendling(skapa_fil = skapa_fil,
                                               output_mapp = Output_mapp,
                                               region_vekt=vald_region,
                                               diag_pendling_andel=TRUE))
  
  # Största företag per kommun
  source("G:/skript/diagram/diag_storsta_arbetsgivare_kommun.R", encoding = "utf-8", echo = FALSE)
  master_lista <- c(master_lista,diag_storsta_arbetsgivare_kommun(valda_kommuner=hamtakommuner(vald_region,tamedlan=FALSE,tamedriket=FALSE),
                                                                  skriv_fil = TRUE,
                                                                  output_mapp = Output_mapp))
  
  ########################
  ##### Företagande ######
  ########################
  
  # Två figurer kopplade till antal arbetsställen och företagsamhet - Företagarna (Excel)
  source(here("Foretagarna_diverse.R"), encoding = "utf-8", echo = FALSE)
  master_lista <- c(master_lista,diag_ranking(skapa_fil = skapa_fil,
                                              output_mapp = Output_mapp,
                                              region_vekt=vald_region,
                                              diag_arbetsstallen=TRUE,
                                              diag_nyforetagsamma=FALSE,
                                              diag_foretagsamma=TRUE))
  
  # Nystartade företag och konkurser - Kolada - 2 figurer - visar antalet nystartade företag och konurser per 1000 invånare och
  source(here("Kolada_nystartade_ftg.R"), encoding = "utf-8", echo = FALSE)
  master_lista <- c(master_lista,diag_nystartade_konk(skapa_fil = skapa_fil,
                                                      region_vekt=vald_region,
                                                      output_mapp = Output_mapp,
                                                      diag_nystartade=TRUE,
                                                      diag_konkurser=TRUE))
  
  ########################
  #####   Beroende  ######
  ########################
  
  # Sysselsatta per bransch (andel)  - pxweb 16 figurer, län och kommun, - Visar hur stor andel av Dalarnas förvärvsarbetande som jobbar i olika branscher (i jämförelse med riket)
  source(here("PXweb_sysselsatta_andel_bransch.R"), encoding = "utf-8", echo = FALSE)
  master_lista <- c(master_lista,diag_sysselsatta_andel(skapa_fil = skapa_fil,
                                                        output_mapp = Output_mapp,
                                                        region_vekt=vald_region,
                                                        lan=TRUE,
                                                        kommun=TRUE))
  
  # Branschbredd - Kolada - 1 figur (inkl. förändring)
  source(here("Kolada_branschbredd.R"), encoding = "utf-8", echo = FALSE)
  master_lista <- c(master_lista,diag_branschbredd(skapa_fil = skapa_fil,
                                                   output_mapp = Output_mapp,
                                                   region_vekt=vald_region))

  
  # Tabell med de fem största branscherna i Dalarna - NMS, både län och kommun.
  # Inget gg-plot objekt skapas utan man får manuellt lägga till i så fall (en png-fil skapas som sedan läses in i markdown)
  # OBS! Funkar för tillfället enbart på regionytan
  # source(here("NMS_branscher_top5.R"), encoding = "utf-8", echo = FALSE)
  # diag_50proc_lonesumma(region_vekt=vald_region,
  #                       output_mapp =Output_mapp,
  #                       skapa_fil=skapa_fil,
  #                       diag_lan=TRUE,
  #                       diag_kommun=TRUE)
 

  # Antal företag för att komma upp i 50 procent av lönesumma (kommun och LA-region) - NMS 2 figurer
  # OBS! Funkar för tillfället enbart på regionytan
  # source(here("NMS_50procent_lonesumma.R"), encoding = "utf-8", echo = FALSE)
  # master_lista <- c(master_lista,diag_50proc_lonesumma(skapa_fil = skapa_fil,
  #                                                      output_mapp = Output_mapp,
  #                                                      region_vekt=vald_region,
  #                                                      diag_kommun=TRUE,
  #                                                      diag_LA=TRUE))

  ##########################################################################################
  #######################           Export till Powerpoint           ####################### 
  ##########################################################################################
  if (skapa_ppt==TRUE){
    
    dag <- paste0(gsub("^0", "", format(Sys.Date(), "%d")), "_")
    man_ar <- format(Sys.Date(), "%b_%Y")
    datum <- paste0(dag, man_ar)
    filnamn_datum=paste0("Kompetensforsorjning_",datum,".pptx")
    output <- here("Output","/")
    
    source("G:/skript/jon/PPT/PPT_jon_slutgiltig.R", encoding = "utf-8", echo = FALSE)
    skapa_ppt_fran_lista(pptlista=master_lista,"Kompetensförsörjning i Dalarnas län",filnamn=filnamn_datum,output_mapp=output)
  }
  return(master_lista)
  
}

