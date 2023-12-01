## =================================================================================================================
# Skript som laddar hem data för arbetsmarknadsstatus från SCB och skriver ut 3 diagram (arbetslöshet, arbetskraftsdeltagande, sysselsättningsgrad)
# Källa  
# =================================================================================================================
pacman::p_load(pxweb,httr,askpass,stringi,svDialogs,tidyverse)

# Laddar in de funktioner som används för att skapa diagram
source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)

#test_list <- diag_arbetsmarknadsstatus(skapa_fil = FALSE,diag_arbetslosthet=FALSE)

diag_arbetsmarknadsstatus <-function(region_vekt = "20", 
                                     output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                     farger=diagramfarger("rus_tre_fokus"),
                                     skapa_fil = TRUE,
                                     diag_arbetslosthet = TRUE,
                                     diag_arbetskraftsdeltagande = TRUE,
                                     diag_sysselsattningsgrad = TRUE){
  
  # ========================================== Inställningar ============================================
  # Text till diagram
  diagram_capt <- c("Källa: SCB:s öppna statistikdatabas, befolkningens arbetsmarknadsstatus (BAS).\nBearbetning: Samhällsanalys, Region Dalarna.\n",
                    "Källa: SCB:s öppna statistikdatabas, befolkningens arbetsmarknadsstatus (BAS).\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Andel i befolkningen som är i arbetskraften (arbetskraftsdeltagande).",
                    "Källa: SCB:s öppna statistikdatabas, befolkningens arbetsmarknadsstatus (BAS).\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Andelen av befolkningen som är sysselsatt (sysselsättningsgrad).")
  
  ValdGeografi <- hamtaregion_kod_namn(region_vekt)$region
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  objektnamn<-c()
  i=1 # Räknare som används för att lägga till objekt i listan
 
  #==========================================================================================================
  # För att komma förbi proxyn
  set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090, username = Sys.getenv("userid"), password = Sys.getenv("pwd")))
  set_config(config(ssl_verifypeer = 0L))
  # =============================================== API-uttag ===============================================
  # "Adresser" till SCBs databas
  url1 <- "https://api.scb.se"
  url2 <- "/OV0104/v1/doris/sv/ssd/AM/AM0210/AM0210A/ArbStatusAr"
  url3 <- paste0(url1, url2)
  
  # Om man vill ta reda vilka variabler som finns i tabellen. 
  # pxvarlist(url3)                        # för att se alla variabler för en tabell
  # pxvardelist(url3, "contentscode")      # för att se alla värden för vald variabel
  
  # Tar bara ut data för de variabler vi vill ha:
  cont_code=c()
  if(diag_arbetslosthet==TRUE) cont_code=c(cont_code, "000006J1")
  if(diag_arbetskraftsdeltagande == TRUE) cont_code=c(cont_code,"000006IY")
  if(diag_sysselsattningsgrad == TRUE) cont_code=c(cont_code,"000006J6")
  
  # Variabler som skall tas ut
  varlista <-  list("Region"=c(hamtakommuner(lan=region_vekt,tamedriket = TRUE)),
                    "Kon"=c("1+2"),
                    "Alder"=c("20-64"),
                    "Fodelseregion"=c("tot"),
                    "ContentsCode"=cont_code,
                    "Tid"=max(hamta_giltiga_varden_fran_tabell(url3, "tid")))
  
  # Uttag av data
  px_uttag <- pxweb_get(url = url3,query = varlista)
  
  # Konverterar data till en Data Frame och tar med regionkod
  arbetsmarknadsstatus_df <- as.data.frame(px_uttag) %>% 
    cbind(regionkod = as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>% 
            select(Region))
  
  arbetsmarknadsstatus_df <- arbetsmarknadsstatus_df %>% 
    rename(regionkod = Region)%>%
    relocate(regionkod, .before = region)
  
  # Tar bort län i länsnamnet
  
  arbetsmarknadsstatus_df$region <-skapa_kortnamn_lan(arbetsmarknadsstatus_df$region)
  
  arbetsmarknadsstatus_df <- arbetsmarknadsstatus_df
  # skapa fokusvariabel för att fokusera på valt län och riket
  arbetsmarknadsstatus_df$fokus <- NA                      # en metod för att få bort warning messages för "Unknown or uninitialised column: `fokus`."
  arbetsmarknadsstatus_df$fokus <- 0
  arbetsmarknadsstatus_df$fokus[arbetsmarknadsstatus_df$region =="Dalarna"] <- 1
  arbetsmarknadsstatus_df$fokus[arbetsmarknadsstatus_df$region =="Riket"] <- 2
  

  if(diag_arbetslosthet==TRUE){
    diagramtitel <- paste0("Arbetslöshet i åldersgruppen 20-64 år ",unique(arbetsmarknadsstatus_df$år))
    diagramfilnamn <- paste0("Arbetslöshet","_kommun",".png")
    objektnamn <-c(objektnamn,paste0("Arbetslöshet","_kommun"))
    
    # Skapar diagram 
    gg_obj <- SkapaStapelDiagram(skickad_df = arbetsmarknadsstatus_df %>% 
                                   mutate(region=ifelse(region=="Riket", "Sverige",region)),  
                                 skickad_x_var = "region", 
                                 skickad_y_var = "arbetslöshet", 
                                 #skickad_x_grupp = "kön",
                                 manual_y_axis_title = "procent",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = farger,
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt[1],
                                 x_var_fokus = "fokus",
                                 diagram_facet = FALSE,
                                 facet_grp = "födelseregion",
                                 facet_scale = "fixed",
                                 facet_legend_bottom = TRUE,
                                 x_axis_sort_value = TRUE,
                                 x_axis_lutning = 45,
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skapa_fil)
    
    
    gg_list[[i]] <- gg_obj
    i=i+1
  }
  
  if(diag_arbetskraftsdeltagande==TRUE){
    diagramtitel <- paste0("Arbetskraftsdeltagande i åldersgruppen 20-64 år ",unique(arbetsmarknadsstatus_df$år))
    diagramfilnamn <- paste0("Arbetskraftsdeltagande","_kommun",".png")
    objektnamn <-c(objektnamn,paste0("Arbetskraftsdeltagande","_kommun"))
    
    # Skapar diagram 
    gg_obj <- SkapaStapelDiagram(skickad_df = arbetsmarknadsstatus_df %>% 
                                   mutate(region=ifelse(region=="Riket", "Sverige",region)), 
                                 skickad_x_var = "region", 
                                 skickad_y_var = "arbetskraftsdeltagande", 
                                 #skickad_x_grupp = "kön",
                                 manual_y_axis_title = "procent",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = farger,
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt[2],
                                 diagram_facet = FALSE,
                                 x_var_fokus = "fokus",
                                 facet_grp = "födelseregion",
                                 facet_scale = "fixed",
                                 facet_legend_bottom = TRUE,
                                 x_axis_sort_value = TRUE,
                                 x_axis_lutning = 45,
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skapa_fil)
    
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  if(diag_sysselsattningsgrad==TRUE){
    diagramtitel <- paste0("Sysselsättningsgrad i åldersgruppen 20-64 år ",unique(arbetsmarknadsstatus_df$år))
    diagramfilnamn <- paste0("Sysselsättningsgrad","_kommun",".png")
    objektnamn <-c(objektnamn,paste0("Sysselsättningsgrad","_kommun"))
    
    # Skapar diagram 
    gg_obj <- SkapaStapelDiagram(skickad_df = arbetsmarknadsstatus_df %>% 
                                   mutate(region=ifelse(region=="Riket", "Sverige",region)), 
                                 skickad_x_var = "region", 
                                 skickad_y_var = "sysselsättningsgrad", 
                                 #skickad_x_grupp = "kön",
                                 manual_y_axis_title = "procent",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = farger,
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt[3],
                                 diagram_facet = FALSE,
                                 x_var_fokus = "fokus",
                                 facet_grp = "födelseregion",
                                 facet_scale = "fixed",
                                 facet_legend_bottom = TRUE,
                                 x_axis_sort_value = TRUE,
                                 x_axis_lutning = 45,
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skapa_fil)
    
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  names(gg_list)<-objektnamn
  return(gg_list)
}

