#
pacman::p_load(tidyverse,openxlsx)

source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)
options(dplyr.summarise.inform = FALSE)

#test=diag_varsel_per_manad(skrivdiagramfil = FALSE)
diag_varsel_per_manad <- function(vald_region = "20",
                                  output_mapp = "G:/Samhällsanalys/API/Fran_R/utskrift/",
                                  diagram_capt = "Källa: Arbetsförmedlingen\nBearbetning: Samhällsanalys, Region Dalarna",
                                  start_tid="1992-01",
                                  antal_som_visas=5,
                                  vald_farg=diagramfarger("gron_sex")[6],
                                  skrivdiagramfil = TRUE
) {
  
  gg_list <- list()  # skapa en tom df att lägga flera ggplot-objekt i (om man skapar flera diagram)
  
  # skapa textsträng med kortnamn för län av vald(a) region(er)
  geografi <- hamtaregion_kod_namn(vald_region)$region %>% skapa_kortnamn_lan()
  
  # om Riket är med i vektorn, byt ut till Sverige
  geografi[geografi == "Riket"] <- "Sverige"
  
  # ================================== nedladdning av fil ============================================
  
  # hämta webbsidan med tidigare statistik på Arbetsförmedlingen och spara som en vektor
  webbsida <- suppressWarnings(readLines("https://arbetsformedlingen.se/statistik/sok-statistik/tidigare-statistik"))
  
  varsel_index <- which(str_detect(webbsida, "varsel"))   # sök alla rader där "varsel" finns med
  xlsx_index <- which(str_detect(webbsida, ".xlsx"))      # sök alla rader där ".xlsx" finns med
  
  fil_index <- varsel_index[varsel_index %in% xlsx_index]    # ta index där båda är med
  fil_strang <- webbsida[fil_index]                          # skapa sträng med det element där båda är med
  
  # i den strängen, ta ut startposition för alla "/download/" som hittar i strängen (det är sökvägar)
  start_sokvagar <- str_locate_all(fil_strang, "/download/")[[1]][,1]  
  
  # funktion för att ta ut fullständig url från de startpositioner vi hittade i raden ovan
  extrahera_sokvag <- function(strang, startpos) {
    
    nystrang <- str_sub(strang, startpos, nchar(strang))
    slutpos <- str_locate(nystrang, '\"')[[1]]-1
    
    retur_strang <- str_sub(nystrang, 1, slutpos)
    retur_strang <- paste0("https://arbetsformedlingen.se", retur_strang)
    return(retur_strang)
  }       
  
  # vi skapar en vektor med fullständiga sökvägar för samtliga excelfiler som finns på webbsidan
  af_urler <- start_sokvagar %>% map_chr(~ extrahera_sokvag(fil_strang, .x))
  
  # ta ut sökväg för varsel per län, dvs en sökväg som innehåller "varsel" och "lan" men inte "bransch"
  varsel_lan_url <- af_urler[str_detect(af_urler, "varsel") & str_detect(af_urler, "lan") & !str_detect(af_urler, "bransch")]   
  
  # spara filen temporärt för att kunna extrahera fliknamn och kolla startrad
  
  td = tempdir()              # skapa temporär mapp
  varsel_fil <- tempfile(tmpdir=td, fileext = ".xlsx")
  
  download.file(varsel_lan_url, destfile = varsel_fil, mode = "wb")       # ladda hem hela filen, mode = "wb" viktigt, annars blir det fel
  fliknamn <- getSheetNames(varsel_fil)               # hämta alla fliknamn ur Excelfilen
  flik_alla_ar <- fliknamn[str_detect(fliknamn, "Månad")]
  
  startrads_test_df <- read.xlsx(varsel_lan_url, sheet = flik_alla_ar, skipEmptyRows = FALSE)
  startrad <- which(!is.na(startrads_test_df[[1]]))[1]-1 
  
  # läs in Excelfilen till en df direkt från url:en
  varsel_df <-read.xlsx(varsel_lan_url, sheet = flik_alla_ar, startRow = startrad)
  
  # ===================================== slut på hämtning av fil =====================================
  
  # bearbeta varsel_df - döp om kolumner
  names(varsel_df)[1] <- "tid"
  names(varsel_df) <- skapa_kortnamn_lan(names(varsel_df))
  
  # pivotera df:n så att det blir long-format
  varsel_df <- varsel_df %>% 
    pivot_longer(2:ncol(varsel_df), names_to = "region", values_to = "antal") %>%
    mutate(ar = str_sub(tid, 1, 4),
           manad = format(as.Date(paste(ar, str_sub(tid, 6,7), "1", sep = "-")), "%b"),
           manad_long = format(as.Date(paste(ar, str_sub(tid, 6,7), "1", sep = "-")), "%B"),
           ar_manad = paste0(ar, " - ", manad),
           manad_ar = paste0(manad, " ", ar),
           manad_ar_long = paste0(manad_long, " ", ar),
           region = ifelse(region == "Riket", "Sverige", region))
  
  max_tid_long <- varsel_df %>% 
    filter(tid == max(tid), region == geografi) %>% 
    select(manad_ar_long) %>% 
    pull()
  
  min_tid_long <- varsel_df %>% 
    filter(tid == start_tid, region == geografi) %>% 
    select(manad_ar_long) %>% 
    pull()
  
  max_tid <- varsel_df %>% 
    filter(tid == max(tid), region == geografi) %>% 
    select(manad_ar) %>% 
    pull()
  
  min_tid <- varsel_df %>% 
    filter(tid == start_tid, region == geografi) %>% 
    select(manad_ar) %>% 
    pull()
  
  # ==================================== skapa diagram =============================================
  
  for (reg in geografi) {
    
    diagram_titel <- paste0("Varsel per månad i ", reg, " ", min_tid_long, " - ", max_tid_long)
    diagramfil <- paste0("varsel_", reg, "_", min_tid, "-", max_tid, ".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = varsel_df %>%
                                   filter(region %in% reg,tid>=start_tid),
                                 skickad_x_var = "tid", 
                                 skickad_y_var = "antal", 
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diagram_capt,
                                 x_axis_storlek = 6,
                                 manual_y_axis_title = "Antal personer berörda av varsel om uppsägning",
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 x_axis_visa_var_xe_etikett = antal_som_visas,
                                 stodlinjer_avrunda_fem = TRUE,
                                 logga_scaling = 25,
                                 skriv_till_diagramfil = skrivdiagramfil, 
                                 manual_color = vald_farg,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil)
    
    gg_list <- c(gg_list, list(gg_obj))        # lägg till ggplot-objektet i gg_list
    names(gg_list)[[length(gg_list)]] <- str_replace(diagramfil, ".png", "")   # döp ggplot-objektet
    
  }
  return(gg_list)
}
