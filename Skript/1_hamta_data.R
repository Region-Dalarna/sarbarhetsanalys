# Skript som hämtar data som används i rapporten "Sårbarhetsanalys av näringslivet i Dalarna".
# Notera att data som hämtas är sådan som finns tillgänglig via API. Övrig data (NMS-databasen eller källor utan API) hämtas direkt i markdownfilen.

# Nödvändiga bibliotek
if (!require("pacman")) install.packages("pacman")
p_load(here)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

# Här hamnar Excelfilerna
Output_mapp_data = "G:/skript/projekt/data/sarbarhetsanalys/"
Output_mapp_figur = here("Figurer","/")


############################
##### Arbetsmarknaden ######
############################

# Arbetslöshet 76
source(here("Skript","diagram_arbetsloshet_76.R"), encoding="UTF-8")
diagram_data_arbetsloshet_76(region_vekt =c("00","20"),
                             output_mapp_data = Output_mapp_data,
                             output_mapp_figur = Output_mapp_figur,
                             vald_farg = diagramfarger("rus_sex"),
                             returnera_figur = FALSE)

# Arbetslöshet och sysselsättningsgrad
source(here("Skript","diagram_arbetsmarknadsstatus_kommun.R"), encoding="UTF-8")
gg_arbetsmarknadsstatus = diagram_arbetsmarknadsstatus_kommun(output_mapp_data = Output_mapp_data,
                                                              output_mapp_figur = Output_mapp_figur,
                                                              diag_arbetskraftsdeltagande = FALSE,
                                                              valda_farger = diagramfarger("rus_tre_fokus"),
                                                              spara_figur = TRUE,
                                                              returnera_figur = TRUE)

# Långtidsarbetslöshet
source(here("Skript","diagram_langtidsarbetsloshet.R"), encoding="UTF-8")
gg_långtidsarbetsloshet = diagram_langtidsarb(region = c("0020"),
                                              ta_med_riket = TRUE,
                                              outputmapp = Output_mapp_figur,
                                              outputmapp_data = Output_mapp_data,
                                              jmf_ar = TRUE,
                                              returnera_figur = TRUE,
                                              spara_figur = TRUE,
                                              spara_data = TRUE,
                                              vald_farg = diagramfarger("rus_sex"))

# Andel som jobbar i offentlig sektor
source(here("Skript","diagram_andel_offentligt.R"), encoding="UTF-8")
gg_andel_offentligt = diagram_andel_offentligt(output_mapp_figur= Output_mapp_figur,
                                               output_mapp_data = Output_mapp_data,
                                               diag_totalt = TRUE,
                                               diag_kon = FALSE,
                                               vald_farg = "rus_sex",
                                               returnera_figur = TRUE,
                                               spara_figur = TRUE,
                                               spara_data = TRUE)

# Andel pendling i kommun  
source("G:/skript/diagram/diag_pendlare_over_kommungrans.R")
gg_pendling = diag_pendling_over_kommungrans(output_mapp_figur = Output_mapp_figur,
                                             output_mapp_data = Output_mapp_data,
                                             spara_data = TRUE,
                                             skapa_fil = TRUE, 
                                             diagramfarg_vektor = diagramfarger("rus_sex"),
                                             enbart_in_ut = TRUE,
                                             diag_absoluta_tal = FALSE,           
                                             diag_procent = TRUE)

# Antal arbetsställen 
source(here("Skript","diagram_arb_stallen_mm_foretagarna.R"), encoding="UTF-8")
gg_arbetsstallen = diag_foretagarna(output_mapp_figur = Output_mapp_figur,
                                    output_mapp_data = Output_mapp_data,
                                    spara_figur = TRUE,
                                    spara_data = TRUE,
                                    diag_arbetsstallen = TRUE,
                                    diag_nyforetagsamma = FALSE,
                                    diag_foretagsamma = FALSE)

# Antal nystartade företag och antalet konkurser
source(here("Skript","diagram_nyst_arbetslosa.R"), encoding="UTF-8")
gg_nyst_konk <- diagram_nystartade_konkurser(output_mapp = Output_mapp_figur,
                                             spara_figur = TRUE, 
                                             vald_farg = "rus_sex",
                                             diag_nystartade = TRUE,           
                                             diag_konkurser = TRUE,
                                             returnera_data = TRUE)

# Andel förvärvsarbetande i olika branscher (såväl län som kommun)
source(here("Skript","diagram_andel_forvarvsarbetande_bransch.R"), encoding="UTF-8")
gg_andel_forv <- diag_sysselsatta_andel(region_vekt = "20", 
                                        output_mapp = Output_mapp_figur,
                                        spara_figur = TRUE, 
                                        diag_lan = TRUE, 
                                        diag_kommun = TRUE, 
                                        returnera_data = TRUE)

# Branschbredd
source(here("Skript","diagram_branschbredd.R"), encoding="UTF-8")
gg_branschbredd = diag_branschbredd(output_mapp = Output_mapp_figur,
                                    valda_farger = "rus_sex",
                                    spara_figur = TRUE,
                                    returnera_data = TRUE)

# Medelålder och demografisk försörjningskvot
source(here("Skript","diagram_medelald_demfors.R"), encoding="UTF-8")
gg_medel_demo <- diag_demografi(outputmapp = Output_mapp_figur,
                                diag_demo_forsorjning = TRUE, 
                                diag_demo_medelalder = TRUE, 
                                valda_farger = "rus_sex",
                                spara_figur = TRUE,
                                returnera_data = TRUE)

# Andel av befolkningen med eftergymnasial utbildning (3 år)
source(here("Skript","diagram_utbniva_andelhog.R"), encoding="UTF-8")
gg_utbniva <- diag_utbniva(output_mapp = Output_mapp_figur,
                           spara_figur = TRUE,
                           returnera_data = TRUE)

# De största företagen i varje kommun
source("G:/skript/diagram/diag_storsta_arbetsgivare_kommun.R", encoding = "utf-8", echo = FALSE)
gg_storsta_foretag <- diag_storsta_arbetsgivare_kommun(valda_kommuner = hamtakommuner("20",tamedlan=FALSE,tamedriket=FALSE),
                                                      skriv_fil = FALSE,
                                                      output_mapp = Output_mapp,
                                                      returnera_data = TRUE)

#####################
##### Inte API #####
####################

# Diverse från företagarna. Data uppdaterades senaste i mitten av november 2023
source(here("Skript","diagram_arb_stallen_mm_foretagarna.R"), encoding="UTF-8")
gg_foretagssamma <- diag_foretagarna(region_vekt = "20",
                                     output_mapp = Output_mapp_figur,
                                     spara_figur = TRUE, 
                                     valda_farger = "rus_tre_fokus",
                                     valda_farger_foretagssamma = "rus_sex",
                                     diag_arbetsstallen = FALSE,
                                     diag_nyforetagsamma = FALSE,
                                     diag_foretagsamma = TRUE,
                                     returnera_data = TRUE)

# Antal företag som utgör 50 procent av den totala lönesumman (NMS)
source(here("Skript","diagram_50_proc_lonesumma_NMS.R"), encoding="UTF-8")
gg_50proc_lonesumma <- diag_50proc_lonesumma(output_mapp = Output_mapp_figur,
                                            spara_figur = TRUE,
                                            returnera_data = TRUE)

# Tabell med de fem största branscherna i Dalarna - NMS, både län och kommun.
# Inget gg-plot objekt skapas utan man får manuellt lägga till i så fall (en png-fil skapas som sedan läses in i markdown)
# OBS! Funkar för tillfället enbart på regionytan
# source(here("NMS_branscher_top5.R"), encoding = "utf-8", echo = FALSE)
# diag_50proc_lonesumma(region_vekt=vald_region,
#                       output_mapp =Output_mapp,
#                       skapa_fil=skapa_fil,
#                       diag_lan=TRUE,
#                       diag_kommun=TRUE)

