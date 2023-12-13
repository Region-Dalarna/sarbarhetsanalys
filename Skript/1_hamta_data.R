# Skript som hämtar data som används i rapporten "Sårbarhetsanalys av näringslivet i Dalarna".
# Notera att data som hämtas är sådan som finns tillgänglig via API. Övrig data (NMS-databasen eller källor utan API) hämtas direkt i markdownfilen.

# Nödvändiga bibliotek
if (!require("pacman")) install.packages("pacman")
p_load(here)

# Här hamnar Excelfilerna
Output_mapp = "G:/skript/projekt/data/sarbarhetsanalys/"
Output_mapp_figur = here("Figurer","/")


############################
##### Arbetsmarknaden ######
############################

# Arbetslöshet 76
source(here("Skript","diagram_arbetsloshet_76.R"), encoding="UTF-8")
gg_arbetsloshet_76 <- diagram_data_arbetsloshet_76(region_vekt =c("00","20"),
                                                   output_mapp_excel = Output_mapp,
                                                   output_mapp_figur = Output_mapp_figur,
                                                   vald_farg = "rus_sex",
                                                   spara_data = FALSE,
                                                   returnera_figur = TRUE)

# Arbetslöshet och sysselsättningsgrad
source(here("Skript","diagram_arbetsmarknadsstatus_kommun.R"), encoding="UTF-8")
gg_arbetsmarknadsstatus = diagram_arbetsmarknadsstatus_kommun(output_mapp_figur = Output_mapp_figur,
                                                             diag_arbetskraftsdeltagande = FALSE,
                                                             diag_farger = "rus_tre_fokus",
                                                             returnera_data = TRUE,
                                                             spara_figur = TRUE,
                                                             returnera_figur = TRUE)

# Långtidsarbetslöshet
source(here("Skript","diagram_langtidsarbetsloshet.R"), encoding="UTF-8")
gg_långtidsarbetsloshet = diagram_langtidsarb(region = c("0020"),
                                              ta_med_riket = TRUE,
                                              outputmapp = Output_mapp_figur,
                                              jmf_ar = TRUE,
                                              returnera_figur = TRUE,
                                              returnera_data = TRUE,
                                              spara_figur = TRUE,
                                              vald_farg = "rus_sex")

# Andel som jobbar i offentlig sektor
source(here("Skript","diagram_andel_offentligt.R"), encoding="UTF-8")
gg_andel_offentligt = diagram_andel_offentligt(output_mapp_figur= Output_mapp_figur,
                                               diag_totalt = TRUE,
                                               diag_kon = FALSE,
                                               vald_farg = "rus_sex",
                                               returnera_data = TRUE,
                                               returnera_figur = TRUE,
                                               spara_figur = TRUE)

# Andel pendling i kommun 
source("G:/skript/diagram/diag_pendlare_over_kommungrans.R")
gg_pendling = diag_pendling_over_kommungrans(output_mapp = Output_mapp_figur,
                                             skapa_fil = TRUE, 
                                             diagramfarg_vektor = diagramfarger("rus_sex"),
                                             enbart_in_ut = TRUE,
                                             diag_absoluta_tal = FALSE,           
                                             diag_procent = TRUE,
                                             returnera_data = TRUE)

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
gg_branschbredd = diag_branschbredd(region_vekt = "0020", 
                                    output_mapp = Output_mapp_figur,
                                    valda_farger = "rus_sex",
                                    spara_figur = TRUE,
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

