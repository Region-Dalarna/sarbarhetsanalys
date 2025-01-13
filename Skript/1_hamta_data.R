# Skript som hämtar data som används i rapporten "Sårbarhetsanalys av näringslivet i Dalarna".
# Notera att data som hämtas är sådan som finns tillgänglig via API. Övrig data (NMS-databasen eller källor utan API) hämtas direkt i markdownfilen.

# Nödvändiga bibliotek
if (!require("pacman")) install.packages("pacman")
p_load(here)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

# Här hamnar Excelfilerna
Output_mapp_data = "G:/skript/projekt/data/sarbarhetsanalys/"
Output_mapp_figur = here("Figurer","/")

spara_figurer <- FALSE

############################
##### Arbetsmarknaden ######
############################

# Arbetslöshet 76
source(here("Skript","diagram_arbetsloshet_76.R"), encoding="UTF-8")
gg_arb_76 <- diagram_data_arbetsloshet_76(region_vekt =c("00","20"),
                                          output_mapp_data = Output_mapp_data,
                                          output_mapp_figur = Output_mapp_figur,
                                          vald_farg = diagramfarger("rus_sex"),
                                          spara_figur = spara_figurer,
                                          returnera_figur = TRUE,
                                          returnera_data = TRUE)

# Arbetslöshet och sysselsättningsgrad
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_arbetsmarknadsstatus_senastear.R", encoding="UTF-8")
gg_arbetsmarknadsstatus <- diagram_arbetsmarknadsstatus(output_mapp_data = Output_mapp_data,
                                                        output_mapp_figur = Output_mapp_figur,
                                                        diag_arbetskraftsdeltagande = FALSE,
                                                        valda_farger = diagramfarger("rus_tre_fokus"),
                                                        spara_figur = spara_figurer,
                                                        returnera_figur = TRUE,
                                                        returnera_data = TRUE)

# Långtidsarbetslöshet
source(here("Skript","diagram_langtidsarbetsloshet.R"), encoding="UTF-8")
gg_langtidsarbetsloshet <- diagram_langtidsarb(region = hamtakommuner("20",tamedlan = TRUE,tamedriket = FALSE),
                                                outputmapp_figur = Output_mapp_figur,
                                                outputmapp_data = Output_mapp_data,
                                                jmf_ar = TRUE,
                                                returnera_figur = TRUE,
                                                returnera_data = TRUE,
                                                spara_figur = spara_figurer,
                                                vald_farg = diagramfarger("rus_sex"))

# Andel som jobbar i offentlig sektor 
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_andel_offentligt_BAS.R")
gg_andel_offentligt <- diagram_andel_offentligt(output_mapp_figur= Output_mapp_figur,
                                               output_mapp_data = Output_mapp_data,
                                               spara_figur = spara_figurer,
                                               returnera_figur = TRUE,
                                               returnera_data = TRUE)

# Andel pendling i kommun  
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diag_pendling_over_kommungrans.R")
gg_pendling <- diag_pendling_over_kommungrans(output_mapp_figur = Output_mapp_figur,
                                             output_mapp_data = Output_mapp_data,
                                             skapa_fil = spara_figurer, 
                                             diagram_capt = "Källa: SCB:s öppna statistikdatabas (RAMS tom 2019, därefter BAS), bearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Pendlingsdata kommer från RAMS tom år 2019 och inkluderar då åldrarna 16-74 år.\nDärefter kommer pendlingsdata från BAS och inkluderar åldrarna 15-74 år, från och med år 2020.",
                                             diagramfarg_vektor = diagramfarger("rus_sex"),
                                             enbart_in_ut = TRUE,
                                             diag_absoluta_tal = FALSE,           
                                             diag_procent = TRUE,
                                             returnera_figur = TRUE,
                                             returnera_data = TRUE)

# Antal nystartade företag och antalet konkurser - Tillväxtanalys istället? Kolla upp
source(here("Skript","diagram_nyst_arbetslosa.R"), encoding="UTF-8")
gg_nyst_konk <- diagram_nystartade_konkurser(output_mapp_figur = Output_mapp_figur,
                                           output_mapp_data = Output_mapp_data,
                                           spara_figur = spara_figurer,
                                           returnera_figur = TRUE,
                                           returnera_data = TRUE,
                                           vald_farg = diagramfarger("rus_sex"),
                                           cont_cod = c("N00999","N00926"))

# Andel förvärvsarbetande i olika branscher (såväl län som kommun)
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_andel_forvarvsarbetande_bransch.R", encoding="UTF-8")
gg_andel_forv <- diag_sysselsatta_andel(output_mapp_figur = Output_mapp_figur,
                                        output_mapp_data = Output_mapp_data,
                                        returnera_figur = TRUE,
                                        spara_figur = spara_figurer, 
                                        diag_lan = TRUE, 
                                        diag_kommun = TRUE,
                                        returnera_data = TRUE)

# Branschbredd
source(here("Skript","diagram_branschbredd.R"), encoding="UTF-8")
gg_branschbredd = diag_branschbredd(output_mapp_figur = Output_mapp_figur,
                                    valda_farger = diagramfarger("rus_sex"),
                                    spara_figur = spara_figurer,
                                    returnera_data = TRUE)

# Medelålder och demografisk försörjningskvot 
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_medelald_demfors.R", encoding="UTF-8")
gg_medel_demo <- diag_demografi(outputmapp_figur = Output_mapp_figur,
                                diag_forsorjningskvot = TRUE, 
                                diag_medelalder = TRUE,
                                spara_figur = spara_figurer,
                                returnera_data = TRUE)

# Andel av befolkningen med eftergymnasial utbildning (3 år)
source(here("Skript","diagram_utbniva_andelhog.R"), encoding="UTF-8")
gg_utbniva <- diag_utbniva(output_mapp = Output_mapp_figur,
                           spara_figur = spara_figurer,
                           returnera_data = TRUE)

# De största företagen i varje kommun
source("G:/skript/diagram/diag_storsta_arbetsgivare_kommun.R", encoding = "utf-8", echo = FALSE)
gg_storsta_foretag <- diag_storsta_arbetsgivare_kommun(valda_kommuner = hamtakommuner("20",tamedlan=FALSE,tamedriket=FALSE),
                                                      skriv_fil = spara_figurer,
                                                      output_mapp = Output_mapp_figur,
                                                      returnera_data = TRUE,
                                                      valda_farger = diagramfarger("rus_sex"))

# Folkmängd - enbart för att slippa skriva folkmängden under kommunfördjupningarna

source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_bef_folkmangd_alder_kon_manad_scb.R")
befolkning_df <- hamta_bef_folkmangd_alder_kon_manad_scb(hamtakommuner("20",F,F),
                                                          tid_koder = "9999",
                                                          kon_klartext = NA)

#####################
##### Inte API #####
####################

# Diverse från företagarna. Data uppdaterades senaste i mitten av januari 2024.

# Antal arbetsställen 
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_arb_stallen_mm_ejAPI.R", encoding="UTF-8")
gg_arbetsstallen <- diag_foretagarna(output_mapp_figur = Output_mapp_figur,
                                     output_mapp_data = Output_mapp_data,
                                     spara_figur = spara_figurer,
                                     returnera_figur = TRUE,
                                     returnera_data = TRUE,
                                     diag_arbetsstallen = TRUE,
                                     diag_nyforetagsamma = FALSE,
                                     diag_foretagsamma = FALSE)

gg_foretagssamma <- diag_foretagarna(region_vekt = "20",
                                     output_mapp_figur = Output_mapp_figur,
                                     spara_figur = spara_figurer,
                                     diag_arbetsstallen = FALSE,
                                     diag_nyforetagsamma = FALSE,
                                     diag_foretagsamma = TRUE,
                                     returnera_data = TRUE)

# source(here("Old","diagram_arb_stallen_mm_foretagarna.R"), encoding="UTF-8")
# gg_foretagssamma <- diag_foretagarna(region_vekt = "20",
#                                      output_mapp_figur = Output_mapp_figur,
#                                      spara_figur = spara_figurer,
#                                      diag_arbetsstallen = FALSE,
#                                      diag_nyforetagsamma = FALSE,
#                                      diag_foretagsamma = TRUE,
#                                      returnera_data = TRUE)

# Antal företag som utgör 50 procent av den totala lönesumman (NMS)
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_50_proc_lonesumma_NMS.R", encoding="UTF-8")
gg_50proc_lonesumma <- diag_50proc_lonesumma(output_mapp = Output_mapp_figur,
                                            spara_figur = spara_figurer,
                                            returnera_data = TRUE)

# Tabell med de fem största branscherna i Dalarna - NMS, både län och kommun.
# Går för tillfället inte att spara tabellerna, utan de returneras i en lista
source(here("Skript","tabell_storsta_bransch_NMS.R"), encoding="UTF-8")
gt_tabell = diag_50proc_lonesumma(region_vekt = "20",
                                  diag_lan = TRUE,
                                  diag_kommun = TRUE,
                                  returnera_data = TRUE)

