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

# Andel pendling i kommun (skript från rapporten kopplad till kompetensförsörjning
source("G:/skript/diagram/diag_pendlare_over_kommungrans.R")
gg_pendling = diag_pendling_over_kommungrans(output_mapp = Output_mapp_figur,
                                             skapa_fil = TRUE, 
                                             diagramfarg_vektor = diagramfarger("rus_sex"),
                                             enbart_in_ut = TRUE,
                                             diag_absoluta_tal = FALSE,           
                                             diag_procent = TRUE,
                                             returnera_data = TRUE)

# Antal nystartade företag och antalet konkurser
source(here("Skript","nystartade_konk.R"), encoding="UTF-8")
hamta_data_nystartade_konk(outputmapp = Output_mapp)


#####################
##### Inte API #####
####################

# Diverse från företagarna
source(here("Skript","foretagarna.R"), encoding="UTF-8")
hamta_data_foretagarna(output_mapp = Output_mapp)

