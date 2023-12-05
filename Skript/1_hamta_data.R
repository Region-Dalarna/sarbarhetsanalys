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
diagram_data_arbetsloshet_76(region_vekt ="20",
                             output_mapp_excel = Output_mapp,
                             output_mapp_figur = Output_mapp_figur,
                             spara_data = TRUE,
                             returnera_figur = FALSE)

# Arbetslöshet och sysselsättningsgrad
source(here("Skript","diagram_arbetsmarknadsstatus_kommun.R"), encoding="UTF-8")
diagram_arbetsmarknadsstatus_kommun(region_vekt = "20",
                                    output_mapp = Output_mapp,
                                    output_mapp_figur = Output_mapp_figur,
                                    spara_data = TRUE,
                                    ta_med_lan = TRUE,
                                    ta_med_riket = TRUE,
                                    diag_arbetslosthet = TRUE,
                                    diag_arbetskraftsdeltagande = FALSE,
                                    diag_sysselsattningsgrad = TRUE,
                                    spara_figur = TRUE,
                                    returnera_figur = FALSE)

# Långtidsarbetslöshet
source("https://raw.githubusercontent.com/Region-Dalarna/uppfoljning_dalastrategin/main/Skript/l%C3%A5ngtidsarbetsl%C3%B6shet.R")
hamta_data_langtidsarb(region = c("0020"),
                       cond_code = c("N03923"),
                       alla_kommuner = TRUE,
                       ta_med_riket = TRUE,
                       outputmapp = Output_mapp,
                       filnamn = "langtidsarbetsloshet.csv"
                       )

# Andel som jobbar i offentlig sektor
source(here("Skript","andel_offentligt.R"), encoding="UTF-8")
hamta_data_andel_offentligt(output_mapp = Output_mapp,
                            output_mapp_figur = Output_mapp_figur,
                            spara_data = TRUE,
                            diag_linje = TRUE)

# Andel pendling i kommun (skript från rapporten kopplad till kompetensförsörjning
source("https://raw.githubusercontent.com/Region-Dalarna/kompetensforsorjning_i_Dalarna/main/Skript/pendling_kommun.R")
hamta_data_pendling_kommun(output_mapp = Output_mapp,
                           senaste_ar = TRUE)

# Antal nystartade företag och antalet konkurser
source(here("Skript","nystartade_konk.R"), encoding="UTF-8")
hamta_data_nystartade_konk(outputmapp = Output_mapp)


#####################
##### Inte API #####
####################

# Diverse från företagarna
source(here("Skript","foretagarna.R"), encoding="UTF-8")
hamta_data_foretagarna(output_mapp = Output_mapp)

