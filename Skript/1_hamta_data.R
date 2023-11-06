# Skript som hämtar data som används i rapporten "Sårbarhetsanalys av näringslivet i Dalarna".
# Notera att data som hämtas är sådan som finns tillgänglig via API. Övrig data (NMS-databasen eller källor utan API) hämtas direkt i markdownfilen.

# Nödvändiga bibliotek
if (!require("pacman")) install.packages("pacman")
p_load(here)

# Här hamnar Excelfilerna
Output_mapp = "G:/skript/projekt/data/sarbarhetsanalys/"

############################
##### Arbetsmarknaden ######
############################

# Arbetslöshet 76
source(here("Skript","arbetsloshet_76.R"), encoding="UTF-8")
hamta_data_arbetsloshet_76(region_vekt="20",
                           output_mapp_excel = Output_mapp,
                           spara_data = TRUE,
                           diag_stapel = FALSE,
                           diag_linje = FALSE)

# Arbetslöshet och sysselsättningsgrad
source(here("Skript","arbetsmarknadsstatus_kommun.R"), encoding="UTF-8")
hamta_data_arbetsmarknadsstatus_kommun(region_vekt = "20",
                                       output_mapp = Output_mapp,
                                       spara_data = TRUE,
                                       ta_med_lan = TRUE,
                                       ta_med_riket = TRUE,
                                       diag_arbetslosthet = TRUE,
                                       diag_arbetskraftsdeltagande = FALSE,
                                       diag_sysselsattningsgrad = TRUE)

# Långtidsarbetslöshet
source(here("Skript","langtidsarbetsloshet.R"), encoding="UTF-8")
hamta_data_langtidsarbetsloshet(output_mapp = Output_mapp,
                                spara_data = TRUE)

# Andel som jobbar i offentlig sektor
source(here("Skript","andel_offentligt.R"), encoding="UTF-8")
hamta_data_andel_offentligt(output_mapp = Output_mapp,
                            spara_data = TRUE)
