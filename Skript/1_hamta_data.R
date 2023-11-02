if (!require("pacman")) install.packages("pacman")
p_load(here)

Output_mapp = "G:/skript/projekt/data/sarbarhetsanalys/"

# Arbetslöshet 76
source(here("Skript","arbetsloshet_76.R"), encoding="UTF-8")
hamta_data_arbetsloshet_76(region_vekt="20",
                           output_mapp_excel = Output_mapp,
                           spara_data = TRUE,
                           diag_stapel = FALSE,
                           diag_linje = FALSE)


source(here("Skript","arbetsmarknadsstatus_kommun.R"), encoding="UTF-8")
hamta_data_arbetsmarknadsstatus_kommun(region_vekt = "20",
                                       output_mapp = Output_mapp,
                                       skapa_fil = TRUE,
                                       ta_med_lan = TRUE,
                                       ta_med_riket = TRUE,
                                       diag_arbetslosthet = TRUE,
                                       diag_arbetskraftsdeltagande = FALSE,
                                       diag_sysselsattningsgrad = TRUE)
