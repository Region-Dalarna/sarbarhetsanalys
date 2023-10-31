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
