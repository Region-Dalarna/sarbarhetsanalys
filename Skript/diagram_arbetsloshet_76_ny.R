diagram_data_arbetsloshet_76 <- function(region_vekt = c("00","20"), # Vilka regioner vill man ha. Jämförs (ej kommuner). Om man vill jämföra kön får bara 1 väljas
                                         output_mapp_figur= "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                         vald_farg = diagramfarger("rus_sex"), # Val av diagramfärger
                                         filnamn_figur = "arbetsloshet_76.png", # Filnamn
                                         spara_figur = FALSE, # Sparar figuren till output_mapp_figur
                                         returnera_data = TRUE){ # Skall data returneras

# ============================================================
# Arbetslöshet i Dalarna och Sverige 1976-2025, med kriser markerade
# ============================================================
# Denna kod bygger på uppdiktad data formad efter kurvan i
# originalfiguren. Byt ut data_generation-delen mot din riktiga
# SCB/AKU-data när du kör detta på riktigt.

library(ggplot2)
library(dplyr)
library(tidyr)

if (!require("pacman")) install.packages("pacman")
p_load(pxweb,
       tidyverse,
       openxlsx)

# Funktioner som behövs
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_pxweb2.R")

gg_list <- list() # Skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
i = 1 # Räknare
objektnamn <- c() # Används för att namnge objekten

###############
# Hämtar data #
###############

# Till och med 04
df_76_04 <- pxweb2_hamta_data(
  tabell = "TAB88",
  query = list(
    Region = region_vekt,
    Kon = c("män","kvinnor"),
    Arbetskraftstillh = c("SYS","ALÖS"),
    ContentsCode = "*",
    Tid = "*")) %>% 
  rename(Antal = value) %>% 
  pivot_wider(names_from = arbetskraftstillhörighet,values_from=Antal ) %>% 
  mutate(arbetsloshet = (arbetslösa/(arbetslösa+sysselsatta))*100) %>% 
  select(-tabellinnehåll,-region_kod)

# Från 05
df_05_ <- pxweb2_hamta_data(
  tabell = "TAB2922",
  query = list(
    Region = region_vekt,
    Arbetskraftstillh = c("ALÖS","SYS"),
    Kon = c("män","kvinnor"),
    ContentsCode = "1000-tal",
    Tid = "*"
  )) %>% 
  rename(Antal = value) %>% 
  pivot_wider(names_from = arbetskraftstillhörighet,values_from=Antal ) %>% 
  mutate(arbetsloshet = (arbetslösa/(arbetslösa+sysselsatta))*100) %>% 
  select(-tabellinnehåll,-region_kod)

df <- rbind(df_76_04,df_05_) %>% 
  mutate(region = skapa_kortnamn_lan(region,byt_ut_riket_mot_sverige = TRUE))

df <- df %>% 
  group_by(region,år) %>% 
  summarize("sysselsatta" = sum(sysselsatta),
            "arbetslösa" = sum(arbetslösa)) %>% 
  mutate(arbetsloshet = (arbetslösa/(sysselsatta+arbetslösa))*100,
         år = as.numeric(år))

if(returnera_data == TRUE){
  assign("arbetsloshet_76", df, envir = .GlobalEnv)
}

# ------------------------------------------------------------
# 2. Definiera kriserna som startpunkter (svårt att exakt datera slutet,
#    så vi markerar bara startåret med en lodrät linje + etikett)
# ------------------------------------------------------------
crises <- tibble(
  crisis     = c("90-talskrisen", "Finanskrisen 2008-09", "Coronapandemin", "Rysslands invasion av Ukraina"),
  start_year = c(1990, 2008, 2020, 2022),
  # Coronapandemin och Ukraina-kriget ligger nära varandra i tid, så de
  # får olika höjd OCH en vit bakgrund (geom_label) så texten syns
  # tydligt även när den korsar datalinjerna
  label_y    = c(10.6, 10.6, 11.4, 10.6)
)

# ------------------------------------------------------------
# 3. Bygg grafen
# ------------------------------------------------------------

# Get the unique regions actually present in the data, in a stable order
region_levels <- unique(df$region)

# Build a named vector: names = region values, values = colors from diagramfarger()
region_colors <- setNames(
  vald_farg[seq_along(region_levels)],
  region_levels
)

diagram_capt <- "Källa: SCB, arbetskraftsundersökningarna (AKU).\nBearbetning: Samhällsanalys, Region Dalarna.\nFrån och med oktober 2007 räknas även studenter som aktivt söker ett arbete och är villiga att ta jobb som arbetslösa.\nFram till 2004, arbetslöshet i åldersgruppen 16-64 år, därefter 15-74 år."
diagramtitel <- paste0("Arbetslöshet i ",glue_collapse(unique(unique(df$region)),sep = ", ", last = " och ")," ",min(df$år),"-",max(df$år))

p <- ggplot() +
  # Lodrät linje vid varje krisstart
  geom_vline(
    data = crises,
    aes(xintercept = start_year),
    color = "grey60", linewidth = 0.4
  ) +
  # Etiketter för kriserna, roterade så de får plats. geom_label (i stället
  # för geom_text) ger en vit bakgrund så texten syns tydligt även där den
  # korsar Dalarna/Sverige-linjerna
  geom_label(
    data = crises,
    aes(x = start_year, y = label_y, label = crisis),
    angle = 90, hjust = 1, vjust = 0.5, size = 3, color = "grey30",
    fill = "white", label.size = 0, label.padding = unit(0.1, "lines"),
    inherit.aes = FALSE
  ) +
  # Linjerna för regionerna
  geom_line(
    data = df,
    aes(x = år, y = arbetsloshet, color = region),
    linewidth = 1.6
  ) +
  scale_color_manual(
    values = region_colors,
    name = NULL
  ) +
  scale_x_continuous(
    breaks = seq(1976, 2025, by = 4),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, " %"),
    breaks = seq(0, 10, by = 2.5),      # större, etiketterade stödlinjer
    minor_breaks = seq(0, 10, by = 0.5), # tunnare stödlinjer varje 0.5 %
    limits = c(0, 12)                    # extra utrymme upptill för kris-etiketterna
  ) +
  labs(
    title = diagramtitel,
    x = NULL,
    y = NULL,
    caption = diagram_capt
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_textbox_simple(
      size = 20,
      width = unit(0.9, "npc"),  # Bredd som proportion av plottens område
      halign = 0.5,  # Centrera texten
      margin = margin(7, 0, 7, 0)),
    plot.title.position = "plot",
    axis.text  = element_text(color = "black"),          # was default grey, now matches bar chart
    axis.title = element_text(color = "black"),
    plot.caption = element_text(
      hjust = 0,vjust = 0, size = 11, face = "italic",
      color = "black"                                    # darker than "grey40"
    ),
    plot.caption.position = "plot",
    plot.margin = margin(t = 10, r = 10, b = 10, l = 0),  # match the bar chart's margins so caption/title left-edge align
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey75", linewidth = 0.6),
    legend.position = "bottom",
    legend.text = element_text(color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

if (spara_figur){
  ggsave(filename = paste0(output_mapp, filnamn_diagram),
         plot = p,
         width = 12,
         height = 7,
         dpi = 300)
}

objektnamn <- c(objektnamn,"arbetsloshet_region")

gg_list[[i]] <- p
i=i+1

names(gg_list) <- objektnamn

return(gg_list)

}



