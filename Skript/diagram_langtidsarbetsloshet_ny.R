diagram_langtidsarb = function(region = "Dalarna", # Finns även: "Blekinge", "Gotland", "Gävleborg", "Halland", "Jämtland", "Jönköping", "Kalmar", "Kronoberg", "Norrbotten", "Skåne", "Stockholm", "Södermanland", "Uppsala", "Värmland", "Västerbotten", "Västernorrland", "Västmanland", "Västra Götaland", "Örebro", "Östergötland"
                               outputmapp_figur = "G:/skript/jon/Slask/", # Här hamnar sparad figur
                               vald_farg = diagramfarger("rus_sex"), #Vilken färg vill vi använda
                               jmf_ar = TRUE, # Om true, jämförs långtidsarbetslöshet för ett intervall om 5 år mellan första och sista år (gäller diagrammet diag_jmf_kommun)
                               diag_jmf_olika_langd = FALSE, # Om true, jämförs olika längd på arbetslöshet över tid. Endast för län
                               diag_jmf_kommun = TRUE, # Om true, jämförs långtidsarbetslöshet för olika kommuner i vald region
                               tid_cols = "Utan arbete mer än 6 månader", # Definition av långtidsarbetslöshet. Max 1 åt gången. Finns även "Utan arbete mer än 12 månader", "Utan arbete mer än 24 månader")
                               spara_figur = TRUE, # Vill man spara figur
                               returnera_data = FALSE){
  
  
  # ========================================== Info ============================================
  # Två diagram för långtidsarbetslöshet:
  # 1: Jämför på kommunnivå för antingen senaste år eller ett antal år. Samtliga kommuner i vald region. Går att ändra defintion av långtidsarbetslöshet (tid_cols)
  # 2: Jämför olika längd på arbetslöshet på länsnivå över tid. 
  # ========================================== Info ============================================
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(openxlsx,
                 here,
                 tidyverse,
                 glue,
                 readxl)
  
  # Funktioner som behövs (hämtas från Git-Hub)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/kvinnor_man_i_Dalarna/refs/heads/main/Skript/func_hamta_data_kvinnor_man.R")
  
  gg_list <- list()
  objektnamn <-c()
 
  # ========================================== Läser in data ============================================
  # URL till arbetsförmedlingen för tidigare statistik/Excel
  af_url <- "https://arbetsformedlingen.se/statistik/sok-statistik/tidigare-statistik-tidsserier"
  
  af_url_lankar <- read_html(af_url) %>%                  # läs in webbsidan ovan
    html_elements("a") %>%                                # hitta alla <a>-objekt
    html_attr("href") %>%                                 # ta ut alla länkar, dvs. <href>-objekt
    purrr::discard(is.na)
  
  avkodade_lankar <- af_url_lankar %>%
    URLdecode()
  
  # Väljer vilka länkar vi skall hämta data från. Här är det långtidsarbetslöshet
  af_index <- which(
    (str_detect(avkodade_lankar, "web-inskrivna-arbetslosa-tid-utan-arbete-lan-kom"))                        # men uteslut "arsgenomsnitt"
  )
  
  # Skapar en lista med länkar som skall hämtas
  inlasfil <- af_url_lankar[af_index] %>%
    paste0("https://arbetsformedlingen.se", .)
  
  tmpfile <- tempfile(fileext = ".xlsx")
  curl::curl_download(inlasfil, tmpfile)
  
  tmp <- tempfile()
  unzip(tmpfile, exdir = tmp)
  
  # Hämtar namn på alla blad i Excel-filen och läser in dem som text
  sheet_names <- excel_sheets(tmpfile)
  
  #data_sheets <- setdiff(sheet_names, "Info")
  
  data_sheets <- c("Total")
  
  all_sheets <- map(data_sheets, ~ read_xlsx(tmpfile, sheet = .x, skip = 4, col_types = "text"))
  names(all_sheets) <- data_sheets  
  
  # # Tar ut data för total och bearbetar den. Skapar även en ny kolumn med åldersgrupper och summerar på län och lägger till som en egen kategori under kommun
  # df_base <- all_sheets$Total %>%
  #   separate(PERIOD, c("Ar", "Manad"), sep = "-") %>%
  #   mutate(
  #     across(c(ARBETSLÖSA,  all_of(tid_cols)), as.numeric),
  #     Ar = as.integer(Ar),
  #     åldersgrupper = case_when(
  #       Ar <= 2022              ~ "Inskrivna arbetslösa 16-64 år",
  #       Ar >= 2023 & Ar <= 2025 ~ "Inskrivna arbetslösa 16-65 år",
  #       Ar >= 2026               ~ "Inskrivna arbetslösa 16-66 år",
  #       TRUE                     ~ NA_character_
  #     )
  #   )
  # 
  # df <- df_base %>%
  #   bind_rows(
  #     df_base %>%
  #       group_by(Ar, Manad, LÄN) %>%
  #       summarise(
  #         across(c(ARBETSLÖSA,  all_of(tid_cols)), \(x) sum(x, na.rm = TRUE)),
  #         .groups = "drop"
  #       ) %>%
  #       mutate(
  #         KOMMUN = LÄN,
  #         åldersgrupper = case_when(
  #           Ar <= 2022              ~ "Inskrivna arbetslösa 16-64 år",
  #           Ar >= 2023 & Ar <= 2025 ~ "Inskrivna arbetslösa 16-65 år",
  #           Ar >= 2026               ~ "Inskrivna arbetslösa 16-66 år",
  #           TRUE                     ~ NA_character_
  #         )
  #       )
  #   ) %>%
  #   mutate(andel_langtidsarbetslos = (.data[[tid_cols]] / ARBETSLÖSA) * 100) %>%
  #   group_by(Ar, LÄN, KOMMUN, åldersgrupper) %>%
  #   summarise(
  #     andel_langtidsarbetslos = mean(andel_langtidsarbetslos, na.rm = TRUE),
  #     .groups = "drop"
  #   )
  
  # Tar ut data för total och bearbetar den. Skapar även en ny kolumn med åldersgrupper och summerar på län och lägger till som en egen kategori under kommun
  
  
  
  if(diag_jmf_kommun == TRUE){
    
    df_base <- all_sheets$Total %>%
      separate(PERIOD, c("Ar", "Manad"), sep = "-") %>%
      mutate(
        across(c(ARBETSLÖSA,  all_of(tid_cols)), as.numeric),
        Ar = as.integer(Ar),
        åldersgrupper = case_when(
          Ar <= 2022              ~ "Inskrivna arbetslösa 16-64 år",
          Ar >= 2023 & Ar <= 2025 ~ "Inskrivna arbetslösa 16-65 år",
          Ar >= 2026               ~ "Inskrivna arbetslösa 16-66 år",
          TRUE                     ~ NA_character_
        )
      )
    
    df <- df_base %>%
      bind_rows(
        df_base %>%
          group_by(Ar, Manad, LÄN) %>%
          summarise(
            across(c(ARBETSLÖSA,  all_of(tid_cols)), \(x) sum(x, na.rm = TRUE)),
            .groups = "drop"
          ) %>%
          mutate(
            KOMMUN = LÄN,
            åldersgrupper = case_when(
              Ar <= 2022              ~ "Inskrivna arbetslösa 16-64 år",
              Ar >= 2023 & Ar <= 2025 ~ "Inskrivna arbetslösa 16-65 år",
              Ar >= 2026               ~ "Inskrivna arbetslösa 16-66 år",
              TRUE                     ~ NA_character_
            )
          )
      ) %>%
      pivot_longer(cols = all_of(tid_cols), names_to = "langtidsdefinition", values_to = "tid_varde") %>%
      mutate(andel_langtidsarbetslos = (tid_varde / ARBETSLÖSA) * 100) %>%
      group_by(Ar, LÄN, KOMMUN, åldersgrupper, langtidsdefinition) %>%
      summarise(
        andel_langtidsarbetslos = mean(andel_langtidsarbetslos, na.rm = TRUE),
        .groups = "drop"
      )
    
    df <- df %>% 
      mutate(LÄN = skapa_kortnamn_lan(LÄN,byt_ut_riket_mot_sverige = TRUE),
             KOMMUN = skapa_kortnamn_lan(KOMMUN,byt_ut_riket_mot_sverige = TRUE)) %>% 
        filter(LÄN %in% region,
               KOMMUN !=  "Uppgift saknas")
    
    #df$KOMMUN = skapa_kortnamn_lan(hamtaregion_kod_namn(df$KOMMUN)$region,byt_ut_riket_mot_sverige = TRUE)

    if(jmf_ar == TRUE){
      # every 5 years from min_year, plus force in max_year
      years_to_plot <- union(seq(min(df$Ar),max(df$Ar), by = 5), max(df$Ar))
      utskrift_df = df %>% 
        filter(Ar %in% years_to_plot)
      vand_sortering = TRUE
      diagram_titel <- paste0("Långtidsarbetslöshet i ",region)
      
    } else{
      utskrift_df = df %>% 
        filter(Ar == max(Ar))
      diagram_titel <- paste0("Långtidsarbetslöshet i ",region," år ",max(df$Ar))
      vand_sortering = FALSE
    } 
    
    if(returnera_data == TRUE){
      assign("langtidsarbetsloshet_kommun_df", utskrift_df, envir = .GlobalEnv)
    }
  
    manader <- str_extract(tid_cols, "\\d+")
    ar_max <- max(df$Ar)
    
    diagram_capt <- paste0("Källa: Arbetsförmedlingen.",
                           "\nBearbetning: Samhällsanalys, Region Dalarna.",
                           "\nDiagramförklaring: Månadsdata. Diagrammet visar årsmedelvärden; för ",ar_max," visas meddelvärdet för",
                           "\nhittils tillgänglia månader. Andel av arbetslösa som varit öppet arbetslösa eller i program med aktivitetsstöd",
                           "\ni minst ", manader, " månader. Till och med 2022 avser statistiken åldersgruppen 16-64 år, mellan 2023 och 2025",
                           "\n16-65 år och från 2026 16-66 år.")
  
    
    diagramfil <- "langtidsarbetsloshet.png"
    objektnamn <- c(objektnamn,"langtidsarbetsloshet")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = utskrift_df,
                                 skickad_x_var = "KOMMUN",
                                 skickad_y_var = "andel_langtidsarbetslos",
                                 skickad_x_grupp = ifelse(jmf_ar == TRUE,"Ar",NA),
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 manual_color = vald_farg,
                                 x_axis_sort_value = TRUE,
                                 x_axis_sort_grp = ifelse(jmf_ar == TRUE,length(unique(utskrift_df$Ar)),NA),
                                 vand_sortering = vand_sortering,
                                 dataetiketter = FALSE,
                                 manual_y_axis_title = "procent",
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diagram_capt,
                                 procent_0_100_10intervaller = TRUE,
                                 stodlinjer_avrunda_fem = TRUE,
                                 output_mapp = outputmapp_figur,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list) <- c(objektnamn)
  
  }
  
  if(diag_jmf_olika_langd == TRUE){
    
    # Tar ut data för total och bearbetar den. Skapar även en ny kolumn med åldersgrupper och summerar på län och lägger till som en egen kategori under kommun
    df_base <- all_sheets$Total %>%
      separate(PERIOD, c("Ar", "Manad"), sep = "-") %>%
      mutate(
        across(c(ARBETSLÖSA, `Utan arbete mer än 6 månader`, `Utan arbete mer än 12 månader`, `Utan arbete mer än 24 månader`), as.numeric),
        Ar = as.integer(Ar),
        åldersgrupper = case_when(
          Ar <= 2022              ~ "Inskrivna arbetslösa 16-64 år",
          Ar >= 2023 & Ar <= 2025 ~ "Inskrivna arbetslösa 16-65 år",
          Ar >= 2026               ~ "Inskrivna arbetslösa 16-66 år",
          TRUE                     ~ NA_character_
        )
      )
    
    df <- df_base %>%
      bind_rows(
        df_base %>%
          group_by(Ar, Manad, LÄN) %>%
          summarise(
            across(c(ARBETSLÖSA, `Utan arbete mer än 6 månader`, `Utan arbete mer än 12 månader`, `Utan arbete mer än 24 månader`), \(x) sum(x, na.rm = TRUE)),
            .groups = "drop"
          ) %>%
          mutate(
            KOMMUN = LÄN,
            åldersgrupper = case_when(
              Ar <= 2022              ~ "Inskrivna arbetslösa 16-64 år",
              Ar >= 2023 & Ar <= 2025 ~ "Inskrivna arbetslösa 16-65 år",
              Ar >= 2026               ~ "Inskrivna arbetslösa 16-66 år",
              TRUE                     ~ NA_character_
            )
          )
      ) %>%
      mutate(
        `0 - 6 månader`   = ARBETSLÖSA - `Utan arbete mer än 6 månader`,
        `6 - 12 månader`  = `Utan arbete mer än 6 månader` - `Utan arbete mer än 12 månader`,
        `12 - 24 månader` = `Utan arbete mer än 12 månader` - `Utan arbete mer än 24 månader`,
        `24 månader -`             = `Utan arbete mer än 24 månader`
      ) %>%
      pivot_longer(
        cols = c(`0 - 6 månader`, `6 - 12 månader`, `12 - 24 månader`, `24 månader -`),
        names_to = "langtidsdefinition", values_to = "tid_varde"
      ) %>%
      mutate(andel_langtidsarbetslos = (tid_varde / ARBETSLÖSA) * 100) %>%
      group_by(Ar, LÄN, KOMMUN, åldersgrupper, langtidsdefinition) %>%
      summarise(
        andel_langtidsarbetslos = mean(andel_langtidsarbetslos, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Ett sätt att kontrollera att summeringen av de olika långtidsarbetslöshetsdefinitionerna stämmer med det totala antalet arbetslösa. Om diff = 0 är allt ok. Om diff != 0 är det något som inte stämmer.
    # Beräkningar verkar stämma
    #
    # sanity_check <- df_base %>%
    #   bind_rows(
    #     df_base %>%
    #       group_by(Ar, Manad, LÄN) %>%
    #       summarise(
    #         across(c(ARBETSLÖSA, `Utan arbete mer än 6 månader`, `Utan arbete mer än 12 månader`, `Utan arbete mer än 24 månader`), \(x) sum(x, na.rm = TRUE)),
    #         .groups = "drop"
    #       ) %>%
    #       mutate(KOMMUN = LÄN)
    #   ) %>%
    #   mutate(
    #     `0 - 6 månader`   = ARBETSLÖSA - `Utan arbete mer än 6 månader`,
    #     `6 - 12 månader`  = `Utan arbete mer än 6 månader` - `Utan arbete mer än 12 månader`,
    #     `12 - 24 månader` = `Utan arbete mer än 12 månader` - `Utan arbete mer än 24 månader`,
    #     `24-`             = `Utan arbete mer än 24 månader`
    #   ) %>%
    #   mutate(bin_sum = `0 - 6 månader` + `6 - 12 månader` + `12 - 24 månader` + `24-`) %>%
    #   mutate(diff = bin_sum - ARBETSLÖSA) %>%
    #   summarise(
    #     max_abs_diff = max(abs(diff), na.rm = TRUE),
    #     n_mismatch   = sum(diff != 0, na.rm = TRUE),
    #     n_na         = sum(is.na(diff))
    #   )
    # 
    # sanity_check
    # 
    # hejsan <- df_base %>%
    #   filter(is.na(`Utan arbete mer än 6 månader`) | is.na(`Utan arbete mer än 12 månader`) | 
    #            is.na(`Utan arbete mer än 24 månader`) | is.na(ARBETSLÖSA)) %>%
    #   count(Ar, LÄN, sort = TRUE) %>%
    #   head(20)
    # 
    df$KOMMUN = skapa_kortnamn_lan(hamtaregion_kod_namn(df$KOMMUN)$region,byt_ut_riket_mot_sverige = TRUE)
    
    if(returnera_data == TRUE){
      assign("langtidsarbetsloshet_olika_langd_df", df, envir = .GlobalEnv)
    }
    
    #manader <- str_extract(tid_cols, "\\d+")
    
    diagram_capt <- paste0("Källa: Arbetsförmedlingen.",
                           "\nBearbetning: Samhällsanalys, Region Dalarna.",
                           "\nDiagramförklaring: Till och med 2022 avser statistiken åldersgruppen 16-64 år, mellan 2023 och 2025",
                           "\n16-65 år och från 2026 16-66 år.")
    
    diagram_titel <- paste0("Tid som arbetslös i ",region)
    diagramfil <- "langtidsarbetsloshet_jmf_langd_tid.png"
    objektnamn <- c(objektnamn,"langtidsarbetsloshet_jmf_langd_tid")
    
    
    gg_obj <- SkapaStapelDiagram(skickad_df = df %>% 
                                   filter(KOMMUN %in% region) %>% 
                                   mutate(Ar = as.character(Ar),
                                          langtidsdefinition = factor(langtidsdefinition, levels = c("0 - 6 månader", "6 - 12 månader", "12 - 24 månader", "24 månader -"))),
                                 skickad_x_var = "Ar",
                                 skickad_y_var = "andel_langtidsarbetslos",
                                 skickad_x_grupp = "langtidsdefinition",
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 manual_color = vald_farg,
                                 dataetiketter = FALSE,
                                 manual_y_axis_title = "procent",
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diagram_capt,
                                 procent_0_100_10intervaller = TRUE,
                                 stodlinjer_avrunda_fem = TRUE,
                                 output_mapp = outputmapp_figur,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list) <- c(objektnamn)
    
  }
  
 return(gg_list)
  
}
