# Hur många företag krävs för att lönesumman i en kommun skall uppgå till 50 % (kumulativt)
# R-skript som skapar data finns på MONA under P1079_Gem/Jon/Sårbarhetsanalys/Ftg_50procent_lonesumma_ny_variant

pacman::p_load(openxlsx,here,tidyverse,gt,webshot2)

# Skript som behövs
source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)

#test_list=diag_50proc_lonesumma(skapa_fil=TRUE,output_mapp = "G:/skript/jon/",diag_kommun=FALSE,diag_kommun_ftg=FALSE)
diag_50proc_lonesumma <- function(region_vekt="20",
                                  output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                  skapa_fil=TRUE,
                                  diag_lan=TRUE,
                                  diag_kommun=TRUE,
                                  diag_kommun_ftg=TRUE){
  
  # ========================================== Inställningar ============================================
  filnamn <- "storsta_branscher.png"
  fil_lan<-paste0(output_mapp,filnamn)
 
  # ========================================== Läser in data ============================================
  # Läser in data från Excel (ursprung arbetsförmedlingen)
  bransch_lan_df <- read.xlsx("G:/skript/data/sarbarhetsanalys/Indata/17_mar_23_ranking_branscher_lonesumma.xlsx",sheet=1)
  bransch_kommun_df <- read.xlsx("G:/skript/data/sarbarhetsanalys/Indata/17_mar_23_ranking_branscher_lonesumma.xlsx",sheet=2)
  foretag_kommun_df <- read.xlsx("G:/skript/data/sarbarhetsanalys/Indata/17_mar_23_ranking_branscher_lonesumma.xlsx",sheet=3)
  
  bransch_lan_df <- bransch_lan_df %>% 
    mutate("andel_lonesumma"=round(andel_lonesumma,0),
           "andel_kon"=round(andel_kon,0),
           "medelalder"=round(medelalder,0),
           "andel_utbildning"=round(andel_utbildning,0),
           "andel_bakgrund"=round(andel_bakgrund,0))
  
  bransch_kommun_df <- bransch_kommun_df %>% 
    mutate("andel_lonesumma"=round(andel_lonesumma,0),
           "andel_kon"=round(andel_kon,0),
           "medelalder"=round(medelalder,0),
           "andel_utbildning"=round(andel_utbildning,0),
           "andel_bakgrund"=round(andel_bakgrund,0))
  
  bransch_kommun_df <-bransch_kommun_df %>% 
    mutate(andel_bakgrund=ifelse((andel_bakgrund*antal_syss/100)<5,"*",andel_bakgrund),
           andel_kon=ifelse((andel_kon*antal_syss/100)<5,"*",andel_kon),
           andel_utbildning=ifelse((andel_utbildning*antal_syss/100)<5,"*",andel_utbildning))
  
  
  if(diag_lan==TRUE){
   # Skapar en tabell med hjälp av paketet gt
    tabell <-bransch_lan_df %>%
      mutate("Bransch"=AstSNI_raps,
             "Lönesumma"=lonesumma,
             "Sysselsatta"=antal_syss,
             "Lönesumma (%)"=andel_lonesumma,
             "Kvinnor (%)"=andel_kon,
             "Högutbildade (%)"=andel_utbildning,
             "Utrikes födda (%)"=andel_bakgrund) %>% 
        select(Bransch,"Sysselsatta","Lönesumma (%)","Kvinnor (%)","Högutbildade (%)","Utrikes födda (%)") %>% 
          gt() %>%
            tab_header(
              title = "De fem största branscherna i Dalarna 2020 inom det privata näringslivet") %>% 
                cols_align(align=c("center"),columns = (2:5)) %>% 
                  tab_options(heading.padding=20,
                              heading.background.color = diagramfarger("rus_sex")[5],
                              table.border.bottom.color = "transparent") %>% 
                    tab_style(locations = cells_column_labels(columns = everything()),
                              style     = list(
                                #Give a thick border below
                                cell_borders(sides = c("top","bottom"), weight = px(3)),
                                #Make text bold
                                cell_text(weight = "bold")
                              )) %>% 
                      tab_source_note(source_note = c("Källa: NMS-databasen (SCB), bearbetning: Samhällsanalys, Region Dalarna")) %>% 
                        tab_footnote(
                          footnote = "Andel av total lönesumma inom det privata näringslivet",
                          locations = cells_column_labels(
                            columns = "Lönesumma (%)")) %>% 
                        tab_footnote(
                          footnote = "Andel inom bransch",
                          locations = cells_column_labels(
                            columns = c("Kvinnor (%)","Högutbildade (%)","Utrikes födda (%)"))) %>% 
                              gtsave(fil_lan)
  }
  
  # Motsvarande som ovan fast för alla kommuner i Dalarna
  
  if(diag_kommun==TRUE){
    # Loopar över kommuner och skapar tabeller
    kommuner=unique(bransch_kommun_df$AstKommun_namn)
    i=1
    while(i<=length(kommuner)){
      filnamn <- paste0("storsta_branscher_",kommuner[i],".png")
      fil_kommun<-paste0(output_mapp,filnamn)
      
      tabell <-bransch_kommun_df %>%
        filter(AstKommun_namn==kommuner[i]) %>% 
          mutate("Bransch"=AstSNI_raps,
                 "Lönesumma"=lonesumma,
                 "Sysselsatta"=antal_syss,
                 "Lönesumma (%)"=andel_lonesumma,
                 "Kvinnor (%)"=andel_kon,
                 "Högutbildade (%)"=andel_utbildning,
                 "Utrikes födda (%)"=andel_bakgrund) %>% 
            select(Bransch,"Sysselsatta","Lönesumma (%)","Kvinnor (%)","Högutbildade (%)","Utrikes födda (%)") %>% 
              gt() %>%
                tab_header(
                  title = paste0("De fem största branscherna i ", kommuner[i] ," 2020 inom det privata näringslivet")) %>% 
                    cols_align(align=c("center"),columns = (2:5)) %>% 
                  tab_options(heading.padding=20,
                              heading.background.color = diagramfarger("rus_sex")[5],
                              table.border.bottom.color = "transparent") %>% 
                    tab_style(locations = cells_column_labels(columns = everything()),
                              style     = list(
                                #Give a thick border below
                                cell_borders(sides = c("top","bottom"), weight = px(3)),
                                #Make text bold
                                cell_text(weight = "bold")
                              )) %>% 
                      tab_source_note(source_note = c("Källa: NMS-databasen (SCB), bearbetning: Samhällsanalys, Region Dalarna, * -> för litet tal")) %>% 
                        tab_footnote(
                          footnote = "Andel av total lönesumma inom det privata näringslivet",
                          locations = cells_column_labels(
                            columns = "Lönesumma (%)")) %>% 
                        tab_footnote(
                          footnote = "Andel inom bransch",
                          locations = cells_column_labels(
                            columns = c("Kvinnor (%)","Högutbildade (%)","Utrikes födda (%)"))) %>% 
                          gtsave(fil_kommun)
      i=i+1
    }
  }
  
  if(diag_kommun_ftg==TRUE){
    # Loopar över kommuner och skapar tabeller
    kommuner=unique(foretag_kommun_df$AstKommun_namn)
    i=1
    while(i<=length(kommuner)){
      filnamn <- paste0("storsta_foretag_",kommuner[i],".png")
      fil_kommun<-paste0(output_mapp,filnamn)
      
      tabell <-foretag_kommun_df %>%
        filter(AstKommun_namn==kommuner[i]) %>% 
        mutate("Bransch"=AstSNI_raps,
               "Lönesumma"=lonesumma_ftg,
               "Sysselsatta"=antal_syss,
               "Lönesumma (%)"=andel_procent,
               "Kvinnor (%)"=andel_kon,
               "Högutbildade (%)"=andel_utbildning,
               "Utrikes födda (%)"=andel_bakgrund) %>% 
        select(Bransch,"Sysselsatta","Lönesumma (%)","Kvinnor (%)","Högutbildade (%)","Utrikes födda (%)") %>% 
        gt() %>%
        tab_header(
          title = paste0("De fem största företagen i ", kommuner[i] ," 2020 inom det privata näringslivet")) %>% 
        cols_align(align=c("center"),columns = (2:5)) %>% 
        tab_options(heading.padding=20,
                    heading.background.color = diagramfarger("rus_sex")[5],
                    table.border.bottom.color = "transparent") %>% 
        tab_style(locations = cells_column_labels(columns = everything()),
                  style     = list(
                    #Give a thick border below
                    cell_borders(sides = c("top","bottom"), weight = px(3)),
                    #Make text bold
                    cell_text(weight = "bold")
                  )) %>% 
        tab_source_note(source_note = c("Källa: NMS-databasen (SCB), bearbetning: Samhällsanalys, Region Dalarna")) %>% 
        tab_footnote(
          footnote = "Andel av total lönesumma inom det privata näringslivet",
          locations = cells_column_labels(
            columns = "Lönesumma (%)")) %>% 
        tab_footnote(
          footnote = "Andel inom bransch",
          locations = cells_column_labels(
            columns = c("Kvinnor (%)","Högutbildade (%)","Utrikes födda (%)"))) %>% 
        gtsave(fil_kommun)
      i=i+1
    }
  }
  
}
