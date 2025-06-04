#----------------------------------------------------------#
# Priprava dilcich objektu -----
#----------------------------------------------------------#
#--------------------------------------------------#
## Agregace po poli 1. radu -----
#--------------------------------------------------#

n2k_druhy_chu_pole1 <- 
  n2k_druhy_pole1eval %>%
  dplyr::group_by(
    kod_chu, 
    DRUH
    ) %>%
  #dplyr::filter(DATUM + years(6) >= current_year) %>%
  dplyr::reframe(
    ROK = toString(unique(ROK)),
    POLE = toString(unique(POLE)),
    NAZEV_LOK = toString(unique(NAZEV_LOK)),
    ID_ND_AKCE = toString(unique(ID_ND_AKCE)),
    CILMON_CHU = max(
      CILMON, 
      na.rm = TRUE
      ),
    POP_POCETPOLE1 = sum(
      ID_IND == "CELKOVE_HODNOCENI" &
        CILMON == 1, 
      na.rm = TRUE
    ),
    POP_POCETPOLE1D = sum(
      ID_IND == "CELKOVE_HODNOCENI" & 
        HOD_IND != "neznámý" &
        HOD_IND != "zhoršený" & 
        HOD_IND != "špatný" & 
        STAV_IND != "NA" & 
        STAV_IND != "0" &
        is.na(STAV_IND) == FALSE & 
        STAV_IND != 0.5 & 
        STAV_IND != 0 &
        CILMON == 1, 
      na.rm = TRUE)
  ) %>%
  dplyr::group_by(
    kod_chu, 
    DRUH
    ) %>%
  dplyr::mutate(
    POP_PROCPOLE1D = round(
      POP_POCETPOLE1D/POP_POCETPOLE1*100,
      3
      ),
    # pokryvnost preferovanych biotopu evd
    STA_HABPOKRYVPRE = {
      x <- biotop_evd$BIOTOP_PROCENTO[biotop_evd$SITECODE == kod_chu & 
                                        biotop_evd$DRUH == DRUH]
      if (length(x) == 0) NA_real_ else unique(x)
    },
    STA_HABPOKRYV = dplyr::case_when(
      is.na(STA_HABPOKRYVPRE) == TRUE ~ NA,
      TRUE ~ STA_HABPOKRYVPRE*100)
    ) %>%
  dplyr::select(
    -STA_HABPOKRYVPRE
    ) %>%
  dplyr::mutate(
    across(
      .cols = 7:ncol(.)-2,
      .fns = ~ as.character(.)
      )
    ) %>%
  tidyr::pivot_longer(
    .,
    cols = c(8:ncol(.)),
    names_to = "ID_IND",
    values_to = "HOD_IND"
    ) %>%
  dplyr::mutate(
    HOD_IND = as.character(HOD_IND)
    ) %>%
  dplyr::distinct() %>%
  dplyr::arrange(ID_ND_AKCE)

#--------------------------------------------------#
## Agregace po lokalite -----
#--------------------------------------------------#
n2k_druhy_chu_lok <- 
  n2k_druhy_lokeval %>%
  dplyr::group_by(
    kod_chu, 
    DRUH
    ) %>%
  dplyr::reframe(
    ## CHU_LOK_SPOLECNE ----
    ROK = toString(unique(ROK)),
    POLE = toString(unique(POLE)),
    NAZEV_LOK = toString(unique(NAZEV_LOK)),
    ID_ND_AKCE = toString(unique(ID_ND_AKCE)),
    CILMON_CHU = max(
      CILMON, 
      na.rm = TRUE
      ),
    CELKOVE_HODNOCENI = NA,
    POP_PRESENCE = dplyr::case_when(ID_IND == "POP_PRESENCE" & grepl("ano", HOD_IND) == TRUE ~ "ano", 
                                    ID_IND == "POP_PRESENCE" & grepl("ne", HOD_IND) == TRUE ~ "ne", 
                                    TRUE ~ NA_character_), 
    POP_POCETMAX = sum(
      dplyr::case_when(
        ID_IND == "POP_POCETMAX" ~ as.numeric(HOD_IND), 
        TRUE ~ NA
        ), 
      na.rm = TRUE
      ), 
    POP_POCETMAX = sum(
      dplyr::case_when(
        ID_IND == "POP_POCETMIN" ~ as.numeric(HOD_IND), 
        TRUE ~ NA
        ), 
      na.rm = TRUE
      ), 
    POP_POCETSUM = sum(
      dplyr::case_when(
        ID_IND == "POP_POCET" & CILMON == 1 ~ as.numeric(HOD_IND),
        TRUE ~ NA
        ), 
      na.rm = TRUE) %>%
      max(),
    POP_POCETDOB = sum(
      dplyr::case_when(
        ID_IND == "POP_POCET" & CELKOVE == 1 & CILMON == 1 ~ as.numeric(HOD_IND), 
        TRUE ~ 0
        ), 
      na.rm = TRUE) %>%
      max(),
    POP_POCETOST = sum(
      dplyr::case_when(
        ID_IND == "POP_POCET" & CELKOVE != 1 & CILMON == 1 ~ as.numeric(HOD_IND), 
        TRUE ~ 0
        ), 
      na.rm = TRUE),
    POP_PROCDOB = POP_POCETDOB/POP_POCETSUM*100,
    ## CHU_LOK_HMYZ ----
    ## CHU_LOK_OSTATNIBEZ ----
    ## CHU_LOK_OBOJZIVELNICI ----
    ## CHU_LOK_RYBY ----
    ## CHU_LOK_SAVCI ----
    POP_POCETZIM = sum(
      case_when(
        ID_IND == "POP_POCET" ~ as.numeric(HOD_IND), 
        TRUE ~ NA
        ), 
      na.rm = TRUE), 
    POP_POCETZIM1 = sum(
      case_when(
        ID_IND == "POP_POCET" ~ as.numeric(HOD_IND), 
        TRUE ~ NA
        ), 
      na.rm = TRUE),
    POP_POCETZIM2 = sum(
      case_when(
        ID_IND == "POP_POCET" ~ as.numeric(HOD_IND), 
        TRUE ~ NA
        ), 
      na.rm = TRUE
      ),
    POP_POCETZIM3 = sum(
      case_when(
        ID_IND == "POP_POCET" ~ as.numeric(HOD_IND), 
        TRUE ~ NA
        ),
      na.rm = TRUE
      ),
    POP_POCETZIMREF = mean(
      POP_POCETZIM1, 
      POP_POCETZIM2, 
      POP_POCETZIM3, 
      na.rm = TRUE
      ),
    POP_VITALZIM = ifelse(
      POP_POCETZIMREF == 0, 
      NA_real_, 
      POP_POCETZIM / POP_POCETZIMREF
      ),
    POP_POCETLETS1 = sum(
      case_when(
        ID_IND == "POP_POCET" ~ as.numeric(HOD_IND), 
        TRUE ~ NA
        ), 
      na.rm = TRUE
      ),
    POP_POCETLETS2 = sum(
      case_when(
        ID_IND == "POP_POCET" ~ as.numeric(HOD_IND), 
        TRUE ~ NA
        ), 
      na.rm = TRUE
      ),
    POP_POCETLET = sum(
      case_when(
        ID_IND == "POP_POCET" ~ as.numeric(HOD_IND), 
        TRUE ~ NA
        ), 
      na.rm = TRUE
      ), 
    POP_POCETLET1 = sum(
      case_when(ID_IND == "POP_POCET" ~ as.numeric(HOD_IND), 
                TRUE ~ NA
                ), 
      na.rm = TRUE
      ), 
    POP_POCETLET2 = sum(
      case_when(ID_IND == "POP_POCET" ~ as.numeric(HOD_IND), 
                TRUE ~ NA
                ), 
      na.rm = TRUE
      ), 
    POP_POCETLET3 = sum(
      case_when(ID_IND == "POP_POCET" ~ as.numeric(HOD_IND), 
                TRUE ~ NA
                ), 
      na.rm = TRUE
      ), 
    POP_POCETLETREF = mean(
      POP_POCETLET1, 
      POP_POCETLET2, 
      POP_POCETLET3, 
      na.rm = TRUE),
    POP_VITALLET = POP_POCETLET/POP_POCETLETREF,
    POP_REPROCHI = POP_POCETLETS2/POP_POCETLETS1,
    ## CHU_LOK_MECHOROSTY ----
    
    ## CHU_LOK_CEVNATE ----
    
    ## CHU_LOK_PTACI ----
    

  )



#--------------------------------------------------#
## Long format pripravneho objektu ---- 
#--------------------------------------------------#

n2k_druhy_chu_lok_long <- 
  n2k_druhy_chu_lok %>%
  dplyr::mutate(
    across(.cols = 7:ncol(.), 
           .fns = ~ as.character(.)
           )
    ) %>%
  tidyr::pivot_longer(
    .,
    cols = c(8:ncol(.)),
    names_to = "ID_IND",
    values_to = "HOD_IND"
    ) %>%
  dplyr::mutate(
    HOD_IND = as.character(HOD_IND)
    ) %>%
  dplyr::distinct() %>%
  dplyr::bind_rows(
    .,
    n2k_druhy_chu_pole1
    ) %>%
  dplyr::arrange(
    ID_ND_AKCE
    ) %>%
  dplyr::right_join(
    .,
    limity %>%
      dplyr::filter(
        UROVEN == "chu"
        ),
    by = c(
      "DRUH" = "DRUH",
      "ID_IND" = "ID_IND"
      )
    ) %>%
  dplyr::filter(
    UROVEN == "chu"
  ) %>%
  dplyr::group_by(
    kod_chu,
    DRUH,
    ID_IND
  ) %>%
  dplyr::arrange(
    HOD_IND
  ) %>%
  dplyr::slice(
    1
    ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    STAV_IND = dplyr::case_when(
      is.na(HOD_IND) == TRUE ~ NA_real_,
      TYP_IND == "min" & as.numeric(HOD_IND) < as.numeric(LIM_IND) ~ 0,
      TYP_IND == "min" & as.numeric(HOD_IND) >= as.numeric(LIM_IND) ~ 1,
      TYP_IND == "max" & as.numeric(HOD_IND) > as.numeric(LIM_IND) ~ 0,
      TYP_IND == "max" & as.numeric(HOD_IND) <= as.numeric(LIM_IND) ~ 1,
      TYP_IND == "val" & HOD_IND != LIM_IND ~ 0,
      TYP_IND == "val" & HOD_IND == LIM_IND ~ 1)) %>%
  dplyr::mutate(
    IND_GRP = dplyr::case_when(
      TYP_IND %in% c("min", "max") ~ "minmax",
      TRUE ~ TYP_IND),
    KLIC = dplyr::case_when(
      is.na(HOD_IND) == TRUE ~ "ne",
      TRUE ~ KLIC
      )
    ) 
n2k_druhy_chu_lok_long <- 
  n2k_druhy_chu_lok_long %>%
  dplyr::bind_rows(
    n2k_druhy_chu_lok_long %>%
      dplyr::distinct(
        kod_chu, 
        DRUH, 
        ROK, 
        ID_ND_AKCE, 
        POLE, 
        NAZEV_LOK, 
        CILMON_CHU
        ) %>%
      dplyr::mutate(
        ID_IND = "CELKOVE_HODNOCENI",
        HOD_IND = NA_character_,
        STAV_IND = NA_real_,
        TYP_IND = NA_character_,
        LIM_IND = NA_character_,
        IND_GRP = NA_character_,
        KLIC = NA_character_
      )
  ) %>%
  # Teď odstraníme všechny sloupce, co začínají na '...'
  dplyr::select(-dplyr::starts_with("..."))

#----------------------------------------------------------#
# Konsolidace uzemi -----
#----------------------------------------------------------#
n2k_druhy_chu <- 
  n2k_druhy_chu_lok_long %>%
  dplyr::group_by(
    kod_chu, 
    DRUH, 
    ID_IND,
    KLIC
    ) %>%
  dplyr::reframe(
    ROK = toString(unique(ROK)),
    POLE = toString(unique(POLE)),
    NAZEV_LOK = toString(unique(NAZEV_LOK)),
    ID_ND_AKCE = toString(unique(ID_ND_AKCE)),
    HOD_IND = toString(na.omit(unique(HOD_IND))),        
    TYP_IND = unique(TYP_IND),
    LIM_IND = unique(LIM_IND),
    JEDNOTKA = unique(JEDNOTKA),
    LIM_INDLIST = unique(LIM_INDLIST),
    STAV_IND = dplyr::case_when(
      is.na(HOD_IND) == TRUE ~ NA_real_,
      IND_GRP == "minmax" & 
        grepl(
          "POP_", 
          ID_IND
          )
      == TRUE
      ~ max(
        as.numeric(STAV_IND), 
        na.rm = TRUE
        ),
      IND_GRP == "minmax" & 
        grepl(
          "POP_", 
          ID_IND
          )
      == FALSE
      ~ min(
        as.numeric(STAV_IND),
        na.rm = TRUE
        ),
      IND_GRP == "val" 
      ~ max(
        as.numeric(STAV_IND),
        na.rm = TRUE
        )
      ),
    KLIC = unique(KLIC),
    UROVEN = unique(UROVEN),
    IND_GRP = unique(IND_GRP),
    CILMON_CHU = max(
      CILMON_CHU, 
      na.rm = TRUE
      )
  ) %>%
  dplyr::distinct() %>%
  dplyr::ungroup() %>%
  dplyr::left_join(., 
                   n2k_druhy_obdobi_chu,
                   by = join_by("kod_chu", "DRUH")) %>%
  dplyr::mutate(
    STAV_IND = ifelse(
      is.infinite(STAV_IND),
      0, 
      STAV_IND
      )
    ) %>%
  dplyr::group_by(
    kod_chu, 
    DRUH
    ) %>%
  dplyr::mutate(
    IND_SUM = sum(
      as.numeric(
        STAV_IND
        ),
      na.rm = TRUE
      ),
    IND_SUMKLIC = sum(
      as.numeric(
        STAV_IND[KLIC == "ano" &
                   UROVEN == "chu" &
                   !is.na(LIM_IND)]
        ), 
      na.rm = TRUE),
    IND_SUMOST = sum(
      as.numeric(
        STAV_IND[KLIC == "ne" &
                   UROVEN == "chu" &
                   !is.na(LIM_IND)]
        ),
      na.rm = TRUE
      ),
    LENIND_SUM = length(unique(ID_IND[UROVEN == "chu" &
                                        !is.na(LIM_IND)]) %>% 
                          na.omit()),
    LENIND_SUMKLIC = length(unique(ID_IND[KLIC == "ano" &
                                            UROVEN == "chu" &
                                            !is.na(LIM_IND) &
                                            !is.na(STAV_IND)]) %>%
                              na.omit()),
    LENIND_SUMOST = length(unique(ID_IND[KLIC == "ne" &
                                           UROVEN == "chu" &
                                           !is.na(LIM_IND) &
                                           !is.na(STAV_IND)]) %>%
                             na.omit()),
    LENIND_NAKLIC = sum(
      KLIC == "ano" &
        UROVEN == "chu" &
        is.na(HOD_IND),
      na.rm = TRUE
    )
  ) %>%
  # pokud sumklic spatny tak spatny, pokud sum osts tak spatny, pokud sumostz tak zhorseny, pokud nic z toho tak dobry, TRUE ~ neznamy pres napojeni na limity
  dplyr::mutate(
    CELKOVE = dplyr::case_when(
      HODNOCENE_OBDOBI_DO + years(6) < current_year ~ NA_real_,
      IND_SUMKLIC < (LENIND_SUMKLIC - 1 - LENIND_NAKLIC) ~ 0,
      IND_SUMKLIC < (LENIND_SUMKLIC - LENIND_NAKLIC) ~ 0.5,
      IND_SUMKLIC >= (LENIND_SUMKLIC - LENIND_NAKLIC) ~ 1,
      TRUE ~ NA_real_
      )
    ) %>%
  dplyr::mutate(
    STAV_IND = dplyr::case_when(
      ID_IND == "CELKOVE_HODNOCENI" ~ as.character(CELKOVE),
      TRUE ~ as.character(STAV_IND)
      )
    ) %>%
  dplyr::mutate(
    HOD_IND = dplyr::case_when(
      HOD_IND == "NaN" ~ NA_character_,
      is.na(HOD_IND) == TRUE ~ NA_character_,
      ID_IND == "CELKOVE_HODNOCENI" & is.na(STAV_IND) == TRUE ~ "neznámý",
      ID_IND == "CELKOVE_HODNOCENI" & STAV_IND == 0 ~ "špatný",
      ID_IND == "CELKOVE_HODNOCENI" & STAV_IND == 0.5 ~ "zhoršený",
      ID_IND == "CELKOVE_HODNOCENI" & STAV_IND == 1 ~ "dobrý",
      ID_IND == "CELKOVE_HODNOCENI" & STAV_IND == "0" ~ "špatný",
      ID_IND == "CELKOVE_HODNOCENI" & STAV_IND == "0.5" ~ "zhoršený",
      ID_IND == "CELKOVE_HODNOCENI" & STAV_IND == "1" ~ "dobrý",
      TRUE ~ HOD_IND
      )
    ) %>%
  dplyr::mutate(
    STAV_IND = dplyr::case_when(
      UROVEN != "chu" ~ "nehodnocen",
      ID_IND != "CELKOVE_HODNOCENI" & is.na(UROVEN) == TRUE ~ "nehodnocen",
      ID_IND != "CELKOVE_HODNOCENI" & is.na(LIM_IND) == TRUE ~ "nehodnocen",
      is.na(STAV_IND) == TRUE ~ "neznámý",
      is.infinite(STAV_IND) == TRUE ~ "neznámý",
      HOD_IND == " " ~ "neznámý",
      ID_IND == "STA_HABPOKRYV" ~ "neznámý",
      is.na(HOD_IND) == TRUE ~ "neznámý",
      STAV_IND == 0 ~ "špatný",
      STAV_IND == 0.5 ~ "zhoršený",
      STAV_IND == 1 ~ "dobrý",
      STAV_IND == "0" ~ "špatný",
      STAV_IND == "0.5" ~ "zhoršený",
      STAV_IND == "1" ~ "dobrý",
      TRUE ~ as.character(STAV_IND)
      )
    ) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  dplyr::arrange(
    DRUH, 
    kod_chu, 
    POLE
    ) %>%
  dplyr::left_join(
    .,
    evl %>%
      sf::st_drop_geometry() %>%
      dplyr::select(
        SITECODE,
        NAZEV
        ),
      by = c("kod_chu" = "SITECODE")) %>%
  dplyr::left_join(.,
                   rp_code,
                   by = join_by("kod_chu")) %>%
  # dplyr::right_join() zajistuje export hodnoceni pouze pro predmety ochrany
  dplyr::right_join(
    ., 
    n2k_oop,
    by = c("kod_chu" = "SITECODE")
    ) %>%
  dplyr::mutate(
    typ_predmetu_hodnoceni = "Druh",
    feature_code = NA,
    trend = "neznámý",
    datum_hodnoceni = Sys.Date()
    ) %>%
  dplyr::filter(
    CILMON_CHU == 1
    ) %>%
  dplyr::rename(
    nazev_chu = NAZEV, 
    druh = DRUH, #feature_code, 
    hodnocene_obdobi_od = HODNOCENE_OBDOBI_OD, 
    hodnocene_obdobi_do = HODNOCENE_OBDOBI_DO, 
    parametr_nazev = ID_IND, 
    parametr_hodnota = HOD_IND, 
    parametr_limit = LIM_IND, 
    parametr_jednotka = JEDNOTKA, 
    stav = STAV_IND
    ) %>%
  dplyr::select(
    typ_predmetu_hodnoceni, kod_chu, nazev_chu, 
    druh, feature_code, 
    hodnocene_obdobi_od, hodnocene_obdobi_do, oop, parametr_nazev, 
    parametr_hodnota, parametr_limit, parametr_jednotka, 
    stav, trend, datum_hodnoceni, pracoviste, ID_ND_AKCE
  ) %>%
  dplyr::left_join(
    ., 
    indikatory_id, 
    by = c("parametr_nazev" = "ind_r")
    ) %>%
  dplyr::mutate(
    parametr_nazev = ind_id,
    pracoviste = gsub(
      ",",
      "",
      pracoviste
      ),
    metodika = 15087
  ) %>%
  dplyr::select(
    -c(
      ind_id,
      ind_popis, 
      #oop,
      ID_ND_AKCE
      )
    ) %>%
  dplyr::distinct()

#----------------------------------------------------------#
# Zapis dat -----
#----------------------------------------------------------#
  
chu_export <-
  function() {
    
    sep_isop <- ";"
    quote_env_isop <- FALSE
    encoding_isop <- "UTF-8"
    
    sep <- ","
    quote_env <- TRUE
    encoding <- "Windows-1250"
    
    write.table(
      n2k_druhy_chu,
      paste0("C:/Users/jonas.gaigr/Documents/state_results/n2k_druhy_chu",
             "_",
             current_year,
             "_",
             gsub(
               "-", 
               "", 
               Sys.Date()
             ),
             "_",
             encoding,
             ".csv"
      ),
      row.names = FALSE,
      sep = sep,
      quote = quote_env,
      fileEncoding = encoding
    )  
    
    write.table(
      n2k_druhy_chu,
      paste0("C:/Users/jonas.gaigr/Documents/state_results/n2k_druhy_chu",
             "_",
             current_year,
             "_",
             gsub(
               "-", 
               "", 
               Sys.Date()
             ),
             "_",
             encoding_isop,
             ".csv"
      ),
      row.names = FALSE,
      sep = sep_isop,
      quote = quote_env_isop,
      fileEncoding = encoding_isop
    )  
    
  }
  
chu_export()

# KONEC SKRIPTU ----