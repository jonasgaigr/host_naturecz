
# LOKALITA LIMITY ----
n2k_druhy_lok_pre <- n2k_druhy_lim %>%
  dplyr::group_by(kod_chu, DRUH, KOD_LOKAL, POLE, ROK, ID_IND) %>%
  dplyr::reframe(
    SKUPINA = unique(SKUPINA),
    NAZEV_LOK = toString(unique(LOKALITA)),
    ID_ND_AKCE = toString(unique(IDX_ND_AKCE)),
    DATUM = max(DATUM, na.rm = TRUE),
    HOD_IND = toString(na.omit(unique(HOD_IND))),        
    TYP_IND = unique(TYP_IND),
    LIM_IND = unique(LIM_IND),
    JEDNOTKA = unique(JEDNOTKA),
    LIM_INDLIST = unique(LIM_INDLIST),
    STAV_IND = dplyr::case_when(IND_GRP == "minmax" & grepl("POP_", ID_IND) == TRUE ~ max(as.numeric(STAV_IND), na.rm = TRUE),
                                IND_GRP == "minmax" & grepl("POP_", ID_IND) == FALSE ~ min(as.numeric(STAV_IND), na.rm = TRUE),
                                IND_GRP == "val" ~ max(as.numeric(STAV_IND), na.rm = TRUE)),
    KLIC = unique(KLIC),
    IND_GRP = unique(IND_GRP),
    UROVEN = (unique(UROVEN)),
    CILMON = max(CILMON, na.rm = TRUE)
  ) %>%
  dplyr::mutate_all(~ ifelse(is.infinite(.), NA, .)) %>% 
  dplyr::ungroup() %>%
  dplyr::distinct()

n2k_druhy_lok <- n2k_druhy_lok_pre %>%
  dplyr::group_by(kod_chu, DRUH, KOD_LOKAL, ROK) %>%
  dplyr::mutate(IND_SUM = as.character(sum(as.numeric(STAV_IND), na.rm = TRUE)),
                IND_SUMKLIC = as.character(sum(as.numeric(STAV_IND[KLIC == "ano" &
                                                                     is.na(LIM_IND) == FALSE]), 
                                               na.rm = TRUE)),
                IND_SUMOST = as.character(sum(as.numeric(STAV_IND[KLIC == "ne" &
                                                                    is.na(LIM_IND) == FALSE]), 
                                              na.rm = TRUE)),
                LENIND_SUM = length(unique(limity$ID_IND[limity$DRUH %in% DRUH &
                                                           limity$UROVEN %in% c("lok") &
                                                           is.na(limity$LIM_IND) == FALSE],
                                           na.rm = TRUE) %>% na.omit()),
                LENIND_SUMKLIC = length(unique(limity$ID_IND[limity$DRUH %in% DRUH & 
                                                               limity$KLIC == "ano" &
                                                               limity$UROVEN %in% c("lok") &
                                                               is.na(limity$LIM_IND) == FALSE],
                                               na.rm = TRUE) %>% na.omit()),
                LENIND_SUMOST = length(unique(limity$ID_IND[limity$DRUH %in% DRUH & 
                                                              limity$KLIC == "ne" &
                                                              limity$UROVEN %in% c("lok") &
                                                              is.na(limity$LIM_IND) == FALSE],
                                              na.rm = TRUE) %>% na.omit())) %>%
  # pokud sumklic spatny tak spatny, pokud sum ost tak spatny, pokud sumost tak zhorseny, pokud nic z toho tak dobry, TRUE ~ neznamy pres napojeni na limity
  dplyr::mutate(CELKOVE = dplyr::case_when(unique(IND_SUMKLIC) < LENIND_SUMKLIC ~ 0,
                                           unique(IND_SUMOST) < (LENIND_SUMOST - 2) ~ 0,
                                           unique(IND_SUMOST) < (LENIND_SUMOST - 1) ~ 0.5,
                                           unique(IND_SUMKLIC) >= LENIND_SUMKLIC &
                                             unique(IND_SUMOST) > (LENIND_SUMOST - 1) ~ 1,
                                           TRUE ~ NA_real_)) %>%
  dplyr::mutate(STAV_IND = dplyr::case_when(ID_IND == "CELKOVE_HODNOCENI" ~ CELKOVE,
                                            TRUE ~ STAV_IND)) %>%
  dplyr::mutate(HOD_IND = dplyr::case_when(ID_IND == "CELKOVE_HODNOCENI" & STAV_IND == 0 ~ "špatný",
                                           ID_IND == "CELKOVE_HODNOCENI" & STAV_IND == 0.5 ~ "zhoršený",
                                           ID_IND == "CELKOVE_HODNOCENI" & STAV_IND == 1 ~ "dobrý",
                                           TRUE ~ HOD_IND)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  dplyr::arrange(ID_ND_AKCE)

n2k_druhy_pole1_idakce <- n2k_druhy_lok %>%
  #dplyr::filter(SKUPINA == "Cévnaté rostliny") %>%
  dplyr::group_by(kod_chu, DRUH, POLE) %>%
  dplyr::arrange(desc(CILMON),
                 desc(ROK), 
                 desc(CELKOVE), 
                 desc(DATUM)) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::pull(ID_ND_AKCE)

n2k_druhy_pole1eval <- n2k_druhy_lok %>%
  dplyr::filter(ID_ND_AKCE %in% n2k_druhy_pole1_idakce) %>%
  dplyr::ungroup()

n2k_druhy_lok_idakce <- n2k_druhy_lok %>%
  dplyr::group_by(kod_chu, DRUH, KOD_LOKAL) %>%
  dplyr::arrange(desc(CILMON),
                 desc(ROK), 
                 desc(CELKOVE), 
                 desc(DATUM)) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::pull(ID_ND_AKCE)

n2k_druhy_lokeval <- n2k_druhy_lok %>%
  #dplyr::filter(SKUPINA == "Cévnaté rostliny") %>%
  dplyr::filter(ID_ND_AKCE %in% n2k_druhy_pole1_idakce) %>%
  dplyr::ungroup()

write_lok <- write.csv(
  n2k_druhy_lokeval %>%
    dplyr::left_join(., 
                     evl %>%
                       sf::st_drop_geometry() %>%
                       dplyr::select(SITECODE, NAZEV),
                     by = c("kod_chu" = "SITECODE")) %>%
    dplyr::left_join(., 
                     n2k_druhy_obdobi_chu,
                     by = join_by("kod_chu", "DRUH")) %>%
    dplyr::left_join(.,
                     rp_code,
                     by = join_by("kod_chu")) %>%
    dplyr::left_join(., 
                     n2k_oop,
                     by = c("kod_chu" = "SITECODE")),
  paste0("C:/Users/jonas.gaigr/Documents/state_results/n2k_druhy_lok_",
         toString(Sys.Date()),
         ".csv"),
  row.names = TRUE,
  fileEncoding = "Windows-1250")
