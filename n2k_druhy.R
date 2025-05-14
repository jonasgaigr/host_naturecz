# PŘÍPRAVA INDIKÁTORŮ ----
n2k_druhy_pre <- n2k_export %>%
  #dplyr::filter(DRUH == "Epidalea calamita") %>%
  dplyr::rename(POLE = POLE_1_RAD) %>% 
  dplyr::mutate(
    # Převedení DRUHu na kategorickou veličinu
    DRUH = as.factor(DRUH),
    # Převedení datumu do vhodného formátu
    DATUM = as.Date(as.character(DATUM_OD), format = '%d.%m.%Y'),
    # Redukce data na den
    DEN = as.numeric(substring(DATUM_OD, 1, 2)),
    # Redukce data na měsíc
    MESIC = as.numeric(substring(DATUM_OD, 4, 5)),
    # Redukce data na rok
    ROK = as.numeric(substring(DATUM_OD, 7, 11)),
    # Izolace kódu EVL
    kod_chu = substr(EVL, 1, 9),
    # Izolace názvu lokality
    nazev_chu = substr(as.character(EVL), 12, nchar(as.character(EVL))),
    KOD_LOKRYB = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<naz_tok>).*(?=</naz_tok>)")),
    KOD_LOKAL = case_when(SKUPINA == "Letouni" ~ LOKALITA,
                          SKUPINA == "Mechy" ~ LOKALITA,
                          SKUPINA == "Motýli" ~ substring(KOD_LOKALITY, 1, 10),
                          SKUPINA == "Ryby a mihule" & is.na(KOD_LOKRYB) == FALSE ~ KOD_LOKRYB,
                          KOD_LOKALITY == "amp216" ~ "CZ0723412",
                          KOD_LOKALITY == "amp222" ~ "CZ0724089_9",
                          KOD_LOKALITY == "amp231" ~ "CZ0724089_19",
                          KOD_LOKALITY == "amp185" ~ "CZ0623345",
                          KOD_LOKALITY == "amp101" ~ "CZ0423006",
                          KOD_LOKALITY == "amp71" ~ "CZ0323158",
                          KOD_LOKALITY == "amp59" ~ "CZ0323144",
                          KOD_LOKALITY == "amp15" ~ "CZ0213790",
                          KOD_LOKALITY == "amp102" ~ "CZ0423215",
                          KOD_LOKALITY == "amp254" ~ "CZ0813455",
                          KOD_LOKALITY == "amp227" ~ "CZ0723410",
                          KOD_LOKALITY == "amp336 (CZ_5)" ~ "CZ0714073_5",
                          KOD_LOKALITY == "amp205 (CZ_3)" ~ "CZ0714073_3",
                          KOD_LOKALITY == "amp337" ~ "CZ0713383",
                          KOD_LOKALITY == "amp129" ~ "CZ0523011",
                          KOD_LOKALITY == "amp334 (CZ_3)" ~ "CZ0523010_3",
                          KOD_LOKALITY == "amp138 (CZ_2)" ~ "CZ0523010_2",
                          KOD_LOKALITY == "amp335 (cz_1)" ~ "CZ0523010_1",
                          KOD_LOKALITY == "amp102" ~ "CZ0423215",
                          KOD_LOKALITY == "amp116" ~ "CZ0513249",
                          KOD_LOKALITY == "amp101" ~ "CZ0423006",
                          KOD_LOKALITY == "amp15" ~ "CZ0213790",
                          KOD_LOKALITY == "amp30" ~ "CZ0213077",
                          KOD_LOKALITY == "amp314 (cz_2)" ~ "CZ0213064_2",
                          KOD_LOKALITY == "amp316 (cz_3)" ~ "CZ0213064_3",
                          KOD_LOKALITY == "amp315 (cz_1)" ~ "CZ0213064_1",
                          KOD_LOKALITY == "CZ0213008" ~ "CZ0213008_1",
                          KOD_LOKALITY == "amp244" ~ "CZ0813457",
                          KOD_LOKALITY == "amp207" ~ "CZ0713385",
                          KOD_LOKALITY == "amp64" ~ "CZ0323143",
                          KOD_LOKALITY == "amp24" ~ "CZ0213787",
                          KOD_LOKALITY == "amp279" ~ "CZ0613335_03",
                          KOD_LOKALITY == "amp110" ~ "CZ0513244",
                          KOD_LOKALITY == "amp339" ~ "CZ0213066_2",
                          KOD_LOKALITY == "amp340" ~ "CZ0213066_1",
                          KOD_LOKALITY == "amp27" ~ "CZ0213058",
                          KOD_LOKALITY == "amp226" ~ "CZ0724429_3",
                          KOD_LOKALITY %in% c("amp211", "amp211" , "amp211", "amp107", "amp99",
                                              "amp53", "amp252", "amp281", "amp280", "amp22",
                                              "amp81", "amp25", "CZ0813450", "CZ0713397") ~ NA_character_,
                          kod_chu == "CZ0623367" ~ "CZ0623367",
                          is.na(KOD_LOKALITY) == TRUE ~ kod_chu,
                          TRUE ~ NA_character_)) %>%
  dplyr::mutate(KOD_LOKAL = dplyr::case_when(is.na(KOD_LOKAL) == TRUE ~ KOD_LOKALITY,
                                             TRUE ~ KOD_LOKAL)) %>%
  dplyr::select(-KOD_LOKRYB) %>%
  dplyr::filter(ROK >= current_year - 12) %>%
  dplyr::mutate(KOD_LOKAL = dplyr::case_when(is.na(KOD_LOKAL) == TRUE ~ LOKALITA,
                                             TRUE ~ KOD_LOKAL)) %>%
  # identifikace dat cileneho monitoringu
  dplyr::mutate(CILMON = dplyr::case_when(ZDROJ %in% CIS_CILMON ~ 1,
                                              PROJEKT == "Monitoring druhů ČR" ~ 1,
                                              DRUH %in% c("Carabus menetriesi pacholei", 
                                                          "Bolbelasmus unicornis") ~ 1,
                                              TRUE ~ 0)) %>%
  filter(SKUPINA == "Cévnaté rostliny") %>%
  #filter(SKUPINA %in% c("Motýli", "Brouci", "Vážky")) %>%
  #filter(SKUPINA %in% c("Letouni", "Savci")) %>%
  # NALEZ_SPOLECNE ---- 
  dplyr::mutate(
    POP_PRESENCE_N = dplyr::case_when(NEGATIVNI == 1 ~ 0,
                                      POCET == 0 ~ 0,
                                      NEGATIVNI == 0 ~ 1,
                                      POCET > 0 ~ 1),
    POP_PRESENCE = dplyr::case_when(POP_PRESENCE_N == 1 ~ "ano",
                                    POP_PRESENCE_N == 0 ~ "ne"),
    POP_POCET = dplyr::case_when(POP_PRESENCE == "ne" ~ 0,
                                 POCITANO %in% limity$JEDNOTKA[limity$DRUH %in% DRUH & limity$ID_IND %in% "POP_POCET"] ~ POCET,
                                 TRUE ~ NA_real_),
    POP_POCETSUM = dplyr::case_when(POP_PRESENCE == "ne" ~ 0,
                                    POCITANO %in% limity$JEDNOTKA[limity$DRUH %in% DRUH & limity$ID_IND %in% "POP_POCETSUM"] ~ POCET,
                                    TRUE ~ NA_real_),
    POP_PLOCHA = dplyr::case_when(POP_PRESENCE == "ne" ~ 0,
                                  POCITANO %in% limity$JEDNOTKA[limity$DRUH %in% DRUH & limity$ID_IND %in% "POP_PLOCHA"] ~ POCET,
                                  TRUE ~ NA_real_),
    POP_REPRO = dplyr::case_when(POP_PRESENCE == "ne" ~ "ne",
                                 POP_PRESENCE == "ano" & POCITANO %in% limity$JEDNOTKA[limity$ID_IND == "POP_REPRO" & limity$druh == DRUH] ~ "ano",
                                 POP_PRESENCE == "ano" & POCITANO %in% !limity$JEDNOTKA[limity$ID_IND == "POP_REPRO" & limity$druh == DRUH] ~ "ano",
                                 TRUE ~ NA_character_),
    POP_POCETNOST = dplyr::case_when(POP_PRESENCE == 0 ~ 0,
                                     POP_POCET > 1000 ~ 5,
                                     REL_POC == "řádově tisíce" ~ 5,
                                     REL_POC == "1001-10 000" ~ 5,
                                     grepl("počet samců: řádově tisíce", POZN_TAX) ~ 5,
                                     POP_POCET > 100 & POP_POCET <= 1000 ~ 4,
                                     REL_POC == "řádově stovky" ~ 4,
                                     REL_POC == "101-1000" ~ 4,
                                     grepl("počet samců: řádově stovky", POZN_TAX) ~ 4,
                                     REL_POC == "cca 100" ~ 4,    
                                     grepl("počet samců: cca 100", POZN_TAX) ~ 4,
                                     grepl("počet samců: řádově vyšší desítky", POZN_TAX) ~ 3,
                                     REL_POC == "řádově vyšší desítky" ~ 3,
                                     POP_POCET > 51 & POP_POCET <= 100 ~ 3,
                                     REL_POC == "řádově nižší desítky" ~ 2,
                                     grepl("počet samců: řádově nižší desítky", POZN_TAX) ~ 2,
                                     REL_POC == "11-100" ~ 2,
                                     POP_POCET > 10 & POP_POCET < 50 ~ 2,
                                     POP_POCET > 0 & POP_POCET <= 10 ~ 1,
                                     REL_POC == "do 10" ~ 1,
                                     REL_POC == "1-10" ~ 1,
                                     grepl("počet samců: do 10", POZN_TAX) ~ 1),
    vliv = stringr::str_extract(STRUKT_POZN, "(?<=<vliv>).*(?=</vliv>)")) %>%
  dplyr::mutate(
    VLV_VLIVY = dplyr::case_when(is.na(vliv) == FALSE ~ vliv,
                                 TRUE ~ stringr::str_extract(STRUKT_POZN, "(?<=<VLV_VLIVY>).*(?=</VLV_VLIVY>)")),
    VLV_VLIVY_NUM = stringr::str_count(STRUKT_POZN, ",")) %>%
  dplyr::mutate(
      # NALEZ_HMYZ ----
      STA_SECCAS = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<sec_nacasovani>).*(?=</sec_nacasovani>)")),
      STA_SECMET = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<sec_celoplosna>).*(?=</sec_celoplosna>)")),
      STA_PRITOMNOSTROSTLIN = dplyr::case_when(ROK < 2021 ~ readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<sta_pritomnostrostlin>).*(?=</sta_pritomnostrostlin>)")),
                                               TRUE ~ readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_PRITOMNOSTROSTLIN>).*(?=</STA_PRITOMNOSTROSTLIN>)"))),
      STA_JASANOKOLI = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_JASANOKOLI>).*(?=</STA_JASANOKOLI>)")),
      STA_MIKRPROSTLINA = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_MIKRPROSTLINA>).*(?=</STA_MIKRPROSTLINA>)")),
      STA_VHSTROMN = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_VHSTROMN>).*(?=</STA_VHSTROMN>)")),
      STA_VHSTROMK = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_VHSTROMK>).*(?=</STA_VHSTROMK>)")),
      STA_PERSPEKTIVA = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_PERSPEKTIVA>).*(?=</STA_PERSPEKTIVA>)")),
      STA_MRTDREVO = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_MRTDREVO>).*(?=</STA_MRTDREVO>)")),
      STA_TEKVODA = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_TEKVODA>).*(?=</STA_TEKVODA>)")),
      STA_POKRYVNOSTDREVIN = readr::parse_number(stringr::str_extract(STRUKT_POZN, "(?<=<STA_POKRYVNOSTDREVIN>).*(?=</STA_POKRYVNOSTDREVIN>)")),
      STA_ZAZEMNENI = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_ZAZEMNENI>).*(?=</STA_ZAZEMNENI>)")),
      STA_SKLADREVO = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_SKLADREVO>).*(?=</STA_SKLADREVO>)")),
      STA_LITVEGET = readr::parse_number(stringr::str_extract(STRUKT_POZN, "(?<=<STA_LITVEGET>).*(?=</STA_LITVEGET>)")),
      STA_SIRKALIT = readr::parse_number(stringr::str_extract(STRUKT_POZN, "(?<=<STA_SIRKALIT>).*(?=</STA_SIRKALIT>)")),
      STA_ZASTIN = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_ZASTIN>).*(?=</STA_ZASTIN>)")),
      STA_MAN = dplyr::case_when(STA_SECCAS == 1 &
                                   STA_SECMET == 1 ~ 1,
                                 STA_SECCAS == 0 |
                                   STA_SECMET == 0 ~ 0),
      STA_LIKVIDACE = dplyr::case_when(grepl(paste(c("vysazování lesů", 
                                                     "odvodňování, meliorace",
                                                     "zalesňování bezlesí",
                                                     "změna zemědělského využívání půdy"), 
                                               collapse = "|"), 
                                         STRUKT_POZN, 
                                         ignore.case = TRUE) ~ "zaznamenána",
                                       STA_SECCAS == 0 ~ "zaznamenána",
                                       STA_MAN == 0 & DRUH == "Phengaris teleius" ~ "nezaznamenána",
                                      CILMON == 1 ~ "nezaznamenána"),
      # NALEZ_OSTATNIBEZ ----
      # NALEZ_OBOJZIVELNICI ----
      STA_STAVVODARYBNIK = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_STAVVODARYBNIK>).*(?=</STA_STAVVODARYBNIK>)")),
      STA_STAVVODALITORAL = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_STAVVODALITORAL>).*(?=</STA_STAVVODALITORAL>)")),
      STA_STAVVODATUNE = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_STAVVODATUNE>).*(?=</STA_STAVVODATUNE>)")),
      STA_STAVVODA = dplyr::case_when(is.na(STA_STAVVODATUNE) == FALSE ~ STA_STAVVODATUNE,
                                      is.na(STA_STAVVODALITORAL) == FALSE ~ STA_STAVVODALITORAL,
                                      is.na(STA_STAVVODARYBNIK) == FALSE ~ STA_STAVVODARYBNIK),
      STA_ZTRATABIO = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_ZOOPLANKTON>).*(?=</STA_ZOOPLANKTON>)")),
      STA_KACHNAPRITOMNOST = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_KACHNAPRITOMNOST>).*(?=</STA_KACHNAPRITOMNOST>)")),
      STA_RYBY = dplyr::case_when(grepl("akvakultur", STRUKT_POZN, ignore.case = TRUE) |
                                    grepl("rybolov", STRUKT_POZN, ignore.case = TRUE) ~ "ano",
                                  TRUE ~ "ne"),
      STA_ZOOPLANKTON = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_ZOOPLANKTON>).*(?=</STA_ZOOPLANKTON>)")),
      STA_MANIPULACE = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_MANIPULACE>).*(?=</STA_MANIPULACE>)")),
      STA_POKRVEGETACE = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_POKRVEGETACE>).*(?=</STA_POKRVEGETACE>)")),
      STA_PRUHLEDNOSTVODA = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_PRUHLEDNOSTVODA>).*(?=</STA_PRUHLEDNOSTVODA>)")),
      STA_PRUHLEDNOSTVODAT = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_PRUHLEDNOSTVODAT>).*(?=</STA_PRUHLEDNOSTVODAT>)")),
      STA_PRUHLEDNOSTVODA = dplyr::case_when(is.na(STA_PRUHLEDNOSTVODAT) == FALSE ~ STA_PRUHLEDNOSTVODAT,
                                             TRUE ~ STA_PRUHLEDNOSTVODA),
      STA_UHYNOBOJZIVELNIK = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_UHYNOBOJZIVELNIK>).*(?=</STA_UHYNOBOJZIVELNIK>)")),
      STA_ZASTINENIHLADINA = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_ZASTINENIHLADINA>).*(?=</STA_ZASTINENIHLADINA>)")),
      STA_ZASTINENILITORAL = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_ZASTINENILITORAL>).*(?=</STA_ZASTINENILITORAL>)")),
      STA_ZASTINENIHLADINA = dplyr::case_when(STA_ZASTINENIHLADINA <= STA_ZASTINENILITORAL ~ STA_ZASTINENILITORAL,
                                             TRUE ~ STA_ZASTINENIHLADINA),
      # NALEZ_RYBY ----
      
      # NALEZ_SAVCI ----
      POP_PRESENCE_ZIMNI = max(POP_PRESENCE[(ROK == ROK & MESIC < 5) | (ROK == ROK - 1 & MESIC > 9)], 
                           na.rm = TRUE),
      POP_PRESENCE_LETNI = max(POP_PRESENCE[(ROK == ROK &
                                       MESIC >= 5 & 
                                       MESIC <= 9)], 
                           na.rm = TRUE),
      # NALEZ MECHOROSTY ----
      POP_POCETMIKROLOK = readr::parse_number(stringr::str_extract(STRUKT_POZN, "(?<=<pocet_ml>).*(?=</pocet_ml>)")),
      POP_TRENDBRY = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<trend_vyvoj>).*(?=</trend_vyvoj>)")),
      POP_ZMENABRY1 = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<SP_POSKOZENI_ROSTLIN>).*(?=</SP_POSKOZENI_ROSTLIN>)")),
      POP_ZMENABRY2 = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<str_strom>).*(?=</str_strom>)")),
      POP_PLOCHAPOP = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<str_strom>).*(?=</str_strom>)")),
      STA_DREVOBRY = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<SUB>).*(?=</SUB>)")),
      STA_STRUKTVEKBRY = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<str_strom>).*(?=</str_strom>)")),
      STA_STRUKTDRUBRY = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<druh_strom>).*(?=</druh_strom>)")),
      # NALEZ_CEVNATE ----
      POP_POCETLODYH = dplyr::case_when(POP_PRESENCE == "ne" ~ 0,
                                        POCITANO %in% limity$JEDNOTKA[limity$DRUH %in% DRUH & limity$ID_IND %in% "POP_POCETSUMLOD"] ~ POCET,
                                        TRUE ~ NA_real_),
      POP_POCETVITAL = dplyr::case_when(POP_PRESENCE == "ne" ~ 0,
                                        POCITANO %in% limity$JEDNOTKA[limity$DRUH %in% DRUH & limity$ID_IND %in% "POP_VITAL"] ~ POCET,
                                        TRUE ~ NA_real_),
      STA_MAN = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<MAN>).*(?=</MAN>)")),
      STA_MANPOTREBAVLIV = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<MAN_POTREBAVLIV>).*(?=</MAN_POTREBAVLIV>)")),
      STA_MANPOTREBA = NA,
      STA_MANVLIV = NA,
      STA_POKRYVNOSTINVAZNI = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_POKRYVNOSTINVAZNI>).*(?=</STA_POKRYVNOSTINVAZNI>)")),
      STA_POKRYVNOSTEXPANZNI = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_POKRYVNOSTEXPANZNI>).*(?=</STA_POKRYVNOSTEXPANZNI>)")),
      STA_POKRSTAR = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_POKRYVNOSTSTARINA>).*(?=</STA_POKRYVNOSTSTARINA>)")),
      STA_POKRDREV = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_POKRYVNOSTDREVIN>).*(?=</STA_POKRYVNOSTDREVIN>)")),
      STA_POKRDREVNIZ = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_POKRYVNOSTDREVINNIZ>).*(?=</STA_POKRYVNOSTDREVINNIZ>)")),
      STA_POKRYVNOSTE2E3 = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_POKRYVNOSTE2E3>).*(?=</STA_POKRYVNOSTE2E3>)")),
      STA_POKRYVNOSTE1 = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_POKRYVNOSTE1>).*(?=</STA_POKRYVNOSTE1>)")),
      STA_POKRYVNOSTE0 = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_POKRYVNOSTE0>).*(?=</STA_POKRYVNOSTE0>)")),
      STA_POKRYVNOSTVOLNAPUDA = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_POKRYVNOSTVOLNAPUDA>).*(?=</STA_POKRYVNOSTVOLNAPUDA>)")),
      STA_ZAPOJENICELK = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_ZAPOJENICELK>).*(?=</STA_ZAPOJENICELK>)")),
      STA_ZAPOJENIKAT = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_ZAPOJENICELK>).*(?=:)")),
      STA_STUPENZACH = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<SP_STUPEN_ZACH_STAN>).*(?=</SP_STUPEN_ZACH_STAN>)")),
      STA_HYDRPOMERY = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_HYDRPOMERY>).*(?=</STA_HYDRPOMERY>)")),
      STA_VLHPOM = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_VLHKOPOMERY>).*(?=</STA_VLHKOPOMERY>)")),
      STA_ZASTINENICELK = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_ZASTINENICELK>).*(?=</STA_ZASTINENICELK>)")),
      SP_POSKOZENI_ROSTLIN = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<SP_POSKOZENI_ROSTLIN>).*(?=</SP_POSKOZENI_ROSTLIN>)")),
      POSKOZENI_ROSTLIN = dplyr::case_when(is.na(SP_POSKOZENI_ROSTLIN) ~ NA_character_,
                                           TRUE ~ paste0(SP_POSKOZENI_ROSTLIN, ";")),
      POSKOZENI_ROSTLIN_KAT = POSKOZENI_ROSTLIN %>%
        str_replace_all(., "Nezjištěno", "0") %>%
        str_replace_all(., "Bez poškození", "0") %>%
        str_replace_all(., "Bez poškození.", "0") %>%
        str_replace_all(., "1-10 %", "1") %>%
        str_replace_all(., "10 %", "1") %>%
        str_replace_all(., "11-50 %", "2") %>%
        str_replace_all(., "10-50 %", "2") %>%
        str_replace_all(., "51-100 %", "3") %>%
        str_replace_all(., "50-100 %", "3") %>%
        str_replace_all(., "100 %", "3")
      ) %>%
  dplyr::distinct() %>%
  dplyr::group_by(IDX_ND_AKCE, DRUH) %>%
  dplyr::mutate(POP_POCETSUM = sum(POP_POCETSUM, na.rm = TRUE),
                POP_POCET = dplyr::case_when(is.na(POP_POCETSUM) == FALSE ~ POP_POCET,
                                             POP_POCETSUM > POP_POCET ~ POP_POCETSUM,
                                             NA ~ POP_POCET)) %>%
  dplyr::group_by(IDX_ND_AKCE, DRUH, DATUM) %>%
  dplyr::mutate(POP_POCETMIN = dplyr::case_when(is.na(POP_POCET) == FALSE ~ POP_POCET,
                                                POP_POCETNOST == 5 ~ 1001,
                                                POP_POCETNOST == 4 ~ 100,
                                                POP_POCETNOST == 3 ~ 10000,
                                                POP_POCETNOST == 2 ~ 10000,
                                                POP_POCETNOST == 1 ~ 10000),
                POP_POCETMAX = dplyr::case_when(is.na(POP_POCET) == FALSE ~ POP_POCET,
                                                POP_POCETNOST == 5 ~ 10000,
                                                POP_POCETNOST == 4 ~ 1000,
                                                POP_POCETNOST == 3 ~ 10000,
                                                POP_POCETNOST == 2 ~ 10000,
                                                POP_POCETNOST == 1 ~ 10000),
                POP_POCETLODYHSUM = sum(POP_POCETLODYH, na.rm = TRUE),
                POP_POCETPLOD = dplyr::case_when(POP_PRESENCE == "ne" ~ 0,
                                                 POCITANO %in% limity$JEDNOTKA[limity$DRUH %in% DRUH & limity$ID_IND %in% "POP_POCETSUMLOD"] ~ POCET,
                                                 TRUE ~ NA_real_),
                POP_VITAL = POP_POCETVITAL/POP_POCET*100) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(ID_ND_NALEZ) %>%
  tidyr::separate_rows(POSKOZENI_ROSTLIN, sep = "; ") %>%
  dplyr::distinct() %>%
  dplyr::mutate(
    POP_POSKPOC = dplyr::case_when(is.na(SP_POSKOZENI_ROSTLIN) == TRUE ~ NA,
                                   SP_POSKOZENI_ROSTLIN == "Nezjištěno" ~ 0,
                                   TRUE ~ sum(!is.na(POSKOZENI_ROSTLIN) & 
                                                POSKOZENI_ROSTLIN != "Nezjištěno")),
    POCET_POSKOZENYCH = readr::parse_number(POSKOZENI_ROSTLIN),
    POCET_POSKOZENYCHSUM = sum(POCET_POSKOZENYCH, na.rm = TRUE),
    PROCENTO_POSKOZENYCH = POCET_POSKOZENYCHSUM/unique(POP_POCETLODYHSUM),
    POSKOZENI_KAT = dplyr::case_when(grepl("Nezjištěno", POSKOZENI_ROSTLIN) ~ 0,
                                     grepl("1-10 %", POSKOZENI_ROSTLIN) ~ 1,
                                     grepl("10 %", POSKOZENI_ROSTLIN) ~ 1,
                                     grepl("11-50 %", POSKOZENI_ROSTLIN) ~ 2,
                                     grepl("10-50 %", POSKOZENI_ROSTLIN) ~ 2,
                                     grepl("51-100 %", POSKOZENI_ROSTLIN) ~ 3,
                                     grepl("50-100 %", POSKOZENI_ROSTLIN) ~ 3,
                                     grepl("100 %", POSKOZENI_ROSTLIN) ~ 3,
                                     TRUE ~ NA_integer_),
    POSKOZENI_LODYHKAT = dplyr::case_when(PROCENTO_POSKOZENYCH == 0 ~ 0,
                                          PROCENTO_POSKOZENYCH > 0 & PROCENTO_POSKOZENYCH <= 0.1 ~ 1,
                                          PROCENTO_POSKOZENYCH > 0.1 & PROCENTO_POSKOZENYCH <= 0.5 ~ 2,
                                          PROCENTO_POSKOZENYCH > 0.5 ~ 3,
                                          TRUE ~ NA_integer_),
    POP_POSKPERC = dplyr::case_when(DRUH == "Adenophora liliifolia" ~ unique(POSKOZENI_LODYHKAT),
                                    TRUE ~ sum(POSKOZENI_KAT, na.rm = TRUE))
    ) %>%
  dplyr::ungroup() %>%
  tidyr::separate_rows(STA_PRUHLEDNOSTVODA, sep = ",") %>%
  tidyr::separate_rows(STA_ZAPOJENICELK, sep = ",") %>%
  dplyr::mutate(STA_ZAPOJENIDRN = dplyr::case_when(nchar(STA_ZAPOJENICELK) > 15 ~ str_match(STA_ZAPOJENICELK, "zapojený::\\s*(.*?)\\s*%")[,2],
                                                    TRUE ~ "0"),
                STA_ZAPOJENIDRN = readr::parse_number(STA_ZAPOJENICELK)) %>%
  dplyr::distinct() 

# LOK_SPOLECNE ----
n2k_druhy_lokpop <- n2k_druhy_pre %>%
  #dplyr::filter(SKUPINA %in% c("Cévnaté rostliny", "Obojživelníci")) %>%
  dplyr::select(-c(ZDROJ:PRESNOST), SKUPINA) %>%
  dplyr::group_by(KOD_LOKAL, ROK, DRUH) %>%
  dplyr::reframe(
    CELKOVE = NA,
    POP_POCETSUMLOKAL = sum(POP_POCET, na.rm = TRUE),
    POP_POCETMIN = min(POP_POCET, na.rm = TRUE), 
    POP_POCETMAX = max(POP_POCET, na.rm = TRUE), 
    POP_POCETMAX = ifelse(is.infinite(POP_POCETMAX), 0, POP_POCETMAX),
    POP_POCETNOSTMAX = NA,
    #POP_POCETSUM = sum(POP_POCET, na.rm = TRUE),
    #CILMON = max(CILMON, na.rm = TRUE),
    # LOK_HMYZ ----
    # LOK_OSTATNIBEZ ----
    # LOK_OBOJZIVELNICI ----
    # LOK_RYBY ----
    # LOK_SAVCI ----
    POP_POCETZIM = max(POP_POCET[(ROK == current_year & MESIC < 5) |
                                   (ROK == current_year - 1 & MESIC > 9)], 
                       na.rm = TRUE), 
    POP_POCETZIM1 = max(POP_POCET[(ROK == current_year - 1 & MESIC < 5) |
                                    (ROK == current_year - 2 & MESIC > 9)], 
                        na.rm = TRUE),
    POP_POCETZIM2 = max(POP_POCET[(ROK == current_year - 2 & MESIC < 5) |
                                    (ROK == current_year - 3 & MESIC > 9)], 
                        na.rm = TRUE),
    POP_POCETZIM3 = max(POP_POCET[(ROK == current_year - 3 & MESIC < 5) |
                                    (ROK == current_year - 4 & MESIC > 9)], 
                        na.rm = TRUE),
    POP_POCETZIMREF = mean(POP_POCETZIM1, POP_POCETZIM2, POP_POCETZIM3, na.rm = TRUE),
    POP_VITALZIM = POP_POCETZIM/POP_POCETZIMREF,
    POP_POCETLETS1 = max(POP_POCET[(ROK == current_year & ((MESIC == 5 & DEN >= 15) | (MESIC == 6 & DEN <= 15)))], 
                         na.rm = TRUE),
    POP_POCETLETS2 = max(POP_POCET[(ROK == current_year & ((MESIC == 6 & DEN > 15) | (MESIC %in% c(7, 8, 9))))], 
                         na.rm = TRUE),
    POP_POCETLET = max(POP_POCET[(ROK == current_year & 
                                MESIC >= 5 & 
                                MESIC <= 9)], 
                       na.rm = TRUE), 
    POP_POCETLET1 = max(POP_POCET[(ROK == current_year - 1 & 
                                 MESIC >= 5 & 
                                 MESIC <= 9)], 
                        na.rm = TRUE), 
    POP_POCETLET2 = max(POP_POCET[(ROK == current_year - 2 & 
                                 MESIC >= 5 & 
                                 MESIC <= 9)], 
                        na.rm = TRUE), 
    POP_POCETLET3 = max(POP_POCET[(ROK == current_year - 3 & 
                                 MESIC >= 5 & 
                                 MESIC <= 9)], 
                        na.rm = TRUE), 
    POP_POCETLETREF = mean(POP_POCETLET1, POP_POCETLET2, POP_POCETLET3, na.rm = TRUE),
    POP_VITALLET = POP_POCETLET/POP_POCETLETREF,
    POP_REPROCHI = POP_POCETLETS2/POP_POCETLETS1,
  ) %>%
  dplyr::ungroup()

# populacni trendy odvozene od posledniho pozorovani POP_POCETMAX[1]
n2k_druhy_lokpop_trend <- n2k_druhy_lokpop %>%
  dplyr::group_by(KOD_LOKAL, DRUH) %>%
  dplyr::arrange(desc(ROK)) %>%
  #dplyr::filter(CILMON == 1 & is.na(POP_POCETMAX) == FALSE & is.infinite(POP_POCETMAX) == FALSE) %>%
  dplyr::reframe(POP_POCETMAXREF = POP_POCETMAX[3],
                 POP_TREND1 = dplyr::case_when(POP_POCETMAX[1] >= POP_POCETMAXREF ~ 1,
                                               POP_POCETMAX[1] < POP_POCETMAXREF ~ 0),
                 POP_TREND2 = dplyr::case_when(POP_POCETMAX[2] >= POP_POCETMAXREF ~ 1,
                                               POP_POCETMAX[2] < POP_POCETMAXREF ~ 0),
                 POP_TREND = sum(POP_TREND1, POP_TREND2, na.rm = TRUE),
                 POP_TRENDLM = coef(lm(POP_POCETMAX ~ ROK))[2],
                 POP_POCETNOSTMAX = max(POP_POCETNOST, na.rm = TRUE),
                 POP_POCETPRUM = dplyr::case_when(POP_POCETNOSTMAX == 1 ~ n()*5,
                                                  POP_POCETNOSTMAX == 2 ~ n()*25,
                                                  POP_POCETNOSTMAX == 3 ~ n()*75,
                                                  POP_POCETNOSTMAX == 4 ~ n()*500,
                                                  POP_POCETNOSTMAX == 5 ~ n()*5000),
                 POP_POCETNMIN = dplyr::case_when(POP_POCETNOSTMAX == 1 ~ n()*1,
                                                  POP_POCETNOSTMAX == 2 ~ n()*11,
                                                  POP_POCETNOSTMAX == 3 ~ n()*51,
                                                  POP_POCETNOSTMAX == 4 ~ n()*101,
                                                  POP_POCETNOSTMAX == 5 ~ n()*1001),
                 POP_POCETNMAX = dplyr::case_when(POP_POCETNOSTMAX == 1 ~ n()*10,
                                                  POP_POCETNOSTMAX == 2 ~ n()*50,
                                                  POP_POCETNOSTMAX == 3 ~ n()*100,
                                                  POP_POCETNOSTMAX == 4 ~ n()*1000,
                                                  POP_POCETNOSTMAX == 5 ~ n()*9000)
  ) %>%
  dplyr::ungroup()
  

# join do konecne tabulky vsech indikatoru
n2k_druhy <- n2k_druhy_pre %>%
  dplyr::left_join(., n2k_druhy_lokpop, 
                   by = join_by(ROK, KOD_LOKAL, DRUH)) %>%
  dplyr::left_join(., n2k_druhy_lokpop_trend, 
                   by = join_by(KOD_LOKAL, DRUH)) %>%
  dplyr::distinct() 

rm(n2k_druhy_lokpop, n2k_druhy_lokpop_trend, n2k_druhy_del)

# HODNOCENE_OBDOBI ----
# hodnocene obdobi - ChU - druh - lokalita - pole
n2k_druhy_obdobi_lok <- n2k_druhy %>%
  dplyr::group_by(kod_chu, DRUH, KOD_LOKAL, POLE) %>%
  dplyr::reframe(
    HODNOCENE_OBDOBI_OD = min(DATUM, na.rm = TRUE),
    HODNOCENE_OBDOBI_DO = max(DATUM, na.rm = TRUE)
  )
# hodnocene obdobi - ChU - druh - pole
n2k_druhy_obdobi_pol <- n2k_druhy %>%
  dplyr::group_by(kod_chu, DRUH, POLE) %>%
  dplyr::reframe(
    HODNOCENE_OBDOBI_OD = min(DATUM, na.rm = TRUE),
    HODNOCENE_OBDOBI_DO = max(DATUM, na.rm = TRUE)
  )
# hodnocene obdobi - ChU - druh
n2k_druhy_obdobi_chu <- n2k_druhy %>%
  dplyr::group_by(kod_chu, DRUH) %>%
  dplyr::reframe(
    HODNOCENE_OBDOBI_OD = min(DATUM, na.rm = TRUE),
    HODNOCENE_OBDOBI_DO = max(DATUM, na.rm = TRUE)
  )

# POSLEDNI_NALEZ ----
# posledni nalez - ChÚ - druh - lokalita - pole
n2k_druhy_posledni_lok <- n2k_druhy %>%
  dplyr::group_by(kod_chu, DRUH, KOD_LOKAL, POLE) %>%
  dplyr::reframe(
    POSLEDNI_NALEZ = max(DATUM, na.rm = TRUE)
  )
# posledni nalez - ChÚ - druh - pole
n2k_druhy_posledni_pol <- n2k_druhy %>%
  dplyr::group_by(kod_chu, DRUH, POLE) %>%
  dplyr::reframe(
    POSLEDNI_NALEZ = max(DATUM, na.rm = TRUE)
  )
# posledni nalez - ChÚ - druh
n2k_druhy_posledni_chu <- n2k_druhy %>%
  dplyr::group_by(kod_chu, DRUH) %>%
  dplyr::reframe(
    POSLEDNI_NALEZ = max(DATUM, na.rm = TRUE)
  )

# NDOP LIMITY ----
n2k_druhy_long <- n2k_druhy %>%
  #dplyr::filter(SKUPINA %in% c(#"Motýli", "Brouci", 
                               #"Cévnaté rostliny", "Mechorosty"
                               #"Obojživelníci"
                               #)) %>%
  dplyr::mutate(across(.cols = ncol_orig:ncol(.), .fns = ~ as.character(.))) %>%
  tidyr::pivot_longer(.,
                      cols = c((ncol_orig+9):ncol(.)),
                      names_to = "ID_IND",
                      values_to = "HOD_IND") %>%
  dplyr::select(-c(ZDROJ:PRESNOST)) %>%
  dplyr::right_join(.,
                    limity %>%
                      dplyr::filter(UROVEN == "lok") %>%
                      dplyr::filter(is.na(LIM_IND) == FALSE), 
                    by = c("DRUH" = "DRUH",
                           "ID_IND" = "ID_IND")) %>%
  dplyr::mutate(IND_GRP = dplyr::case_when(TYP_IND %in% c("min", "max") ~ "minmax",
                                           TRUE ~ TYP_IND)) 

n2k_druhy_lim_pre <- n2k_druhy_long %>%
  #dplyr::filter(SKUPINA %in% c("Motýli", "Brouci", "Cévnaté rostliny", "Mechorosty")) %>%
  dplyr::mutate(STAV_IND = dplyr::case_when(TYP_IND == "min" & as.numeric(HOD_IND) < as.numeric(LIM_IND) ~ 0,
                                            TYP_IND == "min" & as.numeric(HOD_IND) >= as.numeric(LIM_IND) ~ 1,
                                            TYP_IND == "max" & as.numeric(HOD_IND) > as.numeric(LIM_IND) ~ 0,
                                            TYP_IND == "max" & as.numeric(HOD_IND) <= as.numeric(LIM_IND) ~ 1,
                                            TYP_IND == "val" & HOD_IND != LIM_IND ~ 0,
                                            TYP_IND == "val" & HOD_IND == LIM_IND ~ 1)) %>%
  dplyr::group_by(ID_ND_NALEZ, ID_IND, IND_GRP) %>%
  dplyr::mutate(STAV_IND = dplyr::case_when(IND_GRP == "minmax" & grepl("POP_", ID_IND) == TRUE ~ max(as.numeric(STAV_IND), na.rm = TRUE),
                                            IND_GRP == "minmax" & grepl("POP_", ID_IND) == FALSE ~ min(as.numeric(STAV_IND), na.rm = TRUE),
                                            IND_GRP == "val" ~ max(as.numeric(STAV_IND), na.rm = TRUE))) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(STAV_IND = dplyr::case_when(is.infinite(STAV_IND) ~ NA,
                                            TRUE ~ STAV_IND)) %>%
  dplyr::group_by(ID_ND_NALEZ, ID_IND) %>%
  dplyr::arrange(-STAV_IND) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup()

n2k_druhy_lim <- n2k_druhy_lim_pre %>%
  dplyr::group_by(ID_ND_NALEZ) %>%
  dplyr::mutate(CELKOVE_HODNOCENI = as.character(sum(STAV_IND, na.rm = TRUE))) %>%
  dplyr::select(-c(ID_IND:IND_GRP)) %>%
  tidyr::pivot_longer(.,
                      cols = ncol(.),
                      names_to = "ID_IND",
                      values_to = "HOD_IND") %>%
  dplyr::distinct() %>%
  dplyr::bind_rows(., n2k_druhy_lim_pre) %>%
  dplyr::arrange(ID_ND_NALEZ)

ncol_druhy_lim <- ncol(n2k_druhy_lim)

# LOKALITA LIMITY ----
n2k_druhy_lok_pre <- n2k_druhy_lim %>%
  #filter(DRUH %in% c("Myotis myotis", "Rhinolophus hipposideros")) %>%
  #dplyr::filter(SKUPINA %in% c("Cévnaté rostliny", "Obojživelníci")) %>%
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
  #dplyr::filter(SKUPINA %in% c("Cévnaté rostliny")) %>%
  #dplyr::filter(SKUPINA %in% c("Obojživelníci")) %>%
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
  #dplyr::select(-c(IND_SUM:IND_SUMOST)) %>%
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
  #dplyr::filter(SKUPINA == "Cévnaté rostliny") %>%
  dplyr::filter(ID_ND_AKCE %in% n2k_druhy_pole1_idakce) %>%
  dplyr::ungroup()

n2k_druhy_lok_idakce <- n2k_druhy_lok %>%
  #dplyr::filter(SKUPINA == "Cévnaté rostliny") %>%
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

# UZEMI ----
# CHU_POLE ----
n2k_druhy_chu_pole1 <- n2k_druhy_pole1eval %>%
  dplyr::group_by(kod_chu, DRUH) %>%
  #dplyr::filter(DATUM + years(6) >= current_year) %>%
  dplyr::reframe(
    ROK = toString(unique(ROK)),
    POLE = toString(unique(POLE)),
    NAZEV_LOK = toString(unique(NAZEV_LOK)),
    ID_ND_AKCE = toString(unique(ID_ND_AKCE)),
    CILMON = max(CILMON, na.rm = TRUE),
    POP_POCETPOLE1 = sum(
      ID_IND == "CELKOVE_HODNOCENI" &
        CILMON == 1, 
      na.rm = TRUE
      ),
    POP_POCETPOLE1D = sum(
      ID_IND == "CELKOVE_HODNOCENI" & 
        HOD_IND != "zhoršený" & 
        HOD_IND != "špatný" & 
        STAV_IND != "0.5" & 
        STAV_IND != "0" &
        STAV_IND != 0.5 & 
        STAV_IND != 0 &
        CILMON == 1, 
      na.rm = TRUE)
    ) %>%
  dplyr::group_by(kod_chu, DRUH) %>%
  dplyr::mutate(
    POP_PROCPOLE1D = round(POP_POCETPOLE1D/POP_POCETPOLE1*100, 3),
    # CHU_POLE_HMYZ ----
    STA_HABPOKRYVPRE = {
      x <- biotop_evd$BIOTOP_PROCENTO[biotop_evd$SITECODE == kod_chu & 
                                        biotop_evd$DRUH == DRUH]
      if (length(x) == 0) NA_real_ else unique(x)
    },
    STA_HABPOKRYV = dplyr::case_when(is.na(STA_HABPOKRYVPRE) == TRUE ~ NA,
                                     TRUE ~ STA_HABPOKRYVPRE)) %>%
  dplyr::select(-STA_HABPOKRYVPRE) %>%
  #dplyr::select(-c(ID_IND:IND_GRP))  %>%
  dplyr::mutate(across(.cols = 7:ncol(.)-2, .fns = ~ as.character(.))) %>%
  tidyr::pivot_longer(.,
                      cols = c(8:ncol(.)),
                      names_to = "ID_IND",
                      values_to = "HOD_IND") %>%
  dplyr::mutate(HOD_IND = as.character(HOD_IND)) %>%
  dplyr::distinct() %>%
  dplyr::arrange(ID_ND_AKCE)

n2k_druhy_chu_lok <- n2k_druhy_lokeval %>%
  #filter(DRUH %in% c("Myotis myotis", "Rhinolophus hipposideros")) %>%
  dplyr::group_by(kod_chu, DRUH) %>%
  dplyr::reframe(
    # CHU_SPOLECNE ----
    ROK = toString(unique(ROK)),
    POLE = toString(unique(POLE)),
    NAZEV_LOK = toString(unique(NAZEV_LOK)),
    ID_ND_AKCE = toString(unique(ID_ND_AKCE)),
    CILMON = max(CILMON, na.rm = TRUE),
    POP_PRESENCE = dplyr::case_when(ID_IND == "POP_PRESENCE" & grepl("ano", HOD_IND) == TRUE ~ "ano", 
                                    ID_IND == "POP_PRESENCE" & grepl("ne", HOD_IND) == TRUE ~ "ne", 
                                    TRUE ~ NA_character_), 
    POP_POCETMAX = sum(dplyr::case_when(ID_IND == "POP_POCETMAX" ~ as.numeric(HOD_IND), 
                                        TRUE ~ NA), 
                       na.rm = TRUE), 
    POP_POCETMAX = sum(dplyr::case_when(ID_IND == "POP_POCETMIN" ~ as.numeric(HOD_IND), 
                                        TRUE ~ NA), 
                       na.rm = TRUE), 
    POP_POCETSUM = sum(dplyr::case_when(ID_IND == "POP_POCET" & CILMON == 1 ~ as.numeric(HOD_IND), 
                                        TRUE ~ NA), 
                       na.rm = TRUE) %>%
      max(),
    POP_POCETDOB = sum(dplyr::case_when(ID_IND == "POP_POCET" & CELKOVE == 1 & CILMON == 1 ~ as.numeric(HOD_IND), 
                                        TRUE ~ 0), 
                       na.rm = TRUE) %>%
      max(),
    POP_POCETOST = sum(dplyr::case_when(ID_IND == "POP_POCET" & CELKOVE != 1 & CILMON == 1 ~ as.numeric(HOD_IND), 
                                        TRUE ~ 0), 
                       na.rm = TRUE),
    POP_PROCDOB = POP_POCETDOB/POP_POCETSUM*100,
    # CHU_LOK_HMYZ ----
    # CHU_OSTATNIBEZ ----
    # CHU_OBOJZIVELNICI ----
    # CHU_RYBY ----
    # CHU_SAVCI ----
    POP_POCETZIM = sum(case_when(ID_IND == "POP_POCET" ~ as.numeric(HOD_IND), 
                                 TRUE ~ NA), na.rm = TRUE), 
    POP_POCETZIM1 = sum(case_when(ID_IND == "POP_POCET" ~ as.numeric(HOD_IND), 
                                  TRUE ~ NA), na.rm = TRUE),
    POP_POCETZIM2 = sum(case_when(ID_IND == "POP_POCET" ~ as.numeric(HOD_IND), 
                                  TRUE ~ NA), na.rm = TRUE),
    POP_POCETZIM3 = sum(case_when(ID_IND == "POP_POCET" ~ as.numeric(HOD_IND), 
                                  TRUE ~ NA), na.rm = TRUE),
    POP_POCETZIMREF = mean(POP_POCETZIM1, POP_POCETZIM2, POP_POCETZIM3, na.rm = TRUE),
    POP_VITALZIM = ifelse(POP_POCETZIMREF == 0, NA_real_, POP_POCETZIM / POP_POCETZIMREF),
    POP_POCETLETS1 = sum(case_when(ID_IND == "POP_POCET" ~ as.numeric(HOD_IND), 
                                   TRUE ~ NA), na.rm = TRUE),
    POP_POCETLETS2 = sum(case_when(ID_IND == "POP_POCET" ~ as.numeric(HOD_IND), 
                                   TRUE ~ NA), na.rm = TRUE),
    POP_POCETLET = sum(case_when(ID_IND == "POP_POCET" ~ as.numeric(HOD_IND), 
                                 TRUE ~ NA), na.rm = TRUE), 
    POP_POCETLET1 = sum(case_when(ID_IND == "POP_POCET" ~ as.numeric(HOD_IND), 
                                  TRUE ~ NA), na.rm = TRUE), 
    POP_POCETLET2 = sum(case_when(ID_IND == "POP_POCET" ~ as.numeric(HOD_IND), 
                                  TRUE ~ NA), na.rm = TRUE), 
    POP_POCETLET3 = sum(case_when(ID_IND == "POP_POCET" ~ as.numeric(HOD_IND), 
                                  TRUE ~ NA), na.rm = TRUE), 
    POP_POCETLETREF = mean(POP_POCETLET1, POP_POCETLET2, POP_POCETLET3, na.rm = TRUE),
    POP_VITALLET = POP_POCETLET/POP_POCETLETREF,
    POP_REPROCHI = POP_POCETLETS2/POP_POCETLETS1,
    # CHU_MECHOROSTY ----
    
    # CHU_CEVNATE ----
    
    # CHU_PTACI ----
    
  )  %>%
  dplyr::mutate(across(.cols = 7:ncol(.), .fns = ~ as.character(.))) %>%
  tidyr::pivot_longer(.,
                      cols = c(8:ncol(.)),
                      names_to = "ID_IND",
                      values_to = "HOD_IND") %>%
  dplyr::mutate(HOD_IND = as.character(HOD_IND)) %>%
  dplyr::distinct() %>%
  dplyr::bind_rows(., n2k_druhy_chu_pole1) %>%
  dplyr::arrange(ID_ND_AKCE) %>%
  dplyr::right_join(.,
                    limity %>%
                      dplyr::filter(UROVEN == "chu"), 
                    by = c("DRUH" = "DRUH",
                           "ID_IND" = "ID_IND")) %>%
  dplyr::mutate(STAV_IND = dplyr::case_when(TYP_IND == "min" & as.numeric(HOD_IND) < as.numeric(LIM_IND) ~ 0,
                                            TYP_IND == "min" & as.numeric(HOD_IND) >= as.numeric(LIM_IND) ~ 1,
                                            TYP_IND == "max" & as.numeric(HOD_IND) > as.numeric(LIM_IND) ~ 0,
                                            TYP_IND == "max" & as.numeric(HOD_IND) <= as.numeric(LIM_IND) ~ 1,
                                            TYP_IND == "val" & HOD_IND != LIM_IND ~ 0,
                                            TYP_IND == "val" & HOD_IND == LIM_IND ~ 1)) %>%
  dplyr::mutate(IND_GRP = dplyr::case_when(TYP_IND %in% c("min", "max") ~ "minmax",
                                           TRUE ~ TYP_IND),
                KLIC = dplyr::case_when(is.na(HOD_IND) == TRUE ~ "ne",
                                        TRUE ~ KLIC)) 
# CHU ----
n2k_druhy_chu <- n2k_druhy_pole1eval %>%
  dplyr::group_by(kod_chu, DRUH, ID_IND) %>%
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
    STAV_IND = dplyr::case_when(IND_GRP == "minmax" & 
                                  grepl("POP_", ID_IND) == TRUE ~ max(as.numeric(STAV_IND), na.rm = TRUE),
                                IND_GRP == "minmax" & 
                                  grepl("POP_", ID_IND) == FALSE ~ min(as.numeric(STAV_IND), na.rm = TRUE),
                                IND_GRP == "val" ~ max(as.numeric(STAV_IND), na.rm = TRUE)),
    KLIC = unique(KLIC),
    UROVEN = unique(UROVEN),
    IND_GRP = unique(IND_GRP),
    CILMON = max(CILMON, na.rm = TRUE)
  )  %>%
  dplyr::distinct() %>%
  dplyr::ungroup() %>%
  dplyr::left_join(., 
                   n2k_druhy_obdobi_chu,
                   by = join_by("kod_chu", "DRUH")) %>%
  dplyr::mutate(STAV_IND = ifelse(is.infinite(STAV_IND), 0, STAV_IND)) %>%
  dplyr::group_by(kod_chu, DRUH) %>%
  dplyr::mutate(
    IND_SUM = sum(as.numeric(STAV_IND), na.rm = TRUE),
    IND_SUMKLIC = sum(as.numeric(STAV_IND[KLIC == "ano" &
                                            UROVEN == "chu" &
                                            !is.na(LIM_IND)]), 
                      na.rm = TRUE),
    IND_SUMOST = sum(as.numeric(STAV_IND[KLIC == "ne" &
                                           UROVEN == "chu" &
                                           !is.na(LIM_IND)]), 
                     na.rm = TRUE),
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
  dplyr::mutate(CELKOVE = dplyr::case_when(
    HODNOCENE_OBDOBI_DO + years(6) < current_year ~ NA_real_,
    IND_SUMKLIC < (LENIND_SUMKLIC - 1 - LENIND_NAKLIC) ~ 0,
    IND_SUMKLIC < (LENIND_SUMKLIC - LENIND_NAKLIC) ~ 0.5,
    IND_SUMKLIC >= (LENIND_SUMKLIC - LENIND_NAKLIC) ~ 1,
    TRUE ~ NA_real_
  )) %>%
  dplyr::mutate(STAV_IND = dplyr::case_when(ID_IND == "CELKOVE_HODNOCENI" ~ as.character(CELKOVE),
                                            TRUE ~ as.character(STAV_IND))) %>%
  dplyr::mutate(HOD_IND = dplyr::case_when(HOD_IND == "NaN" ~ NA_character_,
                                           is.na(HOD_IND) == TRUE ~ NA_character_,
                                           ID_IND == "CELKOVE_HODNOCENI" & is.na(STAV_IND) == TRUE ~ "neznámý",
                                           ID_IND == "CELKOVE_HODNOCENI" & STAV_IND == 0 ~ "špatný",
                                           ID_IND == "CELKOVE_HODNOCENI" & STAV_IND == 0.5 ~ "zhoršený",
                                           ID_IND == "CELKOVE_HODNOCENI" & STAV_IND == 1 ~ "dobrý",
                                           ID_IND == "CELKOVE_HODNOCENI" & STAV_IND == "0" ~ "špatný",
                                           ID_IND == "CELKOVE_HODNOCENI" & STAV_IND == "0.5" ~ "zhoršený",
                                           ID_IND == "CELKOVE_HODNOCENI" & STAV_IND == "1" ~ "dobrý",
                                           TRUE ~ HOD_IND)) %>%
  dplyr::mutate(STAV_IND = dplyr::case_when(UROVEN != "chu" ~ "nehodnocen",
                                            ID_IND != "CELKOVE_HODNOCENI" & is.na(UROVEN) == TRUE ~ "nehodnocen",
                                            ID_IND != "CELKOVE_HODNOCENI" & is.na(LIM_IND) == TRUE ~ "nehodnocen",
                                            is.na(STAV_IND) == TRUE ~ "neznámý",
                                            is.infinite(STAV_IND) == TRUE ~ "neznámý",
                                            STAV_IND == 0 ~ "špatný",
                                            STAV_IND == 0.5 ~ "zhoršený",
                                            STAV_IND == 1 ~ "dobrý",
                                            STAV_IND == "0" ~ "špatný",
                                            STAV_IND == "0.5" ~ "zhoršený",
                                            STAV_IND == "1" ~ "dobrý",
                                            TRUE ~ as.character(STAV_IND))) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  dplyr::arrange(DRUH, kod_chu, POLE) %>%
  dplyr::left_join(.,
                   evl %>%
                     sf::st_drop_geometry() %>%
                     dplyr::select(SITECODE, NAZEV),
                   by = c("kod_chu" = "SITECODE")) %>%
  dplyr::left_join(.,
                   rp_code,
                   by = join_by("kod_chu")) %>%
  dplyr::left_join(., 
                   n2k_oop,
                   by = c("kod_chu" = "SITECODE")) %>%
  dplyr::mutate(typ_predmetu_hodnoceni = "Druh",
                feature_code = NA,
                trend = "neznámý",
                datum_hodnoceni = Sys.Date()) %>%
  dplyr::filter(CILMON == 1) %>%
  dplyr::rename(nazev_chu = NAZEV, 
                druh = DRUH, #feature_code, 
                hodnocene_obdobi_od = HODNOCENE_OBDOBI_OD, 
                hodnocene_obdobi_do = HODNOCENE_OBDOBI_DO, 
                parametr_nazev = ID_IND, 
                parametr_hodnota = HOD_IND, 
                parametr_limit = LIM_IND, 
                parametr_jednotka = JEDNOTKA, 
                stav = STAV_IND) %>%
  dplyr::select(typ_predmetu_hodnoceni, kod_chu, nazev_chu, 
                druh, feature_code, 
                hodnocene_obdobi_od, hodnocene_obdobi_do, oop, parametr_nazev, 
                parametr_hodnota, parametr_limit, parametr_jednotka, 
                stav, trend, datum_hodnoceni, pracoviste, ID_ND_AKCE
  ) %>%
  dplyr::left_join(., indikatory_id, by = c("parametr_nazev" = "ind_r")) %>%
  dplyr::mutate(
    parametr_nazev = ind_id,
    pracoviste = gsub(",", "", pracoviste),
    metodika = 15087
  ) %>%
  dplyr::filter(grepl("Jihočeského", oop, ignore.case = TRUE) == TRUE
                #  (grepl("Ústeckého", oop, ignore.case = TRUE) == TRUE & druh == "Limoniscus violaceus")
                #grepl("Ústeckého", oop, ignore.case = TRUE) == TRUE
                ) %>%
  dplyr::select(-c(ind_id, ind_popis, #oop,
                   ID_ND_AKCE)) %>%
  dplyr::distinct()
  

sep <- ","
quote_env <- TRUE
encoding <- "Windows-1250"
sep <- ";"
quote_env <- FALSE
encoding <- "UTF-8"
write.table(n2k_druhy_chu,
            paste0("C:/Users/jonas.gaigr/Documents/state_results/n2k_druhy_chu",
                   "_",
                   current_year,
                   "_",
                   gsub("-", "", Sys.Date()),
                   "_",
                   encoding,
                   ".csv"),
            row.names = FALSE,
            sep = sep,
            quote = quote_env,
            fileEncoding = encoding
          )  
# KONEC SKRIPTU ----