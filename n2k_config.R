# LOAD PACKAGES ----
if(!isTRUE(require(tidyverse, quietly = TRUE))) {
  install.packages("tidyverse", dependencies = TRUE); library(tidyverse)
} else {
  require(tidyverse)}

if(!isTRUE(require(sf, quietly = TRUE))) {
  install.packages("sf", dependencies = TRUE); library(sf)
} else {
  require(sf)}

if(!isTRUE(require(sp, quietly = TRUE))) {
  install.packages("sp", dependencies = TRUE); library(sp)
} else {
  require(sp)}

if(!isTRUE(require(proj4, quietly = TRUE))) {
  install.packages("proj4", dependencies = TRUE); library(proj4)
} else {
  require(proj4)}

if(!isTRUE(require(leaflet, quietly = TRUE))) {
  install.packages("leaflet", dependencies = TRUE); library(leaflet)
} else {
  require(leaflet)}

if(!isTRUE(require(openxlsx, quietly = TRUE))) {
  install.packages("openxlsx", dependencies = TRUE); library(openxlsx)
} else {
  require(openxlsx)}

if(!isTRUE(require(remotes, quietly = TRUE))) {
  install.packages("remotes", dependencies = TRUE); library(remotes)
} else {
  require(remotes)}

if(!isTRUE(require(rn2kcz, quietly = TRUE))) {
  remotes::install_github("jonasgaigr/rn2kcz", force = TRUE); library(rn2kcz)
} else {
  require(rn2kcz)}


# ZDROJ CÍLENÉHO MONITORINGU ----
CIS_CILMON <- read.csv("cil_mon_zdroj.csv", fileEncoding = "Windows-1250")
# AKTUÁLNÍ ROK ----
current_year <- 2024

# LIMITY ----
limity_cev <- read.csv(
  "https://raw.githubusercontent.com/jonasgaigr/host_naturecz/main/limity_cevky.csv", 
  fileEncoding = "Windows-1250")
limity <- read.csv(
  "https://raw.githubusercontent.com/jonasgaigr/host_naturecz/main/limity_vse.csv", 
  fileEncoding = "Windows-1250"
  ) %>%
  dplyr::bind_rows(
    ., 
    limity_cev
    ) %>%
  dplyr::group_by(
    DRUH, 
    ID_IND, 
    TYP_IND, 
    UROVEN
    ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    LIM_INDLIST = dplyr::case_when(
      TYP_IND == "max" ~ paste(
        "nejvýš", 
        LIM_IND, 
        JEDNOTKA
        ),
      TYP_IND == "min" ~ paste(
        "alespoň", 
        LIM_IND, 
        JEDNOTKA
        ),
      TYP_IND == "val" ~ paste(
        paste0(
          unique(LIM_IND), 
          collapse = ", "
          )
        ),
      TRUE ~ NA_character_)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(
    DRUH, 
    ID_IND
    ) %>%
  dplyr::mutate(
    LIM_INDLIST = toString(
      na.omit(
        unique(LIM_INDLIST)
        )
      )
    ) %>%
  dplyr::ungroup()

indikatory_id <- read.csv(
  "https://raw.githubusercontent.com/jonasgaigr/host_naturecz/main/cis_indikatory_popis.csv", 
  fileEncoding = "Windows-1250")

cis_evd_perioda <- read.csv("https://raw.githubusercontent.com/jonasgaigr/host_naturecz/main/cis_evd_perioda.csv", 
                            fileEncoding = "Windows-1250"
                            ) %>%
  dplyr::select(
    TAXON, 
    PERIODA
    )

# LOKALITY ----
rn2kcz::load_n2k_sites() 

n2k_oop <- readr::read_csv2(
  "https://raw.githubusercontent.com/jonasgaigr/host_naturecz/main/n2k_oop_25.csv", 
  locale = readr::locale(encoding = "Windows-1250")
  ) %>%
  mutate(oop = gsub(";", ",", oop)) %>%
  dplyr::rename(SITECODE = sitecode) %>%
  dplyr::select(SITECODE, oop)

endpoint <- "http://gis.nature.cz/arcgis/services/Aplikace/Opendata/MapServer/WFSServer?"
caps_url <- base::paste0(endpoint, "request=GetCapabilities&service=WFS")

layer_name_evl <- "Opendata:Evropsky_vyznamne_lokality"
layer_name_po <- "Opendata:Ptaci_oblasti"
layer_name_biotopzvld <- "Opendata:Biotop_zvlaste_chranenych_druhu_velkych_savcu"
getfeature_url_evl <- paste0(
  endpoint,
  "service=WFS&version=2.0.0&request=GetFeature&typeName=", layer_name_evl
)
getfeature_url_po <- paste0(
  endpoint,
  "service=WFS&version=2.0.0&request=GetFeature&typeName=", layer_name_po
)
getfeature_url_biotopzvld <- paste0(
  endpoint,
  "service=WFS&version=2.0.0&request=GetFeature&typeName=", layer_name_biotopzvld
)

evl <- sf::st_read(getfeature_url_evl) %>%
  sf::st_transform(., st_crs("+init=epsg:5514")) %>%
  dplyr::left_join(., n2k_oop, by = "SITECODE") 
po <- sf::st_read(getfeature_url_po) %>%
  sf::st_transform(., st_crs("+init=epsg:5514")) %>%
  dplyr::left_join(., n2k_oop, by = "SITECODE") 
biotop_zvld <- sf::st_read(getfeature_url_biotopzvld) %>%
  sf::st_transform(., st_crs("+init=epsg:5514"))

n2k_union <- sf::st_join(
  evl, 
  po
  )

rp_code <- readr::read_csv2(
  "https://raw.githubusercontent.com/jonasgaigr/host_naturecz/main/n2k_rp_25.csv", 
  locale = readr::locale(encoding = "Windows-1250")
  ) %>%
  dplyr::rename(
    kod_chu = sitecode
    ) %>%
  dplyr::select(
    kod_chu, 
    pracoviste) %>%
  dplyr::mutate(
    pracoviste = gsub(",", 
                      "", 
                      pracoviste
                      )
    )

# BIOTOP EVD HMYZ ----
biotop_evd <- readr::read_csv(
  "https://raw.githubusercontent.com/jonasgaigr/host_naturecz/main/biotopy_evd_hmyz.csv"
  )

# NDOP EXPORT ----
n2k_export <- readr::read_csv2(
  "C:/Users/jonas.gaigr/Documents/host_data/evl_data_export_20250408.csv", 
  locale = readr::locale(encoding = "Windows-1250")
  )
#volna_export <- readr::read_csv("volna_data_export_20250317.csv", locale = readr::locale(encoding = "Windows-1250"))
#n2k_export <- bind_rows(n2k_export, volna_export)

ncol_orig <- ncol(n2k_export)

