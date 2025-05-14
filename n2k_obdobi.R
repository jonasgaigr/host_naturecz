# HODNOCENE_OBDOBI ----
# hodnocene obdobi - ChU - druh - lokalita - pole ----
n2k_druhy_obdobi_lok <- n2k_druhy %>%
  dplyr::group_by(kod_chu, DRUH, KOD_LOKAL, POLE) %>%
  dplyr::reframe(
    HODNOCENE_OBDOBI_OD = min(DATUM, na.rm = TRUE),
    HODNOCENE_OBDOBI_DO = max(DATUM, na.rm = TRUE)
  )
# hodnocene obdobi - ChU - druh - pole ----
n2k_druhy_obdobi_pol <- n2k_druhy %>%
  dplyr::group_by(kod_chu, DRUH, POLE) %>%
  dplyr::reframe(
    HODNOCENE_OBDOBI_OD = min(DATUM, na.rm = TRUE),
    HODNOCENE_OBDOBI_DO = max(DATUM, na.rm = TRUE)
  )
# hodnocene obdobi - ChU - druh ----
n2k_druhy_obdobi_chu <- n2k_druhy %>%
  dplyr::group_by(kod_chu, DRUH) %>%
  dplyr::reframe(
    HODNOCENE_OBDOBI_OD = min(DATUM, na.rm = TRUE),
    HODNOCENE_OBDOBI_DO = max(DATUM, na.rm = TRUE)
  )
