# RESULTS 2024 ----
n2k_hab_klic(3130, "CZ0210027")
hu <- n2k_hab_klic(sites_habitats[1753,5], sites_habitats[1753,1])
habresults_100_110 <- base::matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble()
colnames(habresults_100_110) <- colnames(hu)
for(i in 87:95) {
  habresults_100_110 <- dplyr::bind_rows(habresults_100_110, 
                                         as.data.frame(n2k_hab_klic(sites_habitats[i,5], sites_habitats[i,1])))
}

habresults_x_1_500 <- base::matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble()
colnames(habresults_x_1_500) <- colnames(hu)
habresults_x_501_1000 <- base::matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble()
colnames(habresults_x_501_1000) <- colnames(hu)
habresults_x_1001_1500 <- base::matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble()
colnames(habresults_x_1001_1500) <- colnames(hu)
habresults_x_1501_1893 <- base::matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble()
colnames(habresults_x_1501_1893) <- colnames(hu)

rm(vmb_shp_sjtsk_akt_read, vmb_hab_dbf_23, vmb_pb_dbf_23, vmb_hab_pb_dbf_23)

for(i in 1:500) {
  habresults_x_1_500 <- dplyr::bind_rows(habresults_x_1_500, 
                                       as.data.frame(n2k_hab_klic(sites_habitats[i,5], sites_habitats[i,1])))
}
write.csv2(habresults_x_1_500, 
           "S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/habresults_x_1_500.csv", 
           row.names = FALSE)
for(i in 501:1000) {
  habresults_x_501_1000 <- dplyr::bind_rows(habresults_x_501_1000, 
                                          as.data.frame(n2k_hab_klic(sites_habitats[i,5], sites_habitats[i,1])))
}
write.csv2(habresults_x_501_1000, 
           "S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/habresults_x_501_1000.csv", 
           row.names = FALSE)
for(i in 1001:1500) {
  habresults_x_1001_1500 <- dplyr::bind_rows(habresults_x_1001_1500, 
                                           as.data.frame(n2k_hab_klic(sites_habitats[i,5], sites_habitats[i,1])))
}
write.csv2(habresults_x_1001_1500, 
           "S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/habresults_x_1001_1500.csv", 
           row.names = FALSE)
for(i in 1501:nrow(sites_habitats)) {
  habresults_x_1501_1893 <- dplyr::bind_rows(habresults_x_1501_1893, 
                                           as.data.frame(n2k_hab_klic(sites_habitats[i,5], sites_habitats[i,1])))
}
write.csv2(habresults_x_1501_1893, 
           "S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/habresults_x_1501_1893.csv", 
           row.names = FALSE)

results_habitats_x <- bind_rows(habresults_x_1_500[c(2:nrow(habresults_x_1_500)),], 
                                habresults_x_501_1000[c(2:nrow(habresults_x_501_1000)),],
                                habresults_x_1001_1500[c(2:nrow(habresults_x_1001_1500)),],
                                habresults_x_1501_1893[c(2:nrow(habresults_x_1501_1893)),])

path <- paste0("S:/Složky uživatelů/Gaigr/stanoviste/VMB0/results_habitats_24_", 
               gsub('-','',Sys.Date()), 
               ".csv")
write.csv2(results_habitats_x, 
           path, 
           row.names = FALSE,
           fileEncoding = "Windows-1250")

# RESULTS A1 ----
hu <- hvezdice_eval_a1(sites_habitats[1444,5], sites_habitats[1444,1])
habresults_100_110 <- base::matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble()
colnames(habresults_100_110) <- colnames(hu)
for(i in 1443:1445) {
  habresults_100_110 <- dplyr::bind_rows(habresults_100_110, 
                                         as.data.frame(hvezdice_eval_a1(sites_habitats[i,5], sites_habitats[i,1])))
}



habresults_1_500 <- base::matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble()
colnames(habresults_1_500) <- colnames(hu)
habresults_501_1000 <- base::matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble()
colnames(habresults_501_1000) <- colnames(hu)
habresults_1001_1500 <- base::matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble()
colnames(habresults_1001_1500) <- colnames(hu)
habresults_1501_1893 <- base::matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble()
colnames(habresults_1501_1893) <- colnames(hu)

rm(vmb_shp_sjtsk_a1_read, vmb_shp_sjtsk_a1_read, vmb_hab_dbf_23, vmb_pb_dbf_22, vmb_hab_pb_dbf_22)

for(i in 1:500) {
  habresults_1_500 <- dplyr::bind_rows(habresults_1_500, 
                                       as.data.frame(hvezdice_eval_a1(sites_habitats[i,5], sites_habitats[i,1])))
}
write.csv2(habresults_1_500, 
           "S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/habresults22_1_500.csv", 
           row.names = FALSE)
for(i in 501:1000) {
  habresults_501_1000 <- dplyr::bind_rows(habresults_501_1000, 
                                          as.data.frame(hvezdice_eval_a1(sites_habitats[i,5], sites_habitats[i,1])))
}
write.csv2(habresults_501_1000, 
           "S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/habresults22_501_1000.csv", 
           row.names = FALSE)
for(i in 1001:1500) {
  habresults_1001_1500 <- dplyr::bind_rows(habresults_1001_1500, 
                                           as.data.frame(hvezdice_eval_a1(sites_habitats[i,5], sites_habitats[i,1])))
}
write.csv2(habresults_1001_1500, 
           "S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/habresults22_1001_1500.csv", 
           row.names = FALSE)
for(i in 1501:nrow(sites_habitats)) {
  habresults_1501_1893 <- dplyr::bind_rows(habresults_1501_1893, 
                                           as.data.frame(hvezdice_eval_a1(sites_habitats[i,5], sites_habitats[i,1])))
}
write.csv2(habresults_1501_1893, 
           "S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/habresults22_1501_1893.csv", 
           row.names = FALSE)

results_habitats <- bind_rows(habresults_1_500[c(2:nrow(habresults_1_500)),], 
                              habresults_501_1000[c(2:nrow(habresults_501_1000)),],
                              habresults_1001_1500[c(2:nrow(habresults_1001_1500)),],
                              habresults_1501_1893[c(2:nrow(habresults_1501_1893)),])

path <- paste0("S:/Složky uživatelů/Gaigr/stanoviste/VMB2/results_habitats_A1_", 
               gsub('-','',Sys.Date()), 
               ".csv")
write.csv2(results_habitats, 
           path, 
           row.names = FALSE,
           fileEncoding = "Windows-1250")

