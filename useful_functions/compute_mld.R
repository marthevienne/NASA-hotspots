compute_mld <- function(ctd, eos) {
  require(oce)
  require(dplyr)
  
  ## Compute pressure and potential density anomaly
  ctd <- ctd %>%
    mutate(p = swPressure(depth, lat, eos = getOption("oceEOS", default = eos)),
           sigmaTheta = swSigmaTheta(salinity = SP, 
                                     temperature = t, 
                                     pressure = p, 
                                     longitude = lon, 
                                     latitude = lat, 
                                     eos = getOption("oceEOS", default = eos)))
  
  ## Calculate dSigmaT = sigma_prof - sigma_0
  ctd_dSigmaT <- ctd %>%
    group_by(id) %>%
    mutate(dSigmaT = sigmaTheta - nth(sigmaTheta, 1)) 
  
  # p <- ggplot() +
  #   geom_path(data = ctd_dSigmaT, aes(x = dSigmaT, y = -depth)) +
  #   geom_vline(xintercept = 0.03)
  # 
  # print(p)
  
  ## Spot MLD => first depth where dSigmaT >= 0.03 kg.m-3
  ctd_MLD <- ctd_dSigmaT %>%
    filter(dSigmaT >= 0.03) %>%
    group_by(id) %>%
    arrange(depth, .by_group = T) %>%
    filter(row_number() == 1) %>%
    dplyr::select(c(id, MLD = depth))
  
  return(ctd_MLD)
}

## Test
# df_test <- df[1:1000,]
# toto = compute_mld(df_test, "gsw")
# ctd = df_test
