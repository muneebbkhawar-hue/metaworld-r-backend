options(repos = c(CRAN = "https://cloud.r-project.org"))

packages <- c(
  "plumber",
  "jsonlite",
  "meta",
  "metafor",
  "netmeta",     # nma-api.R
  "RTSA",        # tsa-api.R
  "ggrepel",     # tsa-api.R
  "scales",      # tsa-api.R, nma-api.R
  "robvis",
  "ggplot2",
  "base64enc",
  "svglite",
  "IPDfromKM",   # km-digitizer-api.R - Guyot et al. 2012 KM reconstruction
  "survival",    # km-digitizer-api.R - survfit()/Surv() for the validation summary
  "mada"         # dta-api.R - bivariate/SROC diagnostic test accuracy meta-analysis (Reitsma et al. 2005)
)

install.packages(packages)