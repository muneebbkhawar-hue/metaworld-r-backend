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
  "svglite"
)

install.packages(packages)