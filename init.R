my_packages <- c("tidyverse", "forecast", "rlang", "readr", "plotly", "glue", "shiny")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))
