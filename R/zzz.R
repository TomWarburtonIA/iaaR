#' Ensure tidyverse is installed and optionally attached
.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    if (!requireNamespace("tidyverse", quietly = TRUE)) {
      message("The 'tidyverse' package is not installed.")

      # Ask user if they want to install tidyverse
      install_tidyverse <- utils::askYesNo("Do you want to install and load the 'tidyverse' package now?")

      if (install_tidyverse) {
        tryCatch({
          install.packages("tidyverse")
          if (!requireNamespace("tidyverse", quietly = TRUE)) {
            stop("The 'tidyverse' package could not be installed. Please install it manually.")
          }
          message("The 'tidyverse' package has been installed successfully.")

          # Load tidyverse
          library(tidyverse)
          message("The 'tidyverse' package has been attached successfully.")

        }, error = function(e) {
          message("Error installing or loading 'tidyverse': ", e$message)
          message("Please install 'tidyverse' manually to use this package.")
        })
      } else {
        message("The 'tidyverse' package is not installed. Some functionalities may not be available.")
      }
    } else {
      message("The 'tidyverse' package is already installed.")

      # Optionally load tidyverse if it's not already attached
      if (!("tidyverse" %in% loadedNamespaces())) {
        tryCatch({
          library(tidyverse)
          message("The 'tidyverse' package has been attached successfully.")

        }, error = function(e) {
          message("Error loading 'tidyverse': ", e$message)
          message("Ensure that 'tidyverse' is installed correctly.")
        })
      }
    }
  }
}
