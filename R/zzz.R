#' Ensure tidyverse is installed and attached
.onAttach <- function(libname, pkgname) {
  if (!requireNamespace("tidyverse", quietly = TRUE)) {
    message("The 'tidyverse' packages are required but not installed.")

    # Try to install the package
    tryCatch({
      install.packages("tidyverse")
      if (!requireNamespace("tidyverse", quietly = TRUE)) {
        stop("The 'tidyverse' packages could not be installed. Please install it manually.")
      }
      message("The 'tidyverse' package has been installed successfully.")
    }, error = function(e) {
      message("Error installing 'tidyverse': ", e$message)
      message("Please install 'tidyverse' manually to use this package.")
      return()  # Exit if installation fails
    })
  } else {
    message("The 'tidyverse' package is already installed.")
  }

  # Attach tidyverse
  tryCatch({
    library(tidyverse)
    message("The 'tidyverse' package has been attached successfully.")
  }, error = function(e) {
    message("Error loading 'tidyverse': ", e$message)
    message("Ensure that 'tidyverse' is installed correctly.")
  })
}
