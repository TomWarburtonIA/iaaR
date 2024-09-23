# iaaR

## Description
*iaaR* (indoor air analysis in R) is just a grouping of different functions that generally littered my R scripts. They have been combined into a package to allow me to help neaten scripts and provide others with the utilities combined herein.

Upon installing this package and attaching, tidyverse will additionally be installed and attached (if not already). This package can be used without tidyverse, but certain functions like `confidence_interval` work efficiently using pipelines within `dplyr`, and so to allow for this flexibility tidyverse is installed and attached as standard. This can be opted out of when attaching *iaaR*.

## Example functions
- Statistical calculations
  - Calculate Modified Z-scores with `calculate_mod_z`
  - Calculate confidence intervals with `confidence_interval`
  
- Sample analysis
  - Calculate instrument limits (LOD/LOQ) with `instrument_limits`
  - Detect noise regions with `detect_noise` (intended for use on chromatographic data to remove peaks, but can also be flipped to isolate peaks)

## Installation
You can install the development version of this package from GitHub using the `devtools` package:

### Development version
```
install.packages("remotes")
remotes::install_github("TomWarburtonIA/iaaR")
```
