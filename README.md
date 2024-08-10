# iaaR

## Description
*iaaR* (indoor air analysis in R) is just a grouping of different functions that generally littered my R scripts. They have been combined into a package to allow me to neated my scripts, and provide others with the utilities combined herein.

Upon installing this package and attaching, tidyverse will additionally be installed and attached (if not already). You can opt out of this by  

## Example functions
- Statistical calculations
  - Calculate Modified Z-scores with `calculate_mod_z`
  - Calculate confidence intervals with `confidence_interval`
  
- Instrumental analysis
  - Calculate instrument limits (LOD/LOQ) with `instrument_limits`

## Installation
You can install the development version of this package from GitHub using the `devtools` package:

### Development version
```
install.packages("remotes")
remotes::install_github("TomWarburtonIA/iaaR")
```
