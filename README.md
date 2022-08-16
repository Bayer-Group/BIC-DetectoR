
# DetectoR

## R Shiny app for Clinical Trial Safety Data

<div align="center">

<img src="inst/app/www/AppIcon_BAG_DetectoR_210x210mm_RGB.png" width="20%" height="20%" alt="DetectoR" />

</div>

# DetectoR

The DetectoR R Shiny app provides a handy platform allowing for early
identification of signals and an ongoing monitoring of safety along the
medical product development phase and lifecycle.

## Installation

-   assumes R is installed (install the remotes and usethis R-packages)
-   assumed git is installed
-   add CWL=<your_cwl> and CWL\_password=<your_cwl_password> to
    .Renviron file using usethis::edit\_r\_environ()

``` r
remotes::install_github("Bayer-Group/BIC-DetectoR")
```

## Launch Rshiny app

``` r
library(DetectoR)
DetectoR::start_app()
```
