# FlowlineFinder <img src=".github/img/ff_logo.png" width=40 align="left" />

[!travis](https://travis-ci.org/mikejohnson51/FlowlineFinder.svg?branch=master)](https://travis-ci.org/mikejohnson51/FlowlineFinder.svg?branch=master)

## Description

This project is a continuation of the 'FlowlineFinder' demonstrated at the CUASHI 2017 [HydroInformatics Conference](https://www.cuahsi.org/uploads/pages/img/2017_Hydroinformatics_Program_-_Online_Version.pdf) in Tuscaloosa, AL in collaboration with [Pat Johnson](http://pjohns.github.io/pjohns). Its intention is to help users find, view and download the National Hydrograph Dataset (NHD) and National Water Model Data from either a desktop or mobile application.

Of particular interest is the ability to find nearby NHD reaches in the field.

Much of the data processing and gathering is driven by the [HydroData](http://mikejohnson51.github.io/HydroData/) and NWM packages with a interface driven by R Shiny.

## Local Usage

**Installation**

```R
library(devtools)
install_github("mikejohnson51/FlowlineFinder")
```

**Update National Water Model Forecasting Data**

```R
library(rmarkdown)
rmarkdown::render("nomads_download.Rmd")
```

**Run Application**

```R
FlowlineFinder::run_app()
```

## Demo

![](.github/img/flowline_finder.gif)
