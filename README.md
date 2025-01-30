## wtsROMS

R package with functions to handle ROMS model grids and output files in R 
(including creating base layers for [ggplot2]("https://ggplot2.tidyverse.org") or 
[tmap]("https://github.com/r-tmap/tmap") maps and 
extracting model fields as (2D) [sf](https://r-spatial.github.io/sf/), 
objects, and mapping the fields).

## Installation

wtsROMS can be installed using:

* devtools::install_github("https://github.com/wStockhausen/wtsROMS")

Other packages that need to be installed include dplyr,ggplot2, lubridate, 
magrittr, ncdf4, sf, tmap, tmaptools, and utils, which are all available from CRAN. 

Additional dependencies that you will need to install yourself are:

* wtsGIS (devtools::install_github("https://github.com/wStockhausen/wtsGIS"))
* wtsUtilities (devtools::install_github("https://github.com/wStockhausen/wtsUtilities"))

These, of course, have their own dependencies.

## ROMS grids available in package

ROMS model grids that can be extracted as `sf` dataframe objects using `getGrid(grid_name)` include:

  * "CGOA" - 3 km Coastal Gulf of Alaska grid
  * "Bering10k" - 10 km Berinmg Sea grid
  * "NEP4" - 10 km Northeast Pacific grid

****************************

## NOAA Disclaimer

This repository is a scientific product and is not an official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an 'as is' basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.

Software code created by U.S. Government employees is not subject to copyright in the United States (17 U.S.C. §105). The United States/Department of Commerce reserve all rights to seek and obtain copyright protection in countries other than the United States for Software authored in its entirety by the Department of Commerce. To this end, the Department of Commerce hereby grants to Recipient a royalty-free, nonexclusive license to use, copy, and create derivative works of the Software outside of the United States.

****************************

<img src="https://raw.githubusercontent.com/nmfs-general-modeling-tools/nmfspalette/main/man/figures/noaa-fisheries-rgb-2line-horizontal-small.png" height="75" alt="NOAA Fisheries">

[U.S. Department of Commerce](https://www.commerce.gov/) | [National Oceanographic and Atmospheric Administration](https://www.noaa.gov) | [NOAA Fisheries](https://www.fisheries.noaa.gov/)

****************************

[![pages-build-deployment](https://github.com/wStockhausen/wtsROMS/actions/workflows/pages/pages-build-deployment/badge.svg?branch=gh-pages)](https://github.com/wStockhausen/wtsROMS/actions/workflows/pages/pages-build-deployment)

