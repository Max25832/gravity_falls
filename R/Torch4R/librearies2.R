library(caret)


library(PupillometryR)

library(scales)
library(cowplot)
library(leafem)
library(gsignal)
library(bfast)
library(ggh4x)
library(colorspace)
library(dplyr)
library(feasts)
library(forecast)
library(fpp3)
library(ggplot2)
library(ggpubr)
library(imputeTS)
library(isotree)
library(lubridate)
library(leaflet)
library(plotly)
library(purrr)
library(raster)
library(sf)
library(sp)
library(tidyverse)
library(terra)
library(tsibble)
library(zoo)
library(tidyselect)
library(R6)
library(progress)
library(data.table)
library(abind)  # für Array-Stacking
library(ISOweek)
library(strucchange)


library(coro)
library(torch)
library(glue)




################################################################################
#                             Enviroment variables
################################################################################

HAPPY_COLORS_PALETTE <- c("#01BEFE", "#FF006D", "#FFDD00", "#FF7D00", "#ADFF02", "#8F00FF")

options(ggplot2.discrete.colour = HAPPY_COLORS_PALETTE)
options(ggplot2.discrete.fill = HAPPY_COLORS_PALETTE)

filter<-dplyr::filter
select<-dplyr::select




my_seed<-101
set.seed(my_seed)




