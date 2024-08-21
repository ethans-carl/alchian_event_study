# run everything

### saving formatted data to spreadsheet for 'by - hand' calculations


library(tidyverse)
library(readxl)
library(xtable)
library(lubridate)
library(readr)
library(purrr)
library(modelsummary)
library(paletteer)
library(openxlsx)


graphics.off()
rm(list = ls())


figure_path <- "figures"
table_path <- "figures"

savefile <- FALSE
# this is the theme used to produce some of the figures
source("theme_carleton_alt.R")


Alchian_Data <- read_excel("Alchian_Data_Public.xlsx", 
                           col_types = c("date", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", "numeric", "numeric"))
source("alchian_naive.R")

source("alchian_market_model.R")

source("alchian_FF.R")

source("alchian_3models_prepost.R")
