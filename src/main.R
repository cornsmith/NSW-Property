# Packages ----------------------------------------------------------------
require(tidyverse)
require(rvest)

LV_ZIP_DIR <- "./data/LV/zip"
PSI_ZIP_DIR <- "./data/PSI/zip"


# Process -----------------------------------------------------------------
source("./src/download_LV.R")
source("./src/download_PSI.R")

if(!exists("LV")){
  source("./src/load_LV.R")
}

if(!exists("PSI")){
  source("./src/load_PSI.R")
} 
