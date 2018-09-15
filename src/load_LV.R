# Define file / folder names ----------------------------------------------
LV_csv_dir <- "./data/LV/csv"
LV_bin_dir <- "./data/LV/bin"

# get the filename of the latest zip pack
datafile <- sort(list.files(LV_ZIP_DIR), decreasing = TRUE)[1]

# set the respective binary data filename
savefile <-
  paste(
    LV_bin_dir,
    str_replace(
      string = datafile,
      pattern = ".zip",
      replacement = ".RData"
    ),
    sep = "/"
  )

# check if latest binary data file exists, otherwise load and create it
if (file.exists(savefile)) {
  cat("Latest file already saved. Loading from local...\n")
  load(file = savefile)
} else {
  cat("Unzipping to temp...\n")
  # Unzip -------------------------------------------------------------------
  unzip(zipfile = paste(LV_ZIP_DIR, datafile, sep = "/"),
        exdir = LV_csv_dir)
  
  # Read in -----------------------------------------------------------------
  cat("Reading files...\n")
  LV <- list.files(path = LV_csv_dir,
                   pattern = "*.csv",
                   full.names = TRUE) %>%
    map_dfr(read_csv, col_types = strrep("c", 35)) %>%
    select(-X35)
  
  # Delete unzipped csv files -----------------------------------------------
  cat("Deleting temp files...\n")
  unlink(paste(LV_csv_dir, "*", sep = "/"))
  
  # Clean data --------------------------------------------------------------
  cat("Cleaning data file...\n")
  names(LV) <- str_replace_all(names(LV), " ", "_")
  LV <- LV %>%
    mutate_at(vars(c(contains("VALUE"), AREA)), funs(as.numeric)) %>%
    mutate_at(vars(contains("DATE")), funs(as.Date(., format = "%d/%m/%Y"))) %>%
    mutate(AREA = AREA * if_else(AREA_TYPE == "H", 10000, 1)) %>%
    select(-one_of("AREA_TYPE"))

  # Save as R object --------------------------------------------------------
  cat("Saving data file...\n")
  save(LV, file = savefile)
}


# Clean up ----------------------------------------------------------------
remove(datafile, savefile, LV_csv_dir, LV_bin_dir)
