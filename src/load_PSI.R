# Parameters --------------------------------------------------------------
PSI_dat_dir <- "./data/PSI/dat"
PSI_temp_dir <- "./data/PSI/temp"
PSI_bin_dir <- "./data/PSI/bin"


# Functions ---------------------------------------------------------------
PSI_unzip <- function(datafile){
  cat("Unzipping data zip file...\n")
  # Unzip data file
  unzip(zipfile = paste(PSI_ZIP_DIR, datafile, sep = "/"), 
        junkpaths = TRUE,
        exdir = PSI_temp_dir)
  
  cat("Unzipping next level of zip files in data zip file...\n")
  # next level of zips
  for(zipfile in list.files(PSI_temp_dir, pattern = "\\.zip$", recursive = TRUE)){
    unzip(zipfile = paste(PSI_temp_dir, zipfile, sep = "/"), 
          junkpaths = TRUE,
          exdir = PSI_temp_dir)
  }
  
  # Copy from temp to dat folder
  dat_fn <- list.files(path = PSI_temp_dir, pattern = "\\.DAT$", ignore.case = TRUE)
  
  if(length(dat_fn) > 0){
    file.rename(
      from = paste(PSI_temp_dir, dat_fn, sep = "/"), 
      to = paste(PSI_dat_dir, dat_fn, sep = "/")
    )
  }
  
  # Clear out temp folder of remaining stuff
  unlink(paste(PSI_temp_dir, "*", sep = "/"))
}


PSI_extract_data_from_file <- function(filename){
  # read the complete file
  PSI <- read_lines(filename) 
  
  # remove trailing delimiter
  PSI <- substr(PSI, 1, nchar(PSI) - 1) 
  
  # get year based on last 4 characters of filename (based on 3 character extension)
  file_year <- as.integer(substr(filename, nchar(filename) - 7, nchar(filename) - 4))
  
  # type vector
  PSI_type <- str_replace(PSI, ";(.*)", "")
  
  if(file_year >= 2001){
    
    # split based on types
    # df_A <- data.frame(str_split(string = PSI[PSI_type == "A"], pattern = ";", simplify = TRUE), stringsAsFactors = FALSE)
    df_B <- data.frame(str_split(string = PSI[PSI_type == "B"], pattern = ";", simplify = TRUE), stringsAsFactors = FALSE)
    df_C <- data.frame(str_split(string = PSI[PSI_type == "C"], pattern = ";", simplify = TRUE), stringsAsFactors = FALSE)
    # df_D <- data.frame(str_split(string = PSI[PSI_type == "D"], pattern = ";", simplify = TRUE), stringsAsFactors = FALSE)
    df_Z <- data.frame(str_split(string = PSI[PSI_type == "Z"], pattern = ";", simplify = TRUE), stringsAsFactors = FALSE)
    
    # process Z rows
    colnames(df_Z) <- c(
      "Record_Type", 
      "Total_Records", 
      "Total_B_Records", 
      "Total_C_Records", 
      "Total_D_Records"
    )
    df_Z <- df_Z %>% 
      mutate(
        Total_Records = as.integer(Total_Records),
        Total_B_Records = as.integer(Total_B_Records),
        Total_C_Records = as.integer(Total_C_Records),
        Total_D_Records = as.integer(Total_D_Records),
        Filename = filename
      ) 
    
    # process B rows
    colnames(df_B) <- c(
      "Record_Type",
      "District_Code",
      "Property_Id",
      "Sale_Counter",
      "Download_Datetime",
      "Property_Name",
      "Unit_Number",
      "House_Number",
      "Street_Name",
      "Suburb_Name",
      "Post_Code",
      "Area",
      "Area_Type",
      "Contract_Date",
      "Settlement_Date",
      "Purchase_Price",
      "Zone_Code",
      "Nature_of_Property",
      "Primary_Purpose",
      "Strata_Lot_Number",
      "Component_Code",
      "Sale_Code",
      "Percent_Interest_of_Sale",
      "Dealing_Number"
    )
    df_B <- df_B %>% 
      mutate(
        Download_Datetime = lubridate::ymd_hm(Download_Datetime),
        Area = as.numeric(Area) * if_else(Area_Type == "H", 10000, 1),
        Contract_Date = lubridate::ymd(Contract_Date),
        Settlement_Date = lubridate::ymd(Settlement_Date),
        Purchase_Price = as.numeric(Purchase_Price),
        Nature_of_Property = recode(Nature_of_Property,
                                    "V" = "Vacant", 
                                    "R" = "Residence", 
                                    "3" = "Other"
        ),
        Percent_Interest_of_Sale = coalesce(as.numeric(Percent_Interest_of_Sale), 100)
      ) %>% 
      select(-one_of("Area_Type"))
    
    # process C rows
    colnames(df_C) <- c(
      "Record_Type", 
      "District_Code", 
      "Property_Id", 
      "Sale_Counter", 
      "Download_Datetime",
      "Property_Legal_Description"
    )
    df_C <- df_C %>% 
      # mutate(Download_Datetime = lubridate::ymd_hm(Download_Datetime)) %>% 
      group_by(District_Code, Property_Id, Sale_Counter) %>% 
      summarise(
        Property_Legal_Description = paste(Property_Legal_Description, collapse = " ")
      )
    
    # combine B & C
    df_B <- df_B %>% 
      inner_join(df_C, by = c("District_Code", "Property_Id", "Sale_Counter")) %>% 
      mutate(
        Filename = filename
      )
    
  } else {
    # split based on types
    # df_A <- data.frame(str_split(string = PSI[PSI_type == "A"], pattern = ";", simplify = TRUE), stringsAsFactors = FALSE)
    df_B <- data.frame(str_split(string = PSI[PSI_type == "B"], pattern = ";", simplify = TRUE), stringsAsFactors = FALSE)
    df_Z <- data.frame(str_split(string = PSI[PSI_type == "Z"], pattern = ";", simplify = TRUE), stringsAsFactors = FALSE)
    
    # process Z rows
    df_Z <- df_Z[ , 1:3]
    colnames(df_Z) <- c(
      "Record_Type", 
      "Total_Records", 
      "Total_B_Records"
    )
    df_Z <- df_Z %>% 
      mutate(
        Total_Records = as.integer(Total_Records),
        Total_B_Records = as.integer(Total_B_Records),
        Filename = filename
      )
    
    # process B rows
    colnames(df_B) <- c(
      "Record_Type",
      "District_Code",
      "Source",
      "Valuation_num",
      "Property_Id",
      "Unit_Number",
      "House_Number",
      "Street_Name",
      "Suburb_Name",
      "Post_Code",
      "Contract_Date",
      "Purchase_Price",
      "Property_Legal_Description",
      "Area",
      "Area_Type",
      "Dimensions",
      "Component_Code",
      "Zone_Code",
      "Vendor_Name",
      "Purchaser_Name",
      "Extra_Column_1"
    )
    df_B <- df_B %>% 
      mutate(
        Area = as.numeric(Area) * if_else(Area_Type == "H", 10000, 1),
        Contract_Date = lubridate::dmy(Contract_Date),
        Purchase_Price = as.numeric(Purchase_Price),
        Filename = filename
      ) %>% 
      select(-one_of("Area_Type", "Source", "Vendor_Name", "Purchaser_Name","Extra_Column_1"))
  }
  
  return(
    list(
      B = df_B,
      Z = df_Z
    )
  )
}


# Process -----------------------------------------------------------------
tryCatch(
  load(file = paste(PSI_bin_dir, "PSI.RData", sep = "/")),
  error = function(error_message) message("No saved data, initialising..."),
  warning = function(warning_message) message("No saved data, initialising...")
)

# initialise empty frames for initial run
if(!exists("PSI_B")) PSI_B <- data.frame()
if(!exists("PSI_Z")) PSI_Z <- data.frame()
if(!exists("PSI_processed")) PSI_processed <- character(0)

datafiles <- list.files(PSI_ZIP_DIR, recursive = TRUE)
datafiles <- setdiff(datafiles, PSI_processed)
# TODO: deal with 2001
datafiles <- setdiff(datafiles, "yearly/2001.zip")

for(datafile in datafiles){
  # clear the raw file directory and unzip to it
  unlink(paste(PSI_dat_dir, "*", sep = "/"))
  PSI_unzip(datafile)
  
  # loop through and process each file and save to a single list of data frames
  dat_fn <- list.files(PSI_dat_dir, full.names = TRUE)
  dat_lists <- list(length(dat_fn))
  for(fn in seq_along(dat_fn)){
    print(paste(datafile, dat_fn[fn], fn, length(dat_fn)))
    dat_lists[[fn]] <- PSI_extract_data_from_file(dat_fn[fn])
  }
  
  # append and save to existing data
  PSI_B <- bind_rows(lapply(dat_lists, function(x) x[["B"]]), PSI_B)
  PSI_Z <- bind_rows(lapply(dat_lists, function(x) x[["Z"]]), PSI_Z)
  PSI_processed <- c(PSI_processed, datafile)
  
  save(PSI_B, PSI_Z, PSI_processed, file = paste(PSI_bin_dir, "PSI.RData", sep = "/"))
  
  # clean up
  unlink(paste(PSI_dat_dir, "*", sep = "/"))
  remove(dat_lists, dat_fn, fn)
  gc()
}


# Dedupe ------------------------------------------------------------------
PSI <- PSI_B %>% 
  filter(!(duplicated(Dealing_Number) & !is.na(Dealing_Number)))

#TODO
# merge reference zone codes 2011 vs pre 2011
# Convert typos in Date fields to probable values

# Clean up ----------------------------------------------------------------
remove(PSI_dat_dir, PSI_temp_dir, PSI_bin_dir, 
       PSI_extract_data_from_file, PSI_unzip,
       datafiles, datafile, PSI_processed)
