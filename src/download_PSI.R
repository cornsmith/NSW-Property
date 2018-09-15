# check local folder and download property valuation zip files that are new

# Property Valuation ------------------------------------------------------
PSI_zip_fn <- list.files(PSI_ZIP_DIR, recursive = TRUE)

cat("Getting list of data files from website...\n")
PSI_filelinks <- "https://valuation.property.nsw.gov.au/embed/propertySalesInformation" %>% 
  read_html() %>% 
  html_nodes(".btn-sales-data") %>% 
  html_attr("href")

PSI_filelinks_to_dl <- PSI_filelinks[!str_replace(PSI_filelinks, "(.*)__psi/", "") %in% PSI_zip_fn]

cat("Downloading files...\n")
for(filelink in PSI_filelinks_to_dl){
  destfile <- str_replace(filelink, "(.*)__psi/", "")
  download.file(url = filelink,
                destfile = paste(PSI_ZIP_DIR, destfile, sep = "/"))
}
cat("Download complete\n")


# Clean Up ----------------------------------------------------------------
rm(PSI_filelinks, PSI_filelinks_to_dl, PSI_zip_fn, filelink)
