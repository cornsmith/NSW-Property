# check local folder and download land value summary zip files

# Land Values -------------------------------------------------------------
LV_base_url <- "http://www.valuergeneral.nsw.gov.au/land_value_summaries"

LV_zip_fn <- list.files(LV_ZIP_DIR)

cat("Getting list of data files from website...\n")
LV_filelinks <- LV_base_url %>% 
  paste("lv.php", sep = "/") %>% 
  read_html() %>% 
  html_nodes(".btn-lv-data") %>% 
  html_attr("href")

LV_filelinks_to_dl <- LV_filelinks[!str_replace(LV_filelinks, "(.*)/", "") %in% LV_zip_fn]

cat("Downloading files...\n")
for(filelink in LV_filelinks_to_dl){
  destfile <- str_replace(filelink, "(.*)/", "")
  download.file(url = paste(LV_base_url, filelink, sep = "/"),
                destfile = paste(LV_ZIP_DIR, destfile, sep = "/"))
}
cat("Download complete\n")


# Clean up ----------------------------------------------------------------
rm(LV_base_url, LV_filelinks, LV_filelinks_to_dl, LV_zip_fn, filelink)
