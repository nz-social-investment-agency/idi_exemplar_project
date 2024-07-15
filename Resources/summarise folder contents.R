################################################################################
#' Summarise folder contents and report
#' 2024-07-15
#' Simon Anastasiadis
#' 
#' Instructions:
#' 1) Enter your settings in the user input section
#' 2) Source the file (Ctrl + Shift + S)
#' 3) Review the results printed to R
#' 4) If save = true, for more detail, check the folder for the data
#' 
#' Warning - slow runtime for large folders
################################################################################

## user input ------------------------------------------------------------- ----

# The path and name of the folder you want examined
FOLDER_TO_EXAMINE = "~/Network-Shares/DataLabNas/MAA/MAA20XX-YY"
# easiest option:
# - Go to the Files pane (Ctrl + 5)
# - Navigate to desired folder
# - Click 'More' then 'Set as Working Directory'
# - Copy the path from the console and paste above

# The number of file and folder names to display in results
RESULTS_SIZE = 6
# for example:
# when set to 6, the six largest files will be reported

# Should R save data on every file to CSV
SAVE_ALL_FILE_DATA = FALSE
# output file name = "file descriptions YYYY-MM-DD HH-MM.csv"

## setup ------------------------------------------------------------------ ----

setwd(FOLDER_TO_EXAMINE)
library(dplyr)

## gather info ------------------------------------------------------------ ----

print("Collecting info about files, please wait")
all_files = dir(path = ".", recursive = TRUE, full.names = TRUE)
all_files_info = file.info(all_files)

## tidy info -------------------------------------------------------------- ----

all_files_info$path = rownames(all_files_info)
all_files_info$folder = dirname(all_files_info$path)
all_files_info$file = basename(all_files_info$path)

keep_columns = c("path", "folder", "file", "size", "mtime", "atime")
all_files_info = select(all_files_info, all_of(keep_columns))
rownames(all_files_info) = NULL

all_files_info = mutate(all_files_info, size = round(size / 1024 / 1024, 1))
all_files_info = rename(all_files_info, size_MB = size, last_modified = mtime, last_accessed = atime)

## largest files ---------------------------------------------------------- ----

if(RESULTS_SIZE > 0){
  largest_files = all_files_info %>%
    arrange(-size_MB) %>%
    select(all_of(c("folder", "file", "size_MB"))) %>%
    head(RESULTS_SIZE)
  
  print("----------------------------------------")
  print("Largest files")
  print(as.data.frame(largest_files))
}

## largest folders -------------------------------------------------------- ----

if(RESULTS_SIZE > 0){
  largest_folders = all_files_info %>%
  group_by(folder) %>%
  summarise(size_contents_MB = sum(size_MB)) %>%
  arrange(-size_contents_MB) %>%
  head(RESULTS_SIZE)
  
  print("----------------------------------------")
  print("Largest folders")
  print(as.data.frame(largest_folders))
}

## most files ------------------------------------------------------------- ----

if(RESULTS_SIZE > 0){
  fullest_folders = all_files_info %>%
  group_by(folder) %>%
  summarise(number_files = n()) %>%
  arrange(-number_files) %>%
  head(RESULTS_SIZE)
  
  print("----------------------------------------")
  print("Fullest folders")
  print(as.data.frame(fullest_folders))
}

## save output ------------------------------------------------------------ ----

if(SAVE_ALL_FILE_DATA){
  
  file_time = Sys.time() %>%
    as.character() %>%
    substr(1,16) %>%
    gsub(pattern = ":", replacement = "-")
  
  file_name = paste0("file descriptions ", file_time, ".csv")
  
  write.csv(all_files_info, file_name, row.names = FALSE)
  
  print(paste("Saved file", file_name, "to folder", basename(FOLDER_TO_EXAMINE)))
}

## end -------------------------------------------------------------------- ----

print("Complete!")
