# ICOS-convention Smartflux files are regular zipped csv files with no metadata
# We need to unzip these for Eddypro processing
# Here, we identify new files which need unzipping
# and unzip to raw_files directory
# Peter Levy
# 16/08/2019

rm(list=ls())
library(tools)
library(stringr)
# Define 'not in' operator
"%ni%" <- Negate("%in%")

# CONSTANTS
path_zip <- "/gws/nopw/j04/eddystore/stations/Auch_icos/zip_files" 
path_raw  <- "/gws/nopw/j04/eddystore/stations/Auch_icos/raw_files"
pattern_zip <- "L01_F01.zip" # distinguish the zipped flux files, not all .zip files
pattern_raw <- ".dat"

# get the vector of filenames for zipped and raw files
v_files_raw <- list.files(path_raw, pattern = pattern_raw, full.names = FALSE)
v_files_zip <- list.files(path_zip, pattern = pattern_zip, full.names = FALSE)

# find zip files without a corresponding unzipped raw file
v_needUnzipped <- file_path_sans_ext(v_files_zip) %ni% file_path_sans_ext(v_files_raw)                         
sum(v_needUnzipped) # how many are there?

if (any(v_needUnzipped)){  # only bother unzipping if any need it
  v_files_zip <- v_files_zip[v_needUnzipped]
  v_files_zip <- paste(path_zip, v_files_zip, sep = "/")
  # unzip
  lapply(v_files_zip, unzip, exdir = path_raw, overwrite = FALSE)
  # rename .csv to .dat for Eddypro
  cmd <- paste0("mv ", path_raw, "/", file_path_sans_ext(basename(v_files_zip)), ".csv ",
                       path_raw, "/", file_path_sans_ext(basename(v_files_zip)), ".dat")
  for (i in 1:length(v_files_zip)){
    system(cmd[i], intern = TRUE)
  }
}
# maybe need to rename all .csv files to .dat to work with eddypro