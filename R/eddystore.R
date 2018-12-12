## ----eddystore_pkg, eval=TRUE------------------------------------------------
#' Generate maps of GHG fluxes for the UK.
#'
#' eddystore allows you to produce maps of GHG fluxes for the UK
#' and write these to netCDF files.
#'
#' The only function you're likely to need from \pkg{eddystore} is
#' \code{\link{calcFlux}}. Refer to the vignettes for details
#' of how to use it - use \code{vignette()}.
#' Processing one year of data took 50 mins with 100 processors (11/10/2018)
#' Processing one year of data took 19 mins with 200 processors (12/10/2018)
#' Processing one year of data took 29 mins with 365 processors (12/10/2018)
#' Processing one year of data took ~300 mins with 1 processor
#' Speed-up = 300/20 = 15
time_taken <- c(300, 50, 19, 29)
n_cpu <- c(1, 100, 200, 365)
plot(n_cpu, time_taken)
"_PACKAGE"
#> [1] "_PACKAGE"
rm(list=ls(all=TRUE))
#install.packages("tidyr")
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(reshape2)
# library(openair)

df_proc <- read_excel("eddystore_proc_table.xlsx")
str(df_proc)

# how many intervals to split processing run into
nIntervals = 365
# station for processing run
stationID_proc <- "EasterBush"
# processing configuration for processing run
procID_proc <- "CO2_H2O"
# start/end dates for processing run
startDate_proc <- "2007-01-01 00:00"
endDate_proc   <- "2007-12-31 23:30"
startDate_proc <- as.POSIXct(strptime(startDate_proc, "%Y-%m-%d %H:%M"), tz = "UTC")
endDate_proc   <- as.POSIXct(strptime(endDate_proc,   "%Y-%m-%d %H:%M"), tz = "UTC")
startDate_proc; endDate_proc
difftime(endDate_proc, startDate_proc, units = "days")

intervals <- makeDateIntervals(startDate_proc, endDate_proc, nIntervals)
intervals$startDate
intervals$endDate
eddyproProcFileName <- "N:/0Peter/curr/ECsystem/eddypro/jasmin/eddystore/stations/test/ini/processing.eddypro"
eddyproProcFileName <- "/group_workspaces/jasmin2/eddystore/stations/EB_test/proc/processing.eddypro"
eddyproProcFileName <- "/group_workspaces/jasmin2/eddystore/stations/EasterBush/proc/processing.eddypro"
v_EddyproProcFileNames <- writeEddyproProcFilesForIntervals(eddyproProcFileName, intervals)
jobFileName <- writeJobFileForIntervals(eddyproProcFileName, nIntervals)
jobFileName
cmd <- paste0("bsub < ", jobFileName)
cmd
# submit the jobs and get the time to identify the output files from this batch
err <- system(cmd); job_time <- Sys.time()
err; job_time

## curently "EasterBush" is hard-coded in the path
get_essential_output_df <- function(job_time){
  na.strings = c("NAN", "7999", "-7999","-999","999","9999.99", "-9999.0", "-9999.0000000000000","9999","9999","-9999")
  job_time_ch <- str_split(job_time, "[-: ]")[[1]]
  YYYY <- job_time_ch[1]
  mm <- job_time_ch[2]
  dd <- job_time_ch[3]
  HH <- job_time_ch[4]
  MM <- job_time_ch[5]
  SS <- job_time_ch[6]

  # "essentials" output files created in a ten minute slot, hoping not on a 10-min boundary (i.e. 29 to 30 mins)
  to_match <- paste0("eddypro_job.*_essentials_", YYYY, "-", mm, "-", dd, "T", HH, substr(MM, 1, 1))
  list.files(path = "./stations/EasterBush/output", pattern = to_match, full.names = TRUE)
  files_out <- list.files(path = "./stations/EasterBush/output", pattern = to_match, full.names = TRUE)
  df_essn <- do.call(rbind, lapply(files_out, FUN = read.csv, na.strings = na.strings, header = TRUE, stringsAsFactors = FALSE))
  #dim(df_essn)
  #head(df_essn[,1:9])
  #str(df_essn[,1:9])
  df_essn$TIMESTAMP <- paste(df_essn$date, df_essn$time)
  df_essn <- within(df_essn, datect <- as.POSIXct(strptime(TIMESTAMP, "%Y-%m-%d %H:%M")))
  return(df_essn)
}

# "full output" files are harder to read - header is on line 2, data on line 4: use readr::read_csv instead:
read_full_output <- function(fname){
  df <- readLines(fname)
  # remove first and third lines, leaving header (line 2) and data (line 4 onwards)
  df = df[c(-1, -3)]
  df <- read.csv(textConnection(df), na.strings = na.strings, header = TRUE, stringsAsFactors = FALSE)
  return(df)
}

to_match <- paste0("eddypro_job._full_output_",  YYYY, "-", mm, "-", dd, "T", HH, substr(MM, 1, 1))
list.files(path = "./stations/EasterBush/output", pattern = to_match, full.names = TRUE)
files_out <- list.files(path = "./stations/EasterBush/output", pattern = to_match, full.names = TRUE)
df_full <- do.call(rbind, lapply(files_out, FUN = read_full_output))
dim(df_full)
head(df_full[,1:9])
str(df_full[,1:9])

df_essn <- get_essential_output_df(job_time)
dim(df_essn)
str(df_essn)

p <- ggplot(df_essn, aes(datect, H))
p  <- p + geom_line()
#p  <- p + geom_point()
p

dir(path = "./stations/EasterBush/output")
Filter(function(x) grepl("eddypro_job", x), dir(path = "./stations/EasterBush/output"))

# EB_test works ok
# EasterBush - on;y first job produced output, althopugh no errors
# guess we need to sort out time as well as date?
# using POSIXct time format


makeDateIntervals <- function(startDate_period, endDate_period, nIntervals){
  periodLength <- difftime(endDate_period, startDate_period, units = "days") + 1
  # make sure periods are at least one day long, so we dont have more periods thn days
  if (nIntervals > as.integer(periodLength)) nIntervals <- min(as.integer(intervalLength), nIntervals)

  intervalLength <- periodLength / nIntervals
  # create a sequence of dates
  startDate_interval <- seq(startDate_period, endDate_period, length = (nIntervals+1))[1:nIntervals]
  endDate_interval   <- seq(startDate_period, endDate_period, length = nIntervals+1)[2:(nIntervals+1)] - 1

  for (i in 2:nIntervals){
    startDate_interval[i] <- startDate_interval[i-1] + intervalLength
  }

  for (i in 1:(nIntervals-1)){
    endDate_interval[i]   <- startDate_interval[i+1] - 1
  }
  endDate_interval[nIntervals] <- endDate_period
  #startDate_interval; endDate_interval
  #difftime(endDate_interval, startDate_interval, units = "days")
  
  ## need to check if any interval spans two processing file periods and change accordingly
  return(list(startDate = startDate_interval, 
                endDate = endDate_interval))
}

test <- adjustIntervals(stationID_proc, procID_proc, intervals)
intervals <- test
test <- adjustIntervals(stationID_proc, procID_proc, test)
adjustIntervals <- function(stationID_proc, procID_proc, intervals){
  #i = 2
  for (i in 1:nIntervals){
    # match interval with proc file
    # subset to relevant rows of proc table
    dfs <- subset(df_proc, stationID == stationID_proc &
                           procID == procID_proc)
    # make sure sorted in correct order
    dfs <- arrange(dfs, startDate, endDate)
    # find row where for start of interval
    i_start <- findInterval(intervals$startDate[i], dfs$startDate)
    # find row where for end of interval
    # left.open arg needed where dates are equal
    i_end <- findInterval(intervals$endDate[i], dfs$endDate, left.open=TRUE) + 1
    #findInterval(intervals$endDate[i], dfs$endDate, left.open=TRUE) + 1
    # if rows are not same, interval spans two or more processing configs
    diff_proc <- i_end - i_start
    if (diff_proc == 0) print("No probs")
    if (diff_proc == 1) {
      print("Proc spanning")
      # adjust start of this interval and end of previous interval
      intervals$startDate[i] <- dfs$startDate[(i_start+1)]
      intervals$endDate[(i-1)]  <- dfs$endDate[(i_end-1)]
    }
  }
  return(intervals)
}

df_b <- data.frame(
  interval = 1:nIntervals,
  start = intervals$startDate, 
  end  = intervals$endDate)
#df_b <- gather(df_b, interval, value = date)
df_b <- melt(df_b, id.vars = "interval", value.name = "date") 

df_b$date <- as.POSIXct(df_b$date, origin = "1970-01-01", tz = "GMT")
df_b$y <- 0
df_b$y[df_b$variable == "end"] <- 0.75

df_a <- data.frame(
  interval = 1:nIntervals,
  start = test$startDate, 
  end   = test$endDate)
df_a <- melt(df_a, id.vars = "interval", value.name = "date")
df_a$date <- as.POSIXct(df_a$date, origin = "1970-01-01", tz = "GMT")
df_a$y <- 0
df_a$y[df_a$variable == "end"] <- 1

p <- ggplot(df_b, aes(date, y)) 
p <- p + geom_vline(data = dfs, aes(xintercept = startDate), size = 3)
p <- p + geom_line(colour = "blue")
p <- p + geom_line(data = df_a, colour = "red")
p
 
# using Date
# makeDateIntervals <- function(startDate_ch, endDate_ch, nIntervals){
  # startDate_period <- as.Date(startDate_ch, format = "%Y-%m-%d")
  # endDate_period   <- as.Date(endDate_ch, format = "%Y-%m-%d")
  # periodLength <- difftime(endDate_period, startDate_period, units = "days") + 1
  # # make sure periods are at least one day long, so we dont have more periods thn days
  # if (nIntervals > as.integer(periodLength)) nIntervals <- min(as.integer(intervalLength), nIntervals)

  # intervalLength <- periodLength / nIntervals
  # # create a sequence of dates
  # startDate_interval <- seq(startDate_period, endDate_period, length = (nIntervals+1))[1:nIntervals]
  # endDate_interval   <- seq(startDate_period, endDate_period, length = nIntervals+1)[2:(nIntervals+1)] - 1

  # for (i in 2:nIntervals){
    # startDate_interval[i] <- startDate_interval[i-1] + intervalLength
  # }

  # for (i in 1:(nIntervals-1)){
    # endDate_interval[i]   <- startDate_interval[i+1] - 1
  # }
  # endDate_interval[nIntervals] <- endDate_period
  # # startDate_interval; endDate_interval
  # # difftime(endDate_interval, startDate_interval, units = "days")
  # return(list(startDate = startDate_interval, 
                # endDate = endDate_interval))
# }

writeEddyproProcFilesForIntervals <- function(eddyproProcFileName, intervals){
  nIntervals <- length(intervals$startDate)
  eddyproProcFileName_int <- vector(mode = "character", length = nIntervals)
    
  eddyproProcFile <- readLines(file(eddyproProcFileName, "r+"))
  str(eddyproProcFile)
  # make a vector of file names
  
  for (i in 1:nIntervals){
    # make .eddypro file name for interval 
    eddyproProcFileName_int[i] <- paste0(str_sub(eddyproProcFileName, start = 1, 
      end = str_length(eddyproProcFileName)-8), i, ".eddypro")
    
    # find line where "project" ID is given
    ind <- !is.na(str_match(eddyproProcFile, "project_id="))
    # set flag to use subsets
    eddyproProcFile[ind] <- paste0("project_id=job", i)
    
    # find line where flag for sub-period processing occurs
    ind <- !is.na(str_match(eddyproProcFile, "pr_subset="))
    # set flag to use subsets
    eddyproProcFile[ind] <- "pr_subset=1"
        
    # find indices where start date appears
    ind <- !is.na(str_match(eddyproProcFile, "start_date="))
    # replace with interval start date
    eddyproProcFile[ind] <- paste0(str_sub(eddyproProcFile[ind], start = 1, end = 3), 
      "start_date=", intervals$startDate[i])
    
    # find indices where end date appears
    ind <- !is.na(str_match(eddyproProcFile, "end_date="))
    # replace with interval end date
    eddyproProcFile[ind] <- paste0(str_sub(eddyproProcFile[ind], start = 1, end = 3), 
      "end_date=", intervals$endDate[i])
    print(paste0("Writing ", eddyproProcFileName_int[i]))
    writeLines(eddyproProcFile, con = eddyproProcFileName_int[i])
  }  
  return(eddyproProcFileName_int)
}

writeJobFileForIntervals <- function(eddyproProcFileName, nIntervals){
  # break the full path into dirnames
  dir_name <- str_split(eddyproProcFileName, "/")[[1]]
  # get the station dir name, 2 from the end (i.e. station/proc/processing.eddypro)
  dir_name <- dir_name[length(dir_name)-2]
  # make this the dinstinct job name
  jobFileName <- paste0(dir_name, "_eddyjob.sh")
  con <- file(jobFileName)
  writeLines("#!/bin/bash", con = jobFileName)
  write("#!/bin/bash", file = jobFileName, append = TRUE)
  write("#BSUB -q short-serial", file = jobFileName, append = TRUE)
  write("#BSUB -o %J.out", file = jobFileName, append = TRUE)
  write("#BSUB -e %J.err", file = jobFileName, append = TRUE)
  #write("#BSUB -W 00:10", file = jobFileName, append = TRUE) # this sets an extra limit on wall time used if needed
  write(paste0("#BSUB -J eddyjobArray[1-", nIntervals, "]"), file = jobFileName, append = TRUE)

  # ver 5.1.1 doesnt read processing file argument correctly (at all?)
  #binpath <- "/group_workspaces/jasmin2/eddystore/eddypro-engine-master/bin/eddypro_rp"
  # use ver 6.2.0
  binpath <- "/group_workspaces/jasmin2/eddystore/eddypro-engine_6.2.0/eddypro-engine/bin/linux/eddypro_rp"
  switch_OS <- "-s linux" 
  # assumes processing file is in a subdirectory of main station directory
  switch_env <- paste("-e", dirname(dirname(eddyproProcFileName)))
  # make job array of processing file names
  eddyproProcArray <- paste0(tools::file_path_sans_ext(eddyproProcFileName), 
        "${LSB_JOBINDEX}", ".eddypro")        
  cmd <- paste(binpath, switch_OS, switch_env, eddyproProcArray)
  write(cmd, file = jobFileName, append = TRUE)

  on.exit(close(con))
  return(jobFileName)
}


run_ep_job <- function(siteID, startDate_ch, endDate_ch, nIntervals = 1){
  # get EddyproProcFileName from database
  intervals <- makeDateIntervals(startDate_ch, endDate_ch, nIntervals)
  v_EddyproProcFileNames <- writeEddyproProcFilesForIntervals("EddyproProcFileName", intervals)
  writeJobFilesForIntervals(v_EddyproProcFileName, intervals)
  cmd <- paste(bsub < eddyjob.sh)
  system(cmd)

  return()
}
