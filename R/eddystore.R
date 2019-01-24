## ----eddystore_pkg, eval=TRUE------------------------------------------------
#' Parallel processing of eddy covariance data on JASMIN.
#'
#' eddystore allows you to set up and run processing jobs
#' to calculate eddy covariance fluxes using eddypro on JASMIN.
"_PACKAGE"
#> [1] "_PACKAGE"

#' Collate essential output files
#'
#' Note that curently "EasterBush" is hard-coded in the path - need to pass path argument
#' This function collates all the essential output files.
#' @param job_time A character string for the time the run was started, used to identify the relevant files.
#' @export

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

#' Collate full output files - but not yet working for collating multiple job files
#'
#' "full output" files are harder to read - header is on line 2, data on line 4: use readr::read_csv instead:
#' This function collates all the full output files.
#' This function collates all the full output files.
#' @param fname A character string for the time the run was started, used to identify the relevant files.
#' @export

read_full_output <- function(fname){
  df <- readLines(fname)
  # remove first and third lines, leaving header (line 2) and data (line 4 onwards)
  df = df[c(-1, -3)]
  df <- read.csv(textConnection(df), na.strings = na.strings, header = TRUE, stringsAsFactors = FALSE)
  return(df)
}

#' Make Date Intervals
#'
#' This function creates a number of equally sized time intervals between 
#' the specified start and end times.
#' @param startDate_period A character string for the time the run was started, used to identify the relevant files.
#' @param endDate_period A character string for the time the run was started, used to identify the relevant files.
#' @param nIntervals An integer number of intervals to use to split the processing into.
#' @export
#' @seealso \code{\link{adjustIntervals}} for the adjusting this to match boundaries between processing files.
#' @examples
#' nIntervals = 4
#' startDate_proc <- "2007-01-01 00:00"
#' endDate_proc   <- "2007-12-31 23:30"
#' startDate_period <- as.POSIXct(strptime(startDate_proc, "%Y-%m-%d %H:%M"), tz = "UTC")
#' endDate_period   <- as.POSIXct(strptime(endDate_proc,   "%Y-%m-%d %H:%M"), tz = "UTC")
#' intervals <- makeDateIntervals(startDate_proc, endDate_period, nIntervals)

makeDateIntervals <- function(startDate_period, endDate_period, nIntervals){
  periodLength <- difftime(endDate_period, startDate_period, units = "days") + 1
  intervalLength <- periodLength / nIntervals
  # make sure periods are at least one day long, so we dont have more periods than days
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
  halfHour <- 30*60
  
  ## need to round to nearest half-hour
  startDate_interval <- format(trunc(startDate_interval, units="hours"))
  
  return(list(startDate = startDate_interval, 
                endDate = endDate_interval))
}

#' Adjust Date Intervals
#'
#' This function adjusts the output of makeDateIntervals to match 
#' boundaries between processing files.
#' @param stationID_proc A character string identifying a station in the processing file table.
#' @param procID_proc  A character string identifying a processing setup in the processing file table.
#' @param intervals A list of the start and end times of each interval, created by makeDateIntervals.
#' @export
#' @seealso \code{\link{adjustIntervals}} for the adjusting this to match boundaries between processing files.
#' @examples
#' test <- adjustIntervals(stationID_proc, procID_proc, intervals)

adjustIntervals <- function(stationID_proc, procID_proc, intervals){
  nIntervals <- length(intervals[[1]])
  v_proc_filepath <- vector(mode = "character", length = nIntervals)
  # subset to relevant rows of proc table
  dfs <- subset(df_proc, stationID == stationID_proc &
                         procID == procID_proc)
  # make sure sorted in correct order
  dfs <- arrange(dfs, startDate, endDate)

  # check if there are more processing configs than intervals requested
  if (length(dfs[,1]) > nIntervals){
    #print(paste("You need to set nIntervals to at least", length(dfs[,1])))
    stop(paste("You need to set nIntervals to at least", length(dfs[,1])))
  }
  
  for (i in 1:nIntervals){
    #i = 2
    # match interval with proc file
    # find row where for start of interval
    i_start <- findInterval(intervals$startDate[i], dfs$startDate)
    # find row where for end of interval
    # left.open arg needed where dates are equal
    i_end <- findInterval(intervals$endDate[i], dfs$endDate, left.open=TRUE) + 1

    # if rows are not same, interval spans two or more processing configs
    if (i_end == i_start){
      print("The interval is within a single processing configurations; no adjustment needed.")
      v_proc_filepath[i] <- dfs$proc_filepath[i_start]
    }
    if (i_end != i_start) {
      print("The original interval spanned two processing configurations; adjusting intervals accordingly.")
      # adjust start of this interval and end of previous interval
      intervals$startDate[i]    <- dfs$startDate[(i_start+1)]
      intervals$endDate[(i-1)]  <- dfs$endDate[(i_end-1)]
      v_proc_filepath[i]        <- dfs$proc_filepath[(i_start+1)]
    }
  }
  return(list(proc_filepath = v_proc_filepath,
                  startDate = intervals$startDate, 
                    endDate = intervals$endDate))
}

#' Create Processing Job
#'
#' This function creates an eddypro processing job to be on LOTUS.
#' @param stationID_proc A character string identifying a station in the processing file table.
#' @param procID_proc  A character string identifying a processing setup in the processing file table.
#' @param startDate_period A character string for the time the run was started, used to identify the relevant files.
#' @param endDate_period A character string for the time the run was started, used to identify the relevant files.
#' @param nProcessors An integer number of processors to use, equal to the number of time intervals to split the processing into.
#' @export
#' @seealso \code{\link{adjustIntervals}} for the adjusting this to match boundaries between processing files.
#' @examples
#' myJob <- createJob(stationID_proc, procID_proc, startDate, endDate, nProcessors)

createJob <- function(stationID_proc, procID_proc, startDate, endDate, nProcessors){
  myJob <- makeDateIntervals(startDate, endDate, nProcessors)
  myJob <- adjustIntervals(stationID_proc, procID_proc, myJob)
  v_EddyproProcFileNames <- writeEddyproProcFilesForIntervals(myJob)
  jobFileName <- writeJobFileForIntervals(eddyproProcFileName, nIntervals)
}

#' Write Eddypro Processing files for all intervals
#'
#' This function writes an eddypro processing file for each of the intervals specified.
#' Should eddyproProcFileName actually be looked up in the proc table by stationID_proc, procID_proc in adjustIntervals, 
#' and added to the object returned there?
#' @param eddyproProcFileName A character string identifying the root eddypro file. 
#' @param job A list of the start and end times of each interval, created by makeDateIntervals.
#' @export
#' @seealso \code{\link{adjustIntervals}} for the adjusting this to match boundaries between processing files.
#' @examples
#' eddyproProcFileName <- "/group_workspaces/jasmin2/eddystore/stations/EasterBush/proc/processing.eddypro"
#' v_EddyproProcFileNames <- writeEddyproProcFilesForIntervals(eddyproProcFileName, intervals)

writeEddyproProcFilesForIntervals <- function(job){
  # make a vector of file names
  nIntervals <- length(job$startDate)
  v_eddyproProcFileName <- vector(mode = "character", length = nIntervals)
    
  for (i in 1:nIntervals){
    # read the appropriate .eddypro file for this interval into a character vector
    eddyproProcFile <- readLines(file(job$proc_filepath[i], "r+"))

    # make .eddypro file name for interval 
    v_eddyproProcFileName[i] <- paste0(str_sub(eddyproProcFileName, start = 1, 
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
    print(paste0("Writing ", v_eddyproProcFileName[i]))
    writeLines(eddyproProcFile, con = v_eddyproProcFileName[i])
  }  
  return(v_eddyproProcFileName)
}

#' Write LOTUS batch job files for all intervals
#'
#' This function writes a LOTUS batch job file for each of the intervals specified.
#' Should eddyproProcFileName actually be looked up in the proc table by stationID_proc, procID_proc in adjustIntervals, 
#' and added to the object returned there?
#' @param eddyproProcFileName A character string identifying the root eddypro file. 
#' @param nIntervals An integer number of intervals to use to split the processing into.
#' @export
#' @seealso \code{\link{adjustIntervals}} for the adjusting this to match boundaries between processing files.
#' @examples
#' jobFileName <- writeJobFileForIntervals(eddyproProcFileName, nIntervals)
#' jobFileName

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

# run_ep_job <- function(siteID, startDate_ch, endDate_ch, nIntervals = 1){
  # # get EddyproProcFileName from database
  # intervals <- makeDateIntervals(startDate_ch, endDate_ch, nIntervals)
  # v_EddyproProcFileNames <- writeEddyproProcFilesForIntervals("EddyproProcFileName", intervals)
  # writeJobFilesForIntervals(v_EddyproProcFileName, intervals)
  # cmd <- paste(bsub < eddyjob.sh)
  # system(cmd)
  # return()
# }
