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
#' @seealso \code{\link{adjustIntervals}} for the adjusting this to match boundaries between .eddypro project files.
#' @examples
#' nIntervals = 4
#' startDate_period <- "2006-07-01 00:00"
#' endDate_period   <- "2007-12-31 23:30"
#' startDate_period <- as.POSIXct(strptime(startDate_period, "%Y-%m-%d %H:%M"), tz = "UTC")
#' endDate_period   <- as.POSIXct(strptime(endDate_period,   "%Y-%m-%d %H:%M"), tz = "UTC")
#' intervals <- makeDateIntervals(startDate_period, endDate_period, nIntervals)

makeDateIntervals <- function(startDate_period, endDate_period, nIntervals){
  periodLength <- difftime(endDate_period, startDate_period, units = "days") + 1
  intervalLength <- periodLength / nIntervals
  # make sure periods are at least one day long, so we dont have more periods than days
  if (nIntervals > as.integer(periodLength)) nIntervals <- min(as.integer(intervalLength), nIntervals)
  # recalculate in case nIntervals was changed
  intervalLength <- periodLength / nIntervals

  # create a sequence of dates - initially these are approximate
  startDate_interval <- seq(startDate_period, endDate_period, length = (nIntervals+1))[1:nIntervals]
  endDate_interval   <- seq(startDate_period, endDate_period, length = nIntervals+1)[2:(nIntervals+1)]

  # make the startDates exact using intervalLength
  for (i in 2:nIntervals){
    startDate_interval[i] <- startDate_interval[i-1] + intervalLength
  }

  # round down to start of hour
  startDate_interval <- trunc(startDate_interval, units="hours")

  # make the end time 30 mins before start of the next interval
  # last end time stays
  for (i in 1:(nIntervals-1)){
    endDate_interval[i]   <- startDate_interval[i+1] - 30*60 #   halfHour <- 30*60
  }
  #difftime(endDate_interval, startDate_interval, units = "days")
  return(list(startDate = startDate_interval, 
                endDate = endDate_interval,
                nIntervals = nIntervals))
}

#' Adjust Date Intervals
#'
#' This function adjusts the output of makeDateIntervals to match 
#' boundaries between .eddypro project files.
#' @param stationID_proc A character string identifying a station in the project data frame.
#' @param procID_proc  A character string identifying a processing setup in the project data frame.
#' @param intervals A list of the start and end times of each interval, created by makeDateIntervals.
#' @export
#' @seealso \code{\link{adjustIntervals}} for the adjusting this to match boundaries between processing files.
#' @examples
#' myJob <- adjustIntervals(stationID_proc, procID_proc, intervals)

adjustIntervals <- function(stationID_proc, procID_proc, intervals){
  v_project_filepath <- vector(mode = "character", length = intervals$nIntervals)
  # subset to relevant rows of project data frame
  df_project <- subset(df_project, stationID == stationID_proc &
                         procID == procID_proc)
  # make sure sorted in correct order
  df_project <- arrange(df_project, startDate, endDate)

  # check if there are more .eddypro project files needed than intervals requested
  if (length(df_project[,1]) > intervals$nIntervals){
    #print(paste("You need to set nIntervals to at least", length(df_project[,1])))
    stop(paste("You need to set nIntervals to at least", length(df_project[,1])))
  }
  
  for (i in 1:intervals$nIntervals){
    #i = 1
    # match interval with project data frame
    # find row where for start of interval
    start_row <- findInterval(intervals$startDate[i], df_project$startDate)
    # find row where for end of interval
    # left.open arg needed where dates are equal
    end_row   <- findInterval(intervals$endDate[i],   df_project$endDate, left.open=TRUE) + 1

    # if rows are the same, interval spans only one project file configuration
    if (end_row == start_row){
      print("The interval falls within a single project file configuration; no adjustment needed.")
      v_project_filepath[i] <- df_project$project_filepath[start_row]
    }
    # if rows are not same, interval spans two or more project file configurations
    if (end_row != start_row) {
      print("The original interval spanned two project file configurations; adjusting intervals accordingly.")
      # adjust start of this interval and end of previous interval
      intervals$endDate[(i)]   <- df_project$endDate[(end_row)]
      intervals$startDate[i+1] <- df_project$startDate[(start_row+1)]
      v_project_filepath[i]        <- df_project$project_filepath[(start_row)]
      # intervals$endDate[(i-1)]  <- df_project$endDate[(end_row-1)]
      # intervals$startDate[i]    <- df_project$startDate[(start_row+1)]
      # v_project_filepath[i]        <- df_project$project_filepath[(start_row+1)]
    }
  }
  return(list(project_filepath = v_project_filepath,
                  startDate = intervals$startDate, 
                    endDate = intervals$endDate,
                 nIntervals = intervals$nIntervals))
}

#' Write Eddypro Project files for all intervals
#'
#' This function writes an eddypro project file for each of the intervals specified.
#' @param job A list of the start and end times of each interval, created by makeDateIntervals.
#' @export
#' @seealso \code{\link{adjustIntervals}} for the adjusting this to match boundaries between processing files.
#' @examples
#' eddyproProcFileName <- "/group_workspaces/jasmin2/eddystore/stations/EasterBush/proc/processing.eddypro"
#' myJob <- writeProjectFiles(myJob)

writeProjectFiles <- function(job){
  # make a vector of file names
  nIntervals <- length(job$startDate)
  v_eddyproProjectFileName <- vector(mode = "character", length = nIntervals)

  # make a base project file name, to be incrementally numbered for each interval
  ## could ust use dirname function to do this
  dir_name <- str_split(job$project_filepath[1], "/")[[1]]
  # remove the file name from the end
  dir_name <- dir_name[1:(length(dir_name)-1)]
  dir_name <- str_c(dir_name, collapse = "/")
  # make this the base project file name
  baseProjectFileName <- paste0(dir_name, "/processing.eddypro")
  
  for (i in 1:nIntervals){
    # read the appropriate .eddypro file for this interval into a character vector
    eddyproProjectFileText <- readLines(con <- file(job$project_filepath[i], "r+"))
    close(con)
    
    # make .eddypro file name for interval 
    v_eddyproProjectFileName[i] <- paste0(str_sub(baseProjectFileName, start = 1, 
      end = str_length(baseProjectFileName)-8), i, ".eddypro")
    
    # find line where "project" ID is given
    ind <- !is.na(str_match(eddyproProjectFileText, "project_id="))
    # set flag to use subsets
    eddyproProjectFileText[ind] <- paste0("project_id=job", i)
    
    # find line where flag for sub-period processing occurs
    ind <- !is.na(str_match(eddyproProjectFileText, "pr_subset="))
    # set flag to use subsets
    eddyproProjectFileText[ind] <- "pr_subset=1"
        
    # find indices where start date appears
    ind <- !is.na(str_match(eddyproProjectFileText, "start_date="))
    # replace with interval start date
    eddyproProjectFileText[ind] <- paste0(str_sub(eddyproProjectFileText[ind], start = 1, end = 3), 
      "start_date=", intervals$startDate[i])
    
    # find indices where end date appears
    ind <- !is.na(str_match(eddyproProjectFileText, "end_date="))
    # replace with interval end date
    eddyproProjectFileText[ind] <- paste0(str_sub(eddyproProjectFileText[ind], start = 1, end = 3), 
      "end_date=", intervals$endDate[i])
    print(paste0("Writing ", v_eddyproProjectFileName[i]))
    writeLines(eddyproProjectFileText, con = v_eddyproProjectFileName[i])
  }  
  return(list( project_filepath = job$project_filepath,
                      startDate = job$startDate, 
                        endDate = job$endDate,
                     nIntervals = job$nIntervals,
       v_eddyproProjectFileName = v_eddyproProjectFileName,
            baseProjectFileName = baseProjectFileName))
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
#' jobFileName <- writeJobFile(myJob)
#' jobFileName

writeJobFile <- function(job){
  # break the full path into dirnames
  dir_name <- str_split(job$baseProjectFileName, "/")[[1]]
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
  write(paste0("#BSUB -J eddyjobArray[1-", job$nIntervals, "]"), file = jobFileName, append = TRUE)

  # ver 5.1.1 doesnt read processing file argument correctly (at all?)
  #binpath <- "/group_workspaces/jasmin2/eddystore/eddypro-engine-master/bin/eddypro_rp"
  # use ver 6.2.0
  binpath <- "/group_workspaces/jasmin2/eddystore/eddypro-engine_6.2.0/eddypro-engine/bin/linux/eddypro_rp"
  switch_OS <- "-s linux" 
  # assumes processing file is in a subdirectory of main station directory
  switch_env <- paste("-e", dirname(dirname(job$baseProjectFileName)))
  # make job array of processing file names
  eddyproProcArray <- paste0(tools::file_path_sans_ext(job$baseProjectFileName), 
        "${LSB_JOBINDEX}", ".eddypro")        
  cmd <- paste(binpath, switch_OS, switch_env, eddyproProcArray)
  write(cmd, file = jobFileName, append = TRUE)

  on.exit(close(con))
  return(list = c(job, jobFileName = jobFileName))
}

#' Create Processing Job
#'
#' This function creates an eddypro processing job to be run on LOTUS.
#' @param stationID_proc A character string identifying a station in the project data frame.
#' @param procID_proc  A character string identifying a processing setup in the project data frame.
#' @param startDate_period A character string for the time the run was started, used to identify the relevant files.
#' @param endDate_period A character string for the time the run was started, used to identify the relevant files.
#' @param nProcessors An integer number of processors to use, equal to the number of time intervals to split the processing into.
#' @export
#' @seealso \code{\link{adjustIntervals}} for the adjusting this to match boundaries between project files.
#' @examples
#' myJob <- createJob(stationID_proc, procID_proc, startDate_period, endDate_period, nProcessors)

createJob <- function(stationID_proc, procID_proc, startDate_period, endDate_period, nProcessors){
  intervals <- makeDateIntervals(startDate_period, endDate_period, nProcessors)
  job <- adjustIntervals(stationID_proc, procID_proc, intervals)
  job <- writeProjectFiles(job)
  job <- writeJobFile(job)
  return(job)
}


#' Run Processing Job
#'
#' This function runs an eddypro processing job on LOTUS.
#' Gives: Error in system(command, as.integer(flag), f, stdout, stderr, timeout) : 
#'   character string expected as first argument
#' @param job An eddystore job object made with the createJob function
#' @export
#' @seealso \code{\link{adjustIntervals}} for the adjusting this to match boundaries between project files.
#' @examples
#' myJob <- runJob(myJob)

runJob <- function(jobFileName){
  cmd <- paste0("bsub < ", jobFileName)
  # submit the jobs and get the time to identify the output files from this batch
  err <- system(cmd); job_startTime <- Sys.time()
  err; job_startTime
  return(list = c(job, err = err, job_startTime = job_startTime))
}
