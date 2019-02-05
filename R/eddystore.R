## ----eddystore_pkg, eval=TRUE------------------------------------------------
#' Parallel processing of eddy covariance data on JASMIN.
#'
#' eddystore allows you to set up and run processing jobs
#' to calculate eddy covariance fluxes using eddypro on JASMIN.
"_PACKAGE"
#> [1] "_PACKAGE"

#' Convert paths in Eddypro project files
#'
#' Need to add biomet file path changes biom_file and biom_dir
#'
#' This function changes all references to paths in an uploaded project file to the eddystore path names.
#' @param eddyproProjectPathName Path to the location of the uploaded project file on JASMIN.
#' @param station_dir Path to the location of the station directory on JASMIN e.g. "/gws/nopw/j04/eddystore/stations/EasterBush".
#' @return eddyproProjectPathName_new File name for project file with eddystore paths. The side effect is to write this file.
#' @export
#' @examples
#' eddyproProjectPathName <- "C:/Users/plevy/Documents/eddystore_projects/stations/EasterBush/projects/processing2.eddypro"
#' station_dir <- "C:/Users/plevy/Documents/eddystore_projects/stations/EasterBush"
#' convertProjectPath(eddyproProjectPathName, station_dir)

convertProjectPath <- function(eddyproProjectPathName, station_dir){
    # make file name for new .eddypro project file
    eddyproProjectFileName <- basename(eddyproProjectPathName)
    eddyproProjectPathName_new <- paste0(station_dir, "/projects/", str_sub(eddyproProjectFileName, start = 1, 
      end = str_length(eddyproProjectFileName)-8), ".eddypro")
    
    eddyproProjectPathName_backup <- paste0(station_dir, "/projects/tmp_prj/", str_sub(eddyproProjectFileName, start = 1, 
      end = str_length(eddyproProjectFileName)-8), ".eddypro")
    
    # read the .eddypro file into a character vector
    eddyproProjectFileText <- readLines(con <- file(eddyproProjectPathName, "r+"))
    close(con)

    # make a back-up copy of the original file in tmp_prj    
    writeLines(eddyproProjectFileText, con = eddyproProjectPathName_backup)
    
    # find line where "proj_file" is given
    # bizarrely, "proj_file" is the field for the .metadata file
    ind <- !is.na(str_match(eddyproProjectFileText, "proj_file="))
    # write new text into this line  
    eddyproProjectFileText[ind] <- paste0("proj_file=", station_dir, "/metadata/", basename(eddyproProjectFileText[ind]))  
    
    # find line for output path
    ind <- !is.na(str_match(eddyproProjectFileText, "out_path="))
    # write new text into this line
    eddyproProjectFileText[ind] <- paste0("out_path=", station_dir, "/output") 
        
    # find line for previous output path
    ind <- !is.na(str_match(eddyproProjectFileText, "ex_file="))
    # if entry not empty, write new text into this line
    if (eddyproProjectFileText[ind] != "ex_file="){
      eddyproProjectFileText[ind] <- paste0("ex_file=", station_dir,  "/output/", basename(eddyproProjectFileText[ind])) 
    }
    
    # find line for binned spectral output path
    ind <- !is.na(str_match(eddyproProjectFileText, "sa_bin_spectra="))
    # if entry not empty, write new text into this line
    if (eddyproProjectFileText[ind] != "sa_bin_spectra="){
      eddyproProjectFileText[ind] <- paste0("sa_bin_spectra=", station_dir,  "/output/eddypro_binned_cospectra") 
    }
    # find line for full spectral output path
    ind <- !is.na(str_match(eddyproProjectFileText, "sa_full_spectra="))
    # if entry not empty, write new text into this line
    if (eddyproProjectFileText[ind] != "sa_full_spectra="){
    eddyproProjectFileText[ind] <- paste0("sa_full_spectra=", station_dir,  "/output/eddypro_full_cospectra")      
    }
    
    # find line for previous execution path - not sure if needed
    ind <- !is.na(str_match(eddyproProjectFileText, "ex_dir="))
    # if entry not empty, write new text into this line
    if (eddyproProjectFileText[ind] != "ex_dir="){
    eddyproProjectFileText[ind] <- paste0("ex_dir=", station_dir,  "/output")      
    }
    
    # find line for raw data path
    ind <- !is.na(str_match(eddyproProjectFileText, "data_path="))
    # write new text into this line
    eddyproProjectFileText[ind] <- paste0("data_path=", station_dir,  "/raw_files")      
    
    # write all lines to file
    print(paste0("Writing ", eddyproProjectPathName_new))
    writeLines(eddyproProjectFileText, con = eddyproProjectPathName_new)

 return(eddyproProjectPathName_new)
}

#' Make Date Intervals
#'
#' This function creates a number of equally sized time intervals between 
#' the specified start and end times.
#' @param startDate_period A character string for the time the run was started, used to identify the relevant files.
#' @param endDate_period A character string for the time the run was started, used to identify the relevant files.
#' @param nIntervals An integer number of intervals to use to split the processing into.
#' @return An object with vectors of start and end dates for n intervals .
#' @export
#' @seealso \code{\link{adjustIntervals}} for the adjusting this to match boundaries between .eddypro project files.
#' @examples
#' nIntervals = 4
#' startDate_period <- "2006-07-01 00:00"
#' endDate_period   <- "2007-12-31 23:30"
#' startDate_period <- as.POSIXct(strptime(startDate_period, "%Y-%m-%d %H:%M"), tz = "UTC")
#' endDate_period   <- as.POSIXct(strptime(endDate_period,   "%Y-%m-%d %H:%M"), tz = "UTC")
#' myJob <- makeDateIntervals(startDate_period, endDate_period, nIntervals)

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
#' @param fname_df_project Path to the eddystore project file table.
#' @return An eddystore job object.
#' @export
#' @seealso \code{\link{adjustIntervals}} for the adjusting this to match boundaries between processing files.
#' @examples
#' stationID_proc <- "EasterBush"
#' procID_proc <- "CO2_H2O"
#' nIntervals <- 4
#' startDate_period <- "2006-07-01 00:00"
#' endDate_period   <- "2007-12-31 23:30"
#' startDate_period <- as.POSIXct(strptime(startDate_period, "%Y-%m-%d %H:%M"), tz = "UTC")
#' endDate_period   <- as.POSIXct(strptime(endDate_period,   "%Y-%m-%d %H:%M"), tz = "UTC")
#' fname_df_project = "C:/Users/plevy/Documents/eddystore_projects/df_eddystore_projects.csv"
#' intervals <- makeDateIntervals(startDate_period, endDate_period, nIntervals)
#' myJob <- adjustIntervals(stationID_proc, procID_proc, intervals, fname_df_project = fname_df_project)

adjustIntervals <- function(stationID_proc, procID_proc, intervals, fname_df_project = "/gws/nopw/j04/eddystore/eddystore_projects/df_eddystore_projects.csv"){
  v_project_filepath <- vector(mode = "character", length = intervals$nIntervals)
  # read the eddystore project file table
  df_project <- read.csv(fname_df_project, stringsAsFactors = FALSE)
  df_project$startDate <- as.POSIXct(strptime(df_project$startDate, "%d/%m/%Y %H:%M"), tz = "UTC")
  df_project$endDate   <- as.POSIXct(strptime(df_project$endDate, "%d/%m/%Y %H:%M"), tz = "UTC")
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
#' @return An eddystore job object.
#' @export
#' @seealso \code{\link{adjustIntervals}} for the adjusting this to match boundaries between processing files.
#' @examples
#' stationID_proc <- "EasterBush"
#' procID_proc <- "CO2_H2O"
#' nIntervals <- 4
#' startDate_period <- "2006-07-01 00:00"
#' endDate_period   <- "2007-12-31 23:30"
#' startDate_period <- as.POSIXct(strptime(startDate_period, "%Y-%m-%d %H:%M"), tz = "UTC")
#' endDate_period   <- as.POSIXct(strptime(endDate_period,   "%Y-%m-%d %H:%M"), tz = "UTC")
#' intervals <- makeDateIntervals(startDate_period, endDate_period, nIntervals)
#' fname_df_project = "C:/Users/plevy/Documents/eddystore_projects/df_eddystore_projects.csv"
#' myJob <- adjustIntervals(stationID_proc, procID_proc, intervals, fname_df_project = fname_df_project)
#' myJob <- writeProjectFiles(myJob)

writeProjectFiles <- function(job){
  # make a vector of file names
  nIntervals <- length(job$startDate)
  job$v_eddyproProjectFileName <- vector(mode = "character", length = nIntervals)

  # make a base project file name, to be incrementally numbered for each interval
  dir_name <- str_split(job$project_filepath[1], "/")[[1]]

  # make this the base project file name
  baseProjectFileName <- dir_name[1:(length(dir_name)-1)]
  baseProjectFileName <- str_c(baseProjectFileName, collapse = "/")
  baseProjectFileName <- paste0(baseProjectFileName, "/tmp_prj/processing.eddypro")
  
  # get the station dir name, 2 from the end (i.e. station/proc/processing.eddypro)
  # use as a check that this is same as sationID? Depends on just directory structure
  job$station_name <- dir_name[length(dir_name)-2]
  
  # and find the station directory for the eddypro run, for output etc.
  job$station_dir <- dir_name[1:(length(dir_name)-2)]
  job$station_dir <- str_c(job$station_dir, collapse = "/")
  
  for (i in 1:nIntervals){
    # read the appropriate .eddypro file for this interval into a character vector
    eddyproProjectFileText <- readLines(con <- file(job$project_filepath[i], "r+"))
    close(con)
    
    # make .eddypro file name for interval 
    job$v_eddyproProjectFileName[i] <- paste0(str_sub(baseProjectFileName, start = 1, 
      end = str_length(baseProjectFileName)-8), i, ".eddypro")
    
    # find line where "project" ID is given
    ind <- !is.na(str_match(eddyproProjectFileText, "project_id="))
    # write new text into this line
    eddyproProjectFileText[ind] <- paste0("project_id=job", i)
    
    # find line where flag for sub-period processing occurs
    ind <- !is.na(str_match(eddyproProjectFileText, "pr_subset="))
    # write new text into this line
    eddyproProjectFileText[ind] <- "pr_subset=1"
        
    # find indices where start date appears
    ind <- !is.na(str_match(eddyproProjectFileText, "start_date="))
    # replace with interval start date
    eddyproProjectFileText[ind] <- paste0(str_sub(eddyproProjectFileText[ind], start = 1, end = 3), 
      "start_date=", job$startDate[i])
    
    # find indices where end date appears
    ind <- !is.na(str_match(eddyproProjectFileText, "end_date="))
    # replace with interval end date
    eddyproProjectFileText[ind] <- paste0(str_sub(eddyproProjectFileText[ind], start = 1, end = 3), 
      "end_date=", job$endDate[i])
    print(paste0("Writing ", job$v_eddyproProjectFileName[i]))
    writeLines(eddyproProjectFileText, con = job$v_eddyproProjectFileName[i])
  }

  job$baseProjectFileName <- baseProjectFileName       
  return(job)
}

#' Write LOTUS batch job files for all intervals
#'
#' This function writes a LOTUS batch job file for each of the intervals specified.
#' @param job An eddystore job object made with the writeProjectFiles function.
#' @param binpath Path to the eddy_rp raw data processing binary file.
#' @param switch_OS Switch betweeen linux and windows versions.
#' @param eddystore_path The path to eddystore.
#' @param job_name The job name identifier given in df_job_requests.
#' @param user_email E-mail address to send notifications to.
#' @return An eddystore job object.
#' @export
#' @seealso \code{\link{adjustIntervals}} for the adjusting this to match boundaries between processing files.
#' @examples
#' stationID_proc <- "EasterBush"
#' procID_proc <- "CO2_H2O"
#' nIntervals <- 4
#' startDate_period <- "2006-07-01 00:00"
#' endDate_period   <- "2007-12-31 23:30"
#' startDate_period <- as.POSIXct(strptime(startDate_period, "%Y-%m-%d %H:%M"), tz = "UTC")
#' endDate_period   <- as.POSIXct(strptime(endDate_period,   "%Y-%m-%d %H:%M"), tz = "UTC")
#' intervals <- makeDateIntervals(startDate_period, endDate_period, nIntervals)
#' fname_df_project = "C:/Users/plevy/Documents/eddystore_projects/df_eddystore_projects.csv"
#' myJob <- adjustIntervals(stationID_proc, procID_proc, intervals, fname_df_project = fname_df_project)
#' myJob <- writeProjectFiles(myJob)
#' myJob <- writeJobFile(myJob, binpath = "N:/0Peter/curr/ECsystem/eddypro", 
#'                              switch_OS = "-s linux",
#'                              eddystore_path = "N:/0Peter/curr/ECsystem/eddystore", job_name = "writeJob_example")

writeJobFile <- function(job, binpath = "/gws/nopw/j04/eddystore/eddypro-engine_6.2.0/eddypro-engine/bin/linux/eddypro_rp", 
                            switch_OS = "-s linux",
                            eddystore_path = "/gws/nopw/j04/eddystore", 
                            job_name = "eddytest", user_email = "plevy@ceh.ac.uk"){
  # make a unique name for a bsub queue file
  job_writeTime <- str_replace_all(Sys.time(), c(" " = "_", ":" = "_", "GMT" = ""))
  jobFileName <- paste0(eddystore_path, "/jobs/", job$station_name, "_", job_writeTime, "_eddystoreJob.sh")
  con <- file(jobFileName)
  
  # write contents of bsub queue file
  writeLines("#!/bin/bash", con = jobFileName)
  write("#BSUB -q short-serial", file = jobFileName, append = TRUE) # this sets a maximum of 24 h run-time; use long-serial	for longer
  write("#BSUB -o %J.out", file = jobFileName, append = TRUE)
  write("#BSUB -e %J.err", file = jobFileName, append = TRUE)
  #write("#BSUB -W 00:10", file = jobFileName, append = TRUE) # this sets an extra limit on wall time used if needed
  write(paste0("#BSUB -J ", job_name, "[1-", job$nIntervals, "]"), file = jobFileName, append = TRUE)

  # assumes processing file is in a subdirectory of main station directory
  switch_env <- paste("-e", job$station_dir)
 
  # make job array of processing file names
  eddyproProcArray <- paste0(tools::file_path_sans_ext(job$baseProjectFileName), 
        "${LSB_JOBINDEX}", ".eddypro")        
  cmd <- paste(binpath, switch_OS, switch_env, eddyproProcArray)
  write(cmd, file = jobFileName, append = TRUE)
  on.exit(close(con))
  
  job$jobFileName <- jobFileName; job$job_name <- job_name; job$user_email <- user_email
  return(job)
}

#' Create Processing Job
#'
#' This function creates an eddypro processing job to be run on LOTUS.
#' @param stationID_proc A character string identifying a station in the project data frame.
#' @param procID_proc  A character string identifying a processing setup in the project data frame.
#' @param startDate_period A character string for the time the run was started, used to identify the relevant files.
#' @param endDate_period A character string for the time the run was started, used to identify the relevant files.
#' @param nProcessors An integer number of processors to use, equal to the number of time intervals to split the processing into.
#' @param fname_df_project Path to the eddystore project file table.
#' @param job_name The job name identifier given in df_job_requests.
#' @param user_email E-mail address to send notifications to.
#' @inheritParams writeJobFile
#' @return An eddystore job object.
#' @export
#' @seealso \code{\link{adjustIntervals}} for the adjusting this to match boundaries between project files.
#' @examples
#' stationID_proc <- "EasterBush"
#' procID_proc <- "CO2_H2O"
#' nProcessors <- 4
#' startDate_period <- "2006-07-01 00:00"
#' endDate_period   <- "2007-12-31 23:30"
#' startDate_period <- as.POSIXct(strptime(startDate_period, "%Y-%m-%d %H:%M"), tz = "UTC")
#' endDate_period   <- as.POSIXct(strptime(endDate_period,   "%Y-%m-%d %H:%M"), tz = "UTC")
#' myJob <- createJob(stationID_proc, procID_proc, startDate_period, endDate_period, nProcessors,
#'     fname_df_project = "C:/Users/plevy/Documents/eddystore_projects/df_eddystore_projects.csv", 
#'     eddystore_path = "N:/0Peter/curr/ECsystem/eddystore",
#'     job_name = "createJob_example", user_email = "plevy@ceh.ac.uk")

createJob <- function(stationID_proc, procID_proc, startDate_period, endDate_period, nProcessors, 
    fname_df_project = "/gws/nopw/j04/eddystore/eddystore_projects/df_eddystore_projects.csv", 
    binpath = "/gws/nopw/j04/eddystore/eddypro-engine_6.2.0/eddypro-engine/bin/linux/eddypro_rp", 
    switch_OS = "-s linux", eddystore_path = "/gws/nopw/j04/eddystore", 
    job_name = "eddytest", user_email = "plevy@ceh.ac.uk"){
  intervals <- makeDateIntervals(startDate_period, endDate_period, nProcessors)
  job <- adjustIntervals(stationID_proc, procID_proc, intervals, fname_df_project)
  job <- writeProjectFiles(job)
  job <- writeJobFile(job, binpath, switch_OS, eddystore_path, job_name, user_email)
  return(job)
}


#' Run Processing Job
#'
#' This function runs an eddypro processing job on LOTUS.
#' @param job An eddystore job object made with the createJob function
#' @return An eddystore job object.
#' @export
#' @seealso \code{\link{adjustIntervals}} for the adjusting this to match boundaries between project files.
#' @examples
#' stationID_proc <- "EasterBush"
#' procID_proc <- "CO2_H2O"
#' nIntervals <- 4
#' startDate_period <- "2006-07-01 00:00"
#' endDate_period   <- "2007-12-31 23:30"
#' startDate_period <- as.POSIXct(strptime(startDate_period, "%Y-%m-%d %H:%M"), tz = "UTC")
#' endDate_period   <- as.POSIXct(strptime(endDate_period,   "%Y-%m-%d %H:%M"), tz = "UTC")
#' intervals <- makeDateIntervals(startDate_period, endDate_period, nIntervals)
#' fname_df_project = "C:/Users/plevy/Documents/eddystore_projects/df_eddystore_projects.csv"
#' myJob <- adjustIntervals(stationID_proc, procID_proc, intervals, fname_df_project = fname_df_project)
#' myJob <- writeProjectFiles(myJob)
#' myJob <- writeJobFile(myJob, binpath = "N:/0Peter/curr/ECsystem/eddypro", 
#'                              switch_OS = "-s linux",
#'                              eddystore_path = "N:/0Peter/curr/ECsystem/eddystore",
#'                              job_name = "runJob_example")
#' myJob <- runJob(myJob)

runJob <- function(job){
  cmd <- paste0("bsub < ", job$jobFileName)
  # submit the jobs and get the time to identify the output files from this batch
  job$err <- system(cmd); job$job_startTime <- Sys.time()
  return(job)
}

#' Check Status of Job on LOTUS
#'
#' This function checks an eddypro processing job on LOTUS.
#' For multi-processor jobs, it only checks the first-listed in the job array, 
#' may report "DONE" before all are finished. 
#' @param job An eddystore job object made with the createJob function
#' @return job_status A character message: "RUN" if still running, "DONE" if finished.
#' @export

checkJobStatus <- function(job){
  cmd <- paste("bjobs -a -J", job$job_name)
  # query the job queue to get the status of the job
  bjobs_report <- system(cmd, intern = TRUE)
  # split string on whitespace 
  bjobs_report_ch <- str_split(bjobs_report[2], "\\s+")[[1]]
    bjobs_report_ch
  job_status <- bjobs_report_ch[3]
  job_name   <- bjobs_report_ch[7]
  job_SUBMIT_TIME <- str_c(bjobs_report_ch[8:10], collapse = " ")
  print(paste("Job", job_name, "submitted at", job_SUBMIT_TIME, "is", job_status))  
  return(job_status)
}

#' Collate essential output files
#'
#' Note that curently "EasterBush" is hard-coded in the path - need to pass path argument
#' @param job An eddystore job object made with the createJob function
#' @return A data frame of the concatenated essential output files from each job.
#' @export

get_essential_output_df <- function(job){
  na.strings = c("NAN", "7999", "-7999","-999","999","9999.99", "-9999.0", "-9999.0000000000000","9999","9999","-9999")
  job_time_ch <- str_split(job$job_startTime, "[-: ]")[[1]]
  YYYY <- job_time_ch[1]
  mm <- job_time_ch[2]
  dd <- job_time_ch[3]
  HH <- job_time_ch[4]
  MM <- job_time_ch[5]
  SS <- job_time_ch[6]

  # "essentials" output files created in a ten minute slot, hoping not on a 10-min boundary (i.e. 29 to 30 mins)
  to_match <- paste0("eddypro_job.*_essentials_", YYYY, "-", mm, "-", dd, "T", HH, substr(MM, 1, 1))
  list.files(path = "./stations/Balmoral/output", pattern = to_match, full.names = TRUE)
  files_out <- list.files(path = "./stations/Balmoral/output", pattern = to_match, full.names = TRUE)
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
#' @return A data frame of the concatenated full output files from each job.
#' @export

read_full_output <- function(fname){
  df <- readLines(fname)
  # remove first and third lines, leaving header (line 2) and data (line 4 onwards)
  df = df[c(-1, -3)]
  df <- read.csv(textConnection(df), na.strings = na.strings, header = TRUE, stringsAsFactors = FALSE)
  return(df)
}
