## R code for checking eddystore job requests and
## creating LOTUS jobs and running these.
## To be run as a cron job every 10 mins or half-hour.
## See https://help.jasmin.ac.uk/article/3847-using-cron
## 
## Peter Levy, CEH Edinburgh
## Bush Estate, Penicuik, EH26 0QB, U.K.
## plevy@ceh.ac.uk
## Tel: 0131 445 8556

rm(list=ls(all=TRUE))
library(devtools)
install_github("NERC-CEH/eddystore", auth_token = "cf75f3ae2091f58e6dd664ce9031bee3aa98f0f8")
library(eddystore)


# eddystore automates the processing of eddy covariance flux data using eddypro in parallel
#
# eddystore comprises the JASMIN storage and computation hardware
and three pieces of software: 
1. the eddypro fortran program which performs the flux calculations
2. an R package "eddystore" which contains:
     functions which translate the user processing requirements into computation instructions on jasmin, and
     R scripts which carry these instructions out on a scheduled basis as cron jobs
3. a shiny app which allows jobs to be run on jasmin via a web browser
# Processing jobs can be created manually with a shiny app, or automatically on a scheduled basis.
# The shiny app saves a job request file to dropbox, which is copied by a cron job on jasmin.
# For scheduled jobs, another cron job adds job requests to the file at fixed intervals (daily).
# 
# There are two cron jobs:
# 1. generateAutomaticJobs.R - writes scheduled job requests to df_job_requests.csv every day
# 2. doRequestedJobs.R - create and submit jobs, and handle output
#    Specific tasks are:
# 1. create and run the requested jobs
#      read df_job_requests.csv
#      create and run jobs
#      if submittedOK
#      add to df_jobs with rbind.fill with status vars completedOK, processedOK, ...
# 2. detect when these have finished running, concatenate output and move to /public
#      for jobs in df_jobs 
#       completedOK == FALSE & concatenatedOK == FALSE  = still running, no action
#       completedOK == TRUE  & concatenatedOK == FALSE  = needs concatenating
#       completedOK == TRUE  & concatenatedOK == TRUE   = already concatenated, no action
#     add another task for processing further (met data, gap-filling, sums, plots) here?

# read job request file produced by shiny app
fname_requests <- "N:/0Peter/curr/ECsystem/eddystore/jobs/job_requests/df_job_requests.csv"
if (file.exists(fname_requests) == FALSE){
  # if no jobs requested, file won't exist, so exit
  quit(save = "no") # need to exit completely?
} else {
  df <- read.csv(fname_requests, stringsAsFactors = FALSE)
  df$startDate <- as.POSIXct(strptime(df$startDate, "%d/%m/%Y %H:%M"), tz = "UTC")
  df$endDate   <- as.POSIXct(strptime(df$endDate, "%d/%m/%Y %H:%M"), tz = "UTC")
  n_jobs <- dim(df)[1]
  summary(df)
  df$submitted <- vector(mode = "logical", length = n_jobs)
  df$jobID     <- vector(mode = "logical", length = n_jobs)
  l_jobs       <- vector(mode = "list",    length = n_jobs) # declare a list for job objects
  # add a check? if stationID is in df_projects
}

# check pending jobs in same script?
# if so, only do following if n_jobs > 0
# read data frame of eddypro project files
fname_projects <- "N:/0Peter/curr/ECsystem/eddystore/df_eddystore_projects.csv"
fname_projects <- "/gws/nopw/j04/eddystore/df_eddystore_projects.csv"
df_project <- read.csv(fname_projects, stringsAsFactors = FALSE)
df_project$startDate <- as.POSIXct(strptime(df_project$startDate, "%d/%m/%Y %H:%M"), tz = "UTC")
df_project$endDate   <- as.POSIXct(strptime(df_project$endDate, "%d/%m/%Y %H:%M"), tz = "UTC")
df_project$endDate
summary(df_project)

# for each requested job
for (i in 1:n_jobs){
  #i = 1
  ## pass eddystore path as an argument so we can run on PC
  ## pass df_project as an explicit argument?
  ## otherwise it is implicit
  l_jobs[[i]] <- createJob(df$stationID[i], df$procID[i], df$startDate[i], df$endDate[i], df$nProcessors[i])
  # need to add several arguments in function definition for tracking job
  #l_jobs[[i]] <- createJob(df$user_email[i], df$siteID[i], df$stationID[i], df$procID[i], df$startDate[i], df$endDate[i], df$nProcessors[i])
  l_jobs[[i]] <- runJob(l_jobs[[i]])
  con <- file(paste0("/public/", df$job_name[i], "_output.txt"))
  if (l_jobs[[i]]$err == 0){ # job submission worked
  df$submitted[i] <- TRUE
    txt <- paste("Job", df$job_name[i], "was submitted successfully.")
  } else { # it didnt
    txt <- paste("Job", df$job_name[i], "had an error on submission.")
  }
  writeLines(txt, con)
  close(con)
  Sys.sleep(10) # pause for 10 s
  #do something?
  may be no need if we write output to eddystore/public using 
  #BSUB -o /gws/nopw/j04/eddystore/public/%J.out
  #BSUB -e /gws/nopw/j04/eddystore/public/%J.err
  and e-mail with
  #BSUB -u mail_user
  #BSUB -N
https://www.ibm.com/support/knowledgecenter/en/SSETD4_9.1.2/lsf_command_ref/bsub.1.html
  )  
}

## combine all info one data frame - combine with pre-existing runs in same file?
## does this work for a list?
df <- as.data.frame(df, l_jobs)
# move successful requested jobs to submitted file
write.table(subset(df, submitted == TRUE), 
      file = "./jobs_submitted.csv", append = TRUE)
# and unsuccessful requested jobs to failed file
write.csv(subset(df, submitted == FALSE), 
      file = "/jobs/jobs_failedToSubmit.csv", append = TRUE)
# and delete the original request file 
# check cron job has permission to delete this file
file.remove(fname_requests)
# or rename
#file.rename(fname_requests, paste0(fname_requests, ".processed"))

# read jobs_submitted.csv
fname_submitted <- "./jobs_submitted.csv"
if (file.exists(fname_submitted) == FALSE){
  # if no jobs submitted, file won't exist, so exit
  quit(save = "no") # need to exit completely?
} else { 
  df <- read.csv(fname_submitted, stringsAsFactors = FALSE)
  n_jobs <- dim(df)[1]
  summary(df)
  df$completed <- vector(mode = "logical", length = n_jobs)
  # add a check? if stationID is in df_projects
}

# for each submitted job
for (i in 1:n_jobs){
  #i = 1
  cmd <- paste("bjobs -l ", df$jobID[i])
  err <- system(cmd)
  ## need to see what is returned by bjobs, to see if successfully completed or not 
  con <- file(paste0("/public/", df$job_name[i], "_output.txt"))
  if (err == 0){ # job completed
  df$completed[i] <- TRUE
    txt <- paste("Job", df$job_name[i], "was completed successfully.")
  } else { # it didnt
    txt <- paste("Job", df$job_name[i], "had an error before completion.")
  }
  writeLines(txt, con)
  close(con)
  Sys.sleep(10) # pause for 10 s
}

# another section for completed jobs?
# move successful requested jobs to submitted file
write.table(subset(df, completed == TRUE), 
      file = "./jobs_completed.csv", append = TRUE)
# and unsuccessful requested jobs to failed file
write.csv(subset(df, completed == FALSE), 
      file = "/jobs/jobs_failedToComplete.csv", append = TRUE)
# and delete the original request file 
# check cron job has permission to delete this file
if(file.exists(file.remove(jobs_submitted))
if(file.exists(file.remove(jobs_Notsubmitted)) ??
## this is getting complicated !!!

  ##do things
  do we know output file names?
  concatenate output
  move/copy to /public
  plot, gap-fill, sum
