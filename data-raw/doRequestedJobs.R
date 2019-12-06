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
#library(devtools)
#install_github("NERC-CEH/eddystore", auth_token = "cf75f3ae2091f58e6dd664ce9031bee3aa98f0f8")
library(eddystore)
library(rdrop2)

# check for manually requested jobs produced by shiny app on dropbox
# Providing a previous token stored in a file
#drop_auth(rdstoken = "N:/0Peter/curr/ECsystem/eddystore/scripts/droptoken.rds")
drop_auth(rdstoken = "/gws/nopw/j04/eddystore/scripts/droptoken.rds")
# app_tests is temporary location for testing
# eddystore_jobs is long term location
new_manual_jobs <- drop_exists("eddystore_jobs/df_job_requests.csv")
if (new_manual_jobs){
  df_manual_jobs <- drop_read_csv("eddystore_jobs/df_job_requests.csv", stringsAsFactors = FALSE)
  drop_move("eddystore_jobs/df_job_requests.csv", "eddystore_jobs/df_job_requests_submitted.csv", verbose = TRUE)
  drop_delete("eddystore_jobs/df_job_requests.csv", verbose = FALSE)
}

# check for automated requested jobs on jasmin
#fname_requests <- "N:/0Peter/curr/ECsystem/eddystore/jobs/job_requests/df_job_requests.csv"
fname_requests <- "/gws/nopw/j04/eddystore/jobs/job_requests/df_job_requests.csv"
new_auto_jobs   <- file.exists(fname_requests)
if (new_auto_jobs){
  df_auto_jobs <- read.csv(fname_requests, stringsAsFactors = FALSE)
  # and rename the original request file 
  file.rename(fname_requests, paste0(fname_requests, ".processed"))
}

# if there are job requests to process ...
if (new_auto_jobs | new_manual_jobs){
  # if both auto and manual job requests, combine both
  if (new_auto_jobs & new_manual_jobs)  df <- rbind(df_auto_jobs, df_manual_jobs)
  # if auto but no manual job requests
  if (new_auto_jobs & !new_manual_jobs) df <- df_auto_jobs
  # if no auto but manual job requests
  if (!new_auto_jobs & new_manual_jobs) df <- df_manual_jobs

  df$startDate <- as.POSIXct(strptime(df$startDate, "%d/%m/%Y %H:%M"), tz = "UTC")
  df$endDate   <- as.POSIXct(strptime(df$endDate, "%d/%m/%Y %H:%M"), tz = "UTC")
  n_jobs <- dim(df)[1]
  print(paste("There are", n_jobs, "new jobs to submit."))
  #summary(df)
  df$submitted <- vector(mode = "logical", length = n_jobs)
  df$failed    <- vector(mode = "logical", length = n_jobs)
  df$completed <- vector(mode = "logical", length = n_jobs)
  # declare a list to hold job objects
  l_jobs       <- vector(mode = "list",    length = n_jobs) # declare a list for job objects

  # add a check? if stationID is in df_projects
  # read data frame of eddypro project files
  #fname_projects <- "N:/0Peter/curr/ECsystem/eddystore/df_eddystore_projects.csv"
  fname_projects <- "/gws/nopw/j04/eddystore/eddystore_projects/df_eddystore_projects.csv"
  df_project <- read.csv(fname_projects, stringsAsFactors = FALSE)
  df_project$startDate <- as.POSIXct(strptime(df_project$startDate, "%d/%m/%Y %H:%M"), tz = "UTC")
  df_project$endDate   <- as.POSIXct(strptime(df_project$endDate, "%d/%m/%Y %H:%M"), tz = "UTC")
  # df_project$endDate
  # summary(df_project)

  # for each requested job
  for (i in 1:n_jobs){
    #i = 1
    ## pass eddystore path as an argument so we can run on PC more easily?
    l_jobs[[i]] <- createJob(
                    stationID_proc = df$stationID[i], 
                    procID_proc = df$procID[i], 
                    startDate_period = df$startDate[i], 
                    endDate_period = df$endDate[i], 
                    nProcessors = df$nProcessors[i],
                    #fname_df_project = "C:/Users/plevy/Documents/eddystore_projects/df_eddystore_projects.csv", 
                    # fname_df_project = "/gws/nopw/j04/eddystore/eddystore_projects/df_eddystore_projects.csv", 
                    # binpath = "/gws/nopw/j04/eddystore/eddypro-engine_6.2.0/eddypro-engine/bin/linux/eddypro_rp", 
                    # switch_OS = "-s linux", 
                    #eddystore_path = "N:/0Peter/curr/ECsystem/eddystore", 
                    job_name = df$job_name[i], 
                    user_email = df$user_email[i])

    l_jobs[[i]] <- runJob(l_jobs[[i]])
    Sys.sleep(2) # pause for 2 s to avoid overload
    
    # l_jobs[[i]]$err
    # checkJobCompleted(l_jobs[[i]]$job_name)
    # checkJobFailed(l_jobs[[i]]$job_name)
    # checkJobRunning(l_jobs[[i]]$job_name)
    
    # report progress to files in public folder
    con <- file(paste0("/gws/nopw/j04/eddystore/public/", df$job_name[i], "_report.txt"))
    if (l_jobs[[i]]$err == 0){ # job submission worked
      df$submitted[i] <- TRUE
      txt <- paste("Job", l_jobs[[i]]$job_name, "was submitted successfully at", l_jobs[[i]]$job_startTime)
    } else { # it didnt
      txt <- paste("Job", l_jobs[[i]]$job_name, "had an error when submitted at", l_jobs[[i]]$job_startTime)
    }
    writeLines(txt, con)
    close(con)
  }

  # combine all job requests in one data frame 
  l_df_jobs <- lapply(l_jobs, as.data.frame)
  #str(l_jobs)
  # get first row only in each data frame
  l_df_jobs <- lapply(l_df_jobs, function(l) l[1,])
  df_jobs <- bind_rows(l_df_jobs)
  #str(df_jobs)
  #names(df)
  #names(df_jobs)
  df <- merge(df, df_jobs, by = "job_name", suffixes = c("",".sub"))

  # report unsuccessful requested jobs to failure file
  write.table(subset(df, submitted == FALSE), sep=",", 
        file = "./jobs_failedToSubmit.csv", append = TRUE)
  # and remove them
  df <- subset(df, submitted == TRUE)

  # read previously submitted jobs
  # we add requested jobs to this now they have been successfully submitted
  #df_submitted <- read.csv(file = "/gws/nopw/j04/eddystore/jobs/jobs_submitted.csv", stringsAsFactors = FALSE)
  #load(file = "N:/0Peter/curr/ECsystem/eddystore/eddystore/jobs/jobs_submitted.RData", verbose = TRUE)
  load(file = "/gws/nopw/j04/eddystore/jobs/jobs_submitted.RData", verbose = TRUE)

  df_submitted <- rbind(df_submitted, df)
  # dim(df_submitted)
  # dim(df)
  # write successfully submitted jobs to file
  write.csv(df_submitted, file = "/gws/nopw/j04/eddystore/jobs/jobs_submitted.csv")
  save(df_submitted,      file = "/gws/nopw/j04/eddystore/jobs/jobs_submitted.RData")
}
