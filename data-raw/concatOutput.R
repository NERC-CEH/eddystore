## R code for tracking eddystore jobs submitted to LOTUS jobs.
## If failed, stop tracking; if completed, concatenate output
## Only track jobs submitted in the last 72 hours
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

# reload submitted jobs from file
load(file = "/gws/nopw/j04/eddystore/jobs/jobs_submitted.RData", verbose = TRUE)
df <- df_submitted

# only track jobs submitted in the last 72 hours
df <- subset(df, difftime(Sys.time(), df$job_startTime, units = "hours") < 72)

# and compare with df 
for (i in 1:length(df[,1])){
  #i = 9
  # for each job submitted but not previously completed or failed, 
  # check if it has failed or still running
  completedSinceLastCheck <- FALSE
  if (df$submitted[i] & !df$completed[i] & !df$failed[i]){
    df$failed[i] <- checkJobFailed(df$job_name[i])
    runningNow   <- checkJobRunning(df$job_name[i])
    # if submitted but not now running or failed, it should have completed:
    if (!runningNow & !df$failed[i]){ 
      completedSinceLastCheck <- TRUE
     df$job_endTime[i] <- Sys.time()
     #df$job_runTime[i] <- df$job_endTime[i] - df$job_startTime[i]
    }
  }

  if (completedSinceLastCheck){ # check if newly completed
    df$completed[i] <- TRUE
    con <- file(paste0("/gws/nopw/j04/eddystore/public/", df$job_name[i], "_report.txt"))
    txt <- paste("Job", df$job_name[i], " has finished running.")
    writeLines(txt, con)
    close(con)
    # concatenate output and write to /public
    #str(df_essn)
    #duplicated(df_essn$datect)
    df_essn <- get_essential_output_df(df$job_name[i], df$station_dir[i])
    #df_essn <- get_essential_output_df_byTime(df$job_name[i], df$station_dir[i], df$job_startTime[i])
    df_essn <- df_essn[!duplicated(df_essn$datect), ]
    fname <- paste0("/gws/nopw/j04/eddystore/public/", df$job_name[i], "_essentials.csv")
    write.csv(df_essn, file = fname, row.names = FALSE)
    fname <- paste0("/gws/nopw/j04/eddystore/public/", df$job_name[i], "_essentials.RData")
    save(df_essn, file = fname)
    ## send a e-mail to df$user_email[i] to notify job completed
 }
}

# write submitted jobs to file with updated completion status
df_submitted <- df
save(df_submitted, file = "/gws/nopw/j04/eddystore/jobs/jobs_submitted.RData")
write.csv(df_submitted, file = "/gws/nopw/j04/eddystore/jobs/jobs_submitted.csv")


##other thingsz
#merge with met, plot, gap-fill, sum
