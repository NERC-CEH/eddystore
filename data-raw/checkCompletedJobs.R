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

# this could be a separate cron job
# reload submitted jobs form file
load(file = "/gws/nopw/j04/eddystore/jobs/jobs_submitted.RData", verbose = TRUE)
df <- df_submitted
# for each submitted job, check if now completed
## only recent jobs listed? Returns NA if job_name does not appear?
completedNow <- sapply(df$job_name, checkJobCompleted)

# and compare with df 
for (i in 1:length(df[,1])){
  #i = 1
  if (completedNow[i] == TRUE & df$completed[i] == FALSE){ # check if newly completed
    df$completed[i] <- TRUE
    con <- file(paste0("/gws/nopw/j04/eddystore/public/", df$job_name[i], "_report.txt"))
    txt <- paste("Job", df$job_name[i], "was completed successfully.")
    writeLines(txt, con)
    close(con)
    # concatenate output and move to /public
    #str(df_essn)
    df_essn <- get_essential_output_df(df$job_name[i], df$station_dir[i])
    fname <- paste0("/gws/nopw/j04/eddystore/public/", df$job_name[i], "_essentials.csv")
    write.csv(df_essn, file = fname, row.names = FALSE)
    ## send a e-mail to df$user_email[i] to notify job completed
 }
}

# write successfully submitted jobs to file
df_submitted <- df
save(df_submitted, file = "/gws/nopw/j04/eddystore/jobs/jobs_submitted.RData")

  ##other things
  #merge with met, plot, gap-fill, sum
