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
library(eddystore)

# read job request file produced by shiny app
fname_job_requests <- "N:/0Peter/curr/ECsystem/eddystore/jobs/job_requests/df_job_requests.csv"
if (file.exists(fname_job_requests)){
  df_job_requests <- read.csv(fname_job_requests)
  n_jobs <- dim(df_job_requests)[1]
} else {
  n_jobs <- 0 
}
# if no jobs requested, exit
# n_jobs would be NA or zero?
if (n_jobs == 0) break # is exit completely?

# read data frame of eddypro project files
/gws/nopw/j04/eddystore

for (i in 1:n_jobs){
  
}

either delete the file 
write.csv(fname_job_requests, an empty data frame)