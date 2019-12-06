## R code for reading scheduled eddystore job requests and
## writing job request file.
## To be run as a cron job every day or few hours.
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
Sys.setenv(TZ="UTC")
nSecsPerDay <- 60*60*24

df <- read.csv("/gws/nopw/j04/eddystore/jobs/df_scheduledJobs.csv")
# user_email,job_name,siteID,stationID,procID,nDays,nProcessors
# plevy@ceh.ac.uk,JRA001,AM,Auch_icos,CO2_H2O,4,4
# plevy@ceh.ac.uk,JRA002,EB,EasterBush,CO2_H2O,4,4

# calculate start and end dates based on current time and nDays to process
df$startDate <- Sys.time() - (df$nDays * nSecsPerDay)
df$endDate <- Sys.time()
df$startDate <- trunc(df$startDate, units="hours") # round down to start of hour
df$endDate   <- trunc(df$endDate,   units="hours") # round down to start of hour
df$startDate <- format(df$startDate, "%d/%m/%Y %H:%M") # convert to character
df$endDate   <- format(df$endDate,   "%d/%m/%Y %H:%M") # convert to character

df$job_name <- paste(df$job_name, format(Sys.time(), "%Y_%m_%d_%H_%M"), sep = "_") # convert to character

# get columns in fixed order - matters if anything else appends to the file
df <- with(df, data.frame(user_email,job_name,siteID,stationID,procID,startDate,endDate,nProcessors))

fname_requests <- "/gws/nopw/j04/eddystore/jobs/job_requests/df_job_requests.csv" # JASMIN
#fname_requests <- "./df_job_requests.csv" # local
write.csv(df, file = fname_requests, quote = FALSE, row.names = FALSE)