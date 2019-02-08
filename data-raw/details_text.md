eddystore automates the processing of eddy covariance flux data using eddypro in parallel

eddystore comprises the JASMIN storage and computation hardware
and three pieces of software: 
1. the eddypro fortran program which performs the flux calculations
2. an R package "eddystore" which contains:
     functions which translate the user processing requirements into computation instructions on jasmin, and
     R scripts which carry these instructions out on a scheduled basis as cron jobs
3. a shiny app which allows jobs to be run on jasmin via a web browser
Processing jobs can be created manually with a shiny app, or automatically on a scheduled basis.
The shiny app saves a job request file to dropbox, which is copied by a cron job on jasmin.
For scheduled jobs, another cron job adds job requests to the file at fixed intervals (daily).

There are two cron jobs:
1. scheduleJobs.R - writes scheduled job requests to df_job_requests.csv every day
2. doRequestedJobs.R - create and submit jobs, and handle output
   Specific tasks are:
1. create and run the requested jobs
     read df_job_requests.csv
     create and run jobs
     if submittedOK
     add to df_jobs with rbind.fill with status vars completedOK, processedOK, ...
2. detect when these have finished running, concatenate output and move to /public
     for jobs in df_jobs 
      completedOK == FALSE & concatenatedOK == FALSE  = still running, no action
      completedOK == TRUE  & concatenatedOK == FALSE  = needs concatenating
      completedOK == TRUE  & concatenatedOK == TRUE   = already concatenated, no action
    add another task for processing further (met data, gap-filling, sums, plots) here?

