## ----rendering, eval=FALSE, echo=FALSE-----------------------------------
#  library(rmarkdown)
#  system.time(render("use_eddystore.Rmd", output_file = "use_eddystore.html"))
#  system.time(render("use_eddystore.Rmd", output_file = "use_eddystore.pdf"))
#  system.time(render("use_eddystore.Rmd", output_file = "use_eddystore.docx"))
#  knitr::purl("use_eddystore.Rmd")
#  tools::texi2pdf("use_eddystore.tex")
#  shell.exec("use_eddystore.pdf")

## ----startup, eval=TRUE, echo=TRUE, include=TRUE-------------------------
rm(list=ls(all=TRUE))
library(eddystore)
# library(dplyr)
# library(stringr)
library(RCurl)
#library(foreign)

# eddypro project data on github
#switch to cvs here - don't need readxl, and plain text for version control
url <- "https://raw.githubusercontent.com/NERC-CEH/eddystore_projects/master/df_eddystore_projects.csv"
tmp_txt <- RCurl::getURL(url)                
df_project <- read.csv(textConnection(tmp_txt), stringsAsFactors = FALSE)

# eddypro project data on local disk
#df_project <- read_excel("C:/Users/plevy/Documents/eddystore/data-raw/eddystore_proc_table.xlsx")
str(df_project)
df_project$startDate <- as.POSIXct(strptime(df_project$startDate, "%d/%m/%Y %H:%M"), tz = "UTC")
df_project$endDate   <- as.POSIXct(strptime(df_project$endDate, "%d/%m/%Y %H:%M"), tz = "UTC")
df_project$endDate

getwd()
#setwd("./eddystore/vignettes/")

# ?eddystore
# ?makeDateIntervals
# ?adjustIntervals
# ?writeProjectFiles
# ?writeJobFileForIntervals

# Processing one year of data took 50 mins with 100 processors (11/10/2018)
# Processing one year of data took 19 mins with 200 processors (12/10/2018)
# Processing one year of data took 29 mins with 365 processors (12/10/2018)
# Processing one year of data took ~300 mins with 1 processor
# Speed-up = 300/20 = 15
# time_taken <- c(300, 50, 19, 29)
# n_cpu <- c(1, 100, 200, 365)
# plot(n_cpu, time_taken)



## ----convertProjectPath, eval=FALSE, echo=TRUE, include=TRUE-------------
#  # convert file paths in project file to eddystore locations
#  eddyproProjectPathName <- "/gws/nopw/j04/eddystore/stations/Balmoral/projects/Balmoral.eddypro"
#  eddyproProjectPathName <- "/gws/nopw/j04/eddystore/stations/EasterBush/projects/EB_2013.eddypro"
#  station_dir            <- "/gws/nopw/j04/eddystore/stations/EasterBush"
#  convertProjectPath(eddyproProjectPathName, station_dir)

## ----make_job_request, eval=FALSE, echo=TRUE, include=TRUE---------------
#  

## ----run_example, eval=FALSE, echo=TRUE, include=TRUE--------------------
#  # how many intervals to split processing run into
#  nIntervals = 4
#  # station for processing run
#  stationID_proc <- "EasterBush"
#  # processing configuration for processing run
#  procID_proc <- "CO2_H2O"
#  # start/end dates for processing run
#  startDate_period <- "2006-07-01 00:00"
#  endDate_period   <- "2007-12-31 23:30"
#  startDate_period <- as.POSIXct(strptime(startDate_period, "%Y-%m-%d %H:%M"), tz = "UTC")
#  endDate_period   <- as.POSIXct(strptime(endDate_period,   "%Y-%m-%d %H:%M"), tz = "UTC")
#  startDate_period; endDate_period
#  difftime(endDate_period, startDate_period, units = "days")
#  
#  intervals <- makeDateIntervals(startDate_period, endDate_period, nIntervals)
#  intervals
#  
#  myJob <- adjustIntervals(stationID_proc, procID_proc, intervals)
#  myJob
#  
#  myJob <- writeProjectFiles(myJob)
#  myJob
#  
#  myJob <- writeJobFile(myJob, binpath = "N:/0Peter/curr/ECsystem/eddypro",
#                              switch_OS = "-s linux",
#                              eddystore_path = "N:/0Peter/curr/ECsystem/eddystore")
#  myJob
#  as.data.frame(myJob)
#  View(as.data.frame(myJob))
#  # all in a oner
#  nProcessors <- 4
#  # how many intervals to split processing run into
#  nIntervals = 4
#  # station for processing run
#  stationID_proc <- "Balmoral"
#  # processing configuration for processing run
#  procID_proc <- "CO2_H2O"
#  # start/end dates for processing run
#  startDate_period <- "2018-07-03 00:00"
#  endDate_period   <- "2019-01-31 23:30"
#  startDate_period <- as.POSIXct(strptime(startDate_period, "%Y-%m-%d %H:%M"), tz = "UTC")
#  endDate_period   <- as.POSIXct(strptime(endDate_period,   "%Y-%m-%d %H:%M"), tz = "UTC")
#  startDate_period; endDate_period
#  difftime(endDate_period, startDate_period, units = "days")
#  
#  myJob <- createJob(stationID_proc, procID_proc, startDate_period, endDate_period, nProcessors)
#  
#  # test at command line running outwith queue
#  #/gws/nopw/j04/eddystore/eddypro-engine_6.2.0/eddypro-engine/bin/linux/eddypro_rp -s linux -e /gws/nopw/j04/eddystore/stations/EasterBush /gws/nopw/j04/eddystore/stations/EasterBush/projects/processing2.eddypro
#  
#  myJob <- runJob(myJob)
#  myJob$err
#  
#  # use bjobs at the command line to monitor progress
#  
#  df_essn <- get_essential_output_df(myJob$job_startTime)
#  dim(df_essn)
#  str(df_essn)
#  
#  
#  

## ----old_stuff, eval=FALSE, echo=TRUE, include=TRUE----------------------
#  
#  cmd <- paste0("bsub < ", jobFileName)
#  cmd
#  # submit the jobs and get the time to identify the output files from this batch
#  err <- system(cmd); job_time <- Sys.time()
#  err; job_time
#  
#  
#  to_match <- paste0("eddypro_job._full_output_",  YYYY, "-", mm, "-", dd, "T", HH, substr(MM, 1, 1))
#  list.files(path = "./stations/EasterBush/output", pattern = to_match, full.names = TRUE)
#  files_out <- list.files(path = "./stations/EasterBush/output", pattern = to_match, full.names = TRUE)
#  df_full <- do.call(rbind, lapply(files_out, FUN = read_full_output))
#  dim(df_full)
#  head(df_full[,1:9])
#  str(df_full[,1:9])
#  
#  df_essn <- get_essential_output_df(job_time)
#  dim(df_essn)
#  str(df_essn)
#  
#  p <- ggplot(df_essn, aes(datect, H))
#  p  <- p + geom_line()
#  # p  <- p + geom_point()
#  p
#  
#  dir(path = "./stations/EasterBush/output")
#  Filter(function(x) grepl("eddypro_job", x), dir(path = "./stations/EasterBush/output"))
#  
#  # EB_test works ok
#  # EasterBush - on;y first job produced output, althopugh no errors
#  # guess we need to sort out time as well as date?
#  # using POSIXct time format
#  
#  test <- adjustIntervals(stationID_proc, procID_proc, intervals)
#  intervals <- test
#  test <- adjustIntervals(stationID_proc, procID_proc, test)
#  
#  df_b <- data.frame(
#    interval = 1:nIntervals,
#    start = intervals$startDate,
#    end  = intervals$endDate)
#  # df_b <- gather(df_b, interval, value = date)
#  df_b <- melt(df_b, id.vars = "interval", value.name = "date")
#  
#  df_b$date <- as.POSIXct(df_b$date, origin = "1970-01-01", tz = "GMT")
#  df_b$y <- 0
#  df_b$y[df_b$variable == "end"] <- 0.75
#  
#  df_a <- data.frame(
#    interval = 1:nIntervals,
#    start = test$startDate,
#    end   = test$endDate)
#  df_a <- melt(df_a, id.vars = "interval", value.name = "date")
#  df_a$date <- as.POSIXct(df_a$date, origin = "1970-01-01", tz = "GMT")
#  df_a$y <- 0
#  df_a$y[df_a$variable == "end"] <- 1
#  
#  p <- ggplot(df_b, aes(date, y))
#  p <- p + geom_vline(data = dfs, aes(xintercept = startDate), size = 3)
#  p <- p + geom_line(colour = "blue")
#  p <- p + geom_line(data = df_a, colour = "red")
#  p

