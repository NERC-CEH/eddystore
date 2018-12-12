## *****************************************************************************
## *****************************************************************************
## Run Rscript 
##
## Olivia Hitt, CEH Wallingford, January 2017
## Edited for GHG project 2018 (Hollie Cooper)
##
## Wrapper script for running COSMOS-UK data system scripts.
##
## ''''''''''''
## Command line arguments:
##		1. live/dev - system to run script in
##		2. name of script to run (not including file path)
##		3. (optional) full path to control file. If not included default is control file in SourceDIR
##
## ''''''''''''
## Programme outline
##		1. Get command line args
##		2. Set SourceDIR depending on if running in live or dev environment
##		3. Find control file and read in options from control file
##		4. Load packages
##		5. Source required scripts
##		6. Set database connection
##		7. Set datetime settings in R and Oracle
##		8. Set error reporting options
##		9. Run script!
##
## *****************************************************************************
## *****************************************************************************
## -----------------------------------------------------------------------------
rm(list=ls())

## 1. Get command line args 
#args <- c('dev', 'COSMOS_AddNewSite.r',"P:\\NEC04961_COSMOSUK\\Site Information\\Cochno_Farm\\Technical\\COCHN.xlsx")
#args <- c('dev', 'GHG_DataChecker.r',"E:/LocalData/GHG_Task_3/ghg-scripts-live")
args <- commandArgs(trailingOnly=TRUE)

system <- args[1]
script <- args[2]

## -----------------------------------------------------------------------------
## 2. Set SourceDIR depending on if running in live or dev environment
#PullSourceDIR <- 'P:\\NEC04526_Defra_Lowland_Peatlands\\GHG_Task_3\\ghg-scripts-pull\\Scripts\\cosmos-r\\'

if(system=='live'){
  
  # Set live source directory path
  SourceDIR <- 'P:\\NEC04526_Defra_Lowland_Peatlands\\GHG_Task_3\\ghg-scripts-live\\'
  print('****************************************************')
  print(paste('Running ', script, 'on live GHG system'))
  print('****************************************************')
  
}else{if(system=='dev'){
  
  # Set dev source directory path
  SourceDIR <- 'P:\\NEC04526_Defra_Lowland_Peatlands\\GHG_Task_3\\ghg-scripts\\'
  SourceDIR <- 'E:/LocalData/GHG_Task_3/ghg-scripts-live/'
  print('****************************************************')
  print(paste('Running ',script, 'on developing GHG system'))
  print('****************************************************')
  
}else{
  print('Invalid system option. Enter either live or dev')
  print('Quitting GHG_RunScript...')
  quit()
}}

## -----------------------------------------------------------------------------
## 3. Read in other arguments (varies depending on which script is being run)
##      DataChecker.r : Control file. If no control file given, load from source directory
## Currently only called datachecker, possibly needs changing?
##      GetCalValues.r : Spreadsheet with lab analysis
if(length(args)==2){
  ControlFile <- paste(SourceDIR, 'GHG_DataChecker_ControlFile.txt', sep="")
  print(paste('Loading control file: ', ControlFile))
}else{
  if(length(args)==3){
  ControlFile <- args[3]
  print(paste('Loading control file: ', ControlFile))
  }
}
# Read in control file options
#script = 'GHG_DataChecker.r'
if(script=='GHG_DataChecker.r'){
  df <- read.table(ControlFile, sep='=', col.names=c('Variable', 'Value'),comment.char='', stringsAsFactors=F,
                     strip.white=T)
    
  # Assign options to variables
  start <- as.POSIXct(df[df$Variable=='Start', 'Value'], format='%d-%m-%Y', tz='GMT')
  NDays <- df[df$Variable=='NDays', 'Value']
  Load <- df[df$Variable=='Load', 'Value']
  uid <- df[df$Variable=='uid', 'Value']
  pwd <- df[df$Variable=='pwd', 'Value']
  dsn <- df[df$Variable=='dsn', 'Value']
  LoadFullOutput <- df[df$Variable=='LoadFullOutput', 'Value']
  #LoadSoilMet <- df[df$Variable=='LoadSoilMet', 'Value']
  #LoadPrecip <- df[df$Variable=='LoadPrecip', 'Value']
  LogFILEPATH <- df[df$Variable=='Log', 'Value']
  Sites <- unlist(strsplit((df[df$Variable=='Site', 'Value']), ','))
  
  # Return options to R environment for use in rest of script.
  Settings <- list(start=start, NDays=NDays, dsn=dsn, uid=uid, pwd=pwd, Load=Load,  
  		  LogFILEPATH=LogFILEPATH,Sites=Sites)
  
  list2env(Settings, envir=environment()) # Load control file settings to environment
}
if(script=='COSMOS_GetCalValues.r'){
  SiteInfoCalib_csv <- read.csv(ControlFile)
}

if(script=='COSMOS_Soil_samples_generator.r'){
  #Instrument_locations_csv <- read.csv(ControlFile)
  sites <- args[3]
}
if(script=='COSMOS_AddNewSite.r'){
  siteinfo <- args[3]
}

## -----------------------------------------------------------------------------
## 4. Load packages
## Packages required for scripts"
list.of.packages <- c("RODBC", "sendmailR", "data.table","lattice", "XLConnect", "sp", "reshape2","foreign", "rgdal", "plyr", "spatial")

## Find any packages that are missing and install 
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) {install.packages(new.packages, repos="https://cloud.r-project.org")}

## Load all packages to environemnt
for(p in list.of.packages){require(p, character.only = T)}
#lapply(list.of.packages, require, character.only=T)

## -----------------------------------------------------------------------------
## 5.Source required scripts

source(paste(SourceDIR, 'GHG_Constants.r', sep=""))
source(paste(SourceDIR, 'GHG_RFunctionLibrary.r', sep=""))
source(paste(SourceDIR, 'GHG_FilePaths.r', sep=""))

## -----------------------------------------------------------------------------
## 6. Set database connection
#chan <- odbcConnect(dsn=dbdsn, uid=dbuid, pwd=dbpwd)
# For connecting to the database -----------------------------------------------
dbdsn <- "Test_Oracle"
dbuid <- "BU_FIELD_SITES" 
dbpwd <- "0ig2mtYUL9" 

dsn = 
uid = 
pwd = 

chan <- odbcConnect(dsn=dbdsn, uid=dbuid, pwd=dbpwd, believeNRows=FALSE)
table_name <- "EP_PROC"
sqlColumns(chan, table_name)[,'COLUMN_NAME']

sql <- paste0("SELECT * FROM ", table_name, " WHERE SITEID = '", qrySite, "';")
  sql <- paste("SELECT * FROM ", table_name, " WHERE SITEID = '",qrySite,
               "' AND Date_Time > '",format(as.POSIXct(FirstDateTimedb,format="%d-%b-%Y")-60*60*24,format="%d-%b-%Y"),
               "' AND Date_Time < TO_DATE('",LatestDateStampdbNextDay," 00:30', 'DD-MON-RRRR HH24:MI')",
               " ORDER BY Date_Time;", sep = "")
  dbData <- sqlQuery (chan, sql)
str(dbData)

## -----------------------------------------------------------------------------
## 7. Set datetime settings in R and Oracle
sqlQuery(chan, "alter session set nls_date_format ='DD-MON-YYYY HH24:MI';") # In Oracle
Sys.setenv(TZ="GMT") # In R 

## -----------------------------------------------------------------------------
## 8. Set error reporting options
options(error = quote({SendError(MailFrom, MailTo, geterrmessage(), traceback(), script)}))

## -----------------------------------------------------------------------------
## 9. Run script
starttime <- Sys.time()
print('********************************************')
print(paste(script, ' started at: ', starttime))
print('********************************************')

source(paste(SourceDIR, script, sep=""))

print('********************************************')
print(paste(script, ' finished at: ', Sys.time()))
print(paste('    - Script run time= ', round(Sys.time()-starttime, 2)))
print('********************************************')

# Close RODBC channel
close(chan)
