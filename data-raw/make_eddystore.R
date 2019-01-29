rm(list=ls(all=TRUE))
#devtools::install_github("klutometis/roxygen")
#install.packages(c("devtools", "roxygen2"))
library(devtools)
library(roxygen2)

getwd()
#setwd("..")
setwd("./eddystore")
system.time(check())

# removed frm Depends
         # readxl,
         # readr,
         # dplyr,
         # tidyr,
         # stringr,
         # reshape2
# devtools::use_package("readxl") # Defaults to imports
# devtools::use_package("readr") # Defaults to imports
# devtools::use_package("dplyr") # Defaults to imports
# devtools::use_package("tidyr") # Defaults to imports
# devtools::use_package("stringr") # Defaults to imports
# devtools::use_package("reshape2") # Defaults to imports

# Define geographic projections to be used
# # lat / lon 
# projlonlat <- CRS("+proj=longlat +datum=WGS84")
# # OSGB 1936 / British National Grid 
# projOSGB <-  CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs")
# # UTM zone 34 for Stordalen, N Sweden
# projUTM34    <- CRS("+proj=utm +zone=34 +datum=WGS84 +units=m +no_defs")
# projUTM04 <-  CRS("+proj=utm +zone=4")
# projUTM05 <-  CRS("+proj=utm +zone=5")
# could put these in a vector or list, but how to then reference them?
#list_proj <- list(projlonlat, projOSGB, projUTM04, projUTM05, projUTM34)

# save(projlonlat, projOSGB, projUTM, file = "eddystore.RData")
#load(file = "eddystore.RData", verbose = TRUE)

#create("eddystore") # if it doesn't already exist
# Add functions in files to R/ directory 

#devtools::use_data(projlonlat, projOSGB, projUTM, overwrite = TRUE)
#devtools::use_data(ch4BySector, co2BySector, n2oBySector, internal = TRUE)
#C:/0Peter/curr/ECsystem/Footprint
document()
#devtools::document()
check_man()
#use_vignette("use_eddystore")
clean_vignettes()
build_vignettes()

# build the manual
#Sys.getenv(c("R_TEXI2DVICMD", "R_PAPERSIZE", "RD2PDF_INPUTENC"))
#Sys.setenv(RD2PDF_INPUTENC = "inputenx ")
pack <- "eddystore"
path <- find.package(pack)
system(paste(shQuote(file.path(R.home("bin"), "R")),"CMD", "Rd2pdf", shQuote(path)))
#C:/PROGRA~1/R/R-32~1.4RE/bin/x64/R R CMD Rd2pdf --no-clean N:/0Peter/prop/UKinverseFlux/GHG_TAP/DelD/anthEmis/luv

build()
#build(manual = TRUE, vignettes = FALSE)
#build(binary = TRUE)
#check()
system.time(check())

setwd("..")
install("eddystore")
#install.packages("./eddystore_0.1.tar.gz", repos = NULL, type="source")

detach("package:eddystore", unload=TRUE)
.libPaths()
remove.packages("eddystore")