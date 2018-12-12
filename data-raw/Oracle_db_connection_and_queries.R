source('secrets.R') # Contains Oracle password

# Set timezone to GMT to stop R/Oracle changing dates based on daylight saving time
Sys.setenv(TZ = "GMT")
Sys.setenv(ORA_SDTZ = "GMT")

# Create connection to database
con <- dbConnect(ora,
                   dbname = "budbase.nerc-bush.ac.uk/BUA",
                   username = "DBUSER",
                   password = .oraPwd)
				   

# Data frame from SELECT query
qryStartEndDates <- "select START, END from MARGA"
startEndDates <- dbGetQuery(con, qryStartEndDates)

# Data frame from reading whole table 'MARGA'
marga <- dbReadTable(con, 'MARGA')

# Write new table 'NEW_TABLE' from data frame 'marga'
dbWriteTable(con, 'NEW_TABLE', marga)

# Append data frame to existing table
dbWriteTable(con, 'EXISTING_TABLE', marga, append = TRUE)

# Update existing records in database using an update query
# Either use paste() to create query, or safer (especially for web apps) to use a parameter query.
# For a parameter query, use :1, :2, etc. for each parameter, and either create or use an existing data frame
# to supply the parameters.
res <- dbSendQuery(con, "update MARGA_DATE_RANGE set NOTE = :1 where ID = 1",
                         data = data.frame(NOTE = noteText))
dbClearResult(res) # Frees up database cursors
dbCommit(con) # Essential for the update to occur
				   
# Disconnect from database - always include this at the end of script
dbDisconnect(con)