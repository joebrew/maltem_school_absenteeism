#loading libraries
library(RMySQL)
library(dplyr)
library(DBI)
library(readxl)

# Source some helpers
source('lib/collect_and_disconnect.R')

# Get stuff from config
connection_options <- yaml::yaml.load_file('credentials.yaml')

# Open connection using dplyr
con <- do.call('src_mysql', connection_options)

# Tables to read
tables <- src_tbls(con)

# Read them all in
for (i in 1:length(tables)){
  message(paste0('Reading from the database: ', tables[i]))
  try({assign(tables[i],
              tbl(con, 
                  tables[i]) %>% 
                collect
              ,
              envir = .GlobalEnv)})
}

# Disconnect form the database
dbDisconnect(con)

# Save a snapshot
save.image(paste0('snapshots/',
                  Sys.Date(),
                  '.RData'))

# Get a dataframe of all students for all days
all_students <- 
  form_c_core %>%
  rename(student_id = uri)
