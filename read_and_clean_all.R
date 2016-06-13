# Packages
library(ggthemes)
library(dplyr)
library(xtable)
library(ggplot2)
library(knitr)
library(png)
library(grid)
library(extrafont)
library(tidyr)
library(gridExtra)
library(readxl)
library(raster)
library(maptools)
library(rgeos)
library(readr)

# Helpers
source('helpers.R')

# Loop through all the data from phase 1, reading it in
data_files <- dir('data')
for (i in 1:length(data_files)){
  phase_1_read_data(data_files[i])
}

##### Go through each file, keeping only 
# the necessary columns, and joining together
# (for now, ignoring form d / teachers)

# Narrow down form A core: school
form_a_core <-
  form_a_core %>%
  dplyr::select(SCHOOL_UUID,
                GPS_ALT,
                GPS_ACC,
                DISTRICT,
                TYPE_OF_LATRINE,
                SCHOOL_CONDITIONS,
                SCHOOL_NAME,
                # `valid schools`,
                GPS_LAT,
                GPS_LNG,
                LUNCH,
                NUMBER_OF_CLASSES)
# Read in the valid schools info
# to standardize school names
valid_schools <- read_excel('misc/School names.xlsx')

# Fix names
form_a_core <- 
  form_a_core %>%
  left_join(valid_schools %>%
              dplyr::select(`School names…`,
                            `Correct name`),
            by = c('SCHOOL_NAME' = 'School names…')) %>%
  mutate(SCHOOL_NAME = `Correct name`) %>%
  dplyr::select(-`Correct name`)

# Narrow down form B core: turmas
form_b_core <-
  form_b_core %>%
  dplyr::select(`_URI`,
                SCHOOL_UUID,
                CLASS_UUID,
                CLASS_NAME,
                GRADE,
                FIRST_MONTH_OF_REFERENCE,
                SECOND_MONTH_OF_REFERENCE,
                THIRD_MONTH_OF_REFERENCE)

# Narrow down the form_b_month_n sheets: each day is a turma-festivo pair
form_b_month_one <- 
  form_b_month_one %>%
  mutate(month = 1,
         day = DAYS_ONE) %>%
  dplyr::select(`_PARENT_AURI`,
                month,
                day)
form_b_month_two <- 
  form_b_month_two %>%
  mutate(month = 2,
         day = DAYS_TWO) %>%
  dplyr::select(`_PARENT_AURI`,
                month,
                day)
form_b_month_three <- 
  form_b_month_three %>%
  mutate(month = 3,
         day = DAYS_THREE) %>%
  dplyr::select(`_PARENT_AURI`,
                month,
                day)

# Join all holidays together
holidays <- 
  rbind(form_b_month_one,
        form_b_month_two,
        form_b_month_three)

# Join the turma data to the holidays data
# in order to get a dataframe of each holiday

# First, need to get the actual month in holidays
add_zero <- function(x){
  x <- as.character(x)
  ifelse(nchar(x) == 1,
         paste0(0, x),
         x)
}

# Get real dates into holidays
temp <- 
  holidays %>%
  left_join(form_b_core %>%
              dplyr::select(`_URI`,
                            FIRST_MONTH_OF_REFERENCE,
                            SECOND_MONTH_OF_REFERENCE,
                            THIRD_MONTH_OF_REFERENCE),
            by = c('_PARENT_AURI' = '_URI')) %>%
  mutate(FIRST_MONTH_OF_REFERENCE = add_zero(FIRST_MONTH_OF_REFERENCE),
         SECOND_MONTH_OF_REFERENCE = add_zero(SECOND_MONTH_OF_REFERENCE),
         THIRD_MONTH_OF_REFERENCE = add_zero(THIRD_MONTH_OF_REFERENCE),
         day = add_zero(day))
# rename the buggy january as september (laias instructions)
temp$THIRD_MONTH_OF_REFERENCE[temp$THIRD_MONTH_OF_REFERENCE == '01'] <- '09'

# Divide temp into 3 months, and create dates
temp$date <-
  as.character(ifelse(temp$month == 1,
                      (paste0('2015-', 
                              temp$FIRST_MONTH_OF_REFERENCE, 
                              '-',
                              temp$day)),
                      ifelse(temp$month == 2,
                             (paste0('2015-', 
                                     temp$SECOND_MONTH_OF_REFERENCE, 
                                     '-',
                                     temp$day)),
                             ifelse(temp$month == 3,
                                    (paste0('2015-',
                                            temp$THIRD_MONTH_OF_REFERENCE,
                                            '-',
                                            temp$day)),
                                    NA))))
temp$date <- as.Date(temp$date)

# Removing the buggy 00 day rows
temp <- temp[temp$day != '00',]

# Write over holidays
holidays <- temp %>%
  # dplyr::select(`_PARENT_AURI`, date) %>%
  rename(`turma_id` = `_PARENT_AURI`)
# Holidays is now a one row = one school holiday

# !!!
# What is going on with this:
# table(form_c_days_first$DAYS_OF_ABSENCE_FIRST_MONTH)

# Form C : students
form_c_core <- 
  form_c_core %>%
  dplyr::select(`_URI`, 
                NAME,
                PERMID,
                SCHOOL_UUID,
                CLASS_UUID,
                DOB,
                HOUSEHOLD_NUMBER,
                GENDER)#,
# SCHOOL_NAME)#,
#                 ABSENCE_FIRST_MONTH,
#                 ABSENCE_SECOND_MONTH,
#                 ABSENCE_THIRD_MONTH)

# Form c_days_n: student-absences
form_c_days_first <-
  form_c_days_first %>%
  mutate(month = 1,
         day = DAYS_OF_ABSENCE_FIRST_MONTH) %>%
  dplyr::select(`_PARENT_AURI`,
                month, 
                day)
form_c_days_second <-
  form_c_days_second %>%
  mutate(month = 2,
         day = DAYS_OF_ABSENCE_SECOND_MONTH) %>%
  dplyr::select(`_PARENT_AURI`,
                month, 
                day)
form_c_days_third <-
  form_c_days_third %>%
  mutate(month = 3,
         day = DAYS_OF_ABSENCE_THIRD_MONTH) %>%
  dplyr::select(`_PARENT_AURI`,
                month, 
                day)
# Join all absences together
absences <- 
  rbind(form_c_days_first,
        form_c_days_second,
        form_c_days_third)

# Get real dates into absences !!!
temp <- absences %>%
  # Get student data along with their class
  left_join(form_c_core %>%
              dplyr::select(`_URI`,
                            CLASS_UUID),
            by = c('_PARENT_AURI' = '_URI')) %>%
  rename(student_id = `_PARENT_AURI`) %>%
  # Get the actual months from each student's turma
  left_join(form_b_core %>%
              dplyr::select(CLASS_UUID,
                            FIRST_MONTH_OF_REFERENCE,
                            SECOND_MONTH_OF_REFERENCE,
                            THIRD_MONTH_OF_REFERENCE),
            by = 'CLASS_UUID')

temp <- 
  temp %>%
  mutate(FIRST_MONTH_OF_REFERENCE = add_zero(FIRST_MONTH_OF_REFERENCE),
         SECOND_MONTH_OF_REFERENCE = add_zero(SECOND_MONTH_OF_REFERENCE),
         THIRD_MONTH_OF_REFERENCE = add_zero(THIRD_MONTH_OF_REFERENCE),
         day = add_zero(day))
# rename the buggy january as september
temp$THIRD_MONTH_OF_REFERENCE[temp$THIRD_MONTH_OF_REFERENCE == '01'] <- '09'

# Divide temp into 3 months, and create dates
temp$date <-
  as.character(ifelse(temp$month == 1,
                      (paste0('2015-', 
                              temp$FIRST_MONTH_OF_REFERENCE, 
                              '-',
                              temp$day)),
                      ifelse(temp$month == 2,
                             (paste0('2015-', 
                                     temp$SECOND_MONTH_OF_REFERENCE, 
                                     '-',
                                     temp$day)),
                             ifelse(temp$month == 3,
                                    (paste0('2015-',
                                            temp$THIRD_MONTH_OF_REFERENCE,
                                            '-',
                                            temp$day)),
                                    NA))))
temp$date <- as.Date(temp$date)

# What is with the day 00 rows?  for now, removing them
temp <- temp[temp$day != '00',]

# Subset to only necesary columns
temp <- temp %>%
  dplyr::select(student_id, date)

# Write over absences
absences <- temp 
# Abseces is now a one row = one student absence

#######################################################
#######################################################
#######################################################
#######################################################

# Get possible dates
possible_dates <- seq(as.Date('2015-03-01'), as.Date('2015-12-31'), 1)
# Remove weekends from the date range
possible_dates <- 
  possible_dates[!weekdays(possible_dates) %in% c('Saturday', 'Sunday')]
# Create a dataframe in which one row = one student-day
df <- 
  expand.grid(
    date = possible_dates,
    student_id = sort(unique(form_c_core$`_URI`)))
df$year <- 2015
df$month <- format(df$date, '%m')
df$day <- format(df$date, '%d')

# Join to student data (slow)
df <- 
  df %>%
  left_join(form_c_core %>%
              dplyr::select(`_URI`,
                            PERMID, 
                            CLASS_UUID,
                            DOB,
                            GENDER,
                            SCHOOL_UUID),
            by = c('student_id' = '_URI'))

# Join to class data
df <- 
  df %>%
  left_join(form_b_core %>%
              dplyr::select(SCHOOL_UUID,
                            CLASS_UUID,
                            CLASS_NAME,
                            GRADE,
                            FIRST_MONTH_OF_REFERENCE,
                            SECOND_MONTH_OF_REFERENCE,
                            THIRD_MONTH_OF_REFERENCE),
            by = c('SCHOOL_UUID', 'CLASS_UUID'))

# !!! still a bunch of weird months table(df$THIRD_MONTH_OF_REFERENCE)

# Remove uneligible months
df$keep <- 
  as.numeric(df$month) == as.numeric(df$FIRST_MONTH_OF_REFERENCE) |
  as.numeric(df$month) == as.numeric(df$SECOND_MONTH_OF_REFERENCE) | 
  as.numeric(df$month) == as.numeric(df$THIRD_MONTH_OF_REFERENCE)
df <- df[df$keep,]


# Remove xcess (helper) columns
df$FIRST_MONTH_OF_REFERENCE <-
  df$SECOND_MONTH_OF_REFERENCE <-
  df$THIRD_MONTH_OF_REFERENCE <-
  df$keep <- 
  NULL

# Remove holidays from df
df <- 
  df %>%
  left_join(holidays %>%
              mutate(holiday = TRUE,
                     SCHOOL_UUID = turma_id) %>%
              dplyr::select(SCHOOL_UUID,
                            date,
                            holiday),
            by = c('date', 'SCHOOL_UUID')) %>%
  mutate(holiday = ifelse(is.na(holiday), FALSE, holiday)) %>%
  filter(!holiday) %>%
  dplyr::select(-holiday)

# Now df is a list of eligible absences (student-date pairs)

# Next, define which of the eligible rows are absences
df <- 
  df %>%
  left_join(absences %>%
              mutate(absence = TRUE) %>%
              dplyr::select(date, 
                            student_id, 
                            absence),
            by = c('date', 'student_id')) %>%
  mutate(absence = ifelse(is.na(absence), FALSE, absence))

# Bring in full school information
df <- 
  df %>%
  left_join(form_a_core %>%
              dplyr::select(SCHOOL_UUID, 
                            SCHOOL_NAME,
                            GPS_ALT,
                            GPS_ACC,
                            GPS_LNG,
                            GPS_LAT,
                            DISTRICT,
                            TYPE_OF_LATRINE,
                            SCHOOL_CONDITIONS,
                            LUNCH,
                            NUMBER_OF_CLASSES),
            by = 'SCHOOL_UUID')

# Now data is structured : 1 row = 1 student-day, flagged for absence or not

#######################################################
#######################################################
#######################################################
#######################################################

# Still need to do some minor data cleaning - GPS coordinates, gender,
# School conditions, type of latrine, district

# GPS coordinates
df$GPS_LAT <- df$GPS_LAT / 10000000000
df$GPS_LNG <- df$GPS_LNG / 10000000000
df$GPS_ALT <- df$GPS_ACC <- NULL

# DOB
df$DOB <- as.Date(substr(df$DOB, 1, 10),
                  format = '%d%b%Y')

# Clean out some extra columns
df$`valid schools` <- NULL

# Get cleaner district
df$district <- 
  ifelse(df$DISTRICT == 1, 'Manhiça',
         ifelse(df$DISTRICT == 2, 'Magude', 
                NA))
df$DISTRICT <- NULL

# Get cleaner gender
df$gender <-
  ifelse(df$GENDER == 1, 'Male', 
         ifelse(df$GENDER == 2, 'Female',
                NA))
df$GENDER <- NULL

# Clean up school conditions
df$SCHOOL_CONDITIONS <- 
  ifelse(df$SCHOOL_CONDITIONS == 1, 'Caniço e palha',
         ifelse(df$SCHOOL_CONDITIONS == 2, 'Caniço e chapa de zinco',
                ifelse(df$SCHOOL_CONDITIONS == 3, 'Matope e palha',
                       ifelse(df$SCHOOL_CONDITIONS == 4, 'Matope e chapa de zinco',
                              ifelse(df$SCHOOL_CONDITIONS == 5, 'Madeira e zinco',
                                     ifelse(df$SCHOOL_CONDITIONS == 6, 'Convencional e outras precárias',
                                            ifelse(df$SCHOOL_CONDITIONS == 7, 'Todas de material convencional',
                                                   ifelse(df$SCHOOL_CONDITIONS == 8, 'Outro', NA))))))))

# Clean up type of latrine
df$TYPE_OF_LATRINE <- 
  ifelse(df$TYPE_OF_LATRINE == 1, 'Retrete ligada a fossa séptica',
         ifelse(df$TYPE_OF_LATRINE == 2, 'Latrine Melhorada',
                ifelse(df$TYPE_OF_LATRINE == 3, 'Latrina tradicional melhorada',
                       ifelse(df$TYPE_OF_LATRINE == 4, 'Latrina tradicional não melhorada',
                              ifelse(df$TYPE_OF_LATRINE == 5, 'Sem latrina', NA)))))

# Clean up lunch # 
df$LUNCH <- ifelse(df$LUNCH == 1, TRUE, 
                   ifelse(df$LUNCH == 2, FALSE, 
                          NA))


#####
# GET SPATIAL DATA
moz3 <- getData('GADM', country = 'MOZ', level = 3)
maputo <- moz3[moz3@data$NAME_1 %in% c('Maputo', 'Maputo City'),]
# Fortify maputo
maputo_fortified <- fortify(maputo, region = 'NAME_3')
# Fortify moz3
moz3_fortified <- fortify(moz3, region = 'NAME_3')


###### IDS
# Create student ids for use in the second phase
# Create a simplified students dataframe

# Get nom complet estudiant
df <- df %>%
  left_join(form_c_core %>%
              rename(student_id = `_URI`) %>%
              dplyr::select(student_id, NAME),
            by = 'student_id')

# Get months of reference
df <- df %>%
  left_join(form_b_core %>%
              dplyr::select(SCHOOL_UUID, 
                            FIRST_MONTH_OF_REFERENCE,
                            SECOND_MONTH_OF_REFERENCE,
                            THIRD_MONTH_OF_REFERENCE),
            by = 'SCHOOL_UUID') %>%
  # also get household number
  left_join(form_c_core %>%
              dplyr::select(`_URI`,
                            HOUSEHOLD_NUMBER),
            by = c('student_id' = '_URI'))

# Rename those with no school name to be xUngucha
df <-
  df %>%
  # filter(!is.na(SCHOOL_NAME))
  mutate(SCHOOL_NAME = 
           ifelse(is.na(SCHOOL_NAME),
                  'EPC xUngucha',
                  SCHOOL_NAME))

# Create a dataframe with all student
students <-
  df %>%
  mutate(no_school_name = is.na(SCHOOL_NAME)) %>%
  arrange(no_school_name) %>%
  filter(!duplicated(student_id)) %>%
  # dplyr::select(-date,-student_id, -PERMID,
  #               -CLASS_UUID, -SCHOOL_UUID,
  #               -year, -month, -day) %>%
  dplyr::select(district,
                SCHOOL_NAME,
                NAME,
                gender,
                GRADE,
                CLASS_NAME,
                DOB,
                PERMID)
# Order by district, schoolname
students <-
  students %>%
  arrange(district, SCHOOL_NAME, GRADE, CLASS_NAME, NAME)

# Give district numbers, school numbers and student numbers
students <-
  students %>%
  # district number
  mutate(district_number = ifelse(district == 'Magude', 2, 
                                  ifelse(district == 'Manhiça', 1,
                                         NA))) %>%
  # school number
  mutate(school_number = as.numeric(factor(SCHOOL_NAME))) %>%
  # student number
  group_by(SCHOOL_NAME) %>%
  mutate(student_number = 1:n()) %>%
  ungroup 

students <-
  students %>%
  mutate(district_number = pre_zero(district_number),
         school_number = pre_zero(school_number),
         student_number = pre_zero(student_number, n = 3)) %>%
  # combined number
  mutate(combined_number = paste0(district_number, '-',
                                  school_number, '-',
                                  student_number))

# Keep only the bare minimum
rm(form_a_core,
   form_b_core,
   form_b_month_one,
   form_b_month_two,
   form_b_month_three,
   form_c_core,
   form_c_days_first,
   form_c_days_second,
   form_c_days_third,
   form_d_august,
   form_d_core,
   form_d_directed,
   form_d_july,
   form_d_lectured,
   form_d_november,
   form_d_october,
   form_d_september,
   form_d_subject_lectured,
   holidays,
   temp,
   valid_schools)

# Mark the phase
absences$phase <- 1
df$phase <- 1
students$phase <- 1


############################################################################
############################################################################
############################################################################
##### PHASE 2
############################################################################
############################################################################

#loading libraries
library(RMySQL)
library(dplyr)
library(DBI)
library(readxl)

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

# Save a snapshot
save.image(paste0('snapshots/',
                  Sys.Date(),
                  '.RData'))

# In form_b_2_core (students, phase 2), create a standardly named student_number  
# so as to be compatible with the students dataframe
form_b_2_core <- 
  form_b_2_core %>%
  # get numbers for each element
  mutate(district_number = unlist(lapply(strsplit(student_number, '-'), 
                                         function(x){x[[1]]})),
         school_number = unlist(lapply(strsplit(student_number, '-'), 
                                       function(x){x[[2]]})),
         student_id = unlist(lapply(strsplit(student_number, '-'), 
                                    function(x){x[[3]]}))) %>%
  mutate(district_number = pre_zero(district_number),
         school_number = pre_zero(school_number),
         student_id = pre_zero(student_id, n = 3)) %>%
  # combined number
  mutate(combined_number = paste0(district_number, '-',
                                  school_number, '-',
                                  student_id)) %>%
  # Get rid of the helper columns
  dplyr::select(-district_number, school_number, student_id)

# JOIN ACADEMIC PERFORMANCE DATA TO THE STUDENT DATA

# First get rid of some columns in the 2015 data
form_b_2_core <- 
  form_b_2_core %>%
  dplyr::select(-creator_uri_user,
                -creation_date,
                -last_update_uri_user,
                -ui_version,
                -is_complete,
                -submission_date,
                -marked_as_complete_date,
                -redcapupload)

form_b_2_core2 <- 
  form_b_2_core2 %>%
  dplyr::select(-creator_uri_user,
                -creation_date,
                -last_update_uri_user,
                -last_update_date,
                -ordinal_number,
                -redcapupload,
                -uri)

# Assign years to the form b 2 cores
year_cols <- unique(c(names(form_b_2_core), names(form_b_2_core2)))
year_cols <- year_cols[grepl('avaliation', year_cols)]

# Assign years
for (j in 1:ncol(form_b_2_core)){
  this_column <- names(form_b_2_core)[j]
  if(this_column %in% year_cols){
    names(form_b_2_core)[j] <- paste0(this_column, '_2015')
  }
}
for (j in 1:ncol(form_b_2_core2)){
  this_column <- names(form_b_2_core2)[j]
  if(this_column %in% year_cols){
    names(form_b_2_core2)[j] <- paste0(this_column, '_2016')
  }
}

# Join both the form bs so that we have all academic info in one place
form_b <- 
  inner_join(form_b_2_core,
             form_b_2_core2,
             by = c('uri' = 'parent_auri'))

# Now join all class info to students
# Need to remove a few columns here first to avoid duplicates!
x <- 
  left_join(x = students,
            y = form_b,
            by = 'combined_number')
