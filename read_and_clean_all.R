# TO DO

# 0. Integrate corrections file

# 1. CONSTRUCT DENOMINATORS FOR DIAS LECTIUS AND RECALCULATE ABSENTEEISM, DAYS OFF, ETC.
# - Before February 2016, uses form a 2 days off, these are dias FERIADOS (b = 2016, but ignore)
# --- tot a nivell de escola
# - February-June 2016 uses form a 3, these are not FERIADOS, the are LECTIUS
# --- tot a nivell de turma/class
# In the above, any month which is not "complete" gets no data at all
# For May/June 2016, the true feriados days are in 
# ---form_a_3_class_room_may_info_no
# ---form_a_3_class_room_may_info_no

# 2. MERGE (DIRECT AND INDIRECT) WITH CENSUS

# 3. PROFESSORS
# everything is contained in form c 2
# formc2 = april 2015-2016
# a = 2015; b = 2016
# the numerator is if they missed days
# form_c_2_absenteeism_info_days_absent_<month>
# the denominator is form_a_2_days_off_<month>
# no may and june


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
                student_id,
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
   temp)

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

# Get which network I'm on
# wifi <- readLines(connection(system('nm-tool | grep [*]')))

# Get stuff from config
connection_options <- yaml::yaml.load_file('credentials_joe.yaml')

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
performance <-
  inner_join(form_b_2_core,
             form_b_2_core2,
             by = c('uri' = 'parent_auri')) %>%
  # get rid of those columns which are duplicated in students
  dplyr::select(-gender,
                -student_number,
                -school_number)


# SCHOOL LEVEL ----------------------------------------------------------
# For each school from form a, get the days where class was held

# Get all of days off into one dataframe
days_off <-
  c("form_a_2_days_off_april",
    "form_a_2_days_off_april_b",
    "form_a_2_days_off_august",
    "form_a_2_days_off_february_b",
    "form_a_2_days_off_july",
    "form_a_2_days_off_june",
    "form_a_2_days_off_march_b",
    "form_a_2_days_off_may",
    "form_a_2_days_off_november",
    "form_a_2_days_off_october",
    "form_a_2_days_off_september")
keys <- c('April 2015',
          'April 2016',
          'August 2015',
          'February 2016',
          'July 2015',
          'June 2015',
          'March 2016',
          'May 2015',
          'November 2015',
          'October 2015',
          'September 2015')
months <- c('April',
            'April',
            'August',
            'February',
            'July',
            'June',
            'March',
            'May',
            'November',
            'October',
            'September')
years <- c(2015,
           2016,
           2015,
           2016,
           2015,
           2015,
           2016,
           2015,
           2015,
           2015,
           2015)
days_off_df <- data.frame(form_a_2_days_off_april)[0,]
for (i in 1:length(days_off)){
  x <- get('form_a_2_days_off_april')
  x$src <- days_off[i]
  names(x)[9] <- 'days_off'
  x$month_year <- keys[i]
  x$month <- months[i]
  x$year <- years[i]
  days_off_df <- rbind(days_off_df,
                       x)
}

# Create an actual date column
days_off_df$date <-
  as.Date(paste0(days_off_df$month,
                 ' ',
                 days_off_df$days_off,
                 ' ',
                 days_off_df$year),
          format = '%B %d %Y')
# Select only the columns we need
days_off_df <-
  days_off_df %>%
  dplyr::select(uri, parent_auri, date)
# Get meaningful names in days_off_df
days_off_df <-
  days_off_df %>%
  left_join(form_a_2_core %>%
              dplyr::select(school, uri, district),
            by = c('parent_auri' = 'uri')) %>%
  left_join(valid_schools %>%
              dplyr::select(`School Code`, `Correct name`) %>%
              rename(school = `School Code`,
                     school_name = `Correct name`) %>%
              mutate(school = as.character(school)),
            by = 'school') %>%
  # Keep only those columns we need
  dplyr::select(school, school_name, date)
# Keep only unique
days_off_df <- days_off_df[!duplicated(days_off_df),]
# arrange
days_off_df <- days_off_df %>%
  arrange(date)

# As of now we have three dataframes of interest
# 1. students = a roster
# 2. days_off_df = a dataframe of each schools days off
# 3. performance = the academic performance of each student

# Now create an ABSENCES dataframe ---------
# b_2 = students
# c_2 = teachers
# d_2 = teachers (but crappy data, use c_2 instead)

absence_dfs <-
  c('form_b_2_absentism_info_days_absent_february',
    'form_b_2_absentism_info_days_absent_march',
    'form_b_2_absentism_info_days_absent_april')
absences <- form_b_2_absentism_info_days_absent_april
names(absences)[9] <- 'day'
absences$src <- absences$month <- absences$year <-  NA
absences <- absences[0,]
months <- c('February', 'March', 'April')
years <- c(2016, 2016, 2016)
for (i in 1:length(absence_dfs)){
  this_month <-
    get(absence_dfs[i])
  names(this_month)[9] <- 'day'
  this_month$src <- absence_dfs[i]
  this_month$month <- months[i]
  this_month$year <- years[i]
  absences <- rbind(absences,
                    this_month)
}
# Keep only relevant variables
absences <-
  absences %>%
  mutate(date = as.Date(paste0(month, ' ',
                               day, ' ',
                               year),
                        format = '%B %d %Y')) %>%
  dplyr::select(parent_auri, date)
# Bring in inteligible id numbers
absences <-
  absences %>%
  left_join(form_b_2_core %>%
              dplyr::select(combined_number, uri),
            by = c('parent_auri' = 'uri')) %>%
  dplyr::select(-parent_auri)
# Add an "absent = TRUE" column
absences$absent = TRUE

# As of now we have four dataframes of interest for phase 2
# 1. students = a roster
# 2. days_off_df = a dataframe of each schools days off
# 3. performance = the academic performance of each student
# 4. absences = a student-absence paired set

# We can now construct a dataset with ALL absences/presences
# for all students in 2016
attendance <-
  expand.grid(date = seq(as.Date('2016-02-01'),
                         as.Date('2016-04-30'),
                         by = 1),
              combined_number = sort(unique(students$combined_number)))
# Bring in the school number
attendance <-
  left_join(attendance,
            students %>%
              dplyr::select(combined_number, school_number),
            by = 'combined_number')
# Flag weekends and remove
attendance$dow <- weekdays(attendance$date)
attendance <- attendance %>%
  filter(dow != 'Saturday',
         dow != 'Sunday')
# Flag those no lectivo days from each school
attendance <-
  attendance %>%
  mutate(school_number = as.numeric(as.character(school_number))) %>%
  left_join(days_off_df %>%
              mutate(school_number = as.numeric(as.character(school))) %>%
              dplyr::select(school_number, date) %>%
              mutate(no_lectivo = TRUE),
            by = c('date', 'school_number')) %>%
  mutate(no_lectivo = ifelse(is.na(no_lectivo), FALSE,
                             no_lectivo)) %>%
  # Remove the no lectivo days
  filter(!no_lectivo) %>%
  dplyr::select(-dow, -no_lectivo)

# "Attendnance" is the days they SHOULD have been at
# school
# "Absences" is the days they weren't at school
# we assume days not flagged as absent were attended
# Let's join
attendance <-
  left_join(x = attendance,
            y = absences,
            by = c('date', 'combined_number')) %>%
  mutate(absent = ifelse(is.na(absent), FALSE, TRUE))

########################## CLEAN UP PERFORMANCE
# Clean up performance
x <- performance
# Change the days_off column, which is actually clase de etica
names(x) <- gsub('days_off_august|day_off_august', 'etica', names(x))
# Keep only those columns that matter
x <- x[,names(x) %in% c('cod_inq', 
                        'combined_number',
                        'student_found') |
         grepl('avaliation', names(x))]
# Put into a different dataframe
x_df <- 
  data.frame(year = NA,
             trimester = NA,
             period = NA,
             subject = NA,
             value = NA)

# Clean up the names

# Define function for converting column to dataframe of results
column_to_df <- function(column_name){
  if(grepl('avaliation', column_name)){
    split_name <- unlist(strsplit(x = column_name, split = '_'))
    # Get the letter (which can be mapped to a trimester)
    the_letter <- split_name[nchar(split_name) == 1][1]
    # Map the letter to a trimester and year
    year <- ifelse(the_letter %in% c('a', 'b', 'c'), 2015,
                   ifelse(the_letter == 'd', 2016, NA))
    trimester<- ifelse(the_letter == 'a', 1,
                       ifelse(the_letter == 'b', 2,
                              ifelse(the_letter == 'c', 3,
                                     ifelse(the_letter == 'd', 4, NA))))
    # Get the period
    period <- ifelse('mt' %in% split_name, 'MT',
                     ifelse('ap' %in% split_name, 'AP', NA))
    # Get the subject
    subject <- 
      gsub('_a_|_b_|_c_|_d_', 
           '', 
           gsub('_a_|_b_|_c_|_d_|_2016|_2015|_mt_|_ap_', 
                '_', 
                substr(column_name, 22, nchar(column_name))))
    # Return a dataframe of the results
    data.frame(combined_number = NA,
               year = year,
               trimester = trimester,
               period = period,
               subject = subject,
               value = NA)
  }
}

# Loop through each row of x and get results
results_list <- list()
counter <- 0
for (j in 1:ncol(x)){
  column_name <- names(x)[j]
  if(grepl('avaliation', column_name)){
    message(paste0('column: ', j, '\n'))
    # Create a dataframe of results
    results_template <- column_to_df(names(x)[j])
    for (i in 1:nrow(x)){
      counter <- counter + 1
      # Copy the results
      results <- results_template
      # Get the combined number
      results$combined_number <- x$combined_number[i]
      # Get the grade
      results$value <- as.numeric(x[i, j])
      # Fix the row names
      row.names(results) <- counter
      # Stick into results_list
      results_list[[counter]] <- results
    }
  }
}
# Combine all the results list into one
results_df <- do.call('rbind', results_list)

# Overwrite performance with the more organized data
performance <- results_df
# And remove the other stuff
rm(results_df, x)

# Get df (ie, phase 1) into the same form as attendance
# (ie, phase 2)
df <- df %>%
  dplyr::select(date, absence, student_id) %>%
  left_join(students %>%
              dplyr::select(student_id, combined_number, school_number),
            by = 'student_id') %>%
  dplyr::select(-student_id) %>%
  rename(absent = absence) %>%
  dplyr::select(date, combined_number, school_number, absent)

# Combine both phase 1 and 2 absenteeism
attendance <- 
  rbind(
    # phase 1
    df %>% 
      mutate(phase = 1),
    attendance %>%
      mutate(phase = 2))

# Keep only those students that appear in both phase 1
# and phase 2
attendance <-
  attendance %>%
  group_by(combined_number) %>%
  mutate(both_phases = length(which(phase == 1)) > 0 &
           length(which(phase == 2)) > 0) %>%
  ungroup %>%
  filter(both_phases) %>%
  dplyr::select(-both_phases)

# Remove any references to December of January 
# (smmer break)
attendance <- 
  attendance %>%
  filter(!format(date, '%m') %in% c('12', '01'))

# Remove repetitions
attendance <- attendance[!duplicated(attendance),]

# Remove any of those students flagged as not having complete
# absenteeism information for any period
# In the following tables
# form_b_2_core$absentism_info_absence_february
# form_b_2_core$absentism_info_absence_march
# form_b_2_core$absentism_info_absence_april
# 1 = student found AND has been absent
# 2 = student found AND has not been absent
# 3 = no information on the student
# If 3, we need to remove completely
# AND student_found has to be equal to 1

# Keep only these ids that meet the above conditions
# 
# keep <- form_b_2_core %>%
#   filter(student_found == 1) %>%
#   dplyr::select(combined_number)

# attendance2 <- attendance
#   attendance %>%
#   filter(combined_number %in% keep$combined_number)
# Not doing the above for now
attendance2 <- attendance

# Flag month specific problems
attendance2$flag <- FALSE
attendance2 <- 
  left_join(attendance2,
            form_b_2_core %>%
              dplyr::select(combined_number,
                            absentism_info_absence_february,
                            absentism_info_absence_march,
                            absentism_info_absence_april,
                            student_found))

attendance2$flag <-
  ifelse(format(attendance2$date, '%m-%Y') == '02-2016' &
           !attendance2$absentism_info_absence_february %in% c(1, 2), TRUE,
         ifelse(format(attendance2$date, '%m-%Y') == '03-2016' &
                  !attendance2$absentism_info_absence_march %in% c(1, 2), TRUE,
                ifelse(format(attendance2$date, '%m-%Y') == '04-2016' &
                         !attendance2$absentism_info_absence_april %in% c(1, 2), TRUE, attendance2$flag)))

# Also flag modern entries that have not been found
attendance2$flag <-
  ifelse(format(attendance2$date, '%m-%Y') %in% c('02-2016',
                                                  '03-2016',
                                                  '04-2016') &
           (attendance2$student_found != 1 |
              is.na(attendance2$student_found)),
         TRUE,
         attendance2$flag)
# Remove the flags
attendance2 <- 
  attendance2 %>%
  filter(!flag)
# Overwrite attendance
attendance <- attendance2
rm(attendance2)

save.image('~/Desktop/temp.RData')

######################################################
# Bring in corrections data
library(readxl)
results_list <- list()
for (i in 1:24){ # not doing row 25, since we don't have corrections for that
  message(paste0('Working on school ', i))
  # Read in the data
  this_school <- suppressMessages(read_excel('corrections/Duplicados_Aug 2016.xls',
                                             sheet = paste0('school_', i)))
  this_school <- data.frame(this_school)
  message(paste0('Rows :', nrow(this_school)))
  
  # Identify which row has the correct stuff
  start_here <- which(this_school$NAME == 'NAME') + 1
  # Subset
  this_school <- this_school[start_here:nrow(this_school),]
  # Add to results
  results_list[[i]] <- this_school
}
the_binder <- plyr::rbind.fill
corrections <- do.call('the_binder', results_list)
rm(results_list, the_binder, this_school, i, start_here)

######################################################
# Bring in census data

# Magude  ----------------------------------------------------------
load('census/maltem/2016-07-15_HOUSEHOLD.RData')
locations_magude <- HOUSEHOLD; rm(HOUSEHOLD)
locations_magude$lng <-
  locations_magude$longitude <-
  locations_magude$x <-
  locations_magude$lon <-
  locations_magude$HOUSEHOLD_HEAD_GPS_LNG
locations_magude$lat <-
  locations_magude$latitude <-
  locations_magude$y <-
  locations_magude$HOUSEHOLD_HEAD_GPS_LAT
locations_magude$geo <- 'Magude'
locations_magude$permid <- locations_magude$HOUSEHOLD_HEAD_PERM_ID
locations_magude$house_number <- locations_magude$HOUSEHOLD_HEAD_AGREG_NUM
locations_magude <-
  locations_magude %>% dplyr::select(house_number,
                                     lng,
                                     longitude,
                                     x,
                                     lon,
                                     lat,
                                     latitude,
                                     y,
                                     geo)
# Also get birthday
load('census/maltem/2016-07-15_MEMBER.RData')
magude_member <- MEMBER; rm(MEMBER)
magude_member <-
  magude_member %>%
  dplyr::select(PERM_ID_MEMBER,
                BIRTH_MEMBER,
                MEMBER_GENDER,
                MEMBER_NAME,
                HOUSEHOLD_NUMBER) %>%
  rename(permid = PERM_ID_MEMBER,
         dob = BIRTH_MEMBER,
         gender = MEMBER_GENDER,
         name = MEMBER_NAME,
         house_number = HOUSEHOLD_NUMBER) %>%
  mutate(dob = as.Date(substr(dob, 1, 10)))
# Join
census_magude <-
  left_join(x = magude_member,
            y = locations_magude,
            by = 'house_number') %>%
  mutate(geo = 'Magude')
rm(magude_member)
# Recode gender
census_magude$gender <-
  ifelse(census_magude$gender == '1',
         'male',
         ifelse(census_magude$gender == '2', 
                'female',
                NA))

# Manhica ----------------------------------------------------------
load('census/openhds/2016-07-15_individual.RData')
individual$dob <- as.Date(individual$dob)
individual$house_number <- substr(individual$lastName, 1, 8)
individual$name <- individual$firstName
individual$permid <- individual$lastName
individual <- individual %>%
  dplyr::select(permid, name, house_number, dob, gender)
# Read in coordinates (emailed from Charfudin)
coords <- read_csv('census/openhds/Coordenadas.csv')
names(coords) <- c('house_number', 'region', 'lat', 'lng')
coords$region <- NULL
# Combine
census_manhica <- left_join(individual,
                            coords,
                            by = 'house_number')
rm(individual, coords)

# Convert census_manhica to lat/lng
census_manhica_location <- census_manhica %>% filter(!is.na(lat) & !is.na(lng))
census_manhica_no_location <- census_manhica %>% filter(is.na(lat) | is.na(lng))
sp::coordinates(census_manhica_location) <- ~lng+lat
proj4string(census_manhica_location) <- CRS("+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
census_manhica_location <- spTransform(census_manhica_location, CRS('+proj=longlat'))
# Extract the coordinates
ll <- coordinates(census_manhica_location)
# Add back into the original dataframe
census_manhica_location$x <- ll[,1]
census_manhica_location$y <- ll[,2]
census_manhica_location <- data.frame(census_manhica_location@data)
# change names
census_manhica_location <-
  census_manhica_location %>%
  rename(lng = x,
         lat = y)
# Combine all of manhica back together
census_manhica <-
  rbind(census_manhica_location,
        census_manhica_no_location)
rm(census_manhica_location, census_manhica_no_location)

# Expand coordinates
census_manhica$longitude <-
  census_manhica$x <-
  census_manhica$lon <-
  census_manhica$lng
census_manhica$latitude <-
  census_manhica$y <-
  census_manhica$lat
census_manhica$geo <- 'Manhiça'
census_manhica <-
  census_manhica %>%
  dplyr::select(permid,
                dob,
                gender,
                name,
                house_number,
                lng,
                longitude,
                x,
                lon,
                lat,
                latitude,
                y,
                geo)
# Recode gender
census_manhica$gender <-
  ifelse(census_manhica$gender == 'F',
         'female',
         ifelse(census_manhica$gender == 'M',
                'male', 
                NA))


# Join the censuses ----------------------------------------
census_magude <- data.frame(census_magude)
census_manhica <- data.frame(census_manhica)
for (j in 1:ncol(census_manhica)){
  if(class(census_manhica[,j]) == 'factor'){
    census_manhica[,j] <- as.character(census_manhica[,j])
  }
  if(class(census_magude[,j]) == 'factor'){
    census_magude[,j] <- as.character(census_magude[,j])
  }
}
census <- rbind(census_manhica, census_magude)
rm(census_manhica, census_magude)

save.image('~/Desktop/temp.RData')


# Third phase was to see if there was complete data and which days there was complete data for

##############################################
##############################################
##############################################
##############################################

# Join census data to student data 

x <- left_join(x = students,
               y = census %>%
                 dplyr::select(-gender),
               by = c('PERMID' = 'permid'))

# As of now we have a few dataframes of interest for phase 2
# 1. students = a roster
# 2. performance = the academic performance of each student
# 3. attendance = a student-absence-presence paired set (both phase 1 and 2)
# 4. days_off_df = which non-lectivo days each school had
# 5. census
