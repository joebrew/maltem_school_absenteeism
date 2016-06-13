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

# School names are missing from form_c_2_core
# Therefore, in order to know which school a professor is from
# we have to base it on their creation_date
visit_dates <- data.frame(
  date = c('2016-05-16',
           '2016-05-17',
           '2016-05-20',
           '2016-05-24',
           '2016-05-25',
           '2016-05-26',
           '2016-05-27',
           '2016-05-31',
           '2016-06-06',
           '2016-06-09',
           '2016-06-10',
           '2016-06-13'),
  school_code = c(13, 
                  13,
                  5,
                  12,
                  18,
                  8,
                  8,
                  9,
                  23,
                  1,
                  1,
                  7)
)


# Read in everything from phase 1
source('read_in_phase_1.R')


# PHASE 1 CHECKS
# Errors
errors <- 
  form_c_core %>%
  # Reformat birthday
  mutate(dob = as.Date(substr(DOB, 1, 9), '%d%b%Y')) %>%
  # get school data
  left_join(form_a_core %>%
              dplyr::select(SCHOOL_UUID, 
                            SCHOOL_NAME),
            by = 'SCHOOL_UUID') %>%
  # Get grade data
  left_join(form_b_core %>%
              dplyr::select(GRADE, 
                            CLASS_NAME,
                            FIRST_MONTH_OF_REFERENCE,
                            SECOND_MONTH_OF_REFERENCE,
                            THIRD_MONTH_OF_REFERENCE,
                            CLASS_UUID),
            by = 'CLASS_UUID') %>%
  # Get absenteeism data for each month
  # first
  left_join(form_c_days_first %>%
              group_by(`_PARENT_AURI`) %>%
              summarise(first_month_days = paste0(day, collapse = '|'),
                        first_month_total = n()),
            by = c('_URI' = '_PARENT_AURI')) %>%
  # second
  left_join(form_c_days_second %>%
              group_by(`_PARENT_AURI`) %>%
              summarise(second_month_days = paste0(day, collapse = '|'),
                        second_month_total = n()),
            by = c('_URI' = '_PARENT_AURI')) %>%
  # third
  left_join(form_c_days_third %>%
              group_by(`_PARENT_AURI`) %>%
              summarise(third_month_days = paste0(day, collapse = '|'),
                        third_month_total = n()),
            by = c('_URI' = '_PARENT_AURI')) %>%
  group_by(NAME) %>%
  summarise(entries = n(),
            n_schools = length(unique(SCHOOL_NAME)),
            name_schools = paste0(unique(SCHOOL_NAME), collapse = ', '),
            n_dobs = length(unique(dob)),
            dobs = paste0(unique(dob), collapse = ', '),
            dob_error = any(dob >= '2010-01-01', na.rm = TRUE),
            n_permids = length(unique(PERMID)),
            permids = paste0(unique(PERMID), collapse = ', '),
            n_grades = length(unique(GRADE)),
            grades = paste0(unique(GRADE), collapse = ', '),
            n_class_names = length(unique(CLASS_NAME)),
            class_names = paste0(unique(CLASS_NAME), collapse = ', '),
            n_first_months = length(unique(first_month_days)),
            first_months = paste0(unique(first_month_days), collapse = ', '),
            first_month_too_low = any(first_month_total == 0, na.rm = TRUE),
            first_month_too_high = any(first_month_total >= 18, na.rm = TRUE),
            n_second_months = length(unique(first_month_days)),
            second_months = paste0(unique(second_month_days), collapse = ', '),
            second_month_too_low = any(second_month_total == 0, na.rm = TRUE),
            second_month_too_high = any(second_month_total >= 18, na.rm = TRUE),
            n_third_months = length(unique(third_month_days)),
            third_months = paste0(unique(third_month_days), collapse = ', '),
            third_month_too_low = any(third_month_total == 0, na.rm = TRUE),
            third_month_too_high = any(third_month_total >= 18, na.rm = TRUE)) %>%
  ungroup %>%
  # arrange(desc(n_schools)) #%>%
  # Bring in the student number
  left_join(students %>%
              group_by(NAME) %>%
              summarise(n_student_numbers = length(unique(student_number)),
                        student_numbers = paste0(student_number, collapse = ', ')),
            by = 'NAME') %>%
  filter(n_schools > 1 |
           n_dobs > 1 |
           n_permids > 1 |
           n_grades >1 |
           n_class_names > 1 |
           n_first_months > 1 |
           n_second_months > 1 |
           n_third_months > 1 |
           n_student_numbers > 1 |
           dob_error |
           first_month_too_low |
           first_month_too_high |
           second_month_too_low |
           second_month_too_high |
           third_month_too_low | 
           third_month_too_high) %>%
  # Make dob error more clear
  mutate(dob_error = ifelse(dob_error, 'Wrong dob', ''))

# Create a different table for each school
dir.create('checks_june_2016')
unique_schools <- sort(unique(form_a_core$SCHOOL_NAME))

for (i in 1:length(unique_schools)){
  this_school <- unique_schools[i]
  # subset
  x <- errors[grepl(this_school, errors$name_schools),]
  print(nrow(x))
  # Create a spreadsheet for each school
  write_csv(x, paste0('checks_june_2016/Student level problems at ', this_school, '.csv'))
}

# For each SCHOOL-GRADE-CLASS combination
# What percentage of each subjects' results are missing

# By class percentage of unknown permids 
# and percentage of students not found
missing_permids <-
  form_c_core %>% 
  # Reformat birthday
  mutate(dob = as.Date(substr(DOB, 1, 9), '%d%b%Y')) %>%
  dplyr::select(NAME, PERMID, CLASS_UUID, dob) %>%
  # Get class info
  left_join(form_b_core %>%
              dplyr::select(#GRADE, 
                            CLASS_UUID,
                            CLASS_NAME, 
                            SCHOOL_UUID),
            by = 'CLASS_UUID') %>%
  # # Get school info
  # left_join(form_a_core %>%
  #             dplyr::select(SCHOOL_NAME, SCHOOL_UUID),
  #           by = 'SCHOOL_UUID')
# missing_permids <- missing_permids %>%
  # Get additional student info
  left_join(students %>%
              dplyr::select(DOB, PERMID, combined_number, GRADE, NAME, 
                            SCHOOL_NAME) %>%
              mutate(dob = DOB),
              # dplyr::select(NAME, DOB, PERMID, student_number),
            by = c('NAME', 'dob', 'PERMID')) %>%
  # Get whether students are found
  left_join(form_b_2_core %>%
              dplyr::select(student_number, student_found) %>%
              # get the student number in the same format
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
                                              student_id)), 
            by = 'combined_number') %>%
    group_by(SCHOOL_NAME, GRADE, CLASS_NAME) %>%
    summarise(students = n(),
              missing_permids = length(which(PERMID == '6099-999-99')),
              found = length(which(student_found == '1')),
              not_found = length(which(student_found == '2')),
              unknown = length(which(is.na(student_found)))) %>%
  ungroup %>%
  mutate(percent_missing_permids = missing_permids / students * 100,
         percent_not_found = not_found / (not_found + found) * 100) %>%
  # arrange(desc(percent_missing_permids))
  arrange(SCHOOL_NAME, GRADE, CLASS_NAME)
# Write a csv
write_csv(missing_permids, paste0('checks_june_2016/_missing_perm_ids_and_found.csv'))



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
days_off_df <- data.frame(form_a_2_days_off_april)[0,]
for (i in 1:length(days_off)){
  x <- get('form_a_2_days_off_april')
  x$src <- days_off[i]
  names(x)[9] <- 'days_off'
  x$date <- keys[i]
  days_off_df <- rbind(days_off_df,
                       x)
}

# Non-lective days
too_few_non_lective <- 
  form_a_2_core %>%
  dplyr::select(uri,
                school) %>%
  # Get the days off for each month
  left_join(days_off_df,
            c('uri' = 'parent_auri')) %>%
  group_by(school, date) %>%
    summarise(days_off = length(unique(days_off)))
# NO ERRORS MOTHERFUCKER!!!!!!!!!!!

# 
# 
# 
# # Get a dataframe of all students for all days
# all_students <- 
#   form_c_core %>%
#   rename(student_id = uri)
# 
# # Any number after a letter indicates the phase
# # A number at the end of a dataframe name means 2016
# # In the first phase, form c was for students
# # And in this phase, form b is for students
# 
# all_students <- 
#   form_c_core %>%
#   # Get basic student info
#   dplyr::select(uri, 
#                 name, 
#                 permid, 
#                 dob,
#                 class_uuid,
#                 school_uuid) %>%
#   # Get some geographic info from form a
#   left_join(form_a_core %>%
#               dplyr::select(school_uuid,
#                             district, 
#                             school_number))
#   # Create a student number compatible across phase 1 and 2
#   mutate(district_number = pre_zero(district_number),
#          school_number = pre_zero(school_number),
#          student_number = pre_zero(student_number, n = 3)) %>%
#   # combined number
#   mutate(combined_number = paste0(district_number, '-',
#                                   school_number, '-',
#                                   student_number))  
# # Get basic school info
#   left_join(form_b_core %>%
#               dplyr::select(first_month_of_reference, 
#                             second_month_of_reference,
#                             third_month_of_reference,
#                             class_uuid),
#             by = 'class_uuid')
# 
# # DATES
# # In phase 2, all students were observed
# # only in Feb, Mar, Apr. Ie, no "months of reference"
# all_dates <- seq(as.Date('2015-03-01'),
#                  Sys.Date(),
#                  by = 1)
# 
# 
# 
