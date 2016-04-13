# No scientific notation
options(scipen=999)

#####
# PACKAGES
#####
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
source('helpers.R')

if('cleaned_data.RData' %in% dir()){
  load('cleaned_data.RData')
} else {
  
  # Define function for reading in xls
  read_data <- function(file_name = 'Form A core.xlsx'){
    message(paste0('Reading and cleaning ', file_name))
    # Read in the data
    temp <- read_excel(paste0('data/', file_name))
    # Remove empty rows
    bad_rows <- rep(FALSE, nrow(temp))
    for (row in 1:nrow(temp)){
      empty <- c()
      for (column in 1:ncol(temp)){
        empty[column] <- is.na(temp[row, column])
      }
      if(all(empty)){
        bad_rows[row] <- TRUE
      }
    }
    temp$bad_row <- bad_rows
    temp <- temp[!temp$bad_row,]
    temp$bad_row <- NULL
    # Assign to an appropriate name
    object_name <- 
      gsub(' ', '_', gsub('.xlsx', '', tolower(file_name)))
    assign(object_name,
           temp,
           envir = .GlobalEnv)
    
  }
  
  # Loop through all the data, reading it in
  data_files <- dir('data')
  for (i in 1:length(data_files)){
    read_data(data_files[i])
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
                  `valid schools`,
                  GPS_LAT,
                  GPS_LNG,
                  LUNCH,
                  NUMBER_OF_CLASSES)
  
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
                  GENDER,
                  SCHOOL_NAME)#,
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
  
  # (first, get a the valid.schools for each main school)
  df <- df %>% 
    left_join(form_a_core %>%
                dplyr::select(SCHOOL_UUID,
                              `valid schools`), 
              by = 'SCHOOL_UUID') 
  # (next, remove the extra rows from form_a_core and bring 
  # in simplified/standardized information into df)
  
  # !!! what's going on with valid schools number 212?
  # unique(schools_simple$`valid schools`)
  
  schools_simple <- form_a_core[,c('SCHOOL_UUID', 'SCHOOL_NAME', 'valid schools')]
  # standardize names
  valid_schools <- unique(sort(schools_simple$`valid schools`))
  for (i in valid_schools){
    schools_simple$SCHOOL_NAME[schools_simple$`valid schools` == i] <- 
      schools_simple$SCHOOL_NAME[schools_simple$`valid schools` == i][1]
  }
  schools_simple$`valid schools` <- NULL
  
  # Put the standardized school name into form_a_core 
  form_a_core <- 
    form_a_core %>% 
    dplyr::select(-SCHOOL_NAME) %>%
    left_join(schools_simple, by = 'SCHOOL_UUID')
  # Bring all school data into df
  df <- 
    df %>%
    left_join(form_a_core %>%
                dplyr::select(-`valid schools`), by = 'SCHOOL_UUID')
  
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
  df$DOB <- as.Date(df$DOB)
  
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
  
  # Get the posto for each school only for Magude
  postos <- 
    df %>%
    group_by(SCHOOL_NAME) %>%
    summarise(DISTRICT = first(district)) %>%
    mutate(posto = NA,
           prevalencia = NA) %>%
    filter(DISTRICT == 'Magude')
  
  postos$posto[postos$SCHOOL_NAME == 'EPC DE BOBI'] <- 'Sede'
  postos$prevalencia[postos$SCHOOL_NAME == 'EPC DE BOBI']  <- 10
  
  postos$posto[postos$SCHOOL_NAME == 'EPC DE DUCO'] <- 'Sede'
  postos$prevalencia[postos$SCHOOL_NAME == 'EPC DE DUCO']  <- 10
  
  postos$posto[postos$SCHOOL_NAME == 'EPC DE MAGUIGUANA'] <- 'Sede'
  postos$prevalencia[postos$SCHOOL_NAME == 'EPC DE MAGUIGUANA']  <- 10
  
  postos$posto[postos$SCHOOL_NAME == 'EPC DE MAWANDLA'] <- 'Sede'
  postos$prevalencia[postos$SCHOOL_NAME == 'EPC DE MAWANDLA']  <- 10
  
  postos$posto[postos$SCHOOL_NAME == 'EPC DE MOINE'] <- 'Sede'
  postos$prevalencia[postos$SCHOOL_NAME == 'EPC DE MOINE']  <- 10
  
  postos$posto[postos$SCHOOL_NAME == 'EPC DE MOTAZE'] <- 'Motaze'
  postos$prevalencia[postos$SCHOOL_NAME == 'EPC DE MOTAZE']  <- 8
  
  postos$posto[postos$SCHOOL_NAME == 'EPC DE NWAMBYANA'] <- 'Motaze'
  postos$prevalencia[postos$SCHOOL_NAME == 'EPC DE NWAMBYANA']  <- 8
  
  postos$posto[postos$SCHOOL_NAME == 'EPC DE PANJANE'] <- 'Panjane'
  postos$prevalencia[postos$SCHOOL_NAME == 'EPC DE PANJANE']  <- 10
  
  postos$posto[postos$SCHOOL_NAME == 'EPC DE SIMBE'] <- 'Sede'
  postos$prevalencia[postos$SCHOOL_NAME == 'EPC DE SIMBE']  <- 10
  
  postos$posto[postos$SCHOOL_NAME == 'ESCOLA PRIMARIA CONPLENTA  DE MAGUDE'] <- 'Sede'
  postos$prevalencia[postos$SCHOOL_NAME == 'ESCOLA PRIMARIA CONPLENTA  DE MAGUDE']  <- 10
  
  postos$posto[postos$SCHOOL_NAME == 'ESCOLA PRIMARIA DO 1 GRAU DE NHIWANE'] <- 'Panjane'
  postos$prevalencia[postos$SCHOOL_NAME == 'ESCOLA PRIMARIA DO 1 GRAU DE NHIWANE']  <- 10
  
  postos$posto[postos$SCHOOL_NAME == 'ESCOLA PRIMARIA PRIMEIRO E SEGUNDO GRAU GRACA MACHEL'] <- 'Sede'
  postos$prevalencia[postos$SCHOOL_NAME == 'ESCOLA PRIMARIA PRIMEIRO E SEGUNDO GRAU GRACA MACHEL']  <- 10
  
  #####
  # GET SPATIAL DATA
  moz3 <- getData('GADM', country = 'MOZ', level = 3)
  maputo <- moz3[moz3@data$NAME_1 %in% c('Maputo', 'Maputo City'),]
  # Fortify maputo
  maputo_fortified <- fortify(maputo, region = 'NAME_3')
  # Fortify moz3
  moz3_fortified <- fortify(moz3, region = 'NAME_3')
  save.image('cleaned_data.RData')
}


# Write spreadsheets for laia (requested on 2016-04-13)
if(!dir.exists('~/Desktop/magude')){
  dir.create('~/Desktop/magude')
}
setwd('~/Desktop/magude')
library(readr)
write_csv(df, 'magude_student_absences_and_presences.csv')
