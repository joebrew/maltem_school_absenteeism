# First run "read_and_clean_all.R"

## PHASE 1 CHECKS
# School uniqueness (total of 25 schools)
# Turma uniqueness
# Months of reference between 4 and 11
# Name Uniqueness
# Perm ID Uniqueness
# Perm ID not found
# Student date of birth
# How many students are absent 0 days per month? (do that for each month)
# How many students are absent more than 18 days per month? (do that for each month)
# Perm ID not found %

## PHASE 2 CHECKS

# Minimum of 8 days of non lective days and maximum of 15 for each school
check <- 
  days_off_df %>%
  filter(!is.na(school)) %>%
  mutate(dow = weekdays(date)) %>%
  filter(dow != 'Saturday' & dow != 'Sunday') %>%
  group_by(school) %>%
  summarise(school_name = first(school_name),
            n = n()) %>%
  ungroup %>%
  arrange(desc(n))

check
days_off_df %>% 
  filter(days_off_df$school == 1)

# Ver, por cada escola, numero de estudantes identificados nessa escola para o curos 2016 e numero de estudantes nao encontrados

check <-
  students %>%
  # Get whether they were found or not
  left_join(form_b_2_core %>%
              dplyr::select(student_found,
                            combined_number),
            by = 'combined_number') %>%
  group_by(school_number) %>%
  summarise(school_name = first(SCHOOL_NAME),
            n = length(unique(combined_number)),
            found = length(which(student_found == '1')),
            not_found = length(which(student_found == '2')))

# Informação existente sobre o desempenho escolar para o curso 2015 (ie % de respostas completas, % de NA e % de Null) para as diferentes cadeiras e ao longo dos meses. 
# Lists per school and per class with number and % of students with not found in the new course 2016

# Don't know how to do this one !!!!!!!!!!!


# Informação existente sobre o desempenho escolar para o curso 2016 (ie % de respostas completas, % de NA e % de Null) para as diferentes cadeiras e ao longo dos meses. 
# Lists per school and class on the % of information answered on school achievement (for all subjects together)

# Don't know how to do this one !!!!!!!!!!!

# How many students are absent 0 days per month? (do that for each month)
# Lists per school, with the name, Perm_ID, DOB, grade, class, months of reference and absentism days produced

check <-
  students %>%
  dplyr::select(SCHOOL_NAME, combined_number) %>%
  left_join(absences %>%
              mutate(month = format(date, '%m')) %>%
              group_by(combined_number) %>%
              summarise(absences = length(which(absent)),
                        absences_feb = length(which(absent[month == '02'])),
                        absences_mar = length(which(absent[month == '03'])),
                        absences_apr = length(which(absent[month == '04']))) %>%
              ungroup,
            by = 'combined_number') %>%
  ungroup %>%
  mutate(absences = ifelse(is.na(absences), 0, absences),
         absences_feb = ifelse(is.na(absences_feb), 0, absences_feb),
         absences_mar = ifelse(is.na(absences_mar), 0, absences_mar),
         absences_apr = ifelse(is.na(absences_apr), 0, absences_apr))

check[check$absences == 0,]

# How many students are absent more than 18 days per month? (do that for each month)
# Lists per school, with the name, Perm_ID, DOB, grade, class, months of reference and absentism days produced
check[check$absences >= 18,]

# Are there matches between days reported as being non lective and days that are counted as absent? (there shouldn't be any, af a non lective day cannot be counted as a day absent from school)
# Lists per school, with the student number, name and day that has been reported as being absent although is a non-lective day (if it is too complicated we ignore this check…)

check <- 
  absences %>%
  # get school number
  left_join(students %>%
              dplyr::select(combined_number,
                            school_number) %>%
              rename(school = school_number),
            by = 'combined_number') %>%
  # bring in days off
  left_join(days_off_df %>%
              dplyr::select(school, date) %>%
              mutate(day_off = TRUE),
            by = c('school', 'date')) %>%
  filter(day_off)


# How many profesors x school? 
# Lists per school, with number of professors for which we have info

# !!!!!!!!! NEED LIST OF VISIT DATES FROM LAIA

# prof date of birth? (is any of them younger than 18? Or older than 70?)
# Lists per school, with name of profesor and date of birth which is inconsistent (younger than 18 or older than 70)

check <- 
  form_d_core %>%
  mutate(dob = as.Date(substr(dob, 1,10)))
hist(check$dob, breaks = 100)


# Informação existente sobre absentismo profesores (ie % de respostas completas, % de NA e % de Null) nas diferentes escolas
# Lists of tables with School, month of reference and % of completeness of the info about professors absenteeism

