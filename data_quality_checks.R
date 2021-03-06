# First run "read_and_clean_all.R"

# Absenteeism by month by class
ad_hoc <-
  attendance %>%
  mutate(year_month = format(date, '%Y %m')) %>%
  # Get class info
  left_join(form_b_2_core %>%
              dplyr::select(combined_number, 
                            school_number,
                            grade, 
                            class) %>%
              mutate(class_number = paste0('school:',
                                           school_number, '-',
                                           'grade:', grade, '-',
                                           'class:', class))) %>%
              # dplyr::select(combined_number, class_number)) %>%
  mutate(district = substr(combined_number, 1, 2)) %>%
  group_by(year_month, class_number) %>%
  summarise(students = length(unique(combined_number)),
            classes = length(unique(class_number)),
            absences = length(which(absent)),
            presences = length(which(!absent)),
            student_school_days = n()) %>%
  mutate(absenteeism_rate = absences / student_school_days * 100)

# Write csv for laia
write_csv(ad_hoc, '~/Desktop/monthly_absenteeism_by_class.csv')

# Can you send me please, for year and by month, the number of students for which we do have info and the number of classes? So I wanna know for how many classes we do have april 2015 as reference... and so. As well as total number of students for which we do have info for each month of year 2015.

for_laia <-
  attendance %>%
  mutate(year_month = format(date, '%Y %m')) %>%
  # Get class info
  left_join(form_b_2_core %>%
              dplyr::select(combined_number, 
                            school_number,
                            grade, 
                            class) %>%
              mutate(class_number = paste0('school:',
                                           school_number, '-',
                                           'grade:', grade, '-',
                                           'class:', class)) %>%
              dplyr::select(combined_number, class_number)) %>%
  mutate(district = substr(combined_number, 1, 2)) %>%
  group_by(year_month, district) %>%
  summarise(students = length(unique(combined_number)),
            classes = length(unique(class_number)),
            absences = length(which(absent)),
            presences = length(which(!absent)),
            student_school_days = n()) %>%
  mutate(absenteeism_rate = absences / student_school_days * 100)

# Get which schools have been collected so far
so_far <-
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
            not_found = length(which(student_found == '2'))) %>%
  filter(found + not_found > 10) %>%
  dplyr::select(school_number)
so_far <- as.character(unlist(so_far))

# Plots
x <- 
  attendance %>%
  # Keep only those schools which have been collected so far
  # filter(school_number %in% so_far) %>%
  mutate(year_month = as.Date(paste0(format(date, '%Y-%m'), '-01'))) %>%
  group_by(year_month) %>%
  summarise(rate = length(which(absent)) / n())
ggplot(data = x,
       aes(x = year_month, y = rate)) +
  geom_bar(stat = 'identity', alpha = 0.6)

# Do by school
x <- 
  attendance %>%
  # Keep only those schools which have been collected so far
  # filter(school_number %in% so_far) %>%
  mutate(year_month = as.Date(paste0(format(date, '%Y-%m'), '-01'))) %>%
  group_by(year_month, school_number) %>%
  summarise(rate = length(which(absent)) / n())
ggplot(data = x,
       aes(x = year_month, y = rate,
           group = school_number, color = school_number)) +
  geom_line(alpha = 0.6) +
  geom_point()

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
View(days_off_df %>% 
  filter(days_off_df$school == 1))

# See number of non-lective days per school per month
check <-
  days_off_df %>%
  mutate(month = format(date, '%m'),
         year = format(date, '%Y')) %>%
  group_by(school_name, year, month) %>% tally

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

# Produce list of non identified students
# only for those schools which have any identified students (ie, have been visited)
list_non_identified <- 
  students %>%
  # Get whether they were found or not
  left_join(form_b_2_core %>%
              dplyr::select(student_found,
                            combined_number),
            by = 'combined_number') %>%
  group_by(school_number) %>%
  mutate(visited = length(which(student_found == '1')) > 5) %>%
  ungroup %>%
  filter(visited) %>%
  filter(student_found == '2') %>%
  dplyr::select(district, SCHOOL_NAME, GRADE, CLASS_NAME, NAME)

# Number of students found / not found by inqueridor
check <- 
  form_b_2_core %>%
  group_by(cod_inq) %>%
  summarise(n = n(),
            found = length(which(student_found == '1')),
            not_found = length(which(student_found == '2'))) %>%
  mutate(ratio_found_not_found = found / not_found,
         p_found = found / n * 100)
barplot(t(as.matrix(check[,c('n', 'p_found')])), names.arg = check$cod_inq)

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
         absences_apr = ifelse(is.na(absences_apr), 0, absences_apr)) %>%
  left_join(  students %>%
                # Get whether they were found or not
                left_join(form_b_2_core %>%
                            dplyr::select(student_found,
                                          combined_number),
                          by = 'combined_number') %>%
                group_by(school_number) %>%
                mutate(visited = length(which(student_found == '1')) > 5) %>%
                ungroup %>%
                filter(visited)) %>%
  filter(student_found == '2') %>%
  filter(!is.na(visited))

# Look at 2015 and see if there are students who had no absences in any month
check2 <-
  df %>%
  group_by(month, student_id) %>%
  summarise(absences = length(which(absence))) %>%
  filter(absences == 0)

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
# WE NEED TO CORRECT THIS!

# How many profesors x school? 
# Lists per school, with number of professors for which we have info

# !!!!!!!!! NEED LIST OF VISIT DATES FROM LAIA

# prof date of birth? (is any of them younger than 18? Or older than 70?)
# Lists per school, with name of profesor and date of birth which is inconsistent (younger than 18 or older than 70)

# 2015
check <- 
  form_d_core %>%
  mutate(dob = as.Date(substr(dob, 1,10)))
hist(check$dob, breaks = 100)

# 2016
check <-
  form_c_2_core %>%
  mutate(dob = as.Date(substr(dob, 1,10)))
hist(check$dob, breaks = 100)


# Informação existente sobre absentismo profesores (ie % de respostas completas, % de NA e % de Null) nas diferentes escolas
# Lists of tables with School, month of reference and % of completeness of the info about professors absenteeism

