# Ad hoc exploration with Laia
# April 29, 2016

# Read in and clean up data before beginning ad hoc exploration
source('read_in.R')
library(printr)
library(ggthemes)
library(RColorBrewer)

# Eligible teacher days over time
by_day <- 
  teacher_df %>%
  group_by(date) %>%
  summarise(eligibles = length(unique(teacher_id)),
            absences = length(which(absence))) %>%
  mutate(rate = absences / eligibles * 100)
# gather
gathered <-
  gather(by_day, key, value, eligibles:rate)

# ggplot(data = gathered, # %>% filter(key == 'rate')
ggplot(data = gathered %>% filter(key == 'rate'),
       aes(x = date, y = value, group = key, color = key)) +
  geom_point() +
  geom_line() +
  ylim(0, 100) +
  theme_fivethirtyeight()

# Get teacher-month
temp <- 
  teacher_df %>%
  group_by(month, teacher_id) %>%
  summarise(eligibles = n(),
            absences = length(which(absence))) %>%
  mutate(rate = absences / eligibles * 100)

ggplot(data = temp,
       aes(x = month, y = rate, group = teacher_id, color = teacher_id)) +
  geom_line(size = 2, alpha = 0.6) +
  theme_economist_white() +
  theme(legend.position="none") +
  xlab('Month') +
  ylab('Absenteeism rate') +
  ggtitle('Teacher monthly absenteeism rate') +
  scale_colour_manual(values = colorRampPalette(brewer.pal(9, 'Spectral'))(length(unique(temp$teacher_id))))

# By distrit
by_district <- 
  teacher_df %>%
  group_by(date, DISTRICT) %>%
  summarise(eligibles = length(unique(teacher_id)),
            absences = length(which(absence))) %>%
  mutate(rate = absences / eligibles * 100) %>%
  filter(!is.na(DISTRICT)) %>%
  mutate(district = ifelse(DISTRICT == 1, 'Manhiça', 'Magude'))

ggplot(data = by_district,
       aes(x = date, y = rate, group = district, color = district)) +
  # geom_line(aes(size = eligibles), alpha = 0.5) +
  # geom_point(aes(size = eligibles)) +
  geom_smooth() +
  ylim(0, 100) +
  theme_fivethirtyeight()

# Explore teacher absenteeism over time
by_day <-
  teacher_df %>%
  group_by(date) %>%
  summarise(rate = length(which(absence)) / length(absence) * 100,
            weight = n())

ggplot(data = by_day,
       aes(x = date, y = rate)) +
  # geom_point() + 
  geom_line(aes(size = weight)) +
  ylim(0, 100) +
  theme_fivethirtyeight() 


# Number of student-days by month?
temp <- 
  df %>%
  group_by(month) %>%
  tally %>%
  mutate(p = n / sum(n) * 100)
temp

ggplot(data = temp,
       aes(x = month, y = p)) +
  geom_bar(stat = 'identity', alpha = 0.6)


# Identify schools with no absences in an entire month
temp <-
  df %>%
  group_by(SCHOOL_NAME, month) %>%
  summarise(absences = length(which(absence)),
            presences = length(which(!absence)),
            total = length(absence)) %>%
  mutate(rate = absences / total * 100) %>%
  mutate(variability = max(rate) - min(rate)) %>%
  ungroup %>%
  arrange(p)
temp

# Identify those students with abnormal absentee months
# Create a simplified students dataframe
students <-
  laia %>%
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
temp <-
  df %>%
  group_by(student_id, month) %>%
  summarise(rate = length(which(absence)) / length(absence)) %>%
  mutate(rate = rate * 100) %>%
  mutate(variability = max(rate) - min(rate)) %>%
  left_join(students) %>%
  ungroup %>%
  arrange(SCHOOL_NAME, NAME) %>%
  dplyr::select(-student_id, gender, GRADE, CLASS_NAME, DOB, PERMID) %>%
  ungroup %>%
  arrange(desc(variability))
temp
write_csv(temp, '~/Desktop/variability_by_student_month.csv')

# Examine results by class name
temp <- 
  df %>%
  group_by(SCHOOL_NAME, GRADE, CLASS_NAME, month) %>%
  summarise(rate = length(which(absence)) / length(absence) * 100,
            students = length(unique(student_id))) %>%
  mutate(variability = max(rate) - min(rate)) %>%
  ungroup %>%
  group_by(SCHOOL_NAME, GRADE, CLASS_NAME) %>% 
  arrange(desc(variability))
temp
write_csv(temp, '~/Desktop/variability_by_class_month.csv')
# Get average class absenteeism rate for each class 
temp <-
  temp %>%
  group_by(SCHOOL_NAME, GRADE, CLASS_NAME) %>%
  summarise(average_rate = mean(rate),
            students = mean(students)) %>%
  group_by(SCHOOL_NAME) %>%
  mutate(variability = max(average_rate) - min(average_rate)) %>%
  ungroup %>%
  arrange(desc(variability))
temp
write_csv(temp, '~/Desktop/variability_by_class.csv')

# Variability by student-month
temp <-
  df %>% 
  group_by(month, student_id) %>%
  summarise(rate = length(which(absence)) / length(absence) * 100)
temp

# Plot it
ggplot(data = temp,
       aes(x = rate)) +
  geom_density(fill = 'red', alpha = 0.5) +
  facet_wrap(~month)