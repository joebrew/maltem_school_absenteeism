MALTEM school absenteeism study

# NOTES FROM JUNE 2016
Form A2 Core (not using form A) = schools (has school code) = 1 row per school
  - Form A2 day off April (2015)
  - Form A2 days off April B (2016)
  - One for each month
Form B and B2 Core= students (has student code) = 1 row per student
  - Identificado = existed in 2015 also
      (those that are new don't exist)
  - Motivo de perda = reason why they were lost
  - Escola de destinho = if they went to a new school
  - APs = district-wide tests
  - There is also an evaluation for each class
  - Media trimestral for each class too (in order to standardize) -
      we have this for both years
  - Form B2 Core 2 is the same stuff but for 2016
  -
Form C and C2 = teachers = 1 row per teacher

# NOTES FROM MARCH 2016
Group 1 (transversal) and Group 3 (malaria)

Absenteeism rate/trend across time (by month)
-by district (Magude vs. Manhica)
-Associate each school with each of the areas in the prevalence cross-sectional from 2015
-And see if any areas have had bigger declines, etc.
-by school
-by class


Form A : identificacion of the school
-There should be as many rows as schools, but in fact there are repetitions (one row per class)
-The school_number variable is the actual school
-School_UUID is what appears in all the other spreadsheets
-Most interesting variables: GPS (different projections), DISTRICT, LATRINE, SCHOOL_CONDITIONS, SCHOOL_NAME (simplify and standardize), LUNCH, NUMBER OF CLASSES

Form B: identification of the classes/turmas (identified by SCHOOL_UUID, which can be linked to the first one)
-Most interesting variables: GRADE, REFERENCE MONTHS, TURMA (CLASS_NAME)
-Identification structure: school, grade/curs, turma/class

Form B Month 1, Month 2, Month 3, etc.
-For each month-turma, we want to see DIAS FESTIVOS
-Link by: Form B core$_URI == FORM B MONTH N$_PARENT_AURI
-Each _URI is 

Form C core
-One row per student
-Link by: Form C$CLASS_UUID ==  Form B core$CLASS_UUID
-Interesting rows:
-- ABSENCE_M_MONTH: 2 (no), 1 (1 or more absences that month)
-- Household 6099-999 (no data)
-- PermID
-- Gender: 1 (m) 2 (f)

For C days N(month)
-Each row is a person-absence
-Link by form C core$_URI == form C n month$parent_auri

Form D core
-One row is one teacher
-Same PARENT_AURI stuff as earlier