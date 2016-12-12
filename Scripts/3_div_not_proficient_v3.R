library(dplyr)
library(readr)
library(magrittr)

setwd("Y:/Training Reports/")

# read lms course completion report and create divisional Proficiency variables

data_complete <- read_csv("Data/course_completion_by_user_course_or_department_report.csv",
                     col_names = c("name", "emp_id","uname", "email", "course", "cost.centre.name", "cost.centre", 
                                   "status", "date", "grade", "position", "position.name", "division"),
                     col_types = "cccccccccnccc",
                     skip = 1) %>%
    filter(course != "EMRPRAC Inpatient/APN Nursing Practice Lab",
           course != "EMR Medical (Doctor) Personalisation Lab",
           course != "EMR Oncology Quality Manager and Administration Staff",
           course != "EMR Ambulatory Doctor",
           course != "EMR Super Users for Refresher Ambulatory",
           course != "EMR Super Users for Refresher Inpatient Nurse",
           course != "EMR Reporting for Managers",
           course != "EMR Oncology Research Coordinator") %>% 
    mutate(passed.one = ifelse(grade >= 80, 1, 0),
           in.lms = TRUE) %>% 
    group_by(name) %>% 
    mutate(passed.all = ifelse( sum(passed.one) == n(), TRUE, FALSE),
           passed.none = ifelse( sum(passed.one) == 0, "Not Proficient", "Proficient") ) %>%
    ungroup() %>% 
    group_by(course) %>% 
    mutate(all.zero = ifelse( sum(grade) == 0, TRUE, FALSE) ) %>% 
    ungroup()


# Filter out staff with status "user cancelled"

status_cancelled <- read_csv(file = "Data/emr___face_to_face_sessions_by_user_report.csv", 
                     col_types = "ccccccccccccc",
                     col_names = c("course", "session", "start.date", "room", "duration", "cost.centre.name", 
                                   "department", "cost.centre", "emp_id", "name", "username", "position", "status"),
                     skip = 1) %>% 
    filter(status == "User Cancelled",
           course != "EMRPRAC Inpatient/APN Nursing Practice Lab",
           course != "EMR Medical (Doctor) Personalisation Lab",
           course != "EMR Oncology Quality Manager and Administration Staff",
           course != "EMR Ambulatory Doctor",
           course != "EMR Super Users for Refresher Ambulatory",
           course != "EMR Super Users for Refresher Inpatient Nurse",
           course != "EMR Reporting for Managers",
           course != "EMR Oncology Research Coordinator")

data_complete %<>% anti_join(status_cancelled, c("emp_id", "course")) 


# Add department field from Gary's org tree data

dept_mapping <- read_csv("Data/dept-mapping.csv", col_names = TRUE, col_types = "cc") 
    
data_complete %<>% left_join(dept_mapping, "cost.centre")


# add proficiency variables to staff not in lms table then add to proficiency report

not_in_lms %<>% mutate(passed.one = 0,
                       passed.all = FALSE,
                       passed.none = "Not Proficient") 

data_complete %<>% bind_rows(not_in_lms) 
    

# filter out EMR staff, trainers, proficiency responses  

prof_resp <- read_csv("Responses/proficiency-responses.csv",
                      col_types = "cc")

updated_response_table <- read_csv("Responses/updated-response-table.csv",
                                   col_types = "ccccccc")

response_L <- filter(updated_response_table, action == "L")
response_N <- filter(updated_response_table, action == "N")

data_complete %<>% anti_join(emr_staff, "emp_id") %>% 
    anti_join(med_trainers, "emp_id") %>% 
    anti_join(ct, "emp_id") %>% 
    anti_join(prof_resp, "emp_id") %>% 
    anti_join(response_L, "emp_id") %>% 
    anti_join(response_N, "emp_id")


# Create divisional report - one unique and one with all courses

div_not_prof <- data_complete %>% 
    select(emp_id, name:email, department, division, cost.centre, cost.centre.name, course, status:all.zero)

div_not_prof_dist <- distinct(data_complete, emp_id) %>% 
    select(emp_id, name:email, department, division, cost.centre, cost.centre.name, position:all.zero)

# write to .csv and print results

write_csv(div_not_prof, "Y:/Training Reports/Testing/div_not_prof.csv", col_names = TRUE)
write_csv(div_not_prof_dist, "Y:/Training Reports/Testing/div_not_prof_distinct.csv", col_names = TRUE)
