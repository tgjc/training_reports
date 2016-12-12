library(dplyr)
library(readr)
library(magrittr)

setwd("Y:/Training Reports/")

# read lms course completion report and create divisional Proficiency variables

proficient_users <- read_csv("Data/course_completion_by_user_course_or_department_report_KON.csv",
                          col_names = c("name", "emp_id","uname", "email", "course", "cost.centre.name", "cost.centre", 
                                        "status", "date", "grade", "position", "position.name"),
                          col_types = "cccccccccncc",
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
    group_by(email) %>% 
    mutate(passed.all = ifelse( sum(passed.one) == n(), TRUE, FALSE),
           passed.none = ifelse( sum(passed.one) == 0, "Not Proficient", "Proficient") ) %>%
    ungroup() %>% 
    group_by(course) %>% 
    mutate(all.zero = ifelse( sum(grade) == 0, TRUE, FALSE) ) %>% 
    ungroup()

proficient_users %<>% distinct(email)

write_csv(proficient_users, "Output/proficient_users.csv")
