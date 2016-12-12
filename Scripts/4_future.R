library(dplyr)
library(readr)
library(magrittr)
library(lubridate)

setwd("Y:/Training Reports/")

# read in face to face sessions by user to get date of each course

future <- read_csv(file = "Data/emr___face_to_face_sessions_by_user_report.csv", 
                   col_types = "ccccccccccccc",
                   col_names = c("course", "session", "start.date", "room", "duration", "cost.centre.name", 
                                 "dept", "cost.centre", "emp_id", "name", "username", "position", "status"),
                   skip = 1) %>% 
    filter(status != "User Cancelled",
           course != "EMRPRAC Inpatient/APN Nursing Practice Lab",
           course != "EMR Medical (Doctor) Personalisation Lab") %>% 
    mutate(start.date = dmy(start.date),
           past = ifelse(start.date < ymd("xxx"), 1, 0)) %>% 
    group_by(emp_id, course) %>% 
    mutate(all.past = ifelse( sum(past) == n(), TRUE, FALSE)) %>% 
    filter(all.past == FALSE)


# read in completion report and filter to only staff who have attended training

not_prof_trained <- read_csv("Data/course_completion_by_user_course_or_department_report.csv",
                          col_names = c("name", "emp_id","uname", "email", "course", "dept", "cost.centre", "dept2", 
                                        "status", "date", "grade", "position", "position.name", "division" ),
                          skip = 1) %>%
    select(-(dept2)) %>% 
    filter(course != "EMRPRAC Inpatient/APN Nursing Practice Lab",
           course != "EMR Medical (Doctor) Personalisation Lab") %>% 
    anti_join(future, c("emp_id", "course")) %>% 
    mutate(passed.one = ifelse(grade >= .80, 1, 0)) %>% 
    group_by(name) %>% 
    mutate(passed.all = ifelse( sum(passed.one) == n(), TRUE, FALSE),
           passed.none = ifelse( sum(passed.one) == 0, "Not Proficient", "Proficient") ) %>%
    ungroup()



# filter out EMR staff, trainers, proficiency responses. Create distinct table

prof_resp <- read_csv("Responses/proficiency-responses.csv",
                      col_types = "cc")

not_prof_trained %<>% anti_join(emr_staff, "emp_id") %>% 
    anti_join(med_trainers, "emp_id") %>% 
    anti_join(ct, "emp_id") %>% 
    anti_join(prof_resp, "emp_id")

not_prof_trained_dist <- distinct(not_prof_trained, emp_id)

write_csv(not_prof_trained, "Output/not_prof_trained.csv")
write_csv(not_prof_trained_dist, "Output/not_prof_trained_dist.csv")




