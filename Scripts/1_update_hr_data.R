

library(dplyr)
library(readr)

setwd("Z:/Training Reports/")


# extract new employees from Chris21 update  

hr_original <- read_csv("/Training Reports/Data/hr-list-010316.csv",
                        col_names = c("emp_id", "first.name", "surname", "status1", 
                                      "status2", "title", "prof.div","last.day", "division", "dept", 
                                      "grade1", "grade2", "cost.centre", "cc.name")) %>% 
    select(emp_id:surname, title, last.day:dept, cost.centre:cc.name) %>% 
    filter(is.na(last.day)) %>% 
    select(-last.day)


hr_update <- read_csv("/Training Reports/Data/hr-list-270416.csv",
                      col_names = c("emp_id", "title", "first.name", "surname", "status1", 
                                    "status2", "prof.div","last.day", "division", "dept", 
                                    "grade1", "grade2", "cost.centre", "cc.name")) %>% 
    select(emp_id:surname, last.day:dept, cost.centre:cc.name) %>% 
    filter(is.na(last.day)) %>% 
    distinct(emp_id) %>% 
    select(-last.day)
    
new_emp <- anti_join(hr_update, hr_original, "emp_id")  # <~~ new staff since last Chris21 extract

remove_emp <- anti_join(hr_original, hr_update, "emp_id") # <~~ staff who have left since last Chris21 extract

# read hr list used in original figures (contains small edits from JMc) and add new employees 

hr_list <- read_csv("Data/hr-list-jmc.csv") %>%
    select(-last.day.of.duty) %>% 
    bind_rows(new_emp) %>% 
    anti_join(remove_emp, "emp_id")
    

# write updated hr-list and hr-full to .csv

write_csv(hr_list, "Data/hr-list.csv", col_names = TRUE)

write_csv(hr_update, "Data/hr-list-full.csv", col_names = TRUE)
