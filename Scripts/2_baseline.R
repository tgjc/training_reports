
# Notes: 
# 1. Manually rename staff number field in each spreadsheet to "emp_id"
# 2. When adding new spreadsheets, convert emp_id class to character to allow anti_join() to work
#    (MCRI has "H" prefix) 
# 3. if time allows, automate cleaning of column names and drop unused from hr-list.csv
# 4. add First_last and MCRI/External/Other to ouput file
# 5. fix output text at end of script 



library(dplyr)
library(readr)
library(magrittr)


setwd("Z:/Training Reports/")

# read in initial data tables - HR (Chris21), Payroll (DSU) and LMS (Learning Hero) 

data_c21 <- read_csv("Data/hr-list.csv",
                     col_types = "ccccccccc")

data_dsu <- read_csv(file = "Data/employee_labour_category.csv", 
                      col_types = "ccccccc")

data_lms <- read_csv(file = "Data/emr___face_to_face_sessions_by_user_report_20160513.csv", 
                     col_types = "ccccccccccccc",
                     col_names = c("course", "session", "start.date", "room", "duration", "cost.centre.name", 
                                   "department", "cost.centre", "emp_id", "name", "username", "position", "status"),
                     skip = 1) %>% 
    filter(status != "User Cancelled",
           course != "EMRPRAC Inpatient/APN Nursing Practice Lab",
           course != "EMR Medical (Doctor) Personalisation Lab",
           course != "EMR Oncology Quality Manager and Administration Staff",
           course != "EMR Ambulatory Doctor",
           course != "EMR Super Users for Refresher Ambulatory",
           course != "EMR Super Users for Refresher Inpatient Nurse",
           course != "EMR Reporting for Managers",
           course != "EMR Oncology Research Coordinator")


# Establish baseline of staff who require training. Add labour category field to baseline 

baseline <- select(data_dsu, emp_id, lab_cat_desc) %>% 
    left_join(data_c21, ., "emp_id")


# Filter out EMR staff 

emr_staff <- read_csv("Data/emr_team.csv", col_names = TRUE, col_types = "cc")

baseline %<>% anti_join(emr_staff, "emp_id") 


# Filter medical trainers and CTs

med_trainers <- read_csv(file = "Z:/Training Reports/data/medical-trainers.csv", 
                         col_types = "cc")

ct <- read_csv(file = "Z:/Training Reports/data/credentialed-trainers.csv", 
               col_types = "ccc")

baseline %<>% anti_join(med_trainers, by = "emp_id") %>% 
    anti_join(ct, by = "emp_id")


# Filter out DSU data

mat_leave <- data_dsu %>% filter(MATLEAVE_FLAG == "Y")  # “Y” if currently on maternity leave

long_term_leave <- data_dsu %>% filter(LONGTERMLEAVE_FLAG == "Y") # “Y” if on leave for the whole of the last three months

active_flag <- data_dsu %>% filter(ACTIVE_FLAG == "N") # “N” if not paid within the last three months

baseline %<>% anti_join(mat_leave, "emp_id") %>%   
    anti_join(long_term_leave, "emp_id") %>%    
    anti_join(active_flag, "emp_id")          

# Filter out responses: =================================================

# Filter out medical workforce 

mwf_raw <- read_csv("Responses/mwf.csv", col_types = "ccccccccccc") %>% 
    mutate(training = toupper(training)) 
    
mwf_P0868 <-filter(mwf_raw, cost.centre == "P0868")
mwf_n <- filter(mwf_raw, training == "N")
mwf_l <- filter(mwf_raw, training == "L")

mwf<- bind_rows(mwf_P0868, mwf_n, mwf_l)

baseline %<>%  anti_join(mwf, "emp_id")


# Filter out Education Institute

ed_inst <- read_csv("Responses/ed_inst.csv",
                    col_names = TRUE,
                    col_types = "ccccccccc")

baseline %<>% anti_join(ed_inst, "emp_id")


# Filter out lab services

lab_services <- filter(baseline, dept == "Laboratory Services")

baseline %<>% anti_join(lab_services, "emp_id") 


# Filter out VIHSP

VIHSP <- read_csv("responses/VIHSP.csv", col_types = "ccccccccccc") %>% 
    mutate(requires.training = toupper(requires.training)) %>% 
    filter(requires.training == "N")

baseline %<>% anti_join(VIHSP, "emp_id")


# Filter out ED responses

ed <- read_csv("Responses/ed.csv", col_types = "cccc") %>% 
    mutate(training = toupper(training)) %>% 
    filter(training == "N")

baseline %<>% anti_join(ed, "emp_id")


# Filter out Finance dept response

finance <- read_csv("Responses/finance.csv", col_types = "ccccccc") %>% 
    mutate(training = toupper(EMR.Training.Required)) %>% 
    filter(training == "NO")

baseline %<>% anti_join(finance, "emp_id")


# Filter Strategy and Improvement

strategy <- read_csv("Responses/strategy_improvement.csv", col_types = "ccccccccccc") %>% 
    mutate(training = toupper(training)) %>% 
    filter(training == "N")

baseline %<>% anti_join(strategy, "emp_id")

# Filter out LA collated responses

la <- read_csv("Responses/LA.csv", col_types = "ccccccccccc") %>% 
    mutate(training = toupper(training)) %>% 
    filter(training == "N")

la_2 <- read_csv("Responses/LA-080416.csv", col_types = "ccccccccccc") %>% 
    mutate(training = toupper(training)) %>% 
    filter(training == "N")

la_3 <- read_csv("Responses/LA-140416.csv", col_types = "ccccccccccc") %>% 
    mutate(training = toupper(training)) %>% 
    filter(training == "N")

baseline %<>% anti_join(la, "emp_id") %>% 
    anti_join(la_2, "emp_id") %>% 
    anti_join(la_3, "emp_id")


# Filter out JMc responses

jmc <- read_csv("Responses/JMc_080416.csv", col_types = "ccccccccccc") %>% 
    mutate(training = toupper(training)) %>% 
    filter(training == "N")

baseline %<>% anti_join(jmc, "emp_id") 


# Filter out NAW responses

naw <- read_csv("Responses/NAW.csv", col_types = "ccccccccccc") %>% 
    mutate(training = toupper(training)) %>% 
    filter(training == "N")

baseline %<>% anti_join(naw, "emp_id") 
   

# Filter out Simone Williams responses

sim_w <- read_csv("Responses/SW-150416.csv", col_types = "ccccccccccc") %>% 
    mutate(training = toupper(training)) %>% 
    filter(training == "N")

baseline %<>% anti_join(sim_w, "emp_id") 


# ===================================================================

# Create list of staff not in LMS, baseline and Chris21 for reporting 

not_in_lms <- anti_join(baseline, data_lms, "emp_id") %>% # staff who are in the baseline but not enrolled in a course
    mutate(name = paste(first.name, surname))
                                        # <-- add first last and y/n cols

not_in_baseline <- anti_join(data_lms, baseline, "emp_id") %>% # staff who are enrolled in a course but not in baseline 
    distinct(name)

not_in_c21 <- anti_join(data_lms, hr_update, "emp_id") %>% # staff who are registered for training but not in Chris21
    distinct(emp_id)
    

# write to .csv and print results 

stat_baseline <- nrow(baseline)+nrow(not_in_baseline)
stat_not_in_lms <- nrow(not_in_lms)
stat_percent_enrolled <- 100 * round(1- (stat_not_in_lms / stat_baseline), 2)


write_csv(baseline, "Z:/Training Reports/Output/baseline_output.csv", col_names = TRUE) # <-- does not include not_in_baseline

write_csv(not_in_lms, "Z:/Training Reports/Output/not_in_lms_output.csv", col_names = TRUE)

cat(paste("baseline for reporting =", stat_baseline), 
    paste("Number of staff not on LMS =", stat_not_in_lms),
    paste("Percent enrolled =", stat_percent_enrolled,"%"),
    sep = "\n")

