library(redcapAPI)
library(dplyr)     # group_by and filter
library(lubridate) # now
library(tidyr)
library(xlsx)
source("tokens.R")


# PART2: Export and bind data from all health facilities
my.fields <- c('record_id',
               'study_number',
               'child_dob',
               'his_interviewer_id',
               'his_date',
               'clin_date',
               'clin_interviewer_id',
               'int_date',
               'hh_date'
               )

my.epi.events <- c('epipenta1_v0_recru_arm_1',
               'epipenta2_v1_iptis_arm_1',
               'epipenta3_v2_iptis_arm_1',
               'epivita_v3_iptisp3_arm_1',
               'epimvr1_v4_iptisp4_arm_1',
               'epivita_v5_iptisp5_arm_1',
               'epimvr2_v6_iptisp6_arm_1'
               )
               
               
my.hh.events <- c('hhafter_1st_dose_o_arm_1',
                  'hhafter_2nd_dose_o_arm_1',
                  'hhafter_3rd_dose_o_arm_1',
                  'hhat_18th_month_of_arm_1'
                  )

data <- data.frame()
clinical_history <- data.frame()
vaccination_history <- data.frame()

for (hf in names(kRedcapTokens)) {
  print(paste("Extracting data from", hf))
  
  rcon <- redcapConnection(kRedcapAPIURL, kRedcapTokens[[hf]])
  hf.data <- exportRecords(
    rcon,
    factors            = F,
    labels             = F,
    fields             = my.fields,
    events             = c(my.epi.events, my.hh.events)
  )
  if ((substr(hf, 1, 2)  == "HF") & (substr(hf, 5, 5) == ".")) {
    hf <- substr(hf, 1, 4)
  } else {
    hf <-  hf
  }
  hf.data$hf <- hf
  #Study numbers
  
  filter <- !is.na(hf.data$study_number)
  study_numbers <- hf.data[filter, c("record_id", "study_number")]
  
  #Clinical History without Intervention
  filter <- !is.na(hf.data$clin_date) & is.na(hf.data$int_date)
  only_clin <- hf.data[filter, c("hf", "record_id", "redcap_event_name", "clin_date", 'clin_interviewer_id')]
  only_clin <- merge(only_clin, study_numbers)
  only_clin <- only_clin[, c("hf", "record_id",
                             "study_number",
                             "redcap_event_name",
                             "clin_date",
                             'clin_interviewer_id'
                             )
                         ]
  
  clinical_history <- rbind(clinical_history, only_clin)
  
  
  
  #Vaccination History
  filter <- !is.na(hf.data$his_date) & is.na(hf.data$int_date)
  only_vacc_his <- hf.data[filter, c("hf", "record_id", "redcap_event_name", "his_date", 'his_interviewer_id')]
  only_vacc_his <- merge(only_vacc_his, study_numbers)
  only_vacc_his <- only_vacc_his[, c("hf",
                                     "record_id",
                                     "study_number",
                                     "redcap_event_name",
                                     "his_date",
                                     'his_interviewer_id'
                                     )
                                 ]
  
  vaccination_history <- rbind(vaccination_history, only_vacc_his)
  
}  
  
  
  write.csv(clinical_history, "Clinical History.csv", row.names = F)
  
  write.csv(vaccination_history, "vaccination_history.csv", row.names = F)
  
